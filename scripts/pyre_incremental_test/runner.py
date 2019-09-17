# pyre-strict

import json
import logging
from dataclasses import asdict, dataclass
from time import ctime, time
from typing import Any, Dict, List, Optional, Tuple

from .environment import Environment
from .specification import Specification


LOG: logging.Logger = logging.getLogger(__name__)


class MalformedPyreOutputException(Exception):
    pass


@dataclass
class PyreError:
    line: int
    column: int
    path: str
    description: str

    @staticmethod
    def from_json(input_json: Dict[str, Any]) -> "PyreError":
        try:
            return PyreError(
                line=int(input_json["line"]),
                column=int(input_json["column"]),
                path=input_json["path"],
                description=input_json["description"],
            )
        except KeyError as key:
            raise MalformedPyreOutputException(
                f"Cannot interpret pyre output: missing key '{key}'"
            )


class PyreRunner:
    def __init__(self, environment: Environment, specification: Specification) -> None:
        self._environment = environment
        self._specification = specification

    def prepare_old_state(self) -> None:
        self._specification.old_state.prepare(self._environment)

    def prepare_new_state(self) -> None:
        self._specification.new_state.prepare(self._environment)

    def run_check(self) -> List[PyreError]:
        pyre_check_command = (
            f"pyre {self._specification.pyre_check_pyre_options} "
            "--output=json "
            "--noninteractive "
            f"check {self._specification.pyre_check_options}"
        ).rstrip()
        output = self._environment.checked_run(
            working_directory=self._specification.old_state.get_working_directory(),
            command=pyre_check_command,
            expected_return_codes=(0, 1),
        )
        if output.return_code == 0:
            return []
        else:
            return [PyreError.from_json(x) for x in json.loads(output.stdout)]

    def run_start(self) -> None:
        pyre_start_command = (
            f"pyre {self._specification.pyre_start_pyre_options} "
            "--no-saved-state "
            f"start {self._specification.pyre_start_options}"
        ).rstrip()
        self._environment.checked_run(
            working_directory=self._specification.old_state.get_working_directory(),
            command=pyre_start_command,
        )

    def run_stop(self) -> None:
        self._environment.checked_run(
            working_directory=self._specification.old_state.get_working_directory(),
            command="pyre stop",
        )

    def run_incremental(self) -> List[PyreError]:
        pyre_incremental_command = (
            f"pyre {self._specification.pyre_incremental_pyre_options} "
            "--output=json "
            "--noninteractive "
            f"incremental {self._specification.pyre_incremental_options}"
        ).rstrip()
        output = self._environment.checked_run(
            working_directory=self._specification.old_state.get_working_directory(),
            command=pyre_incremental_command,
            expected_return_codes=(0, 1),
        )
        if output.return_code == 0:
            return []
        else:
            return [PyreError.from_json(x) for x in json.loads(output.stdout)]


def _run_full_check(pyre_runner: PyreRunner) -> Tuple[List[PyreError], int]:
    LOG.info("Running pyre incremental check...")

    LOG.debug("Preparing updated repository state...")
    pyre_runner.prepare_new_state()
    start_time = time()
    LOG.debug("Starting full check...")
    result = pyre_runner.run_check()
    duration = int(time() - start_time)

    LOG.info(f"Pyre full check successfully finished (with {len(result)} errors).")
    return result, duration


def _run_incremental_check(pyre_runner: PyreRunner) -> Tuple[List[PyreError], int]:
    LOG.info("Running pyre incremental check...")

    LOG.debug("Preparing base repository state...")
    pyre_runner.prepare_old_state()
    LOG.debug("Starting pyre server...")
    pyre_runner.run_start()
    LOG.debug("Preparing updated repository state...")
    pyre_runner.prepare_new_state()

    start_time = time()
    LOG.debug("Starting incremental check starts...")
    result = pyre_runner.run_incremental()
    duration = int(time() - start_time)
    LOG.debug(f"Stopping pyre server...")
    pyre_runner.run_stop()

    LOG.info(
        f"Pyre incremental check successfully finished (with {len(result)} errors)."
    )
    return result, duration


@dataclass
class InconsistentOutput:
    full_check_output: List[PyreError]
    incremental_check_output: List[PyreError]

    def to_json(self) -> Dict[str, Any]:
        return {
            "full_check_output": [asdict(e) for e in self.full_check_output],
            "incremental_check_output": [
                asdict(e) for e in self.incremental_check_output
            ],
        }


@dataclass
class ResultComparison:
    discrepancy: Optional[InconsistentOutput]
    full_check_time: int
    incremental_check_time: int

    def to_json(self) -> Dict[str, Any]:
        discrepancy = self.discrepancy
        return {
            "discrepancy": "none" if discrepancy is None else discrepancy.to_json(),
            "full_check_time": self.full_check_time,
            "incremental_check_time": self.incremental_check_time,
        }


def compare_server_to_full(
    environment: Environment, specification: Specification
) -> ResultComparison:
    pyre_runner = PyreRunner(environment, specification)
    incremental_check_output, incremental_check_time = _run_incremental_check(
        pyre_runner
    )
    full_check_output, full_check_time = _run_full_check(pyre_runner)
    discrepancy = (
        None
        if incremental_check_output == full_check_output
        else InconsistentOutput(full_check_output, incremental_check_output)
    )
    return ResultComparison(discrepancy, full_check_time, incremental_check_time)
