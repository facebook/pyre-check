# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import tempfile
from contextlib import contextmanager
from dataclasses import asdict, dataclass
from pathlib import Path
from time import sleep
from typing import Any, Dict, Iterator, List, Mapping, Optional, overload, Tuple

from typing_extensions import Final, Literal

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
    def __init__(
        self,
        environment: Environment,
        specification: Specification,
        working_directory: Path,
    ) -> None:
        self._environment = environment
        self._specification = specification
        self._working_directory = working_directory
        invocation = self._environment.pyre_client_override or "pyre"
        binary_override = self._environment.pyre_binary_override
        if binary_override:
            invocation += f" --binary {binary_override}"
        typeshed_override = self._environment.typeshed_override
        if typeshed_override:
            invocation += f" --typeshed {typeshed_override}"
        self._pyre_invocation: str = invocation

    def update(self) -> List[Mapping[str, int]]:
        incremental_update_logs: List[Mapping[str, int]] = []
        new_state = self._specification.new_state
        updates = new_state.update_steps()
        for expected, update in enumerate(updates):
            update.update(self._environment, self._working_directory)
            while True:
                incremental_update_logs = self.run_profile("incremental_updates")
                if len(incremental_update_logs) > expected:
                    break
                else:
                    sleep(1)
        return incremental_update_logs

    def run_check(self) -> List[PyreError]:
        pyre_check_command = (
            f"{self._pyre_invocation} {self._specification.pyre_check_pyre_options} "
            "--output=json "
            "--noninteractive "
            f"check {self._specification.pyre_check_options}"
        ).rstrip()
        output = self._environment.checked_run(
            working_directory=self._working_directory,
            command=pyre_check_command,
            expected_return_codes=(0, 1),
        )
        if output.return_code == 0:
            return []
        else:
            return [PyreError.from_json(x) for x in json.loads(output.stdout)]

    def run_start(self) -> Mapping[str, int]:
        pyre_start_command = (
            # Use `pyre restart` instead of `pyre start` as the we want to:
            # - Kill existing servers
            # - Force the initial check to finish
            f"{self._pyre_invocation} {self._specification.pyre_start_pyre_options} "
            "--no-saved-state --enable-profiling --noninteractive "
            f"restart {self._specification.pyre_start_options}"
        ).rstrip()
        self._environment.checked_run(
            working_directory=self._working_directory,
            command=pyre_start_command,
            expected_return_codes=(0, 1),
        )
        cold_start_time_phases = self.run_profile("cold_start_phases")
        shared_memory_over_time = self.run_profile("total_shared_memory_size_over_time")
        _, cold_start_total_memory = shared_memory_over_time[0]
        with tempfile.NamedTemporaryFile() as temporary_file:
            self._environment.checked_run(
                working_directory=self._working_directory,
                command=(
                    f"{self._pyre_invocation} "
                    f"query save_server_state('{temporary_file.name}')"
                ),
            )
            saved_state_size = os.stat(temporary_file.name).st_size
        return {
            **cold_start_time_phases,
            "heap_size": cold_start_total_memory,
            "saved_state_size": saved_state_size,
        }

    def run_stop(self) -> None:
        self._environment.checked_run(
            working_directory=self._working_directory,
            command=(
                f"{self._pyre_invocation} {self._specification.pyre_stop_pyre_options} "
                f"stop {self._specification.pyre_stop_options}"
            ),
        )

    def run_incremental(self) -> List[PyreError]:
        pyre_incremental_command = (
            f"{self._pyre_invocation} "
            f"{self._specification.pyre_incremental_pyre_options} "
            "--output=json "
            "--noninteractive "
            f"incremental {self._specification.pyre_incremental_options}"
        ).rstrip()
        output = self._environment.checked_run(
            working_directory=self._working_directory,
            command=pyre_incremental_command,
            expected_return_codes=(0, 1),
        )
        if output.return_code == 0:
            return []
        else:
            return [PyreError.from_json(x) for x in json.loads(output.stdout)]

    @overload
    def run_profile(
        self, output_kind: Literal["incremental_updates"]
    ) -> List[Mapping[str, int]]:
        ...

    @overload  # noqa T20027161
    def run_profile(
        self, output_kind: Literal["cold_start_phases"]
    ) -> Mapping[str, int]:
        ...

    @overload  # noqa T20027161
    def run_profile(
        self, output_kind: Literal["total_shared_memory_size_over_time"]
    ) -> List[Tuple[str, int]]:
        ...

    def run_profile(self, output_kind: str) -> object:  # noqa T20027161
        pyre_profile_command = (
            f"{self._pyre_invocation} " f"profile --profile-output={output_kind}"
        ).rstrip()
        output = self._environment.checked_run(
            working_directory=self._working_directory, command=pyre_profile_command
        )
        return json.loads(output.stdout)


@contextmanager
def _create_pyre_runner(
    environment: Environment, specification: Specification
) -> Iterator["PyreRunner"]:
    with specification.old_state.activate_sandbox(environment) as sandbox_root:
        yield PyreRunner(environment, specification, sandbox_root)


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
class ProfileLogs:
    incremental_update_logs: List[Mapping[str, int]]
    cold_start_log: Mapping[str, int]

    def to_json(self) -> Dict[str, Any]:
        return {
            "incremental_update_logs": self.incremental_update_logs,
            "cold_start_log": self.cold_start_log,
        }

    def total_incremental_check_time(self) -> int:
        return sum(log["total"] for log in self.incremental_update_logs) // 1000

    def full_check_time(self) -> int:
        return sum(duration for _, duration in self.cold_start_log.items()) // 1000


@dataclass
class ResultComparison:
    discrepancy: Final[Optional[InconsistentOutput]]
    profile_logs: ProfileLogs

    def to_json(self, dont_show_discrepancy: bool = False) -> Dict[str, Any]:
        result: Dict[str, Any] = {
            "full_check_time": self.profile_logs.full_check_time(),
            "incremental_check_time": self.profile_logs.total_incremental_check_time(),
            "profile_logs": self.profile_logs.to_json(),
        }
        discrepancy = self.discrepancy
        if dont_show_discrepancy:
            return result
        else:
            result["discrepancy"] = (
                "none" if discrepancy is None else discrepancy.to_json()
            )
            return result


def compare_server_to_full(
    environment: Environment, specification: Specification
) -> ResultComparison:
    LOG.info("Preparing base repository state...")
    with _create_pyre_runner(environment, specification) as pyre_runner:
        LOG.debug("Starting pyre server...")
        cold_start_log = pyre_runner.run_start()
        LOG.debug("Preparing updated repository state...")
        incremental_update_logs = pyre_runner.update()

        LOG.info("Running pyre incremental check...")
        incremental_check_output = pyre_runner.run_incremental()
        LOG.debug("Stopping pyre server...")
        pyre_runner.run_stop()
        LOG.info(
            f"Pyre incremental check successfully finished (with {len(incremental_check_output)} errors)."  # noqa: line too long
        )

        LOG.info("Running pyre full check...")
        full_check_output = pyre_runner.run_check()
        LOG.info(
            f"Pyre full check successfully finished (with {len(full_check_output)} errors)."  # noqa: line too long
        )

    discrepancy = (
        None
        if incremental_check_output == full_check_output
        else InconsistentOutput(full_check_output, incremental_check_output)
    )
    profile_logs = ProfileLogs(incremental_update_logs, cold_start_log)
    return ResultComparison(discrepancy, profile_logs)


def benchmark_server(
    environment: Environment, specification: Specification
) -> ProfileLogs:
    LOG.info("Preparing base repository state...")
    with _create_pyre_runner(environment, specification) as pyre_runner:
        LOG.debug("Starting pyre server...")
        cold_start_log = pyre_runner.run_start()
        LOG.debug("Preparing updated repository state...")
        incremental_update_logs = pyre_runner.update()

        LOG.info("Running pyre incremental check...")
        incremental_check_output = pyre_runner.run_incremental()
        LOG.debug("Stopping pyre server...")
        pyre_runner.run_stop()
        LOG.info(
            f"Pyre incremental check successfully finished (with {len(incremental_check_output)} errors)."  # noqa: line too long
        )
    return ProfileLogs(incremental_update_logs, cold_start_log)
