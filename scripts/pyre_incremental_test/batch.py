# pyre-strict

import json
import logging
from abc import ABC, ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Any, Dict, Iterable, List

from .environment import Environment
from .runner import ResultComparison, compare_server_to_full
from .specification import Specification


LOG: logging.Logger = logging.getLogger(__name__)


@dataclass
class Sample:
    integers: Dict[str, int]
    normals: Dict[str, str]


class RunnerResult(ABC):
    _input: Specification

    def __init__(self, input: Specification) -> None:
        self._input = input

    @property
    def input(self) -> Specification:
        return self._input

    @abstractmethod
    def get_status(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def to_json(self, dont_show_discrepancy: bool) -> Dict[str, Any]:
        raise NotImplementedError

    @abstractmethod
    def to_logger_sample(self) -> Sample:
        raise NotImplementedError


class ExceptionalRunnerResult(RunnerResult):
    def __init__(self, input: Specification) -> None:
        super().__init__(input)

    def get_status(self) -> str:
        return "exception"

    def to_json(self, dont_show_discrepancy: bool) -> Dict[str, Any]:
        return {"status": self.get_status()}

    def to_logger_sample(self) -> Sample:
        return Sample(
            normals={
                "status": self.get_status(),
                "input": json.dumps(self.input.to_json()),
            },
            integers={},
        )


class FinishedRunnerResult(RunnerResult, metaclass=ABCMeta):
    _output: ResultComparison

    def __init__(self, input: Specification, output: ResultComparison) -> None:
        super().__init__(input)
        self._output = output

    def to_logger_sample(self) -> Sample:
        return Sample(
            normals={
                "status": self.get_status(),
                "input": json.dumps(self.input.to_json()),
            },
            integers={
                "full_check_time": self._output.full_check_time,
                "incremental_check_time": self._output.incremental_check_time,
            },
        )


class PassedRunnerResult(FinishedRunnerResult):
    def get_status(self) -> str:
        return "pass"

    def to_json(self, dont_show_discrepancy: bool) -> Dict[str, Any]:
        # Don't bother include the input specification in the result if the test passes.
        return {
            "status": self.get_status(),
            "output": self._output.to_json(dont_show_discrepancy),
        }


class FailedRunnerResult(FinishedRunnerResult):
    def get_status(self) -> str:
        return "fail"

    def to_json(self, dont_show_discrepancy: bool) -> Dict[str, Any]:
        return {
            "status": self.get_status(),
            "input": self.input.to_json(),
            "output": self._output.to_json(dont_show_discrepancy),
        }


def run_single(environment: Environment, input: Specification) -> RunnerResult:
    try:
        LOG.info(f"Running test on state '{input.old_state}' vs '{input.new_state}'")
        output = compare_server_to_full(environment, input)
        if output.discrepancy is None:
            result = PassedRunnerResult(input, output)
        else:
            result = FailedRunnerResult(input, output)
    except Exception:
        result = ExceptionalRunnerResult(input)

    LOG.info(f"Test finished with status = {result.get_status()}")
    return result


def run_batch(
    environment: Environment, inputs: Iterable[Specification]
) -> List[RunnerResult]:
    return [run_single(environment, input) for input in inputs]
