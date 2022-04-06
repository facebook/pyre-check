# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import traceback
from abc import ABC, ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Any, Dict, Iterable, List

from .environment import Environment
from .runner import (
    benchmark_server,
    compare_server_to_full,
    ProfileLogs,
    ResultComparison,
)
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
    _trace: str

    def __init__(self, input: Specification, trace: str) -> None:
        super().__init__(input)
        self._trace = trace

    def get_status(self) -> str:
        return "exception"

    def to_json(self, dont_show_discrepancy: bool) -> Dict[str, Any]:
        return {"status": self.get_status(), "trace": self._trace}

    def to_logger_sample(self) -> Sample:
        return Sample(
            normals={
                "status": self.get_status(),
                "input": json.dumps(self.input.to_json()),
                "exception": self._trace,
            },
            integers={},
        )


class FinishedRunnerResult(RunnerResult, metaclass=ABCMeta):
    _output: ResultComparison

    def __init__(self, input: Specification, output: ResultComparison) -> None:
        super().__init__(input)
        self._output = output

    def to_logger_sample(self) -> Sample:
        full_check_time = self._output.profile_logs.full_check_time()
        incremental_check_time = (
            self._output.profile_logs.total_incremental_check_time()
        )
        return Sample(
            normals={
                "status": self.get_status(),
                "input": json.dumps(self.input.to_json()),
            },
            integers={
                "full_check_time": full_check_time,
                "incremental_check_time": incremental_check_time,
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


class BenchmarkResult(RunnerResult):
    _profile_logs: ProfileLogs

    def __init__(self, input: Specification, profile_logs: ProfileLogs) -> None:
        super().__init__(input)
        self._profile_logs = profile_logs

    def get_status(self) -> str:
        return "benchmark"

    def to_json(self, dont_show_discrepancy: bool) -> Dict[str, Any]:
        return {
            "status": self.get_status(),
            "input": self.input.to_json(),
            "time": self._profile_logs.total_incremental_check_time(),
            "profile_logs": self._profile_logs.to_json(),
        }

    def to_logger_sample(self) -> Sample:
        incremental_check_time = self._profile_logs.total_incremental_check_time()
        return Sample(
            normals={
                "status": self.get_status(),
                "input": json.dumps(self.input.to_json()),
            },
            integers={"incremental_check_time": incremental_check_time},
        )

    def profile_logs(self) -> ProfileLogs:
        return self._profile_logs


def run_single_test(environment: Environment, input: Specification) -> RunnerResult:
    try:
        LOG.info(f"Running test on state '{input.old_state}' vs '{input.new_state}'")
        output = compare_server_to_full(environment, input)
        if output.discrepancy is None:
            result = PassedRunnerResult(input, output)
        else:
            result = FailedRunnerResult(input, output)
    except Exception:
        result = ExceptionalRunnerResult(input, traceback.format_exc())

    LOG.info(f"Test finished with status = {result.get_status()}")
    return result


def run_batch_test(
    environment: Environment, inputs: Iterable[Specification]
) -> List[RunnerResult]:
    return [run_single_test(environment, input) for input in inputs]


def run_single_benchmark(
    environment: Environment, input: Specification
) -> RunnerResult:
    try:
        LOG.info(
            f"Running benchmark on state '{input.old_state}' vs '{input.new_state}'"
        )
        output = benchmark_server(environment, input)
        result = BenchmarkResult(input, output)
    except Exception:
        result = ExceptionalRunnerResult(input, traceback.format_exc())

    LOG.info(f"Test finished with status = {result.get_status()}")
    return result


def run_batch_benchmark(
    environment: Environment, inputs: Iterable[Specification]
) -> List[RunnerResult]:
    return [run_single_benchmark(environment, input) for input in inputs]
