# pyre-strict

import logging
from typing import Any, Dict, Iterable, List, Optional

from .environment import Environment
from .runner import InconsistentOutput, ResultComparison, compare_server_to_full
from .specification import Specification


LOG: logging.Logger = logging.getLogger(__name__)


class RunnerResult:
    _input: Specification
    _output: Optional[ResultComparison]

    def __init__(
        self, input: Specification, output: Optional[ResultComparison] = None
    ) -> None:
        self._input = input
        self._output = output

    @property
    def input(self) -> Specification:
        return self._input

    @property
    def output(self) -> Optional[ResultComparison]:
        return self._output

    def get_discrepancy(self) -> Optional[InconsistentOutput]:
        output = self.output
        if output is None:
            return None
        else:
            return output.discrepancy

    def get_status(self) -> str:
        output = self.output
        if output is None:
            return "exception"
        elif self.get_discrepancy() is None:
            return "pass"
        else:
            return "fail"

    def to_json(self) -> Dict[str, Any]:
        status = self.get_status()
        if status == "exception":
            return {"status": status, "input": self.input.to_json()}
        output = self.output
        assert output is not None
        if output.discrepancy is None:
            # Don't bother include the input specification in the result
            # if the test passes.
            return {"status": status, "output": output.to_json()}
        else:
            return {
                "status": status,
                "output": output.to_json(),
                "input": self.input.to_json(),
            }


def run_single(environment: Environment, input: Specification) -> RunnerResult:
    try:
        LOG.info(f"Running test on state '{input.old_state}' vs '{input.new_state}'")
        output = compare_server_to_full(environment, input)
    except Exception:
        output = None

    result = RunnerResult(input, output)
    LOG.info(f"Test finished with status = {result.get_status()}")
    return result


def run_batch(
    environment: Environment, inputs: Iterable[Specification]
) -> List[RunnerResult]:
    return [run_single(environment, input) for input in inputs]
