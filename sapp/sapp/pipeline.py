import logging
from abc import ABCMeta, abstractmethod
from datetime import datetime, timedelta
from typing import Any, Dict, Generic, List, Optional, Tuple, TypeVar

from .analysis_output import AnalysisOutput


log = logging.getLogger("sapp")

T = TypeVar("T")
T_in = TypeVar("T_in")
T_out = TypeVar("T_out")

Summary = Dict[str, Any]  # blob of objects that gets passed through the pipeline
InputFiles = Tuple[AnalysisOutput, Optional[AnalysisOutput]]
DictEntries = Dict[str, Any]


def time_str(delta: timedelta):
    minutes, seconds = divmod(delta.total_seconds(), 60)
    seconds_string = f"{int(seconds)}s"
    if minutes > 0:
        return f"{int(minutes)}m {seconds_string}"
    return seconds_string


class PipelineStep(Generic[T_in, T_out], metaclass=ABCMeta):
    """Pipeline steps have an input type and an output type.
    T_in and T_out should both be child classes of PipelineData.
    """

    def __init__(self):
        pass

    @abstractmethod
    def run(self, input: T_in, summary: Summary) -> Tuple[T_out, Summary]:
        assert False, "Abstract method called!"
        pass


class Pipeline(object):
    def __init__(self, steps: List[PipelineStep[Any, Any]]):
        self.steps: List[PipelineStep[Any, Any]] = steps

    def run(
        self, first_input, summary: Optional[Summary] = None
    ) -> Tuple[Any, Summary]:
        if summary is None:
            summary = {}
        next_input = first_input
        timing = []
        for step in self.steps:
            start_time = datetime.now()
            next_input, summary = step.run(next_input, summary)
            timing.append((step.__class__.__name__, datetime.now() - start_time))
        log.info(
            "Step timing: %s",
            ", ".join([f"{name} took {time_str(delta)}" for name, delta in timing]),
        )
        return next_input, summary
