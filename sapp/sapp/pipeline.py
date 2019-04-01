from abc import ABCMeta, abstractmethod
from typing import Any, Dict, Generic, Iterable, List, Optional, Tuple, TypeVar

from .analysis_output import AnalysisOutput


T = TypeVar("T")
T_in = TypeVar("T_in")
T_out = TypeVar("T_out")

Summary = Dict[str, Any]  # blob of objects that gets passed through the pipeline
InputFiles = Tuple[AnalysisOutput, Optional[AnalysisOutput]]
DictEntries = Dict[str, Any]


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
        for step in self.steps:
            next_input, summary = step.run(next_input, summary)
        return next_input, summary
