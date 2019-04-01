from typing import Any, Dict, Iterable, Set, Tuple

from .pipeline import DictEntries, PipelineStep, Summary


class WarningCodeFilter(PipelineStep[DictEntries, DictEntries]):
    def __init__(self, codes_to_keep: Set[int]):
        self.codes_to_keep: Set[int] = codes_to_keep

    def _should_skip_issue(self, issue: Dict[str, Any]) -> bool:
        return issue["code"] not in self.codes_to_keep

    def run(self, input: DictEntries, summary: Summary) -> Tuple[DictEntries, Summary]:
        filtered_issues = []
        for issue in input["issues"]:
            if self._should_skip_issue(issue):
                continue
            filtered_issues.append(issue)

        input["issues"] = filtered_issues

        return input, summary
