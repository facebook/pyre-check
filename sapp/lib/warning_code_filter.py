from typing import Any, Dict, Iterable, Set, Tuple

from tools.sapp.base_parser import ParsedTuples, ParseType
from tools.sapp.pipeline import PipelineStep, Summary


class WarningCodeFilter(PipelineStep[ParsedTuples, ParsedTuples]):
    def __init__(self, codes_to_keep: Set[int]):
        self.codes_to_keep: Set[int] = codes_to_keep

    def _should_skip_issue(self, issue: Dict[str, Any]) -> bool:
        return issue["code"] not in self.codes_to_keep

    def _filter_by_code(self, entries: Iterable[Tuple[Any, Any, Any]]):
        for typ, key, e in entries:
            if typ == ParseType.ISSUE and self._should_skip_issue(e):
                continue
            yield (typ, key, e)

    def run(
        self, input: ParsedTuples, summary: Summary
    ) -> Tuple[ParsedTuples, Summary]:
        entries, previous_entries = input
        filtered_entries = self._filter_by_code(entries)

        filtered_previous_entries = None
        if previous_entries:
            filtered_previous_entries = self._filter_by_code(previous_entries)

        return (filtered_entries, filtered_previous_entries), summary
