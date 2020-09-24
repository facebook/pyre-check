# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict, Set, Tuple

from . import DictEntries, PipelineStep, Summary


class WarningCodeFilter(PipelineStep[DictEntries, DictEntries]):
    # pyre-fixme[3]: Return type must be annotated.
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
