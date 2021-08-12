# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses
from typing import List, Sequence

import libcst as cst
from libcst.metadata import CodeRange, PositionProvider


class CoverageCollector(cst.CSTVisitor):
    METADATA_DEPENDENCIES = (PositionProvider,)
    path: str = ""

    @dataclasses.dataclass(frozen=True)
    class FunctionInfo:
        name: cst.Name
        code_range: CodeRange

    def __init__(self) -> None:
        self.covered_functions: List[CoverageCollector.FunctionInfo] = []
        self.uncovered_functions: List[CoverageCollector.FunctionInfo] = []

    def _code_ranges_to_lines(self, code_ranges: Sequence[CodeRange]) -> List[int]:
        lines = set()
        for code_range in code_ranges:
            lines |= set(range(code_range.start.line, code_range.end.line + 1))
        return list(lines)

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        function_info = CoverageCollector.FunctionInfo(
            name=node.name,
            code_range=self.get_metadata(PositionProvider, node),
        )
        if node.returns is not None:
            self.covered_functions.append(function_info)
        else:
            self.uncovered_functions.append(function_info)

    @property
    def covered_lines(self) -> List[int]:
        covered_ranges = [info.code_range for info in self.covered_functions]
        return self._code_ranges_to_lines(covered_ranges)

    @property
    def uncovered_lines(self) -> List[int]:
        uncovered_ranges = [info.code_range for info in self.uncovered_functions]
        return self._code_ranges_to_lines(uncovered_ranges)
