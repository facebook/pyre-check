# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree

# pyre-strict

import json
from pathlib import Path
from typing import Dict, List, Sequence

import libcst as cst

from . import log


class TypeCollector(cst.CSTVisitor):
    def __init__(
        self,
        return_count: int = 0,
        annotated_return_count: int = 0,
        globals_count: int = 0,
        annotated_globals_count: int = 0,
        parameter_count: int = 0,
        annotated_parameter_count: int = 0,
    ) -> None:
        self.return_count = return_count
        self.annotated_return_count = annotated_return_count
        self.globals_count = globals_count
        self.annotated_globals_count = annotated_globals_count
        self.parameter_count = parameter_count
        self.annotated_parameter_count = annotated_parameter_count

    def build_json(self) -> Dict[str, int]:
        return {
            "return_count": self.return_count,
            "annotated_return_count": self.annotated_return_count,
            "globals_count": self.globals_count,
            "annotated_globals_count": self.annotated_globals_count,
            "parameter_count": self.parameter_count,
            "annotated_parameter_count": self.annotated_parameter_count,
        }

    def print_results(self) -> None:
        log.stdout.write(json.dumps(self.build_json()))

    def _check_parameter_annotations(self, parameters: Sequence[cst.Param]) -> None:
        for parameter in list(parameters):
            self.parameter_count += 1
            annotation = parameter.annotation
            if annotation is not None:
                self.annotated_parameter_count += 1

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        self.return_count += 1
        if node.returns is not None:
            self.annotated_return_count += 1

        self._check_parameter_annotations(node.params.default_params)
        self._check_parameter_annotations(node.params.params)

    def visit_Assign(self, node: cst.Assign) -> None:
        self.globals_count += 1

    def visit_AnnAssign(self, node: cst.AnnAssign) -> None:
        self.globals_count += 1
        self.annotated_globals_count += 1


def _get_paths(target_directory: Path) -> List[Path]:
    return [
        path
        for path in target_directory.glob("**/*.py")
        if not path.name.startswith("__") and not path.name.startswith(".")
    ]


def _run_on_directory(directory: Path, collector: TypeCollector) -> TypeCollector:
    files = _get_paths(directory)
    for file in files:
        cst.parse_module(file.read_text()).visit(collector)
    return collector


def _run_on_file(path: Path, collector: TypeCollector) -> TypeCollector:
    cst.parse_module(path.read_text()).visit(collector)
    return collector


def run(path: Path, collector: TypeCollector) -> TypeCollector:
    if path.is_dir():
        return _run_on_directory(path, collector)
    else:
        return _run_on_file(path, collector)
