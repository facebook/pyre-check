# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
from typing import Dict, List, Mapping, Optional, Type  # noqa


class BuildTarget:
    def __init__(
        self, build_file_directory: str, name: str, dependencies: List[str]
    ) -> None:
        self.build_file_directory = build_file_directory
        self.name = name
        self.dependencies = dependencies

    def __str__(self) -> str:
        return "{}(name={})".format(self.rule_name(), self.name)

    def __repr__(self) -> str:
        return str(self)

    @property
    def target(self) -> str:
        return "//{}:{}".format(self.build_file_directory, self.name)

    def rule_name(self) -> str:
        raise NotImplementedError

    @staticmethod
    def parse(call: ast.Call, build_file_directory: str) -> "BuildTarget":
        raise NotImplementedError


class PythonBinary(BuildTarget):
    def rule_name(self) -> str:
        return "python_binary"

    @staticmethod
    def parse(call: ast.Call, build_file_directory: str) -> "PythonBinary":
        keywords = _get_keywords(call)
        name = _get_string(keywords["name"])
        dependencies = [
            _absolutize_target(dependency, build_file_directory)
            for dependency in _get_string_list(keywords.get("deps"))
        ]
        return PythonBinary(build_file_directory, name, dependencies)


def _get_string(tree: ast.AST) -> str:
    assert isinstance(tree, ast.Str)
    return tree.s


def _get_list(tree: ast.AST) -> List[ast.expr]:
    assert isinstance(tree, ast.List)
    return tree.elts


def _get_string_list(tree: Optional[ast.AST]) -> List[str]:
    if not tree:
        return []

    list_of_exprs = _get_list(tree)
    return [_get_string(expr) for expr in list_of_exprs]


def _get_keywords(tree: ast.AST) -> Dict[str, ast.expr]:
    assert isinstance(tree, ast.Call)
    result = {}
    for keyword in tree.keywords:
        if keyword.arg:
            result[keyword.arg] = keyword.value
    return result


def _absolutize_target(target: str, build_file_directory: str) -> str:
    if target.startswith(":"):
        return "//{}{}".format(build_file_directory, target)
    return target


SUPPORTED_RULES = {
    "python_binary": PythonBinary
}  # type: Mapping[str, Type[BuildTarget]]
