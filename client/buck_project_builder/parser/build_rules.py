# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import ast
from typing import Callable, Dict, List, Optional

from ..build_target import (
    BuildTarget,
    NonPythonTarget,
    PythonBinary,
    PythonLibrary,
    PythonUnitTest,
    ThriftLibrary,
)
from ..filesystem import Glob, Sources


def parse_base_information(
    call: ast.Call, build_file_directory: str
) -> BuildTarget.BaseInformation:
    keywords = _get_keywords(call)
    name = _get_string(keywords["name"])
    dependencies = _get_dependencies(build_file_directory, keywords.get("deps"))
    sources = _get_sources(keywords.get("srcs"))
    base_module = (
        _get_string(keywords["base_module"]) if "base_module" in keywords else None
    )
    return BuildTarget.BaseInformation(
        keywords, name, dependencies, sources, base_module
    )


def parse_python_binary(
    call: ast.Call, buck_root: str, build_file_directory: str
) -> PythonBinary:
    base = parse_base_information(call, build_file_directory)
    return PythonBinary(buck_root, build_file_directory, base)


def parse_python_library(
    call: ast.Call, buck_root: str, build_file_directory: str
) -> PythonLibrary:
    base = parse_base_information(call, build_file_directory)
    return PythonLibrary(buck_root, build_file_directory, base)


def parse_python_unittest(
    call: ast.Call, buck_root: str, build_file_directory: str
) -> PythonUnitTest:
    base = parse_base_information(call, build_file_directory)
    return PythonUnitTest(buck_root, build_file_directory, base)


def non_python_target_parser(
    rule_name: str
) -> Callable[[ast.Call, str, str], NonPythonTarget]:
    def parse_non_python_target(
        call: ast.Call, buck_root: str, build_file_directory: str
    ) -> NonPythonTarget:
        base = parse_base_information(call, build_file_directory)
        return NonPythonTarget(buck_root, build_file_directory, base, rule_name)

    return parse_non_python_target


def parse_thrift_library(
    call: ast.Call, buck_root: str, build_file_directory: str
) -> ThriftLibrary:
    keywords = _get_keywords(call)
    name = _get_string(keywords["name"])
    dependencies = _get_dependencies(build_file_directory, keywords.get("deps"))
    base_module = (
        _get_string(keywords["py_base_module"])
        if "py_base_module" in keywords
        else None
    )
    base_information = BuildTarget.BaseInformation(
        keywords, name, dependencies, Sources(), base_module
    )

    thrift_sources_dict = keywords["thrift_srcs"]
    assert isinstance(thrift_sources_dict, ast.Dict)
    thrift_sources = [_get_string(key) for key in thrift_sources_dict.keys]

    return ThriftLibrary(
        buck_root, build_file_directory, base_information, thrift_sources
    )


# AST helper methods
def _get_string(tree: ast.AST) -> str:
    if isinstance(tree, ast.Str):
        return tree.s
    elif isinstance(tree, ast.BinOp):
        assert isinstance(tree.op, ast.Add)
        return _get_string(tree.left) + _get_string(tree.right)
    raise ValueError(
        "Tree of type {} cannot be interpreted as a string.".format(type(tree))
    )


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


# Buck-specific helper methods
def _absolutize_target(target: str, build_file_directory: str) -> str:
    if target.startswith(":"):
        return "//{}{}".format(build_file_directory, target)
    return target


def _get_dependencies(
    build_file_directory: str, dependencies: Optional[ast.AST]
) -> List[str]:
    return [
        _absolutize_target(dependency, build_file_directory)
        for dependency in _get_string_list(dependencies)
    ]


def _get_sources(sources: Optional[ast.AST]) -> Sources:
    if not sources:
        return Sources()

    # Sources can be a list, a glob, or a concatenation of sources.
    def _get_sources(tree: ast.AST) -> Sources:
        if isinstance(tree, ast.List):
            return Sources(files=_get_string_list(tree))
        elif isinstance(tree, ast.Call):
            function = tree.func
            assert isinstance(function, ast.Name) and function.id == "glob"
            patterns = _get_string_list(tree.args[0])
            keywords = _get_keywords(tree)
            exclude = (
                _get_string_list(keywords["exclude"]) if "exclude" in keywords else []
            )

            return Sources(globs=[Glob(patterns, exclude)])
        elif isinstance(tree, ast.BinOp):
            assert isinstance(tree.op, ast.Add)
            left = _get_sources(tree.left)
            right = _get_sources(tree.right)
            return Sources(
                files=left.files + right.files, globs=left.globs + right.globs
            )
        raise ValueError(
            "Tree of type {} unexpected for sources field.".format(type(tree))
        )

    return _get_sources(sources)
