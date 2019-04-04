# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
from typing import Dict, List, Mapping, NamedTuple, Optional, Type  # noqa


class BuildTarget:
    # A minimal set of information that can be parsed from (almost) all targets.
    BaseInformation = NamedTuple(
        "BaseInformation",
        [("keywords", Dict[str, ast.expr]), ("name", str), ("dependencies", List[str])],
    )

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

    @staticmethod
    def parse_base_information(
        call: ast.Call, build_file_directory: str
    ) -> BaseInformation:
        keywords = _get_keywords(call)
        name = _get_string(keywords["name"])
        dependencies = _get_dependencies(build_file_directory, keywords.get("deps"))
        return BuildTarget.BaseInformation(keywords, name, dependencies)


def create_non_python_rule(rule_name: str) -> Type[BuildTarget]:
    """
        There are some rules we don't want to actually build, but we still want
        to parse them if they are used as dependencies.
    """

    class NonPythonTarget(BuildTarget):
        def __init__(
            self, build_file_directory: str, name: str, dependencies: List[str]
        ) -> None:
            super(NonPythonTarget, self).__init__(
                build_file_directory, name, dependencies
            )

        def rule_name(self) -> str:
            return rule_name

        @staticmethod
        def parse(call: ast.Call, build_file_directory: str) -> "NonPythonTarget":
            keywords = _get_keywords(call)
            name = _get_string(keywords["name"])
            return NonPythonTarget(build_file_directory, name, [])

    return NonPythonTarget


class PythonBinary(BuildTarget):
    def __init__(
        self,
        build_file_directory: str,
        name: str,
        dependencies: List[str],
        sources: Optional[List[str]] = None,
    ) -> None:
        super(PythonBinary, self).__init__(build_file_directory, name, dependencies)
        self.sources = sources or []  # type: List[str]

    def rule_name(self) -> str:
        return "python_binary"

    @staticmethod
    def parse(call: ast.Call, build_file_directory: str) -> "PythonBinary":
        base = BuildTarget.parse_base_information(call, build_file_directory)
        sources = _get_sources(base.keywords.get("srcs"))
        return PythonBinary(build_file_directory, base.name, base.dependencies, sources)


class PythonLibrary(BuildTarget):
    def __init__(
        self,
        build_file_directory: str,
        name: str,
        dependencies: List[str],
        sources: List[str],
        base_module: Optional[str] = None,
    ) -> None:
        super(PythonLibrary, self).__init__(build_file_directory, name, dependencies)
        self.sources = sources
        self.base_module = base_module

    def rule_name(self) -> str:
        return "python_library"

    @staticmethod
    def parse(call: ast.Call, build_file_directory: str) -> "PythonLibrary":
        base = BuildTarget.parse_base_information(call, build_file_directory)
        sources = _get_sources(base.keywords.get("srcs"))
        base_module = (
            _get_string(base.keywords["base_module"])
            if "base_module" in base.keywords
            else None
        )
        return PythonLibrary(
            build_file_directory, base.name, base.dependencies, sources, base_module
        )


class PythonUnitTest(BuildTarget):
    def __init__(
        self,
        build_file_directory: str,
        name: str,
        dependencies: List[str],
        sources: List[str],
    ) -> None:
        super(PythonUnitTest, self).__init__(build_file_directory, name, dependencies)
        self.sources = sources

    def rule_name(self) -> str:
        return "python_unittest"

    @staticmethod
    def parse(call: ast.Call, build_file_directory: str) -> "PythonUnitTest":
        base = BuildTarget.parse_base_information(call, build_file_directory)
        sources = _get_sources(base.keywords.get("srcs"))
        return PythonUnitTest(
            build_file_directory, base.name, base.dependencies, sources
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


def _get_sources(sources: Optional[ast.AST]) -> List[str]:
    if not sources:
        return []

    # Sources can be a list, a glob, or a concatenation of sources.
    def _get_sources(tree: ast.AST) -> List[str]:
        if isinstance(tree, ast.List):
            return _get_string_list(tree)
        elif isinstance(tree, ast.Call):
            function = tree.func
            assert isinstance(function, ast.Name) and function.id == "glob"
            return _get_string_list(tree.args[0])
        elif isinstance(tree, ast.BinOp):
            assert isinstance(tree.op, ast.Add)
            return _get_sources(tree.left) + _get_sources(tree.right)
        raise ValueError(
            "Tree of type {} unexpected for sources field.".format(type(tree))
        )

    return _get_sources(sources)


SUPPORTED_RULES = {
    "python_binary": PythonBinary,
    "python_library": PythonLibrary,
    "python_unittest": PythonUnitTest,
    "cpp_python_extension": create_non_python_rule("cpp_python_extension"),
}  # type: Mapping[str, Type[BuildTarget]]
