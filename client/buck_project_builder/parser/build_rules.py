# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import ast
import os
from typing import Callable, Dict, Iterable, List, Optional

from ..build_target import (
    BuildTarget,
    NonPythonTarget,
    PythonBinary,
    PythonLibrary,
    PythonUnitTest,
    PythonWheel,
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

    if "thrift_py_options" in keywords:
        options = _get_string_list(keywords["thrift_py_options"])
        include_json_converters = "json" in options
    else:
        include_json_converters = False

    return ThriftLibrary(
        buck_root,
        build_file_directory,
        base_information,
        thrift_sources,
        include_json_converters,
    )


def parse_python_wheel(
    expressions: Iterable[ast.stmt], buck_root: str, build_file_directory: str
) -> PythonWheel:
    # Python wheels defined at a/b/c get a target name of //a/b/c:c.
    name = os.path.basename(build_file_directory)

    # TODO(T38892701): Support dependencies on wheels.
    # There are some occurrences in the codebase where wheels specify dependencies,
    # but it is not always the case that each version has the same dependencies.
    # We currently compute all dependencies at the beginning of a build, but these
    # dependencies are platform-specific, so we need to incorporate an additional
    # stage to determine platform-specific dependencies into the build process.
    base_information = BuildTarget.BaseInformation({}, name, [], Sources(), None)

    python_wheel_default_calls = []
    python_wheel_calls = []
    for expression in expressions:
        assert isinstance(expression, ast.Expr)
        call = expression.value
        assert isinstance(call, ast.Call)
        named = call.func
        assert isinstance(named, ast.Name)
        rule = named.id
        if rule == "python_wheel_default":
            python_wheel_default_calls.append(call)
        elif rule == "python_wheel":
            python_wheel_calls.append(call)

    # Parse python_wheel_default expression
    if not len(python_wheel_default_calls) == 1:
        raise ValueError(
            "Expected a single `python_wheel_default` expression for wheel {}".format(
                name
            )
        )
    keywords = _get_keywords(python_wheel_default_calls[0])
    platform_versions = keywords["platform_versions"]
    assert isinstance(platform_versions, ast.Dict)
    keys = [_get_string(key) for key in platform_versions.keys]
    values = [_get_string(value) for value in platform_versions.values]
    platforms_to_wheel_version = dict(zip(keys, values))

    # Parse each python_wheel expression
    wheel_versions_mapping = {}
    for python_wheel_call in python_wheel_calls:
        python_wheel_keywords = _get_keywords(python_wheel_call)
        assert (
            "version" in python_wheel_keywords
            and "platform_urls" in python_wheel_keywords
        )
        version = _get_string(python_wheel_keywords["version"])
        platform_urls = python_wheel_keywords["platform_urls"]
        assert isinstance(platform_urls, ast.Dict)
        keys = [_get_string(key) for key in platform_urls.keys]
        values = [_get_string(value) for value in platform_urls.values]
        url_mapping = dict(zip(keys, values))
        wheel_versions_mapping[version] = PythonWheel.VersionedWheel(
            version=version, url_mapping=url_mapping
        )

    return PythonWheel(
        buck_root,
        build_file_directory,
        base_information,
        platforms_to_wheel_version,
        wheel_versions_mapping,
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

    # First, try to parse the tree as a list.
    try:
        list_of_exprs = _get_list(tree)
        return [_get_string(expr) for expr in list_of_exprs]
    except (AssertionError, ValueError):
        pass

    # If that fails, try to parse it as a comma-separated string.
    try:
        comma_separated_values = _get_string(tree)
        return [value.strip() for value in comma_separated_values.split(",")]
    except (AssertionError, ValueError):
        pass

    raise ValueError(
        "Tree of type {} cannot be interpreted as a string list.".format(type(tree))
    )


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
