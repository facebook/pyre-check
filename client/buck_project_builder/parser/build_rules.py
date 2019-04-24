# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import ast
import os
from typing import Callable, Dict, Iterable, List, Mapping, Optional, Tuple

from .. import platform
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
    external_dependencies = _get_external_dependencies(keywords.get("external_deps"))
    version_subdirectory = _get_version_subdirectory(keywords.get("version_subdirs"))

    return BuildTarget.BaseInformation(
        keywords=keywords,
        name=name,
        dependencies=dependencies,
        sources=sources,
        base_module=base_module,
        external_dependencies=external_dependencies,
        version_subdirectory=version_subdirectory,
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
    external_dependencies = _get_external_dependencies(keywords.get("external_deps"))

    base_information = BuildTarget.BaseInformation(
        keywords=keywords,
        name=name,
        dependencies=dependencies,
        sources=Sources(),
        base_module=base_module,
        external_dependencies=external_dependencies,
        version_subdirectory=None,
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

    base_information = BuildTarget.BaseInformation(
        keywords={},
        name=name,
        dependencies=[],
        sources=Sources(),
        base_module=None,
        external_dependencies=[],
        version_subdirectory=None,
    )

    python_wheel_default_calls = []
    python_wheel_calls = []
    for expression in expressions:
        if not isinstance(expression, ast.Expr):
            continue
        call = expression.value
        if not isinstance(call, ast.Call):
            continue
        named = call.func
        if not isinstance(named, ast.Name):
            continue
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
    platforms_to_wheel_version = _get_string_mapping(platform_versions)

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

        dependencies = _get_dependencies(
            build_file_directory, python_wheel_keywords.get("deps")
        )
        external_dependencies = _get_external_dependencies(
            python_wheel_keywords.get("external_deps")
        )

        wheel_versions_mapping[version] = PythonWheel.VersionedWheel(
            version=version,
            url_mapping=url_mapping,
            dependencies=dependencies,
            external_dependencies=external_dependencies,
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


def _get_string_mapping(tree: Optional[ast.AST]) -> Mapping[str, str]:
    if not tree:
        return {}

    assert isinstance(tree, ast.Dict)
    keys = [_get_string(key) for key in tree.keys]
    values = [_get_string(value) for value in tree.values]
    return dict(zip(keys, values))


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


def _get_external_dependency(tree: ast.AST) -> Tuple[str, str]:
    def default_library(base: str) -> str:
        # If no 'library' field specified, assume the base name with a -py suffix.
        return "{}-py".format(base)

    # First, try to parse single strings.
    try:
        base = _get_string(tree)
        return (base, default_library(base))
    except ValueError:
        pass

    # If the dependency is not a string, it should be a tuple.
    assert isinstance(tree, ast.Tuple)
    base = _get_string(tree.elts[0])
    if len(tree.elts) > 2:
        library = _get_string(tree.elts[2])
    else:
        library = default_library(base)
    return (base, library)


def _get_external_dependencies(
    external_dependencies: Optional[ast.AST]
) -> List[Tuple[str, str]]:
    if not external_dependencies:
        return []

    return [
        _get_external_dependency(element)
        for element in _get_list(external_dependencies)
    ]


def _get_sources(sources: Optional[ast.AST]) -> Sources:
    if not sources:
        return Sources()

    # Sources can be a list, a dict, a glob, or a concatenation of sources.
    def _get_sources(tree: ast.AST) -> Sources:
        if isinstance(tree, ast.List):
            file_list = _get_string_list(tree)
            files = dict(zip(file_list, file_list))  # Use an identity mapping.
            return Sources(files=files)
        elif isinstance(tree, ast.Dict):
            files = _get_string_mapping(tree)
            return Sources(files=files)
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
            files = {**left.files, **right.files}
            return Sources(files=files, globs=left.globs + right.globs)
        raise ValueError(
            "Tree of type {} unexpected for sources field.".format(type(tree))
        )

    return _get_sources(sources)


def _get_version_subdirectory(
    version_subdirectories: Optional[ast.AST]
) -> Optional[str]:
    if not version_subdirectories:
        return None

    desired_python_version = platform.get_python_version()

    assert isinstance(version_subdirectories, ast.List)
    for element in version_subdirectories.elts:
        # Each entry in the list is a 2-tuple.
        assert isinstance(element, ast.Tuple) and len(element.elts) == 2
        # The first element is a dictionary with a key mapping to a python
        # version string; the second element is a subdirectory string.
        dictionary = _get_string_mapping(element.elts[0])
        key = "//third-party-buck/{}/build/python:__project__".format(
            platform.get_platform()
        )
        python_version_string = dictionary[key]
        python_version = platform.parse_python_version(python_version_string)
        if (
            python_version[0] == desired_python_version[0]  # Major versions match.
            and python_version >= desired_python_version
        ):
            return _get_string(element.elts[1])
