# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import unittest
from typing import List, Optional

from .. import build_rules
from ...filesystem import Glob, Sources


def _get_expression(tree: ast.AST) -> ast.expr:
    assert isinstance(tree, ast.Module)
    assert len(tree.body) > 0
    expression = tree.body[0]
    assert isinstance(expression, ast.Expr)
    return expression.value


def _get_call(tree: ast.AST) -> ast.Call:
    call = _get_expression(tree)
    assert isinstance(call, ast.Call)
    return call


PYTHON_BINARY_TARGET_1 = """
python_binary(
    name = "binary_target_1",
    main_module = "some.project.main",
    deps = [":another_target", "//some/other" + ":target"],
    external_deps = ["foo"]
)
"""

PYTHON_BINARY_TARGET_2 = """
python_binary(
    name = "binary_target_2",
    deps = [],
    srcs = ["a.py"],
)
"""

PYTHON_BINARY_TARGET_3 = """
python_binary(
    name = 1234,
)
"""

PYTHON_LIBRARY_TARGET_1 = """
python_library(
    name = "library_target_1",
    srcs = ["a.py", "b.py"],
    deps = [":other_target"],
    external_deps = ["foo", "bar"],
)
"""

PYTHON_LIBRARY_TARGET_2 = """
python_library(
    name = "library_target_2",
    base_module = "a.b.c",
    srcs = glob(["folder/*.py", "other/**/*.py"], exclude=["other/exclude/**.py"]),
)
"""

PYTHON_LIBRARY_TARGET_3 = """
python_library(
    name = "library_target_3",
    srcs = ["a.py", "b.py"] + glob(["folder/*.py", "other/**/*.py"]),
)
"""

PYTHON_UNIT_TEST_TARGET = """
python_unittest(
    name = "test_target",
    srcs = glob(["tests/*.py"]),
    deps = [":library_target_1"],
)
"""

NON_PYTHON_TARGET = """
non_python(
    name = "non_python_target",
    field = 1234,
    other_field = "abc",
    deps = [":other_target"],
)
"""

THRIFT_LIBRARY_TARGET_1 = """
thrift_library(
    name = "thrift_target_1",
    languages = ["cpp", "py"],
    thrift_srcs = {
        "foo.thrift": [],
        "bar.thrift": [],
    },
)
"""

THRIFT_LIBRARY_TARGET_2 = """
thrift_library(
    name = "thrift_target_2",
    thrift_srcs = {
        "baz.thrift": []
    },
    py_base_module = "foo.bar",
    deps = [
        ":thrift_target_1-py"
    ],
    external_deps = [
        ("foo", None, "foo-lib")
    ],
    thrift_py_options = ["json", "other"]
)
"""

PYTHON_WHEEL_TARGET_1 = """
python_wheel(
    platform_urls = {
        "py3-platform007": "platform007_1.0_url",
        "py3-gcc-5-glibc-2.23": "gcc_1.0_url",
    },
    version = "1.0",
    deps = ["//some:target"],
    external_deps = [("foo", None, "foo-lib")]
)

python_wheel(
    platform_urls = {
        "py3-platform007": "platform007_2.0_url",
        "py3-gcc-5-glibc-2.23": "gcc_2.0_url",
    },
    version = "2.0",
    deps = ["//other:target"],
)

python_wheel_default(
    platform_versions = {
        "py3-platform007": "1.0",
        "py3-gcc-5-glibc-2.23": "2.0",
    },
)
"""

PYTHON_WHEEL_TARGET_2 = """
python_wheel(
    platform_urls = {
        "py3-gcc-5-glibc-2.23": "gcc_1.0_url",
    },
    version = "1.0",
    deps = ["//other:target"],
)

python_wheel(
    platform_urls = {
        "py3-gcc-5-glibc-2.23": "gcc_2.0_url",
    },
    version = "2.0",
)

python_wheel_default(
    platform_versions = {
        "py3-gcc-5-glibc-2.23": "2.0",
    },
)
"""


class BuildRuleTest(unittest.TestCase):
    def assert_sources_equal(
        self,
        sources: Sources,
        files: Optional[List[str]] = None,
        globs: Optional[List[Glob]] = None,
    ) -> None:
        self.assertListEqual(sources.files, files or [])
        self.assertListEqual(sources.globs, globs or [])

    def test_get_string_list(self):
        tree = ast.parse('["a", "b", "c"]')
        expression = _get_expression(tree)
        self.assertListEqual(build_rules._get_string_list(expression), ["a", "b", "c"])

        tree = ast.parse('"a,b,c"')
        expression = _get_expression(tree)
        self.assertListEqual(build_rules._get_string_list(expression), ["a", "b", "c"])

        tree = ast.parse('"a , b ,c"')
        expression = _get_expression(tree)
        self.assertListEqual(build_rules._get_string_list(expression), ["a", "b", "c"])

        tree = ast.parse('"a"')
        expression = _get_expression(tree)
        self.assertListEqual(build_rules._get_string_list(expression), ["a"])

    def test_get_external_dependencies(self):
        tree = ast.parse('["foo", "bar"]')
        expression = _get_expression(tree)
        self.assertListEqual(
            build_rules._get_external_dependencies(expression),
            [("foo", "foo-py"), ("bar", "bar-py")],
        )

        tree = ast.parse('[("foo", None, "foo-lib"), ("bar", None, "lib")]')
        expression = _get_expression(tree)
        self.assertListEqual(
            build_rules._get_external_dependencies(expression),
            [("foo", "foo-lib"), ("bar", "lib")],
        )

        tree = ast.parse('[("foo", None), ("bar",)]')
        expression = _get_expression(tree)
        self.assertListEqual(
            build_rules._get_external_dependencies(expression),
            [("foo", "foo-py"), ("bar", "bar-py")],
        )

        tree = ast.parse('["foo", ("bar", ">=2.7", "bar-lib")]')
        expression = _get_expression(tree)
        self.assertListEqual(
            build_rules._get_external_dependencies(expression),
            [("foo", "foo-py"), ("bar", "bar-lib")],
        )

    def test_python_binary(self):
        tree = ast.parse(PYTHON_BINARY_TARGET_1)
        call = _get_call(tree)
        target = build_rules.parse_python_binary(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:binary_target_1")
        self.assertEqual(target.name, "binary_target_1")
        self.assertListEqual(
            target.dependencies,
            ["//some/project:another_target", "//some/other:target"],
        )
        self.assertListEqual(target.external_dependencies, [("foo", "foo-py")])
        self.assert_sources_equal(target.sources, files=[], globs=[])
        self.assertIsNone(target.base_module)

        tree = ast.parse(PYTHON_BINARY_TARGET_2)
        call = _get_call(tree)
        target = build_rules.parse_python_binary(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:binary_target_2")
        self.assertEqual(target.name, "binary_target_2")
        self.assertListEqual(target.dependencies, [])
        self.assertListEqual(target.external_dependencies, [])
        self.assert_sources_equal(target.sources, files=["a.py"])
        self.assertIsNone(target.base_module)

        tree = ast.parse(PYTHON_BINARY_TARGET_3)
        call = _get_call(tree)
        self.assertRaises(
            ValueError, build_rules.parse_python_binary, call, "/ROOT", "some/project"
        )

    def test_python_library(self):
        tree = ast.parse(PYTHON_LIBRARY_TARGET_1)
        call = _get_call(tree)
        target = build_rules.parse_python_library(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:library_target_1")
        self.assertEqual(target.name, "library_target_1")
        self.assertIsNone(target.base_module)
        self.assert_sources_equal(target.sources, files=["a.py", "b.py"])
        self.assertListEqual(target.dependencies, ["//some/project:other_target"])
        self.assertListEqual(
            target.external_dependencies, [("foo", "foo-py"), ("bar", "bar-py")]
        )

        tree = ast.parse(PYTHON_LIBRARY_TARGET_2)
        call = _get_call(tree)
        target = build_rules.parse_python_library(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:library_target_2")
        self.assertEqual(target.name, "library_target_2")
        self.assertEqual(target.base_module, "a.b.c")
        self.assert_sources_equal(
            target.sources,
            globs=[Glob(["folder/*.py", "other/**/*.py"], ["other/exclude/**.py"])],
        )
        self.assertListEqual(target.dependencies, [])
        self.assertListEqual(target.external_dependencies, [])

        tree = ast.parse(PYTHON_LIBRARY_TARGET_3)
        call = _get_call(tree)
        target = build_rules.parse_python_library(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:library_target_3")
        self.assertEqual(target.name, "library_target_3")
        self.assertIsNone(target.base_module)
        self.assert_sources_equal(
            target.sources,
            files=["a.py", "b.py"],
            globs=[Glob(["folder/*.py", "other/**/*.py"], [])],
        )
        self.assertListEqual(target.dependencies, [])
        self.assertListEqual(target.external_dependencies, [])

    def test_python_unittest(self):
        tree = ast.parse(PYTHON_UNIT_TEST_TARGET)
        call = _get_call(tree)
        target = build_rules.parse_python_unittest(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:test_target")
        self.assertEqual(target.name, "test_target")
        self.assert_sources_equal(target.sources, globs=[Glob(["tests/*.py"], [])])
        self.assertListEqual(target.dependencies, ["//some/project:library_target_1"])
        self.assertListEqual(target.external_dependencies, [])
        self.assertIsNone(target.base_module)

    def test_non_python_target(self):
        tree = ast.parse(NON_PYTHON_TARGET)
        call = _get_call(tree)
        target = build_rules.non_python_target_parser("non_python")(
            call, "/ROOT", "some/project"
        )
        self.assertEqual(target.target, "//some/project:non_python_target")
        self.assertEqual(target.name, "non_python_target")
        self.assertListEqual(target.dependencies, [])
        self.assertListEqual(target.external_dependencies, [])
        self.assert_sources_equal(target.sources, files=[], globs=[])
        self.assertIsNone(target.base_module)

    def test_thrift_library(self):
        tree = ast.parse(THRIFT_LIBRARY_TARGET_1)
        call = _get_call(tree)
        target = build_rules.parse_thrift_library(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:thrift_target_1")
        self.assertEqual(target.name, "thrift_target_1")
        self.assertListEqual(target.dependencies, [])
        self.assertListEqual(target.external_dependencies, [])
        self.assertListEqual(
            sorted(target._thrift_sources), sorted(["foo.thrift", "bar.thrift"])
        )
        self.assertIsNone(target.base_module)
        self.assertFalse(target._include_json_converters)

        tree = ast.parse(THRIFT_LIBRARY_TARGET_2)
        call = _get_call(tree)
        target = build_rules.parse_thrift_library(call, "/ROOT", "some/project")
        self.assertEqual(target.target, "//some/project:thrift_target_2")
        self.assertEqual(target.name, "thrift_target_2")
        self.assertListEqual(target.dependencies, ["//some/project:thrift_target_1-py"])
        self.assertListEqual(target.external_dependencies, [("foo", "foo-lib")])
        self.assertListEqual(sorted(target._thrift_sources), sorted(["baz.thrift"]))
        self.assertEqual(target.base_module, "foo.bar")
        self.assertTrue(target._include_json_converters)

    def test_python_wheel(self):
        tree = ast.parse(PYTHON_WHEEL_TARGET_1)
        assert isinstance(tree, ast.Module)
        target = build_rules.parse_python_wheel(
            tree.body, "/ROOT", "some/project/wheel"
        )
        self.assertEqual(target.target, "//some/project/wheel:wheel")
        self.assertEqual(target.name, "wheel")
        self.assertListEqual(target.dependencies, ["//some:target"])
        self.assertListEqual(target.external_dependencies, [("foo", "foo-lib")])
        self.assertEqual(target._platform, "py3-platform007")
        self.assertEqual(target._version, "1.0")
        self.assertEqual(target._url, "platform007_1.0_url")

        tree = ast.parse(PYTHON_WHEEL_TARGET_2)
        assert isinstance(tree, ast.Module)
        target = build_rules.parse_python_wheel(
            tree.body, "/ROOT", "some/project/wheel"
        )
        self.assertEqual(target.target, "//some/project/wheel:wheel")
        self.assertEqual(target.name, "wheel")
        self.assertListEqual(target.dependencies, [])
        self.assertEqual(target._platform, "py3-gcc-5-glibc-2.23")
        self.assertEqual(target._version, "2.0")
        self.assertEqual(target._url, "gcc_2.0_url")
