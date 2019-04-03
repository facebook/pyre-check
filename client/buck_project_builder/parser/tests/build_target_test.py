# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast
import unittest

from ..build_target import PythonBinary, PythonLibrary


def _get_call(tree: ast.AST) -> ast.Call:
    assert isinstance(tree, ast.Module)
    assert len(tree.body) > 0
    expression = tree.body[0]
    assert isinstance(expression, ast.Expr)
    call = expression.value
    assert isinstance(call, ast.Call)
    return call


PYTHON_BINARY_TARGET_1 = """
python_binary(
    name = "binary_target_1",
    main_module = "some.project.main",
    deps = [":another_target", "//some/other:target"]
)
"""

PYTHON_BINARY_TARGET_2 = """
python_binary(
    name = "binary_target_2",
    deps = []
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
)
"""

PYTHON_LIBRARY_TARGET_2 = """
python_library(
    name = "library_target_2",
    base_module = "a.b.c",
    srcs = glob(["folder/*.py", "other/**/*.py"]),
)
"""

PYTHON_LIBRARY_TARGET_3 = """
python_library(
    name = "library_target_3",
    srcs = ["a.py", "b.py"] + glob(["folder/*.py", "other/**/*.py"]),
)
"""


class BuildTargetTest(unittest.TestCase):
    def test_python_binary(self):
        tree = ast.parse(PYTHON_BINARY_TARGET_1)
        call = _get_call(tree)
        target = PythonBinary.parse(call, "some/project")
        self.assertEqual(target.target, "//some/project:binary_target_1")
        self.assertEqual(target.name, "binary_target_1")
        self.assertListEqual(
            target.dependencies,
            ["//some/project:another_target", "//some/other:target"],
        )

        tree = ast.parse(PYTHON_BINARY_TARGET_2)
        call = _get_call(tree)
        target = PythonBinary.parse(call, "some/project")
        self.assertEqual(target.target, "//some/project:binary_target_2")
        self.assertEqual(target.name, "binary_target_2")
        self.assertListEqual(target.dependencies, [])

        tree = ast.parse(PYTHON_BINARY_TARGET_3)
        call = _get_call(tree)
        self.assertRaises(AssertionError, PythonBinary.parse, call, "some/project")

    def test_python_library(self):
        tree = ast.parse(PYTHON_LIBRARY_TARGET_1)
        call = _get_call(tree)
        target = PythonLibrary.parse(call, "some/project")
        self.assertEqual(target.target, "//some/project:library_target_1")
        self.assertEqual(target.name, "library_target_1")
        self.assertIsNone(target.base_module)
        self.assertListEqual(target.sources, ["a.py", "b.py"])
        self.assertListEqual(target.dependencies, ["//some/project:other_target"])

        tree = ast.parse(PYTHON_LIBRARY_TARGET_2)
        call = _get_call(tree)
        target = PythonLibrary.parse(call, "some/project")
        self.assertEqual(target.target, "//some/project:library_target_2")
        self.assertEqual(target.name, "library_target_2")
        self.assertEqual(target.base_module, "a.b.c")
        self.assertListEqual(target.sources, ["folder/*.py", "other/**/*.py"])
        self.assertListEqual(target.dependencies, [])

        tree = ast.parse(PYTHON_LIBRARY_TARGET_3)
        call = _get_call(tree)
        target = PythonLibrary.parse(call, "some/project")
        self.assertEqual(target.target, "//some/project:library_target_3")
        self.assertEqual(target.name, "library_target_3")
        self.assertIsNone(target.base_module)
        self.assertListEqual(
            target.sources, ["a.py", "b.py", "folder/*.py", "other/**/*.py"]
        )
        self.assertListEqual(target.dependencies, [])
