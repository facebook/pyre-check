# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import call, patch

from .. import filesystem
from ..build_target import PythonBinary, PythonLibrary, PythonUnitTest
from ..filesystem import Glob, Sources
from .test_common import base


class BuildTargetTest(unittest.TestCase):
    def test_build_python_binary(self):
        target = PythonBinary(
            "/ROOT",
            "project",
            base(
                "binary",
                sources=Sources(files=["a.py"], globs=[Glob(["foo/*.py"], [])]),
            ),
        )

        with patch.object(
            filesystem,
            "resolve_sources",
            return_value=["/ROOT/project/a.py", "/ROOT/project/foo/b.py"],
        ), patch.object(filesystem, "add_symbolic_link") as add_symbolic_link:
            target.build("/out")
            add_symbolic_link.assert_has_calls(
                [
                    call("/out/project/a.py", "/ROOT/project/a.py"),
                    call("/out/project/foo/b.py", "/ROOT/project/foo/b.py"),
                ]
            )

    def test_build_python_library(self):
        target = PythonLibrary(
            "/ROOT", "project", base("library", sources=Sources(files=["a.py", "b.py"]))
        )
        with patch.object(
            filesystem,
            "resolve_sources",
            return_value=["/ROOT/project/a.py", "/ROOT/project/b.py"],
        ), patch.object(filesystem, "add_symbolic_link") as add_symbolic_link:
            target.build("/out")
            add_symbolic_link.assert_has_calls(
                [
                    call("/out/project/a.py", "/ROOT/project/a.py"),
                    call("/out/project/b.py", "/ROOT/project/b.py"),
                ]
            )

        # base_module should be respected.
        target = PythonLibrary(
            "/ROOT",
            "project",
            base(
                "library",
                sources=Sources(files=["a.py", "b.py"]),
                base_module="foo.bar.baz",
            ),
        )
        with patch.object(
            filesystem,
            "resolve_sources",
            return_value=["/ROOT/project/a.py", "/ROOT/project/b.py"],
        ), patch.object(filesystem, "add_symbolic_link") as add_symbolic_link:
            target.build("/out")
            add_symbolic_link.assert_has_calls(
                [
                    call("/out/foo/bar/baz/a.py", "/ROOT/project/a.py"),
                    call("/out/foo/bar/baz/b.py", "/ROOT/project/b.py"),
                ]
            )

    def test_build_python_unittest(self):
        target = PythonUnitTest(
            "/ROOT",
            "project",
            base("test", sources=Sources(globs=[Glob(["tests/*.py"], [])])),
        )

        with patch.object(
            filesystem,
            "resolve_sources",
            return_value=[
                "/ROOT/project/tests/test_a.py",
                "/ROOT/project/tests/test_b.py",
            ],
        ), patch.object(filesystem, "add_symbolic_link") as add_symbolic_link:
            target.build("/out")
            add_symbolic_link.assert_has_calls(
                [
                    call(
                        "/out/project/tests/test_a.py", "/ROOT/project/tests/test_a.py"
                    ),
                    call(
                        "/out/project/tests/test_b.py", "/ROOT/project/tests/test_b.py"
                    ),
                ]
            )
