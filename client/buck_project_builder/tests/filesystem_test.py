# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
import unittest
from typing import Iterable
from unittest.mock import call, patch

from .. import filesystem
from ..filesystem import Glob, Sources, build_thrift_stubs, link_paths, resolve_sources


class FilesystemTest(unittest.TestCase):
    def assert_paths_match(self, first: Iterable[str], second: Iterable[str]) -> None:
        self.assertListEqual(sorted(first), sorted(second))

    @patch.object(filesystem, "get_filesystem")
    def test_resolve_sources(self, mock_filesystem):
        mock_list = mock_filesystem.return_value.list
        directory = "/project"

        sources = resolve_sources(directory, Sources(files=["a.py", "b.py"]))
        self.assert_paths_match(sources, ["/project/a.py", "/project/b.py"])

        # Duplicates are filtered out.
        sources = resolve_sources(directory, Sources(files=["a.py", "b.py", "a.py"]))
        self.assert_paths_match(sources, ["/project/a.py", "/project/b.py"])

        mock_list.return_value = ["dir/c.py", "dir/d.py"]
        sources = resolve_sources(directory, Sources(globs=[Glob(["dir/*.py"], [])]))
        self.assert_paths_match(sources, ["/project/dir/c.py", "/project/dir/d.py"])
        mock_list.assert_called_once_with("/project", ["dir/*.py"], exclude=[])
        mock_list.reset_mock()

        mock_list.return_value = [
            "/project/dir/c.py",
            "/project/dir/d.py",
            "/project/other_dir/g.py",
        ]
        sources = resolve_sources(
            directory, Sources(globs=[Glob(["dir/*.py", "other_dir/*.py"], [])])
        )
        self.assert_paths_match(
            sources,
            ["/project/dir/c.py", "/project/dir/d.py", "/project/other_dir/g.py"],
        )
        mock_list.assert_called_once_with(
            "/project", ["dir/*.py", "other_dir/*.py"], exclude=[]
        )
        mock_list.reset_mock()

        mock_list.return_value = ["dir/foo/e.py", "dir/foo/f.py", "other_dir/g.py"]
        sources = resolve_sources(
            directory,
            Sources(globs=[Glob(["dir/**/*.py", "other_dir/*.py"], ["dir/*.py"])]),
        )
        self.assert_paths_match(
            sources,
            [
                "/project/dir/foo/e.py",
                "/project/dir/foo/f.py",
                "/project/other_dir/g.py",
            ],
        )
        mock_list.assert_called_once_with(
            "/project", ["dir/**/*.py", "other_dir/*.py"], exclude=["dir/*.py"]
        )
        mock_list.reset_mock()

        # Excludes only apply to the glob they're used in.
        mock_list.side_effect = [
            ["dir/foo/e.py", "dir/foo/f.py", "other_dir/g.py"],
            ["dir/c.py", "dir/d.py"],
        ]
        sources = resolve_sources(
            directory,
            Sources(
                globs=[
                    Glob(["dir/**/*.py", "other_dir/*.py"], ["dir/*.py"]),
                    Glob(["dir/*.py"], []),
                ]
            ),
        )
        self.assert_paths_match(
            sources,
            [
                "/project/dir/c.py",
                "/project/dir/d.py",
                "/project/dir/foo/e.py",
                "/project/dir/foo/f.py",
                "/project/other_dir/g.py",
            ],
        )
        mock_list.assert_has_calls(
            [
                call(
                    "/project", ["dir/**/*.py", "other_dir/*.py"], exclude=["dir/*.py"]
                ),
                call("/project", ["dir/*.py"], exclude=[]),
            ]
        )
        mock_list.clear()

        # Globs and regular files work together.
        mock_list.side_effect = [["dir/c.py", "dir/d.py"], ["other_dir/g.py"]]
        sources = resolve_sources(
            directory,
            Sources(
                files=["a.py", "b.py"],
                globs=[Glob(["dir/*.py"], []), Glob(["other_dir/*.py"], [])],
            ),
        )
        self.assert_paths_match(
            sources,
            [
                "/project/a.py",
                "/project/b.py",
                "/project/dir/c.py",
                "/project/dir/d.py",
                "/project/other_dir/g.py",
            ],
        )
        mock_list.assert_has_calls(
            [
                call("/project", ["dir/*.py"], exclude=[]),
                call("/project", ["other_dir/*.py"], exclude=[]),
            ]
        )

    def test_link_paths(self):
        with patch.object(filesystem, "add_symbolic_link") as add_symbolic_link:
            link_paths(["/src/a.py", "/src/b/c.py"], "/src", "/output")
            add_symbolic_link.assert_has_calls(
                [
                    call("/output/a.py", "/src/a.py"),
                    call("/output/b/c.py", "/src/b/c.py"),
                ]
            )

        with patch.object(filesystem, "add_symbolic_link") as add_symbolic_link:
            link_paths(["/src/a.py", "/src/b/c.py"], "/src", "/src/.pyre")
            add_symbolic_link.assert_has_calls(
                [
                    call("/src/.pyre/a.py", "/src/a.py"),
                    call("/src/.pyre/b/c.py", "/src/b/c.py"),
                ]
            )

    def test_build_thrift_stubs(self):
        with patch.object(subprocess, "call") as subprocess_call:
            build_thrift_stubs(
                "/root", ["project/foo/bar.thrift", "project/foo/baz.thrift"], "/out"
            )
            subprocess_call.assert_has_calls(
                [
                    call(
                        [
                            "thrift",
                            "--gen",
                            "mstch_pyi",
                            "-I",
                            ".",
                            "--templates",
                            "thrift/compiler/generate/templates",
                            "-o",
                            "/out",
                            "project/foo/bar.thrift",
                        ],
                        stderr=subprocess.PIPE,
                        cwd="/root",
                    ),
                    call(
                        [
                            "thrift",
                            "--gen",
                            "mstch_pyi",
                            "-I",
                            ".",
                            "--templates",
                            "thrift/compiler/generate/templates",
                            "-o",
                            "/out",
                            "project/foo/baz.thrift",
                        ],
                        stderr=subprocess.PIPE,
                        cwd="/root",
                    ),
                ]
            )
