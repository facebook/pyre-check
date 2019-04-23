# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
import subprocess
import unittest
import urllib.request
import zipfile
from typing import Iterable
from unittest.mock import call, patch

from .. import filesystem
from ..filesystem import (
    Glob,
    Sources,
    build_thrift_stubs,
    download_and_extract_zip_file,
    resolve_source_mapping,
)
from .test_common import identity_mapping


class FilesystemTest(unittest.TestCase):
    @patch.object(filesystem, "get_filesystem")
    def test_resolve_source_mapping(self, mock_filesystem):
        mock_list = mock_filesystem.return_value.list
        source_directory = "/project"
        output_directory = "/out"

        sources = resolve_source_mapping(
            source_directory,
            output_directory,
            Sources(files=identity_mapping(["a.py", "b.py"])),
        )
        self.assertDictEqual(
            sources, {"/project/a.py": "/out/a.py", "/project/b.py": "/out/b.py"}
        )

        mock_list.return_value = ["dir/c.py", "dir/d.py"]
        sources = resolve_source_mapping(
            source_directory, output_directory, Sources(globs=[Glob(["dir/*.py"], [])])
        )
        self.assertDictEqual(
            sources,
            {
                "/project/dir/c.py": "/out/dir/c.py",
                "/project/dir/d.py": "/out/dir/d.py",
            },
        )
        mock_list.assert_called_once_with("/project", ["dir/*.py"], exclude=[])
        mock_list.reset_mock()

        mock_list.return_value = ["dir/c.py", "dir/d.py", "other_dir/g.py"]
        sources = resolve_source_mapping(
            source_directory,
            output_directory,
            Sources(globs=[Glob(["dir/*.py", "other_dir/*.py"], [])]),
        )
        self.assertDictEqual(
            sources,
            {
                "/project/dir/c.py": "/out/dir/c.py",
                "/project/dir/d.py": "/out/dir/d.py",
                "/project/other_dir/g.py": "/out/other_dir/g.py",
            },
        )
        mock_list.assert_called_once_with(
            "/project", ["dir/*.py", "other_dir/*.py"], exclude=[]
        )
        mock_list.reset_mock()

        mock_list.return_value = ["dir/foo/e.py", "dir/foo/f.py", "other_dir/g.py"]
        sources = resolve_source_mapping(
            source_directory,
            output_directory,
            Sources(globs=[Glob(["dir/**/*.py", "other_dir/*.py"], ["dir/*.py"])]),
        )
        self.assertDictEqual(
            sources,
            {
                "/project/dir/foo/e.py": "/out/dir/foo/e.py",
                "/project/dir/foo/f.py": "/out/dir/foo/f.py",
                "/project/other_dir/g.py": "/out/other_dir/g.py",
            },
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
        sources = resolve_source_mapping(
            source_directory,
            output_directory,
            Sources(
                globs=[
                    Glob(["dir/**/*.py", "other_dir/*.py"], ["dir/*.py"]),
                    Glob(["dir/*.py"], []),
                ]
            ),
        )
        self.assertDictEqual(
            sources,
            {
                "/project/dir/c.py": "/out/dir/c.py",
                "/project/dir/d.py": "/out/dir/d.py",
                "/project/dir/foo/e.py": "/out/dir/foo/e.py",
                "/project/dir/foo/f.py": "/out/dir/foo/f.py",
                "/project/other_dir/g.py": "/out/other_dir/g.py",
            },
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
        sources = resolve_source_mapping(
            source_directory,
            output_directory,
            Sources(
                files=identity_mapping(["a.py", "b.py"]),
                globs=[Glob(["dir/*.py"], []), Glob(["other_dir/*.py"], [])],
            ),
        )
        self.assertDictEqual(
            sources,
            {
                "/project/a.py": "/out/a.py",
                "/project/b.py": "/out/b.py",
                "/project/dir/c.py": "/out/dir/c.py",
                "/project/dir/d.py": "/out/dir/d.py",
                "/project/other_dir/g.py": "/out/other_dir/g.py",
            },
        )
        mock_list.assert_has_calls(
            [
                call("/project", ["dir/*.py"], exclude=[]),
                call("/project", ["other_dir/*.py"], exclude=[]),
            ]
        )

        # Non-identity source mappings should work.
        sources = resolve_source_mapping(
            source_directory,
            output_directory,
            Sources(files={"a.py": "foo/bar/a.py", "b.py": "foo/bar/baz/b.py"}),
        )
        self.assertDictEqual(
            sources,
            {
                "/project/a.py": "/out/foo/bar/a.py",
                "/project/b.py": "/out/foo/bar/baz/b.py",
            },
        )

    def test_build_thrift_stubs(self):
        with patch.object(subprocess, "call") as subprocess_call:
            build_thrift_stubs(
                "/root",
                ["project/foo/bar.thrift", "project/foo/baz.thrift"],
                "/out",
                include_json_converters=False,
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

        # If include_json_converters is True, the :json option should be specified.abs
        with patch.object(subprocess, "call") as subprocess_call:
            build_thrift_stubs(
                "/root",
                ["project/foo/bar.thrift", "project/foo/baz.thrift"],
                "/out",
                include_json_converters=True,
            )
            subprocess_call.assert_has_calls(
                [
                    call(
                        [
                            "thrift",
                            "--gen",
                            "mstch_pyi:json",
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
                            "mstch_pyi:json",
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

    def test_download_and_extract_zip_file(self):
        with patch.object(urllib.request, "urlopen"), patch.object(
            io, "BytesIO"
        ), patch.object(zipfile, "ZipFile"):
            download_and_extract_zip_file("https://pypi.facebook.com/blahblah", "/out")

        self.assertRaises(
            ValueError, download_and_extract_zip_file, "https://pypi.org/blah", "/out"
        )
