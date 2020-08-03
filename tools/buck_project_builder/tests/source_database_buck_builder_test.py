# Copyright 2004-present Facebook.  All rights reserved.

import json
import shutil
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from testslide import TestCase

from .. import source_database_buck_builder


class SourceDatabaseBuckBuilderTest(TestCase):
    def setUp(self) -> None:
        super().setUp()
        self._query_arguments = [
            "query",
            "--json",
            'kind("python_binary|python_library|python_test", "%s") '
            "- attrfilter(labels, generated, %s) "
            "+ attrfilter(labels, unittest-library, %s) "
            "- attrfilter(labels, no_pyre, %s)",
            "//foo/bar/...",
            "//bar:baz",
        ]

    def test_get_buck_query_arguments(self) -> None:
        arguments = source_database_buck_builder._get_buck_query_arguments(
            specifications=["//foo/bar/...", "//bar:baz"], mode=None
        )
        self.assertEqual(arguments, self._query_arguments)

    def test_get_buck_query_arguments__with_mode(self) -> None:
        arguments = source_database_buck_builder._get_buck_query_arguments(
            specifications=["//foo/bar/...", "//bar:baz"], mode="foo"
        )
        self.assertEqual(
            arguments,
            [
                "query",
                "--json",
                "@mode/foo",
                'kind("python_binary|python_library|python_test", "%s") '
                "- attrfilter(labels, generated, %s) "
                "+ attrfilter(labels, unittest-library, %s) "
                "- attrfilter(labels, no_pyre, %s)",
                "//foo/bar/...",
                "//bar:baz",
            ],
        )

    def test_query_targets(self) -> None:
        query_output = {
            "//foo/bar/...": ["//foo/bar:baz", "//foo/bar:tests-library"],
            "//bar:baz": ["//bar:baz"],
        }
        self.mock_callable(
            source_database_buck_builder, "_buck", allow_private=True
        ).for_call(self._query_arguments).to_return_value(json.dumps(query_output))

        self.assertEqual(
            source_database_buck_builder._query_targets(
                ["//foo/bar/...", "//bar:baz"], mode=None
            ),
            ["//foo/bar:baz", "//foo/bar:tests-library", "//bar:baz"],
        )

    def test_buck_build_arguments(self) -> None:
        self.assertEqual(
            source_database_buck_builder._get_buck_build_arguments(
                ["//foo/bar:baz", "//foo/bar:tests-library"]
            ),
            [
                "build",
                "--show-full-json-output",
                "//foo/bar:baz#source-db",
                "//foo/bar:tests-library#source-db",
            ],
        )

    # pyre-fixme[56]: Argument `json` to decorator factory
    # `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(json, "loads")
    @patch.object(Path, "read_text")
    def test_load_source_databases(
        self, read_text: MagicMock, loads: MagicMock
    ) -> None:
        expected_database = {
            "sources": {"bar.py": "some/other/bar.py"},
            "dependencies": {"foo.py": "some/foo.py"},
        }
        loads.return_value = expected_database
        source_databases = source_database_buck_builder._load_source_databases(
            {"//foo:bar#source-db": "/some/bar#source-db/db.json"}
        )
        self.assertEqual(source_databases, {"//foo:bar#source-db": expected_database})

    def test_merge_source_databases(self) -> None:
        actual = source_database_buck_builder._merge_source_databases(
            {
                "hello": {
                    "sources": {
                        "foo.py": "foo.py",
                        "duplicate.py": "duplicate_in_hello.py",
                    },
                    "dependencies": {
                        "bar.pyi": "buck-out/bar.pyi",
                        "bar.cpp": "bar.cpp",
                    },
                },
                "foo": {
                    "sources": {},
                    "dependencies": {
                        "foo2.pyi": "buck-out/foo2.pyi",
                        "bar2.cpp": "bar2.cpp",
                        "duplicate.py": "duplicate_in_foo.py",
                        "__manifest__.py": "__manifest__.py",
                        "__test_modules__.py": "__test_modules__.py",
                        "__test_main__.py": "__test_main__.py",
                    },
                },
            }
        )
        self.assertEqual(
            actual,
            {
                "foo.py": "foo.py",
                "duplicate.py": "duplicate_in_foo.py",
                "bar.pyi": "buck-out/bar.pyi",
                "foo2.pyi": "buck-out/foo2.pyi",
            },
        )

    # pyre-fixme[56]: Argument `shutil` to decorator factory
    # `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(shutil, "rmtree")
    @patch.object(Path, "exists")
    @patch.object(Path, "mkdir")
    @patch.object(Path, "symlink_to")
    def test_build_link_tree(
        self,
        symlink_to: MagicMock,
        make_directory: MagicMock,
        exists: MagicMock,
        remove_tree: MagicMock,
    ) -> None:
        source_database_buck_builder._build_link_tree(
            {"foo.py": "foo.py", "bar/baz.pyi": "buck-out/bar.pyi"},
            Path("foo_directory"),
            Path("/root"),
        )
        self.assertEqual(
            make_directory.call_args_list,
            [
                call(parents=True),
                call(parents=True, exist_ok=True),
                call(parents=True, exist_ok=True),
            ],
        )
        self.assertEqual(
            symlink_to.call_args_list,
            [call(Path("/root/foo.py")), call(Path("/root/buck-out/bar.pyi"))],
        )

    @patch.object(source_database_buck_builder, "_build_link_tree")
    @patch.object(source_database_buck_builder, "_load_source_databases")
    @patch.object(source_database_buck_builder, "_build_targets")
    # pyre-fixme[56]: Argument
    #  `tools.pyre.tools.buck_project_builder.source_database_buck_builder` to
    #  decorator factory `unittest.mock.patch.object` could not be resolved in a global
    #  scope.
    @patch.object(source_database_buck_builder, "_query_targets")
    def test_build(
        self,
        query_targets: MagicMock,
        build_targets: MagicMock,
        load_source_databases: MagicMock,
        build_link_tree: MagicMock,
    ) -> None:
        load_source_databases.return_value = {
            "hello": {"sources": {"foo.py": "foo.py"}, "dependencies": {}},
            "foo": {"sources": {}, "dependencies": {"bar.pyi": "buck-out/bar.pyi"}},
        }
        source_database_buck_builder._build(
            ["//foo/bar/..."],
            output_directory=Path("output_directory"),
            buck_root=Path("buck_root"),
            mode=None,
        )
        query_targets.assert_called_once()
        build_targets.assert_called_once()
        build_link_tree.assert_called_once_with(
            {"foo.py": "foo.py", "bar.pyi": "buck-out/bar.pyi"},
            Path("output_directory"),
            Path("buck_root"),
        )
