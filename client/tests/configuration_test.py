# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import dataclasses
import hashlib
import json
import os
import site
import sys
import unittest
from pathlib import Path
from typing import NamedTuple, Optional
from unittest.mock import MagicMock, PropertyMock, call, mock_open, patch

from .. import command_arguments, configuration
from ..configuration import (
    Configuration,
    InvalidConfiguration,
    PartialConfiguration,
    SimpleSearchPathElement,
    SitePackageSearchPathElement,
    SubdirectorySearchPathElement,
    _relativize_root,
    create_search_paths,
    merge_partial_configurations,
)
from ..find_directories import CONFIGURATION_FILE, LOCAL_CONFIGURATION_FILE


class PartialConfigurationTest(unittest.TestCase):
    def test_create_from_command_arguments(self) -> None:
        configuration = PartialConfiguration.from_command_arguments(
            command_arguments.CommandArguments(
                local_configuration=None,
                version=False,
                debug=False,
                sequential=False,
                strict=True,
                additional_checks=[],
                show_error_traces=False,
                output="output",
                enable_profiling=False,
                enable_memory_profiling=False,
                noninteractive=False,
                logging_sections=None,
                log_identifier="",
                logger="logger",
                formatter="formatter",
                targets=["//foo", "//bar"],
                use_buck_builder=False,
                use_buck_source_database=True,
                source_directories=["a", "b"],
                filter_directory=None,
                buck_mode=None,
                no_saved_state=False,
                search_path=["x", "y"],
                binary="binary",
                buck_builder_binary="buck_builder_binary",
                exclude=["excludes"],
                typeshed="typeshed",
                save_initial_state_to=None,
                load_initial_state_from=None,
                changed_files_path=None,
                saved_state_project=None,
                dot_pyre_directory=Path(".pyre"),
                features=None,
            )
        )
        self.assertEqual(configuration.binary, "binary")
        self.assertEqual(configuration.buck_builder_binary, "buck_builder_binary")
        self.assertEqual(configuration.dot_pyre_directory, Path(".pyre"))
        self.assertListEqual(list(configuration.excludes), ["excludes"])
        self.assertEqual(configuration.formatter, "formatter")
        self.assertEqual(configuration.logger, "logger")
        self.assertListEqual(
            list(configuration.search_path),
            [SimpleSearchPathElement("x"), SimpleSearchPathElement("y")],
        )
        source_directories = configuration.source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(list(source_directories), ["a", "b"])
        targets = configuration.targets
        self.assertIsNotNone(targets)
        self.assertListEqual(list(targets), ["//foo", "//bar"])
        self.assertEqual(configuration.typeshed, "typeshed")
        self.assertEqual(configuration.use_buck_builder, False)
        self.assertEqual(configuration.use_buck_source_database, True)

    def test_create_from_string_success(self) -> None:
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"autocomplete": True})
            ).autocomplete,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"binary": "foo"})).binary,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"buck_builder_binary": "foo"})
            ).buck_builder_binary,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"disabled": True})).disabled,
            True,
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"do_not_ignore_all_errors_in": ["foo", "bar"]})
                ).do_not_ignore_all_errors_in
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"dot_pyre_directory": "foo"})
            ).dot_pyre_directory,
            Path("foo"),
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"excludes": "foo"})
                ).excludes
            ),
            ["foo"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"excludes": ["foo", "bar"]})
                ).excludes
            ),
            ["foo", "bar"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"extensions": [".foo", ".bar"]})
                ).extensions
            ),
            [".foo", ".bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"formatter": "foo"})
            ).formatter,
            "foo",
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"ignore_all_errors": ["foo", "bar"]})
                ).ignore_all_errors
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"logger": "foo"})).logger,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"number_of_workers": 42})
            ).number_of_workers,
            42,
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"critical_files": ["foo", "bar"]})
                ).other_critical_files
            ),
            ["foo", "bar"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"search_path": "foo"})
                ).search_path
            ),
            [SimpleSearchPathElement("foo")],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps(
                        {"search_path": ["foo", {"root": "bar", "subdirectory": "baz"}]}
                    )
                ).search_path
            ),
            [
                SimpleSearchPathElement("foo"),
                SubdirectorySearchPathElement("bar", "baz"),
            ],
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"strict": True})).strict, True
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"taint_models_path": "foo"})
                ).taint_models_path
            ),
            ["foo"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"taint_models_path": ["foo", "bar"]})
                ).taint_models_path
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"typeshed": "foo"})).typeshed,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"use_buck_builder": True})
            ).use_buck_builder,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"use_buck_source_database": True})
            ).use_buck_source_database,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"version": "abc"})
            ).version_hash,
            "abc",
        )

        self.assertIsNone(PartialConfiguration.from_string("{}").source_directories)
        source_directories = PartialConfiguration.from_string(
            json.dumps({"source_directories": ["foo", "bar"]})
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(list(source_directories), ["foo", "bar"])

        self.assertIsNone(PartialConfiguration.from_string("{}").targets)
        targets = PartialConfiguration.from_string(
            json.dumps({"targets": ["//foo", "//bar"]})
        ).targets
        self.assertIsNotNone(targets)
        self.assertListEqual(list(targets), ["//foo", "//bar"])

        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"version": "abc"})).file_hash,
            None,
        )
        file_content = json.dumps({"version": "abc", "saved_state": "xyz"})
        self.assertEqual(
            PartialConfiguration.from_string(file_content).file_hash,
            hashlib.sha1(file_content.encode("utf-8")).hexdigest(),
        )

    def test_create_from_string_failure(self) -> None:
        def assert_raises(content: str) -> None:
            with self.assertRaises(InvalidConfiguration):
                PartialConfiguration.from_string(content)

        assert_raises("")
        assert_raises("{")
        assert_raises(json.dumps({"autocomplete": 42}))
        assert_raises(json.dumps({"binary": True}))
        assert_raises(json.dumps({"buck_builder_binary": ["."]}))
        assert_raises(json.dumps({"disabled": "False"}))
        assert_raises(json.dumps({"do_not_ignore_all_errors_in": "abc"}))
        assert_raises(json.dumps({"dot_pyre_directory": {}}))
        assert_raises(json.dumps({"excludes": 42}))
        assert_raises(json.dumps({"extensions": {"derp": 42}}))
        assert_raises(json.dumps({"formatter": 4.2}))
        assert_raises(json.dumps({"ignore_all_errors": [1, 2, 3]}))
        assert_raises(json.dumps({"ignore_infer": [False, "bc"]}))
        assert_raises(json.dumps({"logger": []}))
        assert_raises(json.dumps({"number_of_workers": "abc"}))
        assert_raises(json.dumps({"critical_files": "abc"}))
        assert_raises(json.dumps({"source_directories": "abc"}))
        assert_raises(json.dumps({"strict": 42}))
        assert_raises(json.dumps({"taint_models_path": True}))
        assert_raises(json.dumps({"taint_models_path": ["foo", 42]}))
        assert_raises(json.dumps({"targets": "abc"}))
        assert_raises(json.dumps({"typeshed": ["abc"]}))
        assert_raises(json.dumps({"use_buck_builder": "derp"}))
        assert_raises(json.dumps({"use_buck_source_database": 4.2}))
        assert_raises(json.dumps({"version": 123}))

    def test_merge(self) -> None:
        # Unsafe features like `getattr` has to be used in this test to reduce boilerplates.

        def create_configuration(name: str, value: object) -> PartialConfiguration:
            return dataclasses.replace(PartialConfiguration(), **{name: value})

        # Overwriting behaves correctly when:
        # - If both base config and overriding config are absent, result is None.
        # - If one of the config is presented, result is the that config.
        # - If both base config and overriding config are present, result is the
        #   overriding config.
        def assert_overwritten(attribute_name: str) -> None:
            # The actual value doesn't really matter. We only care about equalities.
            # This is obviously not type-safe but it does save a significant amount
            # of keystrokes.
            base_value = object()
            override_value = object()
            self.assertIsNone(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, None),
                        override=create_configuration(attribute_name, None),
                    ),
                    attribute_name,
                )
            )
            self.assertEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, base_value),
                        override=create_configuration(attribute_name, None),
                    ),
                    attribute_name,
                ),
                base_value,
            )
            self.assertEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, None),
                        override=create_configuration(attribute_name, override_value),
                    ),
                    attribute_name,
                ),
                override_value,
            )
            self.assertEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, base_value),
                        override=create_configuration(attribute_name, override_value),
                    ),
                    attribute_name,
                ),
                override_value,
            )

        def assert_appended(attribute_name: str) -> None:
            # The actual value doesn't really matter. We only care about equalities.
            # This is obviously not type-safe but it does save a significant amount
            # of keystrokes.
            base_value = object()
            override_value = object()
            self.assertListEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, [base_value]),
                        override=create_configuration(attribute_name, [override_value]),
                    ),
                    attribute_name,
                ),
                [base_value, override_value],
            )

        def assert_raise_when_overridden(attribute_name: str) -> None:
            # The actual value doesn't really matter. We only care about equalities.
            # This is obviously not type-safe but it does save a significant amount
            # of keystrokes.
            base_value = object()
            override_value = object()
            with self.assertRaises(InvalidConfiguration):
                merge_partial_configurations(
                    base=create_configuration(attribute_name, base_value),
                    override=create_configuration(attribute_name, override_value),
                )

        assert_overwritten("autocomplete")
        assert_overwritten("buck_builder_binary")
        assert_overwritten("disabled")
        assert_appended("do_not_ignore_all_errors_in")
        assert_overwritten("dot_pyre_directory")
        assert_appended("excludes")
        assert_appended("extensions")
        assert_overwritten("file_hash")
        assert_overwritten("formatter")
        assert_appended("ignore_all_errors")
        assert_appended("ignore_infer")
        assert_overwritten("logger")
        assert_overwritten("number_of_workers")
        assert_appended("other_critical_files")
        assert_appended("search_path")
        assert_raise_when_overridden("source_directories")
        assert_overwritten("strict")
        assert_appended("taint_models_path")
        assert_raise_when_overridden("targets")
        assert_overwritten("typeshed")
        assert_overwritten("use_buck_builder")
        assert_overwritten("use_buck_source_database")
        assert_overwritten("version_hash")


class MockCompletedProcess(NamedTuple):
    returncode: int
    stdout: str


class ConfigurationIntegrationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        # The Pyre environment variables change the outcome of tests.
        if "PYRE_CLIENT" in os.environ:
            del os.environ["PYRE_CLIENT"]
        if "PYRE_BINARY" in os.environ:
            del os.environ["PYRE_BINARY"]
        if "PYRE_TYPESHED" in os.environ:
            del os.environ["PYRE_TYPESHED"]

    @patch("os.path.abspath", side_effect=lambda path: path)
    @patch("os.path.isdir", return_value=True)
    @patch("os.path.exists")
    @patch(
        "os.path.expanduser", side_effect=lambda path: path.replace("~", "/home/user")
    )
    @patch("os.access", return_value=True)
    @patch("builtins.open")
    @patch("hashlib.sha1")
    @patch("json.loads")
    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "getenv", return_value=None)
    @patch.object(Configuration, "_validate")
    def test_init(
        self,
        configuration_validate,
        os_environ,
        json_load,
        sha1,
        builtins_open,
        access,
        _expanduser,
        exists,
        isdir,
        _abspath,
    ) -> None:
        sha1_mock = MagicMock()
        sha1_mock.hexdigest = lambda: "HASH"
        sha1.return_value = sha1_mock
        exists.return_value = True
        json_load.side_effect = [
            {
                "source_directories": ["a"],
                "logger": "/usr/logger",
                "ignore_all_errors": ["buck-out/dev/gen"],
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.source_directories, ["a"])
        self.assertEqual(configuration.targets, [])
        self.assertEqual(configuration.logger, "/usr/logger")
        self.assertEqual(configuration._ignore_all_errors, ["buck-out/dev/gen"])
        self.assertEqual(configuration.file_hash, None)

        # Local configurations
        json_load.side_effect = [
            {"source_directories": ["a"]},
            {"source_directories": ["a"]},
            {},
        ]
        with self.assertRaises(InvalidConfiguration):
            configuration = Configuration(
                "", local_root="local/path", dot_pyre_directory=Path("/.pyre")
            )
            self.assertEqual(configuration.source_directories, ["local/path/a"])

        json_load.side_effect = [{"source_directories": ["a"]}, {"version": "abc"}, {}]
        configuration = Configuration("local/path", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.source_directories, ["local/path/a"])
        self.assertEqual(configuration._ignore_all_errors, [])

        # Configuration fields
        json_load.side_effect = [{"targets": ["//a/b/c"], "disabled": 1}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.targets, ["//a/b/c"])
        self.assertEqual(configuration.source_directories, [])
        self.assertEqual(configuration.version_hash, "unversioned")
        self.assertEqual(configuration.logger, None)
        self.assertEqual(configuration.file_hash, None)
        self.assertTrue(configuration.disabled)

        json_load.side_effect = [{"typeshed": "TYPESHED/"}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "TYPESHED/")
        self.assertEqual(configuration.file_hash, None)

        with patch.object(
            site, "getsitepackages", return_value=["/mock/site0", "/mock/site1"]
        ), patch("os.path.exists", return_value=True):
            json_load.side_effect = [{"search_path": [{"site-package": "abc"}]}]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertIn(
                SitePackageSearchPathElement("/mock/site0", "abc"),
                configuration.get_existent_search_paths(),
            )
            self.assertIn(
                SitePackageSearchPathElement("/mock/site1", "abc"),
                configuration.get_existent_search_paths(),
            )

        json_load.side_effect = [
            {
                "search_path": ["additional/"],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "workers": 20,
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        with patch("os.path.exists", return_value=True):
            self.assertEqual(
                configuration.get_existent_search_paths(),
                [SimpleSearchPathElement("additional/")],
            )
        self.assertEqual(configuration.number_of_workers, 20)
        self.assertEqual(configuration.taint_models_path, [])
        self.assertEqual(configuration.file_hash, None)
        self.assertEqual(configuration.strict, False)

        json_load.side_effect = [
            {
                "search_path": ["additional/"],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "workers": 20,
                "strict": True,
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        with patch("os.path.exists", return_value=True):
            self.assertEqual(
                configuration.get_existent_search_paths(),
                [SimpleSearchPathElement("additional/")],
            )
        self.assertEqual(configuration.number_of_workers, 20)
        self.assertEqual(configuration.taint_models_path, [])
        self.assertEqual(configuration.file_hash, None)
        self.assertEqual(configuration.strict, True)

        json_load.side_effect = [
            {
                "search_path": [
                    "additional/",
                    {"root": "root/", "subdirectory": "subdirectory"},
                ],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "workers": 20,
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        with patch("os.path.exists", return_value=True):
            self.assertListEqual(
                [
                    element.path()
                    for element in configuration.get_existent_search_paths()
                ],
                ["additional/", "root/subdirectory"],
            )
        self.assertEqual(configuration.number_of_workers, 20)
        self.assertEqual(configuration.file_hash, None)
        self.assertEqual(configuration.taint_models_path, [])

        json_load.side_effect = [
            {
                "search_path": [
                    "//additional/",
                    {"root": "//root/", "subdirectory": "subdirectory"},
                ],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "workers": 20,
            },
            {},
        ]
        configuration = Configuration("project_root", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "project_root/TYPE/VERSION/SHED/")
        with patch("os.path.exists", return_value=True):
            self.assertListEqual(
                [
                    element.command_line_argument()
                    for element in configuration.get_existent_search_paths()
                ],
                ["project_root/additional/", "project_root/root/$subdirectory"],
            )
        self.assertEqual(configuration.number_of_workers, 20)
        self.assertEqual(configuration.file_hash, None)
        self.assertEqual(configuration.taint_models_path, [])

        json_load.side_effect = [
            {
                "search_path": [{"woot": "root/", "subdirectory": "subdirectory"}],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "workers": 20,
            },
            {},
        ]
        with self.assertRaises(InvalidConfiguration):
            Configuration("", dot_pyre_directory=Path("/.pyre"))

        json_load.side_effect = [
            {
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "taint_models_path": ".pyre/taint_models",
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(configuration.taint_models_path, [".pyre/taint_models"])

        json_load.side_effect = [
            {
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "taint_models_path": [".pyre/taint_models_1", ".pyre/taint_models_2"],
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(
            configuration.taint_models_path,
            [".pyre/taint_models_1", ".pyre/taint_models_2"],
        )

        def directory_side_effect(path: str) -> str:
            if path.endswith(CONFIGURATION_FILE):
                return "/root"
            elif path.endswith(LOCAL_CONFIGURATION_FILE):
                return "/root/local"
            else:
                return path

        with patch("os.path.dirname", side_effect=directory_side_effect):
            json_load.side_effect = [
                {"binary": "some/dir/pyre.bin", "typeshed": "some/typeshed"},
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration.binary, "/root/some/dir/pyre.bin")
            self.assertEqual(configuration.typeshed, "/root/some/typeshed")
            self.assertIsNone(configuration.buck_builder_binary)

            json_load.side_effect = [
                {"binary": "~/some/dir/pyre.bin", "typeshed": "~/some/typeshed"},
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration.binary, "/home/user/some/dir/pyre.bin")
            self.assertEqual(configuration.typeshed, "/home/user/some/typeshed")

            json_load.side_effect = [
                {
                    "binary": "some/%V/pyre.bin",
                    "typeshed": "some/%V/typeshed",
                    "version": "VERSION",
                },
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration.binary, "/root/some/VERSION/pyre.bin")
            self.assertEqual(configuration.typeshed, "/root/some/VERSION/typeshed")

            json_load.side_effect = [
                {
                    "binary": "~/some/%V/pyre.bin",
                    "typeshed": "~/some/%V/typeshed",
                    "version": "VERSION",
                },
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration.binary, "/home/user/some/VERSION/pyre.bin")
            self.assertEqual(configuration.typeshed, "/home/user/some/VERSION/typeshed")

            json_load.side_effect = [
                {"buck_builder_binary": "/some/dir/buck_builder"},
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(
                configuration.buck_builder_binary, "/some/dir/buck_builder"
            )

            json_load.side_effect = [
                {"buck_builder_binary": "some/dir/buck_builder"},
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(
                configuration.buck_builder_binary, "/root/some/dir/buck_builder"
            )

            json_load.side_effect = [
                {"ignore_all_errors": ["abc/def", "/abc/def", "~/abc/def"]},
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(
                configuration._ignore_all_errors,
                ["/root/abc/def", "/abc/def", "/home/user/abc/def"],
            )

            json_load.side_effect = [
                {
                    "taint_models_path": ".pyre/taint_models",
                    "version": "VERSION",
                    "typeshed": "/TYPE/%V/SHED/",
                },
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(
                configuration.taint_models_path, ["/root/.pyre/taint_models"]
            )
            json_load.side_effect = [
                {
                    "taint_models_path": ".pyre/taint_models",
                    "source_directories": ["."],
                },
                {"version": "VERSION", "typeshed": "/TYPE/%V/SHED/"},
            ]
            configuration = Configuration(
                project_root="/root",
                local_root="/root/local",
                dot_pyre_directory=Path("/.pyre"),
            )
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(
                configuration.taint_models_path, ["/root/local/.pyre/taint_models"]
            )
            json_load.side_effect = [
                {
                    "taint_models_path": ".pyre/taint_models",
                    "source_directories": ["."],
                },
                {
                    "version": "VERSION",
                    "taint_models_path": "global/taint_models",
                    "typeshed": "/TYPE/%V/SHED/",
                },
            ]
            configuration = Configuration(
                project_root="/root",
                local_root="/root/local",
                dot_pyre_directory=Path("/.pyre"),
            )
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(
                configuration.taint_models_path,
                ["/root/local/.pyre/taint_models", "/root/global/taint_models"],
            )

        json_load.side_effect = [
            {
                "version": "VERSION",
                "typeshed": "/TYPE/%V/SHED/",
                "saved_state": "some_name",
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
        self.assertEqual(configuration.file_hash, "HASH")

        json_load.side_effect = [
            {
                "search_path": [
                    "~/simple",
                    {"root": "~/simple", "subdirectory": "subdir"},
                ],
                "typeshed": "~/typeshed",
                "source_directories": ["a", "~/b"],
                "binary": "~/bin",
            },
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        with patch("os.path.exists", return_value=True):
            self.assertListEqual(
                [
                    element.command_line_argument()
                    for element in configuration.get_existent_search_paths()
                ],
                ["/home/user/simple", "/home/user/simple$subdir"],
            )
        self.assertEqual(configuration.typeshed, "/home/user/typeshed")
        self.assertEqual(configuration.source_directories, ["a", "/home/user/b"])
        self.assertEqual(configuration.binary, "/home/user/bin")

        # Test manual loading of the binary
        json_load.side_effect = [{}, {}]
        configuration = Configuration(
            project_root="", binary="some/file/path/", dot_pyre_directory=Path("/.pyre")
        )
        self.assertEqual(configuration.binary, "some/file/path/")

        # Test manual loading of typeshed directory.
        json_load.side_effect = [{}, {}]
        configuration = Configuration(
            project_root="",
            typeshed="some/directory/path/",
            dot_pyre_directory=Path("/.pyre"),
        )
        self.assertEqual(configuration.typeshed, "some/directory/path/")

        json_load.side_effect = [{"binary": "/binary"}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.binary, "/binary")

        json_load.side_effect = [{"version": "VERSION", "binary": "/%V/binary"}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.binary, "/VERSION/binary")

        # Test version override
        with patch.object(os, "getenv", return_value="VERSION_HASH"):
            json_load.side_effect = [{}, {}]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration.version_hash, "VERSION_HASH")

        with patch.object(os, "getenv", return_value="VERSION_HASH"):
            json_load.side_effect = [
                {"version": "NOT_THIS_VERSION", "typeshed": "/TYPE/%V/SHED/"},
                {},
            ]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION_HASH/SHED/")

        # Test buck builder fields
        json_load.side_effect = [{"use_buck_builder": True}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertTrue(configuration.use_buck_builder)
        json_load.side_effect = [{"use_buck_builder": False}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertFalse(configuration.use_buck_builder)
        json_load.side_effect = [{}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertFalse(configuration.use_buck_builder)

        # Test multiple definitions of the ignore_all_errors files.
        json_load.side_effect = [
            {"ignore_all_errors": ["buck-out/dev/gen"]},
            {"ignore_all_errors": ["buck-out/dev/gen2"]},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration._ignore_all_errors, ["buck-out/dev/gen"])
        # Normalize number of workers if zero.
        json_load.side_effect = [{"typeshed": "/TYPESHED/", "workers": 0}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.typeshed, "/TYPESHED/")

        # Test excludes
        json_load.side_effect = [{"exclude": "regexp"}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.excludes, ["regexp"])

        json_load.side_effect = [{"exclude": ["regexp1", "regexp2"]}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.excludes, ["regexp1", "regexp2"])

        json_load.side_effect = [{"exclude": ["regexp1", "regexp2"]}, {}]
        configuration = Configuration(
            project_root="",
            excludes=["regexp3", "regexp4"],
            dot_pyre_directory=Path("/.pyre"),
        )
        self.assertEqual(
            configuration.excludes, ["regexp3", "regexp4", "regexp1", "regexp2"]
        )

        # Test extensions
        json_load.side_effect = [{"extensions": [".a", ".b"]}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.extensions, [".a", ".b"])

        json_load.side_effect = [{}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.autocomplete, False)
        json_load.side_effect = [{"autocomplete": True}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.autocomplete, True)

        json_load.side_effect = [{}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.other_critical_files, [])
        json_load.side_effect = [{"critical_files": ["critical", "files"]}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration.other_critical_files, ["critical", "files"])

        json_load.side_effect = [{}, {}]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(configuration._do_not_ignore_errors_in, [])
        json_load.side_effect = [
            {"do_not_ignore_errors_in": ["directory1", "directory2"]},
            {},
        ]
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        self.assertEqual(
            configuration._do_not_ignore_errors_in, ["directory1", "directory2"]
        )

    @patch("os.path.isfile")
    @patch("os.path.isdir")
    @patch("os.path.exists")
    @patch("os.access")
    # Need to patch this method because this test messes around with
    # isfile/isdir via the patches above. When test optimizations are
    # applied, _apply_defaults goes crazy; hence mock it so it doesn't
    # run - it's not important for this test anyway.
    @patch.object(Configuration, "_apply_defaults")
    @patch.object(Configuration, "_validate")
    @patch.object(Configuration, "_read")
    @patch(f"{configuration.__name__}.expand_relative_path")
    @patch(f"{configuration.__name__}.find_parent_directory_containing_file")
    @patch(f"{configuration.__name__}.Path.resolve")
    @patch("json.loads")
    def test_configurations(
        self,
        json_loads,
        path_resolve,
        find_parent_directory_containing_file,
        expand_relative_path,
        configuration_read,
        configuration_validate,
        configuration_defaults,
        os_access,
        os_path_exists,
        os_path_isdir,
        os_path_isfile,
    ) -> None:
        # Do not expand test paths against real filesystem
        expand_relative_path.side_effect = lambda root, path: path

        # Assume no nested configurations.
        find_parent_directory_containing_file.return_value = None

        # Assume all paths are valid.
        os_access.return_value = True
        os_path_exists.return_value = True

        # Test configuration directories.
        os_path_isdir.return_value = True
        os_path_isfile.return_value = False

        with patch.object(
            Configuration, "source_directories", new_callable=PropertyMock, create=True
        ) as attribute_mock:
            attribute_mock.return_value = ["."]
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            configuration_read.assert_has_calls([call(CONFIGURATION_FILE)])
            self.assertEqual(configuration.local_root, None)

            configuration_read.reset_mock()
            configuration = Configuration(
                project_root="/",
                local_root="original",
                dot_pyre_directory=Path("/.pyre"),
            )
            configuration_read.assert_has_calls(
                [
                    call("original/" + LOCAL_CONFIGURATION_FILE),
                    call("/" + CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(configuration.local_root, "original")

            configuration_read.reset_mock()
            configuration = Configuration(
                project_root="/", local_root="local", dot_pyre_directory=Path("/.pyre")
            )
            configuration_read.assert_has_calls(
                [
                    call("local/" + LOCAL_CONFIGURATION_FILE),
                    call("/" + CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(configuration.local_root, "local")

            configuration_read.reset_mock()
            configuration = Configuration(
                project_root="/", local_root="local", dot_pyre_directory=Path("/.pyre")
            )
            configuration_read.assert_has_calls(
                [
                    call("local/" + LOCAL_CONFIGURATION_FILE),
                    call("/" + CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(configuration.local_root, "local")

            # Test configuration files.
            os_path_isdir.return_value = False
            os_path_isfile.return_value = True
            configuration_read.reset_mock()
            configuration = Configuration(
                project_root="/", local_root="local", dot_pyre_directory=Path("/.pyre")
            )
            configuration_read.assert_has_calls(
                [
                    call("local/.pyre_configuration.local"),
                    call("/" + CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(configuration.local_root, "local")

    @patch("builtins.open", mock_open())  # pyre-fixme[56]
    @patch("os.path.isfile")
    @patch("os.path.isdir")
    @patch("os.path.exists")
    @patch("os.access")
    @patch.object(Configuration, "_apply_defaults")
    @patch.object(Configuration, "_validate")
    @patch(f"{configuration.__name__}.expand_relative_path")
    @patch(f"{configuration.__name__}.find_parent_directory_containing_file")
    @patch(f"{configuration.__name__}.Path.resolve")
    @patch("json.loads")
    def test_nested_configurations(
        self,
        json_loads,
        path_resolve,
        find_parent_directory_containing_file,
        expand_relative_path,
        configuration_validate,
        configuration_defaults,
        os_access,
        os_path_exists,
        os_path_isdir,
        os_path_isfile,
    ) -> None:
        # Do not expand test paths against real filesystem
        expand_relative_path.side_effect = lambda root, path: path

        # Assume all paths are valid.
        os_access.return_value = True
        os_path_exists.return_value = True

        # Properly ignored nested local configurations.
        find_parent_directory_containing_file.side_effect = [Path("root"), None]
        path_resolve.side_effect = [Path("root/local"), Path("root/local")]
        os_path_isdir.return_value = True
        os_path_isfile.return_value = False
        json_loads.side_effect = [
            {
                "source_directories": ["a"],
                "binary": "abc",
                "logger": "/usr/logger",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "strict": False,
                "extensions": [".a", ".b", ""],
                "ignore_all_errors": ["root/local"],
            },
            {},
            {
                "source_directories": ["a"],
                "binary": "abc",
                "logger": "/usr/logger",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "strict": False,
                "extensions": [".a", ".b", ""],
            },
            {},
        ]
        try:
            Configuration(
                project_root="root",
                local_root="root/local",
                dot_pyre_directory=Path("/.pyre"),
            )
        except BaseException:
            self.fail("Configuration should not raise.")

        # Improperly ignored nested local configurations.
        find_parent_directory_containing_file.side_effect = [Path("root"), None]
        path_resolve.side_effect = [
            Path("root/local"),
            Path("/.pyre"),
            Path("not_local"),
        ]
        json_loads.side_effect = [
            {
                "source_directories": ["a"],
                "binary": "abc",
                "logger": "/usr/logger",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "strict": False,
                "ignore_all_errors": ["not_local"],
                "extensions": [".a", ".b", ""],
            },
            {},
            {
                "source_directories": ["a"],
                "binary": "abc",
                "logger": "/usr/logger",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "strict": False,
                "extensions": [".a", ".b", ""],
            },
            {},
        ]
        with self.assertRaises(InvalidConfiguration):
            Configuration(
                project_root="root",
                local_root="root/local",
                dot_pyre_directory=Path("/.pyre"),
            )

        # Project and local configurations both specifying sources.
        path_resolve.reset_mock()
        find_parent_directory_containing_file.side_effect = [None, None]
        json_loads.side_effect = [
            {
                "source_directories": ["local_sources"],
                "strict": False,
                "extensions": [".a", ".b", ""],
            },
            {
                "source_directories": ["project_sources"],
                "binary": "abc",
                "logger": "/usr/logger",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "strict": False,
                "extensions": [".a", ".b", ""],
            },
        ]
        with self.assertRaises(InvalidConfiguration):
            Configuration(
                project_root="root",
                local_root="root/local",
                dot_pyre_directory=Path("/.pyre"),
            )

    @patch("os.path.isfile")
    @patch("os.path.isdir")
    @patch("os.path.exists")
    @patch.object(Configuration, "_validate")
    def test_nonexisting_local_configuration(
        self, configuration_validate, os_path_exists, os_path_isdir, os_path_isfile
    ) -> None:
        # Test that a non-existing local configuration directory was provided.
        os_path_exists.return_value = False
        os_path_isdir.return_value = True
        os_path_isfile.return_value = False
        with self.assertRaises(InvalidConfiguration):
            Configuration(
                project_root="/", local_root="local", dot_pyre_directory=Path("/.pyre")
            )

        # Test that a non-existing local configuration file was provided.
        os_path_exists.return_value = False
        os_path_isdir.return_value = False
        os_path_isfile.return_value = True
        with self.assertRaises(InvalidConfiguration):
            Configuration(
                project_root="/",
                local_root="local/.some_configuration",
                dot_pyre_directory=Path("/.pyre"),
            )

        with self.assertRaises(InvalidConfiguration):
            Configuration(
                project_root="/",
                local_root="local/.some_configuration",
                dot_pyre_directory=Path("/.pyre"),
            )

        # Test an existing local directory, without a configuration file.
        os_path_exists.side_effect = lambda path: not path.endswith(".local")
        os_path_isdir.return_value = lambda path: not path.endswith(".local")
        os_path_isfile.return_value = lambda path: path.endswith(".local")
        with self.assertRaises(InvalidConfiguration):
            Configuration(
                project_root="/",
                local_root="localdir",
                dot_pyre_directory=Path("/.pyre"),
            )

        with self.assertRaises(InvalidConfiguration):
            Configuration(
                project_root="/",
                local_root="localdir",
                dot_pyre_directory=Path("/.pyre"),
            )

    @patch("os.path.abspath", side_effect=lambda path: path)
    @patch("os.path.isdir", return_value=True)
    @patch("os.path.exists")
    @patch("os.access", return_value=True)
    @patch("os.listdir", side_effect=[["3"], ["3"]])
    @patch("builtins.open")
    @patch("json.loads")
    @patch("hashlib.sha1")
    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "getenv", return_value=None)
    def test_validate_configuration(
        self,
        os_environ,
        sha1,
        json_loads,
        builtins_open,
        listdir,
        access,
        exists,
        isdir,
        _abspath,
    ) -> None:
        exists.return_value = True
        try:
            json_loads.side_effect = [
                {
                    "source_directories": ["a"],
                    "binary": "abc",
                    "logger": "/usr/logger",
                    "version": "VERSION",
                    "typeshed": "TYPE/%V/SHED/",
                    "strict": False,
                    "ignore_all_errors": ["buck-out/dev/gen"],
                    "extensions": [".a", ".b", ""],
                },
                {},
            ]
            Configuration("", dot_pyre_directory=Path("/.pyre"))
        except BaseException:
            self.fail("Configuration should not raise.")

        with self.assertRaises(InvalidConfiguration):
            json_loads.side_effect = [
                {
                    "source_directories": ["a"],
                    "binary": "abc",
                    "logger": "/usr/logger",
                    "version": "VERSION",
                    "typeshed": "TYPE/%V/SHED/",
                    "ignore_all_errors": ["buck-out/dev/gen"],
                    "extensions": [".a", "b"],
                },
                {},
            ]
            Configuration("", dot_pyre_directory=Path("/.pyre"))

    @patch.object(Configuration, "_read")
    @patch.object(Configuration, "_override_version_hash")
    @patch.object(Configuration, "_resolve_versioned_paths")
    @patch.object(Configuration, "_validate")
    def test_find_binary(
        self, _validate, _resolve_versioned_paths, _override_version_hash, _read
    ) -> None:
        # The PYRE_BINARY environment variable may change the result of this test,
        # as the configuration lets it override the actual search.
        if "PYRE_BINARY" in os.environ:
            del os.environ["PYRE_BINARY"]

        def accept_tmp(argument: str) -> Optional[str]:
            if argument == "/tmp/pyre/bin/pyre.bin":
                return argument
            return None

        with patch.object(sys, "argv", ["/tmp/pyre/bin/pyre"]), patch(
            "shutil.which", side_effect=accept_tmp
        ):
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration._binary, "/tmp/pyre/bin/pyre.bin")
        with patch.object(sys, "argv", ["/tmp/unknown/bin/pyre"]), patch(
            "shutil.which", side_effect=accept_tmp
        ):
            configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
            self.assertEqual(configuration._binary, None)

    @patch.object(Configuration, "_validate")
    def test_get_binary_version(self, _validate) -> None:
        configuration = Configuration("", dot_pyre_directory=Path("/.pyre"))
        configuration._binary = "<binary>"

        def assert_version(
            returncode: int, stdout: str, expected: Optional[str]
        ) -> None:
            with patch(
                "subprocess.run",
                return_value=MockCompletedProcess(returncode, stdout=stdout),
            ):
                self.assertEqual(expected, configuration.get_binary_version())

        assert_version(
            returncode=0, stdout="facefacefaceb00", expected="facefacefaceb00"
        )
        assert_version(
            returncode=0, stdout=" facefacefaceb00\n", expected="facefacefaceb00"
        )
        assert_version(returncode=1, stdout="facefacefaceb00", expected=None)

    def test_relativize_root__project_relative_path(self) -> None:
        self.assertEqual(
            _relativize_root("//foo/bar.py", project_root="hello", relative_root=None),
            "hello/foo/bar.py",
        )
        # Globs should be untouched.
        self.assertEqual(
            _relativize_root(
                "//foo/*/bar.py", project_root="hello", relative_root=None
            ),
            "hello/foo/*/bar.py",
        )

    def test_relativize_root__not_project_relative(self) -> None:
        self.assertEqual(
            _relativize_root("foo/bar.py", project_root="hello", relative_root=None),
            "foo/bar.py",
        )
        # Globs should be untouched.
        self.assertEqual(
            _relativize_root("foo/*/bar.py", project_root="hello", relative_root=None),
            "foo/*/bar.py",
        )


class SearchPathElementTest(unittest.TestCase):
    def test_create(self) -> None:
        self.assertListEqual(
            create_search_paths("foo", site_roots=[]), [SimpleSearchPathElement("foo")]
        )
        self.assertListEqual(
            create_search_paths({"root": "foo", "subdirectory": "bar"}, site_roots=[]),
            [SubdirectorySearchPathElement("foo", "bar")],
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=[]), []
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=["site0"]),
            [SitePackageSearchPathElement("site0", "foo")],
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=["site1"]),
            [SitePackageSearchPathElement("site1", "foo")],
        )

        with self.assertRaises(InvalidConfiguration):
            create_search_paths({}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_search_paths({"foo": "bar"}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_search_paths({"root": "foo"}, site_roots=[])

    def test_path(self) -> None:
        self.assertEqual(SimpleSearchPathElement("foo").path(), "foo")
        self.assertEqual(SubdirectorySearchPathElement("foo", "bar").path(), "foo/bar")
        self.assertEqual(SitePackageSearchPathElement("foo", "bar").path(), "foo/bar")

    def test_command_line_argument(self) -> None:
        self.assertEqual(SimpleSearchPathElement("foo").command_line_argument(), "foo")
        self.assertEqual(
            SubdirectorySearchPathElement("foo", "bar").command_line_argument(),
            "foo$bar",
        )
        self.assertEqual(
            SitePackageSearchPathElement("foo", "bar").command_line_argument(),
            "foo$bar",
        )

    def test_expand_root(self) -> None:
        self.assertEqual(
            SimpleSearchPathElement("simple/path").expand_root(
                project_root="root", relative_root=None
            ),
            SimpleSearchPathElement("simple/path"),
        )
        self.assertEqual(
            SimpleSearchPathElement("simple/path").expand_root(
                project_root="root", relative_root="root/local_project"
            ),
            SimpleSearchPathElement("root/local_project/simple/path"),
        )
        self.assertEqual(
            SimpleSearchPathElement("//simple/path").expand_root(
                project_root="root", relative_root="root/local_project"
            ),
            SimpleSearchPathElement("root/simple/path"),
        )

        self.assertEqual(
            SubdirectorySearchPathElement("path", "sub").expand_root(
                project_root="root", relative_root=None
            ),
            SubdirectorySearchPathElement("path", "sub"),
        )
        self.assertEqual(
            SubdirectorySearchPathElement("path", "sub").expand_root(
                project_root="root", relative_root="root/local_project"
            ),
            SubdirectorySearchPathElement("root/local_project/path", "sub"),
        )
        self.assertEqual(
            SubdirectorySearchPathElement("//path", "sub").expand_root(
                project_root="root", relative_root="root/local_project"
            ),
            SubdirectorySearchPathElement("root/path", "sub"),
        )

        self.assertEqual(
            SitePackageSearchPathElement("site_root", "package").expand_root(
                project_root="root", relative_root=None
            ),
            SitePackageSearchPathElement("site_root", "package"),
        )
        self.assertEqual(
            SitePackageSearchPathElement("site_root", "package").expand_root(
                project_root="root", relative_root="root/local_project"
            ),
            SitePackageSearchPathElement("site_root", "package"),
        )
        self.assertEqual(
            SitePackageSearchPathElement("//site_root", "package").expand_root(
                project_root="root", relative_root="root/local_project"
            ),
            SitePackageSearchPathElement("//site_root", "package"),
        )
