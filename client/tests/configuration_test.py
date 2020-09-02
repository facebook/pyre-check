# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import os
import site
import sys
import unittest
from pathlib import Path
from typing import Any, NamedTuple, Optional, cast
from unittest.mock import MagicMock, PropertyMock, call, mock_open, patch

from .. import configuration
from ..configuration import Configuration, InvalidConfiguration, SearchPathElement
from ..exceptions import EnvironmentException
from ..find_directories import CONFIGURATION_FILE, LOCAL_CONFIGURATION_FILE


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
        configuration = Configuration("")
        self.assertEqual(configuration.source_directories, ["a"])
        self.assertEqual(configuration.targets, [])
        self.assertEqual(configuration.logger, "/usr/logger")
        self.assertEqual(configuration.ignore_all_errors, ["buck-out/dev/gen"])
        self.assertEqual(configuration.file_hash, None)

        # Local configurations
        json_load.side_effect = [
            {"source_directories": ["a"]},
            {"source_directories": ["a"]},
            {},
        ]
        with self.assertRaises(EnvironmentException):
            configuration = Configuration("", "local/path")
            self.assertEqual(configuration.source_directories, ["local/path/a"])

        json_load.side_effect = [{"source_directories": ["a"]}, {"version": "abc"}, {}]
        configuration = Configuration("local/path", log_directory="/.pyre/local/path")
        self.assertEqual(configuration.source_directories, ["local/path/a"])
        self.assertEqual(configuration.ignore_all_errors, ["/.pyre/local/path"])

        # Configuration fields
        json_load.side_effect = [{"targets": ["//a/b/c"], "disabled": 1}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.targets, ["//a/b/c"])
        self.assertEqual(configuration.source_directories, [])
        self.assertEqual(configuration.version_hash, "unversioned")
        self.assertEqual(configuration.logger, None)
        self.assertEqual(configuration.file_hash, None)
        self.assertTrue(configuration.disabled)

        json_load.side_effect = [{"typeshed": "TYPESHED/"}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "TYPESHED/")
        self.assertEqual(configuration.file_hash, None)

        with patch.object(os.path, "isdir", return_value=False):
            json_load.side_effect = [{"search_path": [{"site-package": "abc"}]}]
            with self.assertRaises(InvalidConfiguration):
                configuration = Configuration("")

        with patch.object(
            site, "getsitepackages", return_value=["/mock/site0", "/mock/site1"]
        ):
            with patch.object(
                os.path,
                "isdir",
                side_effect=lambda path: path.startswith("/mock/site0"),
            ):
                json_load.side_effect = [{"search_path": [{"site-package": "abc"}]}]
                configuration = Configuration("")
                self.assertIn("/mock/site0$abc", configuration.search_path)
            with patch.object(
                os.path,
                "isdir",
                side_effect=lambda path: path.startswith("/mock/site1"),
            ):
                json_load.side_effect = [{"search_path": [{"site-package": "abc"}]}]
                configuration = Configuration("")
                self.assertIn("/mock/site1$abc", configuration.search_path)

        json_load.side_effect = [
            {
                "search_path": ["additional/"],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "workers": 20,
            },
            {},
        ]
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(
            configuration.search_path,
            [SearchPathElement("additional/", subdirectory=None)],
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
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(
            configuration.search_path,
            [SearchPathElement("additional/", subdirectory=None)],
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
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(
            configuration.search_path, ["additional/", "root/$subdirectory"]
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
        configuration = Configuration("project_root")
        self.assertEqual(configuration.typeshed, "project_root/TYPE/VERSION/SHED/")
        self.assertEqual(
            configuration.search_path,
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
            Configuration("")

        json_load.side_effect = [
            {
                "search_path": "simple_string/",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "taint_models_path": ".pyre/taint_models",
            },
            {},
        ]
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(configuration.search_path, ["simple_string/"])
        self.assertEqual(configuration.taint_models_path, [".pyre/taint_models"])

        json_load.side_effect = [
            {
                "search_path": "simple_string/",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "taint_models_path": [".pyre/taint_models_1", ".pyre/taint_models_2"],
            },
            {},
        ]
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(configuration.search_path, ["simple_string/"])
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
            configuration = Configuration("")
            self.assertEqual(configuration.binary, "/root/some/dir/pyre.bin")
            self.assertEqual(configuration.typeshed, "/root/some/typeshed")
            self.assertIsNone(configuration.buck_builder_binary)

            json_load.side_effect = [
                {"binary": "~/some/dir/pyre.bin", "typeshed": "~/some/typeshed"},
                {},
            ]
            configuration = Configuration("")
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
            configuration = Configuration("")
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
            configuration = Configuration("")
            self.assertEqual(configuration.binary, "/home/user/some/VERSION/pyre.bin")
            self.assertEqual(configuration.typeshed, "/home/user/some/VERSION/typeshed")

            json_load.side_effect = [
                {"buck_builder_binary": "/some/dir/buck_builder"},
                {},
            ]
            configuration = Configuration("")
            self.assertEqual(
                configuration.buck_builder_binary, "/some/dir/buck_builder"
            )

            json_load.side_effect = [
                {"buck_builder_binary": "some/dir/buck_builder"},
                {},
            ]
            configuration = Configuration("")
            self.assertEqual(
                configuration.buck_builder_binary, "/root/some/dir/buck_builder"
            )

            json_load.side_effect = [
                {"ignore_all_errors": ["abc/def", "/abc/def", "~/abc/def"]},
                {},
            ]
            configuration = Configuration("")
            self.assertEqual(
                configuration.ignore_all_errors,
                ["/root/abc/def", "/abc/def", "/home/user/abc/def"],
            )

            json_load.side_effect = [
                {
                    "taint_models_path": ".pyre/taint_models",
                    "search_path": "simple_string/",
                    "version": "VERSION",
                    "typeshed": "/TYPE/%V/SHED/",
                },
                {},
            ]
            configuration = Configuration("")
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(configuration.search_path, ["/root/simple_string/"])
            self.assertEqual(
                configuration.taint_models_path, ["/root/.pyre/taint_models"]
            )
            json_load.side_effect = [
                {
                    "taint_models_path": ".pyre/taint_models",
                    "source_directories": ["."],
                },
                {
                    "search_path": "simple_string/",
                    "version": "VERSION",
                    "typeshed": "/TYPE/%V/SHED/",
                },
            ]
            configuration = Configuration(
                project_root="/root", local_root="/root/local"
            )
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(configuration.search_path, ["/root/simple_string/"])
            self.assertEqual(
                configuration.taint_models_path, ["/root/local/.pyre/taint_models"]
            )
            json_load.side_effect = [
                {
                    "taint_models_path": ".pyre/taint_models",
                    "source_directories": ["."],
                },
                {
                    "search_path": "simple_string/",
                    "version": "VERSION",
                    "taint_models_path": "global/taint_models",
                    "typeshed": "/TYPE/%V/SHED/",
                },
            ]
            configuration = Configuration(
                project_root="/root", local_root="/root/local"
            )
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(configuration.search_path, ["/root/simple_string/"])
            self.assertEqual(
                configuration.taint_models_path,
                ["/root/local/.pyre/taint_models", "/root/global/taint_models"],
            )

        json_load.side_effect = [
            {
                "search_path": "simple_string/",
                "version": "VERSION",
                "typeshed": "/TYPE/%V/SHED/",
                "saved_state": "some_name",
            },
            {},
        ]
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
        self.assertEqual(configuration.search_path, ["simple_string/"])
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
        configuration = Configuration("")
        self.assertEqual(
            configuration.search_path, ["/home/user/simple", "/home/user/simple$subdir"]
        )
        self.assertEqual(configuration.typeshed, "/home/user/typeshed")
        self.assertEqual(configuration.source_directories, ["a", "/home/user/b"])
        self.assertEqual(configuration.binary, "/home/user/bin")

        # Test manual loading of the binary
        json_load.side_effect = [{}, {}]
        configuration = Configuration(project_root="", binary="some/file/path/")
        self.assertEqual(configuration.binary, "some/file/path/")

        # Test manual loading of typeshed directory.
        json_load.side_effect = [{}, {}]
        configuration = Configuration(project_root="", typeshed="some/directory/path/")
        self.assertEqual(configuration.typeshed, "some/directory/path/")

        json_load.side_effect = [{"binary": "/binary"}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.binary, "/binary")

        json_load.side_effect = [{"version": "VERSION", "binary": "/%V/binary"}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.binary, "/VERSION/binary")

        # Test version override
        with patch.object(os, "getenv", return_value="VERSION_HASH"):
            json_load.side_effect = [{}, {}]
            configuration = Configuration("")
            self.assertEqual(configuration.version_hash, "VERSION_HASH")

        with patch.object(os, "getenv", return_value="VERSION_HASH"):
            json_load.side_effect = [
                {"version": "NOT_THIS_VERSION", "typeshed": "/TYPE/%V/SHED/"},
                {},
            ]
            configuration = Configuration("")
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION_HASH/SHED/")

        # Test buck builder fields
        json_load.side_effect = [{"use_buck_builder": True}, {}]
        configuration = Configuration("")
        self.assertTrue(configuration.use_buck_builder)
        json_load.side_effect = [{"use_buck_builder": False}, {}]
        configuration = Configuration("")
        self.assertFalse(configuration.use_buck_builder)
        json_load.side_effect = [{}, {}]
        configuration = Configuration("")
        self.assertFalse(configuration.use_buck_builder)

        # Test multiple definitions of the ignore_all_errors files.
        json_load.side_effect = [
            {"ignore_all_errors": ["buck-out/dev/gen"]},
            {"ignore_all_errors": ["buck-out/dev/gen2"]},
        ]
        configuration = Configuration("")
        self.assertEqual(configuration.ignore_all_errors, ["buck-out/dev/gen"])
        # Normalize number of workers if zero.
        json_load.side_effect = [{"typeshed": "/TYPESHED/", "workers": 0}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.typeshed, "/TYPESHED/")

        # Test excludes
        json_load.side_effect = [{"exclude": "regexp"}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.excludes, ["regexp"])

        json_load.side_effect = [{"exclude": ["regexp1", "regexp2"]}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.excludes, ["regexp1", "regexp2"])

        json_load.side_effect = [{"exclude": ["regexp1", "regexp2"]}, {}]
        configuration = Configuration(project_root="", excludes=["regexp3", "regexp4"])
        self.assertEqual(
            configuration.excludes, ["regexp3", "regexp4", "regexp1", "regexp2"]
        )

        # Test extensions
        json_load.side_effect = [{"extensions": [".a", ".b"]}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.extensions, [".a", ".b"])

        json_load.side_effect = [{}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.autocomplete, False)
        json_load.side_effect = [{"autocomplete": True}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.autocomplete, True)

        json_load.side_effect = [{}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.other_critical_files, [])
        json_load.side_effect = [{"critical_files": ["critical", "files"]}, {}]
        configuration = Configuration("")
        self.assertEqual(configuration.other_critical_files, ["critical", "files"])

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
            configuration = Configuration("")
            configuration_read.assert_has_calls([call(CONFIGURATION_FILE)])
            self.assertEqual(configuration.local_root, None)

            configuration_read.reset_mock()
            configuration = Configuration(project_root="/", local_root="original")
            configuration_read.assert_has_calls(
                [
                    call("original/" + LOCAL_CONFIGURATION_FILE),
                    call("/" + CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(configuration.local_root, "original")

            configuration_read.reset_mock()
            configuration = Configuration(project_root="/", local_root="local")
            configuration_read.assert_has_calls(
                [
                    call("local/" + LOCAL_CONFIGURATION_FILE),
                    call("/" + CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(configuration.local_root, "local")

            configuration_read.reset_mock()
            configuration = Configuration(project_root="/", local_root="local")
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
            configuration = Configuration(project_root="/", local_root="local")
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
            Configuration(project_root="root", local_root="root/local")
        except BaseException:
            self.fail("Configuration should not raise.")

        # Improperly ignored nested local configurations.
        find_parent_directory_containing_file.side_effect = [Path("root"), None]
        path_resolve.side_effect = [Path("root/local"), Path("not_local")]
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
        with self.assertRaises(EnvironmentException):
            Configuration(project_root="root", local_root="root/local")

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
        with self.assertRaises(EnvironmentException):
            Configuration(project_root="root", local_root="root/local")

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
        with self.assertRaises(EnvironmentException):
            Configuration(project_root="/", local_root="local")

        # Test that a non-existing local configuration file was provided.
        os_path_exists.return_value = False
        os_path_isdir.return_value = False
        os_path_isfile.return_value = True
        with self.assertRaises(EnvironmentException):
            Configuration(project_root="/", local_root="local/.some_configuration")

        with self.assertRaises(EnvironmentException):
            Configuration(project_root="/", local_root="local/.some_configuration")

        # Test an existing local directory, without a configuration file.
        os_path_exists.side_effect = lambda path: not path.endswith(".local")
        os_path_isdir.return_value = lambda path: not path.endswith(".local")
        os_path_isfile.return_value = lambda path: path.endswith(".local")
        with self.assertRaises(EnvironmentException):
            Configuration(project_root="/", local_root="localdir")

        with self.assertRaises(EnvironmentException):
            Configuration(project_root="/", local_root="localdir")

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
            Configuration("")
        except BaseException:
            self.fail("Configuration should not raise.")

        with self.assertRaises(EnvironmentException):
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
            Configuration("")

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
            configuration = Configuration("")
            self.assertEqual(configuration._binary, "/tmp/pyre/bin/pyre.bin")
        with patch.object(sys, "argv", ["/tmp/unknown/bin/pyre"]), patch(
            "shutil.which", side_effect=accept_tmp
        ):
            configuration = Configuration("")
            self.assertEqual(configuration._binary, None)

    @patch.object(Configuration, "_validate")
    def test_get_binary_version(self, _validate) -> None:
        configuration = Configuration("")
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


class SearchPathElementTest(unittest.TestCase):
    @patch("os.path.isdir", return_value=True)
    def test_expand(self, isdir) -> None:
        element = SearchPathElement.expand(path="simple/path", project_root="root")
        self.assertEqual(element.path(), "simple/path")
        self.assertEqual(element.subdirectory, None)

        element = SearchPathElement.expand(
            path="simple/path",
            project_root="root",
            path_relative_to="root/local_project",
        )
        self.assertEqual(element.path(), "root/local_project/simple/path")
        self.assertEqual(element.subdirectory, None)

        element = SearchPathElement.expand(path="//simple/path", project_root="root")
        self.assertEqual(element.path(), "root/simple/path")
        self.assertEqual(element.subdirectory, None)

        element = SearchPathElement.expand(
            path="//simple/path",
            project_root="root",
            path_relative_to="root/local_project",
        )
        self.assertEqual(element.path(), "root/simple/path")
        self.assertEqual(element.subdirectory, None)

        # Test search paths with subdirectories
        element = SearchPathElement.expand(
            path={"root": "path", "subdirectory": "sub"}, project_root="root"
        )
        self.assertEqual(element.path(), "path/sub")
        self.assertEqual(element.subdirectory, "sub")

        element = SearchPathElement.expand(
            path={"root": "path", "subdirectory": "sub"},
            project_root="root",
            path_relative_to="root/local_project",
        )
        self.assertEqual(element.path(), "root/local_project/path/sub")
        self.assertEqual(element.subdirectory, "sub")

        element = SearchPathElement.expand(
            path={"root": "//path", "subdirectory": "sub"}, project_root="root"
        )
        self.assertEqual(element.path(), "root/path/sub")
        self.assertEqual(element.subdirectory, "sub")

        element = SearchPathElement.expand(
            path={"root": "//path", "subdirectory": "sub"},
            project_root="root",
            path_relative_to="root/local_project",
        )
        self.assertEqual(element.path(), "root/path/sub")
        self.assertEqual(element.subdirectory, "sub")

    @patch("os.path.isdir", return_value=True)
    def test_site_packages(self, isdir) -> None:
        with patch.object(
            site, "getsitepackages", return_value=["/mock/site0", "/mock/site1"]
        ):
            element = SearchPathElement.expand(
                path={"site-package": "abc"},
                project_root="root",
                path_relative_to="root/local_project",
            )
            self.assertEqual(element.path(), "/mock/site1/abc")
            self.assertEqual(element.subdirectory, "abc")
