# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import hashlib  # noqa
import os
import shutil  # noqa
import site
import sys
import unittest
from typing import Any, Optional, cast
from unittest.mock import MagicMock, call, patch

from .. import CONFIGURATION_FILE, number_of_workers
from ..configuration import (  # noqa
    Configuration,
    InvalidConfiguration,
    SearchPathElement,
)
from ..exceptions import EnvironmentException


class ConfigurationTest(unittest.TestCase):
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
        configuration = Configuration()
        self.assertEqual(configuration.source_directories, ["a"])
        self.assertEqual(configuration.targets, [])
        self.assertEqual(configuration.logger, "/usr/logger")
        self.assertEqual(configuration.ignore_all_errors, ["buck-out/dev/gen"])
        self.assertEqual(configuration.file_hash, None)

        json_load.side_effect = [
            {"source_directories": ["a"]},
            {"source_directories": ["a"]},
            {},
        ]
        configuration = Configuration("local/path")
        self.assertEqual(configuration.source_directories, ["local/path/a"])

        json_load.side_effect = [{"targets": ["//a/b/c"], "disabled": 1}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.targets, ["//a/b/c"])
        self.assertEqual(configuration.source_directories, [])
        self.assertEqual(configuration.version_hash, "unversioned")
        self.assertEqual(configuration.logger, None)
        self.assertEqual(configuration.ignore_all_errors, [])
        self.assertEqual(configuration.file_hash, None)
        self.assertTrue(configuration.disabled)

        json_load.side_effect = [{"typeshed": "TYPESHED/"}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "TYPESHED/")
        self.assertEqual(configuration.number_of_workers, number_of_workers())
        self.assertEqual(configuration.file_hash, None)

        python_paths = site.getsitepackages()
        json_load.side_effect = [{"search_path": [{"site-package": "abc"}]}]
        configuration = Configuration()
        for python_path in python_paths:
            self.assertIn("{}$abc".format(python_path), configuration.search_path)

        json_load.side_effect = [
            {
                "search_path": ["additional/"],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "workers": 20,
            },
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(configuration.search_path, [SearchPathElement("additional/")])
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
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(configuration.search_path, [SearchPathElement("additional/")])
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
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(
            configuration.search_path, ["additional/", "root/$subdirectory"]
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
            Configuration()

        json_load.side_effect = [
            {
                "search_path": "simple_string/",
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
                "taint_models_path": ".pyre/taint_models",
            },
            {},
        ]
        configuration = Configuration()
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
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "TYPE/VERSION/SHED/")
        self.assertEqual(configuration.search_path, ["simple_string/"])
        self.assertEqual(
            configuration.taint_models_path,
            [".pyre/taint_models_1", ".pyre/taint_models_2"],
        )

        def directory_side_effect(path: str) -> str:
            if path.endswith(".pyre_configuration"):
                return "/root"
            elif path.endswith(".pyre_configuration.local"):
                return "/root/local"
            else:
                return path

        with patch("os.path.dirname", side_effect=directory_side_effect):
            json_load.side_effect = [
                {"binary": "some/dir/pyre.bin", "typeshed": "some/typeshed"},
                {},
            ]
            configuration = Configuration()
            self.assertEqual(configuration.binary, "/root/some/dir/pyre.bin")
            self.assertEqual(configuration.typeshed, "/root/some/typeshed")

            json_load.side_effect = [
                {"binary": "~/some/dir/pyre.bin", "typeshed": "~/some/typeshed"},
                {},
            ]
            configuration = Configuration()
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
            configuration = Configuration()
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
            configuration = Configuration()
            self.assertEqual(configuration.binary, "/home/user/some/VERSION/pyre.bin")
            self.assertEqual(configuration.typeshed, "/home/user/some/VERSION/typeshed")

            json_load.side_effect = [
                {"ignore_all_errors": ["abc/def", "/abc/def", "~/abc/def"]},
                {},
            ]
            configuration = Configuration()
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
            configuration = Configuration()
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(configuration.search_path, ["simple_string/"])
            self.assertEqual(
                configuration.taint_models_path, ["/root/.pyre/taint_models"]
            )
            json_load.side_effect = [
                {"taint_models_path": ".pyre/taint_models"},
                {
                    "search_path": "simple_string/",
                    "version": "VERSION",
                    "typeshed": "/TYPE/%V/SHED/",
                },
            ]
            configuration = Configuration(
                local_configuration="/root/local/.pyre_configuration.local"
            )
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(configuration.search_path, ["simple_string/"])
            self.assertEqual(
                configuration.taint_models_path, ["/root/local/.pyre/taint_models"]
            )
            json_load.side_effect = [
                {"taint_models_path": ".pyre/taint_models"},
                {
                    "search_path": "simple_string/",
                    "version": "VERSION",
                    "taint_models_path": "global/taint_models",
                    "typeshed": "/TYPE/%V/SHED/",
                },
            ]
            configuration = Configuration(
                local_configuration="/root/local/.pyre_configuration.local"
            )
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION/SHED/")
            self.assertEqual(configuration.search_path, ["simple_string/"])
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
        configuration = Configuration()
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
        configuration = Configuration()
        self.assertEqual(
            configuration.search_path, ["/home/user/simple", "/home/user/simple$subdir"]
        )
        self.assertEqual(configuration.typeshed, "/home/user/typeshed")
        self.assertEqual(configuration.source_directories, ["a", "/home/user/b"])
        self.assertEqual(configuration.binary, "/home/user/bin")

        # Test loading of additional directories in the search path
        # via environment $PYTHONPATH.
        json_load.side_effect = [
            {"search_path": ["json/", "file/"], "typeshed": "/TYPESHED/"},
            {},
        ]
        with patch.object(os, "getenv", return_value="additional/:directories/"):
            with patch.object(os.path, "isdir", return_value=True):
                configuration = Configuration(
                    search_path=["command/", "line/"], preserve_pythonpath=True
                )
                self.assertEqual(configuration.typeshed, "/TYPESHED/")
                self.assertEqual(
                    configuration.search_path,
                    [
                        SearchPathElement("additional/"),
                        SearchPathElement("directories/"),
                        *[SearchPathElement(i) for i in sys.path if os.path.isdir(i)],
                        SearchPathElement("command/"),
                        SearchPathElement("line/"),
                        SearchPathElement("json/"),
                        SearchPathElement("file/"),
                    ],
                )

        # Test case where we ignore the PYTHONPATH environment variable.
        json_load.side_effect = [
            {"search_path": ["json/", "file/"], "typeshed": "/TYPESHED/"},
            {},
        ]
        with patch.object(os, "getenv", return_value="additional/:directories/"):
            with patch.object(os.path, "isdir", return_value=True):
                configuration = Configuration(
                    search_path=["command/", "line/"], preserve_pythonpath=False
                )
                self.assertEqual(configuration.typeshed, "/TYPESHED/")
                self.assertEqual(
                    configuration.search_path, ["command/", "line/", "json/", "file/"]
                )

        # Test manual loading of the binary
        json_load.side_effect = [{}, {}]
        configuration = Configuration(binary="some/file/path/")
        self.assertEqual(configuration.binary, "some/file/path/")

        # Test manual loading of typeshed directory.
        json_load.side_effect = [{}, {}]
        configuration = Configuration(typeshed="some/directory/path/")
        self.assertEqual(configuration.typeshed, "some/directory/path/")

        json_load.side_effect = [{"binary": "/binary"}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.binary, "/binary")

        json_load.side_effect = [{"version": "VERSION", "binary": "/%V/binary"}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.binary, "/VERSION/binary")

        # Test version override
        with patch.object(os, "getenv", return_value="VERSION_HASH"):
            json_load.side_effect = [{}, {}]
            configuration = Configuration()
            self.assertEqual(configuration.version_hash, "VERSION_HASH")

        with patch.object(os, "getenv", return_value="VERSION_HASH"):
            json_load.side_effect = [
                {"version": "NOT_THIS_VERSION", "typeshed": "/TYPE/%V/SHED/"},
                {},
            ]
            configuration = Configuration()
            self.assertEqual(configuration.typeshed, "/TYPE/VERSION_HASH/SHED/")

        # Test buck builder fields
        json_load.side_effect = [{"use_buck_builder": True}, {}]
        configuration = Configuration()
        self.assertTrue(configuration.use_buck_builder)
        json_load.side_effect = [{"use_buck_builder": False}, {}]
        configuration = Configuration()
        self.assertFalse(configuration.use_buck_builder)
        json_load.side_effect = [{}, {}]
        configuration = Configuration()
        self.assertFalse(configuration.use_buck_builder)

        # Test multiple definitions of the ignore_all_errors files.
        json_load.side_effect = [
            {"ignore_all_errors": ["buck-out/dev/gen"]},
            {"ignore_all_errors": ["buck-out/dev/gen2"]},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.ignore_all_errors, ["buck-out/dev/gen"])

        # Normalize number of workers if zero.
        json_load.side_effect = [{"typeshed": "/TYPESHED/", "workers": 0}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "/TYPESHED/")
        self.assertEqual(configuration.number_of_workers, number_of_workers())

        # Test excludes
        json_load.side_effect = [{"exclude": "regexp"}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.excludes, ["regexp"])

        json_load.side_effect = [{"exclude": ["regexp1", "regexp2"]}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.excludes, ["regexp1", "regexp2"])

        json_load.side_effect = [{"exclude": ["regexp1", "regexp2"]}, {}]
        configuration = Configuration(excludes=["regexp3", "regexp4"])
        self.assertEqual(
            configuration.excludes, ["regexp3", "regexp4", "regexp1", "regexp2"]
        )

        # Test extensions
        json_load.side_effect = [{"extensions": [".a", ".b"]}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.extensions, [".a", ".b"])

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
    def test_configurations(
        self,
        configuration_validate,
        configuration_defaults,
        os_access,
        os_path_exists,
        os_path_isdir,
        os_path_isfile,
    ) -> None:
        # Assume all paths are valid.
        os_access.return_value = True
        os_path_exists.return_value = True

        # Try with directories first.
        os_path_isdir.return_value = True
        os_path_isfile.return_value = False

        with patch.object(Configuration, "_read") as Configuration_read:
            configuration = Configuration()
            Configuration_read.assert_has_calls([call(CONFIGURATION_FILE)])
            self.assertEqual(configuration.local_configuration, None)

        with patch.object(Configuration, "_read") as Configuration_read:
            configuration = Configuration(local_configuration="original")
            Configuration_read.assert_has_calls(
                [
                    call("original/" + CONFIGURATION_FILE + ".local"),
                    call(CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(
                configuration.local_configuration,
                "original/" + CONFIGURATION_FILE + ".local",
            )
        with patch.object(Configuration, "_read") as Configuration_read:
            configuration = Configuration(local_configuration="local")
            Configuration_read.assert_has_calls(
                [
                    call("local/" + CONFIGURATION_FILE + ".local"),
                    call(CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(
                configuration.local_configuration,
                "local/" + CONFIGURATION_FILE + ".local",
            )
        with patch.object(Configuration, "_read") as Configuration_read:
            configuration = Configuration(local_configuration="local")
            Configuration_read.assert_has_calls(
                [
                    call("local/" + CONFIGURATION_FILE + ".local"),
                    call(CONFIGURATION_FILE),
                ]
            )
            self.assertEqual(
                configuration.local_configuration,
                "local/" + CONFIGURATION_FILE + ".local",
            )

        # Try with regular configuration files then.
        os_path_isdir.return_value = False
        os_path_isfile.return_value = True
        with patch.object(Configuration, "_read") as Configuration_read:
            configuration = Configuration(
                local_configuration="local/.some_configuration"
            )
            Configuration_read.assert_has_calls(
                [call("local/.some_configuration"), call(CONFIGURATION_FILE)]
            )
            self.assertEqual(
                configuration.local_configuration, "local/.some_configuration"
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
        with self.assertRaises(EnvironmentException):
            Configuration(local_configuration="local")

        # Test that a non-existing local configuration file was provided.
        os_path_exists.return_value = False
        os_path_isdir.return_value = False
        os_path_isfile.return_value = True
        with self.assertRaises(EnvironmentException):
            Configuration(local_configuration="local/.some_configuration")

        with self.assertRaises(EnvironmentException):
            Configuration(local_configuration="local/.some_configuration")

        # Test an existing local directory, without a configuration file.
        os_path_exists.side_effect = lambda path: not path.endswith(".local")
        os_path_isdir.return_value = lambda path: not path.endswith(".local")
        os_path_isfile.return_value = lambda path: path.endswith(".local")
        with self.assertRaises(EnvironmentException):
            Configuration(local_configuration="localdir")

        with self.assertRaises(EnvironmentException):
            Configuration(local_configuration="localdir")

    @patch("os.path.isdir")
    @patch.object(Configuration, "_validate")
    def test_empty_configuration(self, configuration_validate, os_path_isdir) -> None:
        os_path_isdir.return_value = False
        # If typeshed is importable, find_typeshed() will behave
        # differently because its 'import typeshed' will
        # succeed. Hence, poison the module cache as described here:
        # https://docs.python.org/3.6/reference/import.html#the-module-cache
        sys.modules["typeshed"] = cast(Any, None)

        with patch.object(Configuration, "_read"):
            # __init__.py is in the parent directory.
            directory = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
            bundled_typeshed_calls = []
            environment_typeshed_calls = []
            while True:
                bundled_typeshed_calls.append(
                    call(os.path.join(directory, "pyre_check/typeshed/"))
                )
                environment_typeshed_calls.append(
                    call(os.path.join(directory, "typeshed/"))
                )
                parent_directory = os.path.dirname(directory)
                if parent_directory == directory:
                    break
                directory = parent_directory
            calls = bundled_typeshed_calls + environment_typeshed_calls

            configuration = Configuration()
            os_path_isdir.assert_has_calls(calls)
            self.assertEqual(configuration.source_directories, [])
            self.assertEqual(configuration.targets, [])
            self.assertEqual(configuration.version_hash, "unversioned")
            self.assertEqual(configuration.logger, None)
            self.assertEqual(configuration.ignore_all_errors, [])
            self.assertFalse(configuration.disabled)
            self.assertEqual(configuration._typeshed, None)
            self.assertEqual(configuration.excludes, [])
            self.assertEqual(configuration.extensions, [])

    @patch("os.path.abspath", side_effect=lambda path: path)
    @patch("os.path.isdir", return_value=True)
    @patch("os.path.exists")
    @patch("os.access", return_value=True)
    @patch("os.listdir", side_effect=[["stdlib"], ["3"]])
    @patch("builtins.open")
    @patch("json.loads")
    @patch("hashlib.sha1")
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
            Configuration()
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
            Configuration()

    @patch.object(Configuration, "_read")
    @patch.object(Configuration, "_override_version_hash")
    @patch.object(Configuration, "_resolve_versioned_paths")
    @patch.object(Configuration, "_validate")
    def test_find_binary(
        self, _validate, _resolve_versioned_paths, _override_version_hash, _read
    ):
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
            configuration = Configuration()
            self.assertEqual(configuration._binary, "/tmp/pyre/bin/pyre.bin")
        with patch.object(sys, "argv", ["/tmp/unknown/bin/pyre"]), patch(
            "shutil.which", side_effect=accept_tmp
        ):
            configuration = Configuration()
            self.assertEqual(configuration._binary, None)
