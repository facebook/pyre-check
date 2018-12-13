# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import sys
import unittest
from unittest.mock import call, patch

from .. import CONFIGURATION_FILE, EnvironmentException, number_of_workers
from ..configuration import (  # noqa
    Configuration,
    InvalidConfiguration,
    SearchPathElement,
)


class ConfigurationTest(unittest.TestCase):
    @patch("os.path.abspath", side_effect=lambda path: path)
    @patch("os.path.isdir", return_value=True)
    @patch("os.path.exists")
    @patch("os.access", return_value=True)
    @patch("builtins.open")
    @patch("json.load")
    @patch.object(os, "getenv", return_value=None)
    @patch.object(Configuration, "_validate")
    def test_init(
        self,
        configuration_validate,
        os_environ,
        json_load,
        builtins_open,
        access,
        exists,
        isdir,
        _abspath,
    ) -> None:
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
        self.assertTrue(configuration.disabled)

        json_load.side_effect = [{"typeshed": "TYPESHED/"}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "TYPESHED/")
        self.assertEqual(configuration.number_of_workers, number_of_workers())

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
        self.assertEqual(configuration.taint_models_path, None)

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
        self.assertEqual(configuration.taint_models_path, None)

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
        self.assertEqual(configuration.taint_models_path, ".pyre/taint_models")

        # Test loading of additional directories in the search path
        # via environment $PYTHONPATH.
        json_load.side_effect = [
            {"search_path": ["json/", "file/"], "typeshed": "TYPESHED/"},
            {},
        ]
        with patch.object(os, "getenv", return_value="additional/:directories/"):
            with patch.object(os.path, "isdir", return_value=True):
                configuration = Configuration(
                    search_path=[
                        SearchPathElement("command/"),
                        SearchPathElement("line/"),
                    ],
                    preserve_pythonpath=True,
                )
                self.assertEqual(configuration.typeshed, "TYPESHED/")
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
            {"search_path": ["json/", "file/"], "typeshed": "TYPESHED/"},
            {},
        ]
        with patch.object(os, "getenv", return_value="additional/:directories/"):
            with patch.object(os.path, "isdir", return_value=True):
                configuration = Configuration(
                    search_path=[
                        SearchPathElement("command/"),
                        SearchPathElement("line/"),
                    ],
                    preserve_pythonpath=False,
                )
                self.assertEqual(configuration.typeshed, "TYPESHED/")
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
                {"version": "NOT_THIS_VERSION", "typeshed": "TYPE/%V/SHED/"},
                {},
            ]
            configuration = Configuration()
            self.assertEqual(configuration.typeshed, "TYPE/VERSION_HASH/SHED/")

        # Test multiple definitions of the ignore_all_errors files.
        json_load.side_effect = [
            {"ignore_all_errors": ["buck-out/dev/gen"]},
            {"ignore_all_errors": ["buck-out/dev/gen2"]},
        ]
        configuration = Configuration()
        self.assertEqual(
            configuration.ignore_all_errors, ["buck-out/dev/gen", "buck-out/dev/gen2"]
        )

        # Normalize number of workers if zero.
        json_load.side_effect = [{"typeshed": "TYPESHED/", "workers": 0}, {}]
        configuration = Configuration()
        self.assertEqual(configuration.typeshed, "TYPESHED/")
        self.assertEqual(configuration.number_of_workers, number_of_workers())

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
            Configuration_read.assert_has_calls(
                [call(CONFIGURATION_FILE + ".local"), call(CONFIGURATION_FILE)]
            )
            self.assertEqual(configuration.local_configuration, None)

        with patch.object(Configuration, "_read") as Configuration_read:
            configuration = Configuration(local_configuration="original")
            Configuration_read.assert_has_calls(
                [
                    call("original/" + CONFIGURATION_FILE + ".local"),
                    call(CONFIGURATION_FILE + ".local"),
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
                    call(CONFIGURATION_FILE + ".local"),
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
                    call(CONFIGURATION_FILE + ".local"),
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
                [
                    call("local/.some_configuration"),
                    call(CONFIGURATION_FILE + ".local"),
                    call(CONFIGURATION_FILE),
                ]
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
        sys.modules["typeshed"] = None

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
