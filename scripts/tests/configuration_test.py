# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import unittest

from unittest.mock import (
    call,
    patch,
)

from .. import CONFIGURATION_FILE
from ..configuration import Configuration  # noqa


class ConfigurationTest(unittest.TestCase):
    @patch('json.load')
    @patch.object(os, 'getenv', return_value=None)
    def test_init(self, os_environ, json_load) -> None:
        json_load.side_effect = [
            {"source_directories": ["a"], "logger": "/usr/logger"},
            {},
        ]

        configuration = Configuration()
        self.assertEqual(configuration.source_directories, ["a"])
        self.assertEqual(configuration.targets, [])
        self.assertEqual(configuration.logger, "/usr/logger")

        json_load.side_effect = [
            {"targets": ["//a/b/c"], "disabled": 1},
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.targets, ["//a/b/c"])
        self.assertEqual(configuration.source_directories, [])
        self.assertEqual(configuration.get_version_hash(), None)
        self.assertEqual(configuration.logger, None)
        self.assertTrue(configuration.disabled)

        json_load.side_effect = [
            {"typeshed": "TYPESHED/"},
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.get_search_path(), ['TYPESHED/'])

        json_load.side_effect = [
            {
                "search_path": ["additional/"],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
            },
            {},
        ]
        configuration = Configuration()
        self.assertEqual(
            configuration.get_search_path(),
            ['additional/', 'TYPE/VERSION/SHED/'])

        json_load.side_effect = [
            {"binary": "/binary"},
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.get_binary(), '/binary')

        json_load.side_effect = [
            {"version": "VERSION", "binary": "/%V/binary"},
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.get_binary(), '/VERSION/binary')

        with patch.object(os, 'getenv', return_value='VERSION_HASH'):
            json_load.side_effect = [{}, {}]
            configuration = Configuration()
            self.assertEqual(configuration.get_version_hash(), 'VERSION_HASH')

    @patch('os.path.isfile')
    def test_configurations(self, os_path_isfile) -> None:
        os_path_isfile.return_value = False

        with patch.object(Configuration, '_read') as Configuration_read:
            Configuration()
            Configuration_read.assert_has_calls([
                call(CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE),
            ])

        with patch.object(Configuration, '_read') as Configuration_read:
            Configuration(original_directory='original')
            Configuration_read.assert_has_calls([
                call('original/' + CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE),
            ])
        with patch.object(Configuration, '_read') as Configuration_read:
            Configuration(local_configuration='local')
            Configuration_read.assert_has_calls([
                call('local/' + CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE),
            ])
        with patch.object(Configuration, '_read') as Configuration_read:
            Configuration(
                original_directory='original',
                local_configuration='local')
            Configuration_read.assert_has_calls([
                call('local/' + CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE),
            ])

        os_path_isfile.return_value = True
        with patch.object(Configuration, '_read') as Configuration_read:
            Configuration(local_configuration='local/.some_configuration')
            Configuration_read.assert_has_calls([
                call('local/.some_configuration'),
                call(CONFIGURATION_FILE + '.local'),
                call(CONFIGURATION_FILE),
            ])
