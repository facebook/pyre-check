# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import unittest

from unittest.mock import patch

from ..configuration import Configuration  # noqa


class ConfigurationTest(unittest.TestCase):
    @patch('json.load')
    @patch.object(os, 'getenv', return_value=None)
    def test_init(self, os_environ, json_load) -> None:
        json_load.side_effect = [
            {"link_trees": ["a"], "logger": "/usr/logger"},
            {},
        ]

        configuration = Configuration()
        self.assertEqual(configuration.link_trees, ["a"])
        self.assertEqual(configuration.targets, [])
        self.assertEqual(configuration.logger, "/usr/logger")

        json_load.side_effect = [
            {"targets": ["//a/b/c"], "disabled": 1},
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.targets, ["//a/b/c"])
        self.assertEqual(configuration.link_trees, [])
        self.assertEqual(configuration.get_version_hash(), None)
        self.assertEqual(configuration.logger, None)
        self.assertTrue(configuration.disabled)

        json_load.side_effect = [
            {"typeshed": "TYPESHED/"},
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.get_stub_roots(), ['TYPESHED/'])

        json_load.side_effect = [
            {
                "additional_stub_roots": ["additional/"],
                "version": "VERSION",
                "typeshed": "TYPE/%V/SHED/",
            },
            {},
        ]
        configuration = Configuration()
        self.assertEqual(
            configuration.get_stub_roots(),
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
