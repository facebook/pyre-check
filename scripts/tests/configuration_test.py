# Copyright 2004-present Facebook.  All rights reserved.

import unittest

from unittest.mock import patch

from tools.pyre.scripts.configuration import Configuration


class ConfigurationTest(unittest.TestCase):
    @patch('json.load')
    def test_init(self, json_load):
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
