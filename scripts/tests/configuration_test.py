# Copyright 2004-present Facebook.  All rights reserved.

import unittest

from unittest.mock import patch

from tools.pyre.scripts.configuration import Configuration


class ConfigurationTest(unittest.TestCase):
    @patch('json.load')
    def test_init(self, json_load):
        json_load.side_effect = [{"link_trees": ["a"]}, {}]

        configuration = Configuration()
        self.assertEqual(configuration.link_trees, ["a"])
        self.assertEqual(configuration.targets, [])

        json_load.side_effect = [
            {"targets": ["//a/b/c"], "disabled": 1},
            {},
        ]
        configuration = Configuration()
        self.assertEqual(configuration.targets, ["//a/b/c"])
        self.assertEqual(configuration.link_trees, [])
        self.assertEqual(configuration.version_hash, None)
        self.assertTrue(configuration.disabled)
