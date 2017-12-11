# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest

from unittest.mock import patch, MagicMock

from .. import (
    buck,
    EnvironmentException,
    find_project_root,
    resolve_link_trees,
)


class InitTest(unittest.TestCase):
    @patch('os.path.isfile')
    def test_find_configuration(self, os_mock_isfile):
        os_mock_isfile.side_effect = [False, False, False, True]
        self.assertEqual(
            find_project_root("/a/b/c/d"),
            "/a")
        os_mock_isfile.side_effect = [True]
        self.assertEqual(
            find_project_root("/a"),
            "/a")
        os_mock_isfile.side_effect = [False, False]
        self.assertEqual(
            find_project_root("/a/b"),
            "/a/b")

    @patch.object(buck, 'generate_link_trees')
    def test_resolve_link_trees(self, buck_link_trees):
        arguments = MagicMock()
        arguments.link_tree = []
        configuration = MagicMock()
        configuration.link_trees = []
        buck_link_trees.return_value = []

        with self.assertRaises(EnvironmentException):
            resolve_link_trees(arguments, configuration)

        arguments.link_tree = ['argument_link_tree']
        buck_link_trees.return_value = ['buck_link_tree']
        self.assertEqual(
            resolve_link_trees(arguments, configuration),
            set(['argument_link_tree', 'buck_link_tree']))

        configuration.link_trees = ['configuration_link_tree']
        self.assertEqual(
            resolve_link_trees(arguments, configuration),
            set([
                'argument_link_tree',
                'buck_link_tree',
                'configuration_link_tree',
            ]))

        arguments.target = None
        self.assertEqual(
            resolve_link_trees(arguments, configuration),
            set([
                'argument_link_tree',
                'buck_link_tree',
                'configuration_link_tree',
            ]))
