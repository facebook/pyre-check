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
    resolve_source_directories,
)


class InitTest(unittest.TestCase):
    @patch('os.path.isfile')
    def test_find_configuration(self, os_mock_isfile) -> None:
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
    def test_resolve_source_directories(self, buck_source_directories) -> None:
        arguments = MagicMock()
        arguments.source_directory = []
        arguments.original_directory = '/root'
        configuration = MagicMock()
        configuration.source_directories = []
        buck_source_directories.return_value = []

        with self.assertRaises(EnvironmentException):
            resolve_source_directories(arguments, configuration)

        arguments.source_directory = ['argument_source_directory']
        buck_source_directories.return_value = ['buck_source_directory']
        self.assertEqual(
            resolve_source_directories(arguments, configuration),
            set(['argument_source_directory', 'buck_source_directory']))

        configuration.source_directories = ['configuration_source_directory']
        self.assertEqual(
            resolve_source_directories(arguments, configuration),
            set([
                'argument_source_directory',
                'buck_source_directory',
            ]))

        arguments.target = None
        self.assertEqual(
            resolve_source_directories(arguments, configuration),
            set([
                'argument_source_directory',
                'buck_source_directory',
            ]))

        arguments.target = None
        arguments.source_directory = None
        self.assertEqual(
            resolve_source_directories(arguments, configuration),
            set([
                'configuration_source_directory',
                'buck_source_directory',
            ]))
