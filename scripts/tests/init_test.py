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

    def test_resolve_source_directories(self) -> None:
        arguments = MagicMock()
        arguments.source_directory = []
        arguments.original_directory = '/root'
        arguments.build = False
        configuration = MagicMock()
        configuration.source_directories = []

        with self.assertRaises(EnvironmentException):
            resolve_source_directories(arguments, configuration)

        # Arguments override configuration.
        with patch.object(
                buck,
                'generate_source_directories',
                return_value=[]) as buck_source_directories:
            arguments.source_directory = ['arguments_source_directory']
            configuration.source_directories = [
                'configuration_source_directory',
            ]

            source_directories = resolve_source_directories(
                arguments,
                configuration)
            buck_source_directories.assert_called_with(set(), build=False)
            self.assertEqual(source_directories, {'arguments_source_directory'})

        with patch.object(
                buck,
                'generate_source_directories',
                return_value=['arguments_target']) as buck_source_directories:
            arguments.source_directory = []
            arguments.target = ['arguments_target']
            configuration.source_directories = [
                'configuration_source_directory',
            ]

            source_directories = resolve_source_directories(
                arguments,
                configuration)
            buck_source_directories.assert_called_with(
                {'arguments_target'},
                build=False)
            self.assertEqual(source_directories, {'arguments_target'})

        return

        # Configuration is picked up when no arguments provided.
        with patch.object(
                buck,
                'generate_source_directories',
                return_value=[]) as buck_source_directories:
            arguments.source_directory = []
            arguments.target = []
            arguments.build = True
            configuration.targets = ['configuration_target']
            configuration.source_directories = [
                'configuration_source_directory',
            ]

            source_directories = resolve_source_directories(
                arguments,
                configuration)
            buck_source_directories.assert_called_with(
                {'configuration_target'},
                build=True)
            self.assertEqual(
                source_directories,
                {'configuration_target', 'configuration_source_directory'})
