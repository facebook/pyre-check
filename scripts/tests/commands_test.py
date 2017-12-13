# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import os
import unittest
import signal

from unittest.mock import call, patch, mock_open, MagicMock

from .. import (
    commands,
    EnvironmentException,
)
from ..error import Error


def mock_arguments():
    arguments = MagicMock()

    arguments.debug = False
    arguments.show_error_traces = False
    arguments.verbose = False
    arguments.logging_sections = None
    arguments.check_unannotated = False
    arguments.current_directory = '.'

    return arguments


def mock_configuration():
    configuration = MagicMock()
    configuration.link_trees = ['.']
    configuration.get_stub_roots = MagicMock()
    return configuration


class PersistentTest(unittest.TestCase):
    @patch.object(commands.Persistent, '_run_null_server', return_value=None)
    def test_persistent(self, run_null_server):
        arguments = mock_arguments()
        configuration = mock_configuration()

        # Check start without watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            commands.Persistent(
                arguments,
                configuration,
                link_trees=['.']).run()
            call_client.assert_called_once_with(
                command=commands.PERSISTENT,
                link_trees=['.'],
                capture_output=False)

        # Check start of null server.
        with patch.object(commands.Command, '_call_client') as call_client:
            call_client.side_effect = EnvironmentException('derp')
            arguments.no_watchman = True
            try:
                commands.Persistent(
                    arguments,
                    configuration,
                    link_trees=['.']).run()
            except (commands.ClientException, EnvironmentException):
                commands.Persistent(
                    arguments,
                    configuration,
                    link_trees=['.']).on_client_exception()
            call_client.assert_called_once_with(
                command=commands.PERSISTENT,
                link_trees=['.'],
                capture_output=False)
            run_null_server.assert_called_once()


class ErrorHandlingTest(unittest.TestCase):
    @patch.object(Error, '__init__', return_value=None)
    @patch.object(Error, '__hash__', return_value=0)
    def test_get_errors(self, error_hash, create_error):
        arguments = mock_arguments()
        directory = os.getcwd()
        arguments.original_directory = directory
        arguments.current_directory = directory[directory.find('/'):]
        configuration = mock_configuration()
        handler = commands.ErrorHandling(
            arguments,
            configuration,
            link_trees=['d/e', 'f', 'f/g'])
        result = MagicMock()
        error = MagicMock()
        error_dictionary = {'path': 'target'}
        error.__getitem__.side_effect = error_dictionary.__getitem__

        result.link_tree = 'f/g'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(False)])

        result.link_tree = 'h/i'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(False)])

        arguments.target = ['//f/g:target']
        configuration.targets = []
        handler = commands.ErrorHandling(
            arguments,
            configuration,
            link_trees=[])
        result.link_tree = 'f/g'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(False)])

        result.link_tree = 'h/i'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(True)])


class CheckTest(unittest.TestCase):
    def test_check(self):
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['stub', 'root']

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Check(arguments, configuration, link_trees=['.']).run()
            call_client.assert_called_once_with(
                command=commands.CHECK,
                link_trees=['.'],
                flags=['-stub-roots', 'stub,root'])


class CheckIncremental(unittest.TestCase):
    def test_incremental(self):
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['stub', 'root']

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Incremental(
                arguments,
                configuration,
                link_trees=['.']).run()
            call_client.assert_called_once_with(
                command=commands.INCREMENTAL,
                link_trees=['.'],
                flags=['-stub-roots', 'stub,root'])


class StartTest(unittest.TestCase):
    def test_start(self):
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['root']

        # Check start without watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            commands.Start(arguments, configuration, link_trees=['.']).run()
            call_client.assert_called_once_with(
                command=commands.START,
                link_trees=['.'],
                flags=['-stub-roots', 'root'])

        # Check start with watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = False
            commands.Start(arguments, configuration, link_trees=['.']).run()
            call_client.assert_has_calls([
                call(
                    command=commands.WATCHMAN,
                    link_trees=['.'],
                    flags=['-daemonize']),
                call(
                    command=commands.START,
                    link_trees=['.'],
                    flags=['-stub-roots', 'root']),
            ])

        # Check start with terminal.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            arguments.terminal = True
            commands.Start(arguments, configuration, link_trees=['.']).run()
            call_client.assert_called_once_with(
                command=commands.START,
                link_trees=['.'],
                flags=['-terminal', '-stub-roots', 'root'])


class RestartTest(unittest.TestCase):
    def test_restart(self):
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['root']

        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            commands.Restart(arguments, configuration, link_trees=['.']).run()
            call_client.assert_has_calls(
                [
                    call(command=commands.STOP,
                         link_trees=['.']),
                    call(command=commands.START,
                         link_trees=['.'],
                         flags=['-stub-roots', 'root']),

                ],
                any_order=True)


class KillTest(unittest.TestCase):
    @patch('os.kill')
    @patch('os.path.exists')
    def test_kill(self, os_path_exists, os_kill):
        os_path_exists.result = True

        arguments = mock_arguments()
        configuration = mock_configuration()

        with patch(
                'tools.pyre.scripts.commands.open',
                mock_open(read_data='11')) as open:
            commands.Kill(
                arguments,
                configuration,
                link_trees=['/some/link/tree/']).run()
            open.assert_called_with(
                '/some/link/tree/.pyre/watchman/watchman.pid',
                'r')
            os_kill.assert_has_calls(
                [call(11, signal.SIGINT), call(11, signal.SIGINT)])
