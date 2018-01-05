# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import io
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
    arguments.current_directory = '.'
    arguments.original_directory = '/original/directory/'

    return arguments


def mock_configuration():
    configuration = MagicMock()
    configuration.source_directories = ['.']
    configuration.get_stub_roots = MagicMock()
    return configuration


class CommandTest(unittest.TestCase):
    def test_relative_path(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()

        self.assertEqual(
            commands.Command(
                arguments,
                configuration,
                [])._relative_path('/original/directory/path'),
            'path')
        self.assertEqual(
            commands.Command(
                arguments,
                configuration,
                [])._relative_path('/original/directory/'),
            '.')

    @patch('os.kill')
    def test_state(self, os_kill) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()

        state = commands.Command(arguments, configuration, [])._state()
        self.assertEqual(state.running, [])
        self.assertEqual(state.dead, [])

        with patch('builtins.open', mock_open()) as open:
            open.side_effect = [io.StringIO('1'), io.StringIO('derp')]
            state = commands.Command(
                arguments,
                configuration,
                ['link/tree/one', 'link/tree/two'])._state()
            self.assertEqual(state.running, ['link/tree/one'])
            self.assertEqual(state.dead, ['link/tree/two'])


class PersistentTest(unittest.TestCase):
    @patch.object(commands.Persistent, '_run_null_server', return_value=None)
    def test_persistent(self, run_null_server) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()

        # Check start without watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            commands.Persistent(
                arguments,
                configuration,
                source_directories=['.']).run()
            call_client.assert_called_once_with(
                command=commands.PERSISTENT,
                source_directories=['.'],
                capture_output=False)

        # Check start of null server.
        with patch.object(commands.Command, '_call_client') as call_client:
            call_client.side_effect = EnvironmentException('derp')
            arguments.no_watchman = True
            try:
                commands.Persistent(
                    arguments,
                    configuration,
                    source_directories=['.']).run()
            except (commands.ClientException, EnvironmentException):
                commands.Persistent(
                    arguments,
                    configuration,
                    source_directories=['.']).on_client_exception()
            call_client.assert_called_once_with(
                command=commands.PERSISTENT,
                source_directories=['.'],
                capture_output=False)
            run_null_server.assert_called_once()


class ErrorHandlingTest(unittest.TestCase):
    @patch.object(Error, '__init__', return_value=None)
    @patch.object(Error, '__hash__', return_value=0)
    def test_get_errors(self, error_hash, create_error) -> None:
        arguments = mock_arguments()
        directory = os.getcwd()
        arguments.original_directory = directory
        arguments.current_directory = directory[directory.find('/'):]
        configuration = mock_configuration()
        handler = commands.ErrorHandling(
            arguments,
            configuration,
            source_directories=['d/e', 'f', 'f/g'])
        result = MagicMock()
        error = MagicMock()
        error_dictionary = {'path': 'target'}
        error.__getitem__.side_effect = error_dictionary.__getitem__

        result.source_directory = 'f/g'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(False)])

        result.source_directory = 'h/i'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(False)])

        arguments.target = ['//f/g:target']
        configuration.targets = []
        handler = commands.ErrorHandling(
            arguments,
            configuration,
            source_directories=[])
        result.source_directory = 'f/g'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(False)])

        result.source_directory = 'h/i'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(True)])


class CheckTest(unittest.TestCase):
    def test_check(self) -> None:
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['stub', 'root']

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Check(
                arguments,
                configuration,
                source_directories=['.']).run()
            call_client.assert_called_once_with(
                command=commands.CHECK,
                source_directories=['.'],
                flags=['-stub-roots', 'stub,root'])


class IncrementalTest(unittest.TestCase):
    @patch.object(commands.Command, '_state')
    @patch.object(commands, 'Start')
    def test_incremental(self, commands_Start, commands_Command_state) -> None:
        state = MagicMock()
        state.running = ['running']
        state.dead = []
        commands_Command_state.return_value = state

        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['stub', 'root']

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Incremental(
                arguments,
                configuration,
                source_directories=['running']).run()
            call_client.assert_called_once_with(
                command=commands.INCREMENTAL,
                source_directories=['running'],
                flags=['-stub-roots', 'stub,root'])

        state.running = ['running']
        state.dead = ['dead']
        commands_Command_state.return_value = state

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Incremental(
                arguments,
                configuration,
                source_directories=['running', 'dead']).run()
            commands_Start.assert_called_with(
                arguments,
                configuration,
                ['dead'])
            call_client.assert_called_once_with(
                command=commands.INCREMENTAL,
                source_directories=['running', 'dead'],
                flags=['-stub-roots', 'stub,root'])


class StartTest(unittest.TestCase):
    def test_start(self) -> None:
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['root']

        # Check start without watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            commands.Start(
                arguments,
                configuration,
                source_directories=['.']).run()
            call_client.assert_called_once_with(
                command=commands.START,
                source_directories=['.'],
                flags=['-stub-roots', 'root'])

        # Check start with watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = False
            commands.Start(
                arguments,
                configuration,
                source_directories=['.']).run()
            call_client.assert_has_calls([
                call(
                    command=commands.WATCHMAN,
                    source_directories=['.'],
                    flags=['-daemonize']),
                call(
                    command=commands.START,
                    source_directories=['.'],
                    flags=['-stub-roots', 'root']),
            ])

        # Check start with terminal.
        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            arguments.terminal = True
            commands.Start(
                arguments,
                configuration,
                source_directories=['.']).run()
            call_client.assert_called_once_with(
                command=commands.START,
                source_directories=['.'],
                flags=['-terminal', '-stub-roots', 'root'])


class StopTest(unittest.TestCase):
    @patch.object(commands.Command, '_state')
    def test_restart(self, commands_Command_state) -> None:
        state = MagicMock()
        state.running = ['.']
        commands_Command_state.return_value = state

        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['root']

        # Check start without watchman.
        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Stop(
                arguments,
                configuration,
                source_directories=['.', 'not_running']).run()
            call_client.assert_called_once_with(
                command=commands.STOP,
                source_directories=['.'])

        state.running = []
        commands_Command_state.return_value = state
        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Stop(
                arguments,
                configuration,
                source_directories=['.', 'not_running']).run()
            call_client.assert_not_called()


class RestartTest(unittest.TestCase):
    @patch.object(commands.Command, '_state')
    def test_restart(self, commands_Command_state) -> None:
        state = MagicMock()
        state.running = ['.']
        commands_Command_state.return_value = state

        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_stub_roots.return_value = ['root']

        with patch.object(commands.Command, '_call_client') as call_client:
            arguments.no_watchman = True
            commands.Restart(
                arguments,
                configuration,
                source_directories=['.']).run()
            call_client.assert_has_calls(
                [
                    call(command=commands.STOP, source_directories=['.']),
                    call(
                        command=commands.START,
                        source_directories=['.'],
                        flags=['-stub-roots', 'root']),
                ],
                any_order=True)


class KillTest(unittest.TestCase):
    @patch('os.kill')
    @patch('os.path.exists')
    def test_kill(self, os_path_exists, os_kill) -> None:
        os_path_exists.result = True

        arguments = mock_arguments()
        configuration = mock_configuration()

        with patch(
                'tools.pyre.scripts.commands.open',
                mock_open(read_data='11')) as open:
            commands.Kill(
                arguments,
                configuration,
                source_directories=['/some/link/tree/']).run()
            open.assert_called_with(
                '/some/link/tree/.pyre/watchman/watchman.pid',
                'r')
            os_kill.assert_has_calls(
                [call(11, signal.SIGINT), call(11, signal.SIGINT)])
