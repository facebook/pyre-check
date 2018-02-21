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
    shared_source_directory,
    EnvironmentException,
)
from ..error import Error


def mock_arguments():
    arguments = MagicMock()

    arguments.debug = False
    arguments.show_error_traces = False
    arguments.verbose = False
    arguments.logging_sections = None
    arguments.log_identifier = None
    arguments.current_directory = '.'
    arguments.original_directory = '/original/directory/'

    return arguments


def mock_configuration():
    configuration = MagicMock()
    configuration.source_directories = ['.']
    configuration.get_search_path = MagicMock()
    return configuration


class CommandTest(unittest.TestCase):
    def test_relative_path(self) -> None:
        arguments = mock_arguments()
        configuration = mock_configuration()

        self.assertEqual(
            commands.Command(
                arguments,
                configuration, [])._relative_path('/original/directory/path'),
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
    @patch('tools.pyre.scripts.shared_source_directory.merge')
    @patch.object(commands.Persistent, '_run_null_server', return_value=None)
    def test_persistent(self, run_null_server, merge_directories) -> None:
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
            commands.Persistent(
                arguments,
                configuration,
                source_directories=["first", "second"]).run()
            call_client.assert_called_with(
                command=commands.PERSISTENT,
                source_directories=['.pyre/shared_source_directory'],
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
            call_client.assert_has_calls(
                [
                    call(command=commands.PERSISTENT,
                         source_directories=['.'],
                         capture_output=False),
                    call(command=commands.START,
                         flags=['-project-root', '.', '-stub-roots', ''],
                         source_directories=['.']),
                ])
            run_null_server.assert_called_once()

        # Check null server initialize output
        command = commands.Persistent(
            arguments,
            configuration,
            source_directories=['.'])
        self.assertEqual(
            command._initialize_response(5),
            "Content-Length: 59\r\n\r\n"
            '{"jsonrpc": "2.0", "id": 5, "result": {"capabilities": {}}}\r\n'
        )

    @patch('select.select')
    @patch('sys.stdout', new_callable=io.StringIO)
    @patch.object(commands.Command, '_call_client')
    def test_null_server(self, call_client, stdout, select) -> None:
        input = """
        {
            "jsonrpc": "2.0",
            "id": 0,
            "method": "initialize",
            "params": {
              "processId": 2588352,
              "rootPath": "/data/users/a/instagram/instagram-server",
              "rootUri": "file:///data/users/a/instagram/instagram-server",
              "capabilities": {
                "workspace": {
                  "applyEdit": true
                }
              }
            }
        }
        """
        stdin = io.StringIO(
            "Content-Length: {}\r\n\r\n{}\r\n".format(len(input), input))

        select.return_value = ([stdin], [], [])
        # Check null server output when a valid input is given.
        commands.Persistent(
            mock_arguments(),
            mock_configuration(),
            source_directories=['.'],
        )._run_null_server(timeout=0)
        json = '{"jsonrpc": "2.0", "id": 0, "result": {"capabilities": {}}}'
        self.assertEqual(
            stdout.getvalue(),
            "Content-Length: 59\r\n\r\n{}\r\n".format(json))


class ErrorHandlingTest(unittest.TestCase):
    @patch.object(Error, '__init__', return_value=None)
    @patch.object(Error, '__hash__', return_value=0)
    def test_get_errors(self, error_hash, create_error) -> None:
        arguments = mock_arguments()
        arguments.original_directory = '/test'
        arguments.current_directory = '/'
        configuration = mock_configuration()
        handler = commands.ErrorHandling(
            arguments,
            configuration,
            source_directories=[])
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

        arguments.original_directory = '/f/g/target'
        arguments.target = ['//f/g:target']
        configuration.targets = []
        handler = commands.ErrorHandling(
            arguments,
            configuration,
            source_directories=[])

        result.source_directory = 'h/i'
        with patch.object(json, 'loads', return_value=[error]):
            handler._get_errors([result])
            handler._get_errors([result], True)
            create_error.assert_has_calls([call(False), call(False)])


class CheckTest(unittest.TestCase):
    @patch('subprocess.check_output')
    @patch('os.path.realpath')
    def test_check(self, realpath, check_output) -> None:
        realpath.side_effect = lambda x: x
        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ['stub', 'root']

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Check(
                arguments,
                configuration,
                source_directories=['.']).run()
            call_client.assert_called_once_with(
                command=commands.CHECK,
                source_directories=['.'],
                flags=['-project-root', '.', '-stub-roots', 'stub,root'])

        with patch.object(commands.Command, '_call_client') as call_client:
            check_output.return_value = b""
            commands.Check(
                arguments,
                configuration,
                source_directories=['roofus', 'dufus']).run()
            call_client.assert_called_once_with(
                command=commands.CHECK,
                source_directories=[
                    '.pyre/shared_source_directory_{}'.format(os.getpid()),
                ],
                flags=['-project-root', '.', '-stub-roots', 'stub,root'])


class IncrementalTest(unittest.TestCase):
    @patch('tools.pyre.scripts.shared_source_directory.missing')
    @patch('tools.pyre.scripts.shared_source_directory.merge')
    @patch.object(commands.Command, '_state')
    @patch.object(commands, 'Start')
    @patch.object(commands, 'Stop')
    def test_incremental(
            self,
            commands_Stop,
            commands_Start,
            commands_Command_state,
            merge,
            missing) -> None:
        missing.return_value = []
        state = MagicMock()
        state.running = ['running']
        state.dead = []
        commands_Command_state.return_value = state

        arguments = mock_arguments()

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ['stub', 'root']

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Incremental(
                arguments,
                configuration,
                source_directories=['running']).run()
            call_client.assert_called_once_with(
                command=commands.INCREMENTAL,
                source_directories=['running'],
                flags=['-project-root', '.', '-stub-roots', 'stub,root'])

        state.running = ['running']
        state.dead = ['dead']
        missing.return_value = ['dead']
        commands_Command_state.return_value = state

        with patch.object(commands.Command, '_call_client') as call_client:
            commands.Incremental(
                arguments,
                configuration,
                source_directories=['running', 'dead']).run()
            commands_Stop.assert_called_with(
                arguments,
                configuration,
                ['.pyre/shared_source_directory'])

            commands_Start.assert_called_with(
                arguments,
                configuration,
                ['running', 'dead'])
            call_client.assert_has_calls(
                [
                    call(command=commands.INCREMENTAL,
                         source_directories=['.pyre/shared_source_directory'],
                         flags=[
                             '-project-root',
                             '.',
                             '-stub-roots',
                             'stub,root']),
                ],
                any_order=True)


class StartTest(unittest.TestCase):
    @patch('fcntl.lockf')
    @patch('tools.pyre.scripts.shared_source_directory.merge')
    def test_start(self, merge_directories, lock_file) -> None:
        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ['root']

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
                flags=['-project-root', '.', '-stub-roots', 'root'])

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
                    flags=['-project-root', '.', '-daemonize']),
                call(
                    command=commands.START,
                    source_directories=['.'],
                    flags=['-project-root', '.', '-stub-roots', 'root']),
            ])

        # Check start with multiple source directories
        with patch('builtins.open', mock_open()) as open:
            with patch.object(commands.Command, '_call_client') as call_client:
                arguments.no_watchman = False
                commands.Start(
                    arguments,
                    configuration,
                    source_directories=['x', 'y']).run()
                call_client.assert_has_calls([
                    call(
                        command=commands.WATCHMAN,
                        source_directories=['.pyre/shared_source_directory'],
                        flags=['-project-root', '.', '-daemonize']),
                    call(
                        command=commands.START,
                        source_directories=['.pyre/shared_source_directory'],
                        flags=['-project-root', '.', '-stub-roots', 'root']),
                ])
                merge_directories.assert_called_once()
                open().write.assert_has_calls([call('x\n'), call('y\n')])


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
                flags=[
                    '-project-root',
                    '.',
                    '-terminal',
                    '-stub-roots',
                    'root'])


class StopTest(unittest.TestCase):
    @patch.object(commands.Command, '_state')
    def test_restart(self, commands_Command_state) -> None:
        state = MagicMock()
        state.running = ['.']
        commands_Command_state.return_value = state

        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ['root']

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
    @patch.object(commands, 'Stop')
    @patch.object(commands, 'Start')
    def test_restart(self, commands_Start, commands_Stop) -> None:
        state = MagicMock()
        state.running = ['.']

        arguments = mock_arguments()
        arguments.terminal = False

        configuration = mock_configuration()
        configuration.get_search_path.return_value = ['root']

        source_directories = ['.']

        with patch.object(commands, 'Stop') as commands_Stop, \
                patch.object(commands, 'Start') as commands_Start, \
                patch.object(commands, 'Incremental') as commands_Incremental:
            commands.Restart(
                arguments,
                configuration,
                source_directories,
                blocking=False)._run()
            commands_Stop.assert_called_with(
                arguments,
                configuration,
                source_directories)
            commands_Start.assert_called_with(
                arguments,
                configuration,
                source_directories)
            commands_Incremental.assert_not_called()

        with patch.object(commands, 'Stop') as commands_Stop, \
                patch.object(commands, 'Start') as commands_Start, \
                patch.object(commands, 'Incremental') as commands_Incremental:
            commands.Restart(
                arguments,
                configuration,
                source_directories)._run()
            commands_Stop.assert_called_with(
                arguments,
                configuration,
                source_directories)
            commands_Incremental.assert_called_with(
                arguments,
                configuration,
                source_directories)
            commands_Start.assert_not_called()


class KillTest(unittest.TestCase):
    @patch('os.kill')
    @patch('os.path.exists')
    def test_kill(self, os_path_exists, os_kill) -> None:
        os_path_exists.result = True

        arguments = mock_arguments()
        arguments.with_fire = False
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

    @patch('os.path.realpath')
    @patch('subprocess.run')
    def test_kill_all(self, run, realpath) -> None:
        realpath.side_effect = ["/test-binary"]
        arguments = mock_arguments()
        arguments.with_fire = True
        configuration = mock_configuration()
        commands.Kill(
            arguments,
            configuration,
            source_directories=[]).run()
        run.assert_called_with(
            ['pkill', '-f', '^/test-binary'])
