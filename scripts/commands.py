# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import json
import logging
import os
import re
import select
import shutil
import signal
import subprocess
import sys
import threading
import time
import traceback

from collections import namedtuple
from typing import List, Optional

from . import (
    buck,
    shared_source_directory,
    EnvironmentException,
    log,
    log_statistics,
    SUCCESS,
    TEXT,
)
from .error import Error


LOG = logging.getLogger(__name__)

CHECK = 'check'
RAGE = 'rage'
INCREMENTAL = 'incremental'
KILL = 'kill'
PERSISTENT = 'persistent'
RESTART = 'restart'
START = 'start'
STOP = 'stop'
WATCHMAN = 'watchman'


class ClientException(Exception):
    pass


class Result:
    def __init__(self, source_directory, code, output) -> None:
        self.source_directory = source_directory
        self.code = code
        self.output = output

    def check(self) -> None:
        if self.code != SUCCESS:
            raise ClientException(
                'Client exited with error code {}:'
                '\n{}'.format(self.code, self.output))


class Command:
    _buffer: List[str] = []
    _call_client_terminated: bool = False

    def __init__(
            self,
            arguments,
            configuration,
            source_directories,
            should_merge_directories=False) -> None:
        self._arguments = arguments
        self._configuration = configuration

        self._source_directories = source_directories
        self._debug = arguments.debug
        self._strict = arguments.strict
        self._show_error_traces = arguments.show_error_traces
        self._verbose = arguments.verbose
        self._logging_sections = arguments.logging_sections

        self._original_directory = arguments.original_directory
        self._current_directory = arguments.current_directory
        self._should_merge_directories = should_merge_directories

        self._original_source_directories = self._source_directories
        self._shared_directory_name = shared_source_directory\
            .get_directory_name()

    def _run(self) -> None:
        pass

    def run(self) -> None:
        if len(self._source_directories) > 1:
            if self._should_merge_directories:
                shared_source_directory.merge(
                    self._shared_directory_name,
                    self._source_directories)
            self._source_directories = [self._shared_directory_name]
        self._run()

    def _flags(self):
        flags = []
        if self._debug:
            flags.extend(['-debug', '-sequential'])
        if self._strict:
            flags.extend(['-strict'])
        if self._show_error_traces:
            flags.append('-show-error-traces')
        if self._verbose:
            flags.append('-verbose')
        if self._logging_sections:
            flags.extend(['-logging-sections', self._logging_sections])
        if self._current_directory:
            flags.extend(['-project-root', self._current_directory])
        return flags

    def _read_stdout(self, stdout) -> None:
        self._buffer = []
        for line in stdout:
            self._buffer.append(line.decode())

    def _read_stderr(self, stream, _source_directory) -> None:
        buffer = None
        log_pattern = re.compile(
            r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} (\w+) (.*)')
        try:
            for line in stream:
                if self._call_client_terminated:
                    return
                line = line.decode().rstrip()
                match = log_pattern.match(line)
                if match:
                    if buffer:
                        buffer.flush()
                    buffer = log.Buffer(
                        section=match.groups()[0],
                        data=[match.groups()[1]])
                elif buffer:
                    buffer.append(line)
            if buffer:
                buffer.flush()
        except Exception:
            pass

    def _call_client(
            self,
            command,
            source_directories,
            flags=None,
            capture_output: bool = True):
        if not flags:
            flags = []

        results = []
        for source_directory in source_directories:
            if not os.path.isdir(source_directory):
                raise EnvironmentException(
                    '`{}` is not a link tree.'.format(source_directory))

            client_command = [
                self._configuration.get_binary(),
                command,
            ]
            client_command.extend(flags)
            client_command.append(source_directory)

            LOG.debug('Running `%s`', ' '.join(client_command))
            with subprocess.Popen(
                    client_command,
                    stdout=subprocess.PIPE if capture_output else None,
                    stderr=subprocess.PIPE) as process:

                # Read stdout output
                if capture_output:
                    stdout_reader = threading.Thread(
                        target=self._read_stdout,
                        args=(process.stdout,))
                    stdout_reader.daemon = True
                    stdout_reader.start()

                # Read the error output and print it.
                self._call_client_terminated = False
                stderr_reader = threading.Thread(
                    target=self._read_stderr,
                    args=(process.stderr, source_directory))
                stderr_reader.daemon = True
                stderr_reader.start()

                # Wait for the process to finish and clean up.
                process.wait()
                self._call_client_terminated = True
                if capture_output:
                    stdout_reader.join()

                output = ''
                if capture_output:
                    output = "\n".join(self._buffer)
                if '[' in output:
                    output = output[output.index('['):]
                if process.returncode != 0 and capture_output:
                    output = "".join(self._buffer)

                results.append(
                    Result(
                        source_directory=source_directory,
                        code=process.returncode,
                        output=output))
        return results

    def _check_results(self, results) -> None:
        for result in results:
            result.check()

    def _relative_path(self, path) -> str:
        return os.path.relpath(path, self._original_directory)

    def _state(self):
        running = []
        dead = []

        for source_directory in self._source_directories:
            pid_path = os.path.join(source_directory, '.pyre/server/server.pid')
            try:
                with open(pid_path) as file:
                    pid = int(file.read())
                    os.kill(pid, 0)  # throws if process is not running
                running.append(source_directory)
            except Exception:
                dead.append(source_directory)

        return State(running, dead)

    def _server_string(self, source_directories=None):
        if not source_directories:
            source_directories = self._source_directories
        return "server{}".format('' if len(source_directories) < 2 else 's')

    def _source_directory_string(self, source_directories=None):
        if not source_directories:
            source_directories = self._source_directories
        return ',\n   '.join(
            '`{}`'.format(self._relative_path(source_directory))
            for source_directory
            in source_directories)

    def _variable_spacing_string(self, newline=False):
        return '\n   ' if newline else ' '

    def on_client_exception(self) -> None:
        pass


class Persistent(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Persistent, self).__init__(
            arguments,
            configuration,
            source_directories)

    def _run(self) -> None:
        arguments = self._arguments
        try:
            results = self._call_client(
                command=PERSISTENT,
                source_directories=self._source_directories,
                capture_output=False)
            self._check_results(results)
        except (ClientException,
                EnvironmentException,
                subprocess.CalledProcessError):
            arguments.terminal = False
            Restart(
                arguments,
                self._configuration,
                self._original_source_directories,
                blocking=False).run()

            if len(self._source_directories) > 1:
                source_directories = [
                    shared_source_directory.get_directory_name(),
                ]
            else:
                source_directories = self._source_directories
            results = self._call_client(
                command=PERSISTENT,
                source_directories=source_directories,
                capture_output=False)
            self._check_results(results)

    def on_client_exception(self) -> None:
        self._run_null_server(timeout=300)

    def _initialize_response(self, request_id: int) -> str:
        response = json.dumps({
            "jsonrpc": "2.0",
            "id": request_id,
            "result": {"capabilities": {}},
        })
        return "Content-Length: {}\r\n\r\n{}\r\n".format(
            len(response),
            response)

    def _run_null_server(self, timeout: Optional[int] = None) -> None:
        log_statistics(
            'perfpipe_pyre_events',
            self._arguments,
            self._configuration,
            normals={'name': 'null_server_launch'})
        to_read, _, _ = select.select([sys.stdin], [], [], 3.0)
        request_id = 0
        if to_read:
            standard_input = to_read[0]
            # Read content of the form Content-Length:n\r\n\r\n{jsonmessage}
            line = standard_input.readline()
            match = re.match(r"Content-Length: (?P<bytes>[0-9]+)", line)
            if match:
                length = int(match.group('bytes'))
                standard_input.readline()
                serialized_json = standard_input.read(length)
                try:
                    parsed = json.loads(serialized_json)
                    request_id = parsed["id"]
                # This is a catch-all to ensure that the null server always
                # gets spawned.
                except Exception:
                    pass

        sys.stdout.write(self._initialize_response(request_id))
        sys.stdout.flush()
        start_time = int(time.time())
        while True:
            if timeout is not None and\
               timeout <= (int(time.time()) - start_time):
                break
            time.sleep(10)


class ErrorHandling(Command):
    def __init__(
            self,
            arguments,
            configuration,
            source_directories,
            should_merge_directories=False) -> None:
        super(ErrorHandling, self).__init__(
            arguments,
            configuration,
            source_directories,
            should_merge_directories)
        self._verbose = arguments.verbose
        self._output = arguments.output
        self._local_paths = {
            buck.presumed_target_root(target)
            for target
            in configuration.targets + (arguments.target or [])
        }

    def _print(self, errors):
        if not self._verbose:
            errors = [error for error in errors if not error.is_external()]

        if errors:
            length = len(errors)
            LOG.error(
                "Found %d type error%s!", length, "s" if length > 1 else "")
        else:
            LOG.log(log.SUCCESS, "No type errors found")

        if self._output == TEXT:
            log.stdout.write(
                '\n'.join([repr(error) for error in sorted(list(errors))]))
        else:
            log.stdout.write(json.dumps([error.__dict__ for error in errors]))

    def _get_errors(self, results, exclude_dependencies: bool = False):
        errors = set()

        for result in results:
            result.check()

            try:
                results = json.loads(result.output)
            except (json.JSONDecodeError, ValueError):
                raise ClientException(
                    'Invalid output: `{}`.'.format(result.output))

            for error in results:
                path = os.path.realpath(
                    os.path.join(result.source_directory, error['path']))
                external = True
                internal_paths = [self._current_directory]
                if exclude_dependencies and len(self._local_paths) > 0:
                    internal_paths = map(
                        lambda path: self._original_directory + '/' + path,
                        self._local_paths)

                for internal_path in internal_paths:
                    if path.startswith(internal_path):
                        external = False
                        # Relativize path.
                        error['path'] = self._relative_path(path)
                errors.add(Error(external, **error))

        return errors


class Check(ErrorHandling):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Check, self).__init__(
            arguments,
            configuration,
            source_directories,
            should_merge_directories=True)
        self._log_identifier = arguments.log_identifier
        self._shared_directory_name = (
            shared_source_directory.get_directory_name(suffix=str(os.getpid())))

    def _run(self, retries: int = 1) -> None:
        flags = self._flags()
        flags.extend([
            '-stub-roots',
            ','.join(self._configuration.get_search_path()),
        ])
        if self._log_identifier:
            flags.extend(['-log-identifier', self._log_identifier])
        results = self._call_client(
            command=CHECK,
            source_directories=self._source_directories,
            flags=flags)
        errors = self._get_errors(results)
        try:
            shutil.rmtree(self._shared_directory_name)
        except OSError:
            pass
        self._print(errors)


class Incremental(ErrorHandling):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Incremental, self).__init__(
            arguments,
            configuration,
            source_directories)

    def _read_stderr(self, _stream, source_directory):
        stderr_file = os.path.join(
            source_directory,
            '.pyre/server/server.stdout')
        with subprocess.Popen(
                ['tail', '-f', stderr_file],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL) as stderr_tail:
            super(Incremental, self)._read_stderr(
                stderr_tail.stdout,
                source_directory)

    def _run(self) -> None:
        if len(self._original_source_directories) > 1:
            missing = shared_source_directory.missing(
                self._original_source_directories)
            if missing:
                LOG.info(
                    "The existing pyre server which doesn't analyze the"
                    " following source directories:\n{}".format(
                        "\n".join(missing)))
                Stop(
                    self._arguments,
                    self._configuration,
                    self._source_directories).run()
        dead = self._state().dead
        if dead:
            LOG.warning(
                '%s not running at%s%s.%sStarting...',
                self._server_string(dead).capitalize(),
                self._variable_spacing_string(
                    len(self._original_source_directories) > 1),
                self._source_directory_string(self._original_source_directories),
                self._variable_spacing_string(
                    len(self._original_source_directories) > 1))
            arguments = self._arguments
            arguments.terminal = False
            arguments.no_watchman = False
            Start(
                arguments,
                self._configuration,
                self._original_source_directories).run()

        flags = self._flags()
        flags.extend([
            '-stub-roots',
            ','.join(self._configuration.get_search_path())
        ])

        if dead:
            LOG.warning("Server initializing...")
        else:
            LOG.warning("Waiting for server...")

        results = self._call_client(
            command=INCREMENTAL,
            source_directories=self._source_directories,
            flags=flags)

        try:
            self._check_results(results)
            errors = self._get_errors(results)
            self._print(errors)
        except ClientException as exception:
            LOG.error("%s", str(exception))
            if log.get_yes_no_input("Restart the server?"):
                arguments = self._arguments
                Stop(
                    arguments,
                    self._configuration,
                    self._source_directories).run()
                self.run()
            else:
                exit(-1)


class Rage(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Rage, self).__init__(arguments, configuration, source_directories)
        self._arguments.command = RAGE

    def _run(self) -> None:
        results = self._call_client(
            command=RAGE,
            source_directories=self._source_directories,
            capture_output=False)
        self._check_results(results)


class Start(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Start, self).__init__(
            arguments,
            configuration,
            source_directories,
            should_merge_directories=True)
        self._terminal = arguments.terminal
        self._no_watchman = arguments.no_watchman

    def _run(self) -> None:
        try:
            with shared_source_directory.try_lock(".pyre/client.lock"):
                for source_directory in self._source_directories:
                    # This unsafe call is OK due to the client lock always
                    # being acquired before starting a server - no server can
                    # spawn in the interim which would cause a race.
                    try:
                        with shared_source_directory.try_lock(
                            os.path.join(
                                source_directory,
                                ".pyre",
                                "server",
                                "server.lock")):
                            pass
                    except OSError:
                        LOG.warn(
                            "Server at %s exists, skipping.",
                            source_directory)
                        continue

                    shared_source_directory.write_existing(
                        self._original_source_directories)
                    flags = self._flags()
                    flags.append('-daemonize')
                    if not self._no_watchman:
                        results = self._call_client(
                            command=WATCHMAN,
                            source_directories=self._source_directories,
                            flags=flags)

                    flags = self._flags()
                    if self._terminal:
                        flags.append('-terminal')
                    flags.extend([
                        '-stub-roots',
                        ','.join(self._configuration.get_search_path()),
                    ])
                    results = self._call_client(
                        command=START,
                        source_directories=self._source_directories,
                        flags=flags)
                    self._check_results(results)

        except OSError:
            LOG.warning("Server is already running")
            return None


State = namedtuple('State', ['running', 'dead'])


class Stop(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Stop, self).__init__(arguments, configuration, source_directories)

    def _run(self) -> None:
        running = self._state().running
        if running:
            results = self._call_client(
                command=STOP,
                source_directories=running)
            self._check_results(results)
            LOG.info(
                'Stopped %s at%s%s',
                self._server_string(running),
                self._variable_spacing_string(len(running) > 1),
                self._source_directory_string(running))
            shared_source_directory.remove_list()
        else:
            LOG.info('No %s running', self._server_string())


class Restart(Command):
    def __init__(
            self,
            arguments,
            configuration,
            source_directories,
            blocking=True) -> None:
        super(Restart, self).__init__(
            arguments,
            configuration,
            source_directories)
        self._blocking = blocking

    def _run(self) -> None:
        Stop(
            self._arguments,
            self._configuration,
            self._source_directories).run()
        if self._blocking:
            Incremental(
                self._arguments,
                self._configuration,
                self._original_source_directories).run()
        else:
            Start(
                self._arguments,
                self._configuration,
                self._original_source_directories).run()


class Kill(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Kill, self).__init__(arguments, configuration, source_directories)
        self.with_fire = arguments.with_fire

    def _run(self) -> None:
        if self.with_fire:
            self._kill_all_processes()
            return
        running = self._state().running

        for source_directory in running:
            self._kill(
                os.path.join(source_directory, '.pyre/server/server.pid'))
            self._remove_if_exists(
                os.path.join(source_directory, '.pyre/server/server.lock'))
            self._remove_if_exists(
                os.path.join(source_directory, '.pyre/server/server.sock'))
            self._kill(
                os.path.join(source_directory, '.pyre/watchman/watchman.pid'))
            self._remove_if_exists(
                os.path.join(source_directory, '.pyre/watchman/watchman.lock'))

        if not running:
            LOG.warning("No %s running", self._server_string(running))
        else:
            LOG.info(
                "Terminated %s at%s%s",
                self._server_string(running),
                self._variable_spacing_string(len(running) > 1),
                self._source_directory_string(running))

    def _kill(self, path) -> None:
        if not os.path.exists(path):
            LOG.debug('No process running at `%s`', path)
            return

        try:
            LOG.debug('Terminating process at `%s`', path)
            with open(path, "r") as file:
                pid = int(file.read())
                if pid > 0:
                    os.kill(int(pid), signal.SIGINT)
                os.remove(path)
        except (IOError, OSError) as error:
            LOG.error("Encountered error during kill: %s", str(error))
            LOG.debug(traceback.format_exc())

    def _kill_all_processes(self) -> None:
        """
            Kills all processes that have the same binary as the one specified
            in the # configuration.
        """
        subprocess.run(["pkill", "-f", "^{}".format(
            os.path.realpath(self._configuration.get_binary())
        )])


    def _remove_if_exists(self, path) -> None:
        try:
            os.remove(path)
        except OSError:
            pass
