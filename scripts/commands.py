# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import re
import signal
import subprocess
import sys
import threading
import time
import traceback

from collections import namedtuple

from . import (
    buck,
    EnvironmentException,
    log,
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
                'Client exited with error code {}'.format(self.code))


class Command:
    def __init__(self, arguments, configuration, source_directories) -> None:
        self._arguments = arguments
        self._configuration = configuration

        self._source_directories = source_directories
        self._debug = arguments.debug
        self._show_error_traces = arguments.show_error_traces
        self._verbose = arguments.verbose
        self._logging_sections = arguments.logging_sections

        self._original_directory = arguments.original_directory

    def run(self) -> None:
        pass

    def _flags(self):
        flags = []
        if self._debug:
            flags.extend(['-debug', '-sequential'])
        if self._show_error_traces:
            flags.append('-show-error-traces')
        if self._verbose:
            flags.append('-verbose')
        if self._logging_sections:
            flags.extend(['-logging-sections', self._logging_sections])
        return flags

    def _read_stdout(self, stdout) -> None:
        self._buffer = []
        for line in stdout:
            self._buffer.append(line.decode())

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
                    reader = threading.Thread(
                        target=self._read_stdout,
                        args=(process.stdout,))
                    reader.daemon = True
                    reader.start()

                # Read the error output and print it.
                buffer = None
                log_pattern = re.compile(
                    r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} (\w+) (.*)')
                for line in process.stderr:
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

                # Wait for the process to finish and clean up.
                process.wait()
                if capture_output:
                    reader.join()

                output = ''
                if capture_output:
                    output = "\n".join(self._buffer)
                if '[' in output:
                    output = output[output.index('['):]

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
        return ', '.join(
            '`{}`'.format(self._relative_path(source_directory))
            for source_directory
            in source_directories)

    def on_client_exception(self) -> None:
        pass


class Persistent(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Persistent, self).__init__(
            arguments,
            configuration,
            source_directories)

    def run(self) -> None:
        arguments = self._arguments
        try:
            results = self._call_client(
                command=PERSISTENT,
                source_directories=self._source_directories,
                capture_output=False)
            self._check_results(results)
        except (ClientException, subprocess.CalledProcessError):
            arguments.terminal = False
            Restart(
                arguments,
                self._configuration,
                self._source_directories).run()

            results = self._call_client(
                command=PERSISTENT,
                source_directories=self._source_directories,
                capture_output=False)
            self._check_results(results)

    def on_client_exception(self) -> None:
        self._run_null_server()

    def _run_null_server(self) -> None:
        # Read content of the form Content-Length:n\r\n\r\n{jsonmessage}
        line = sys.stdin.readline()
        try:
            length = int(re.match(r"Content-Length: (?P<bytes>[0-9]+)", line)
                         .group('bytes'))
        except AttributeError:
            length = 0
        # skip empty line
        sys.stdin.readline()
        serialized_json = sys.stdin.read(length)
        try:
            parsed = json.loads(serialized_json)
            request_id = parsed["id"]
        except Exception:
            request_id = 1
        response = {
            "jsonrpc": "2.0",
            "id": request_id,
            "result": {"capabilities": {}},
        }
        sys.stdout.write(str(response))
        sys.stdout.flush()
        while True:
            time.sleep(10)


class ErrorHandling(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(ErrorHandling, self).__init__(
            arguments,
            configuration,
            source_directories)
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
                internal_paths = [self._original_directory]
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
            source_directories)

    def run(self, retries: int = 1) -> None:
        flags = self._flags()
        flags.extend([
            '-stub-roots',
            ','.join(self._configuration.get_stub_roots()),
        ])

        results = self._call_client(
            command=CHECK,
            source_directories=self._source_directories,
            flags=flags)
        errors = self._get_errors(results)
        self._print(errors)


class Incremental(ErrorHandling):
    NOT_RUNNING = 2

    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Incremental, self).__init__(
            arguments,
            configuration,
            source_directories)

    def run(self) -> None:
        dead = self._state().dead
        if dead:
            LOG.warning(
                '%s not running at %s. Starting...',
                self._server_string(dead).capitalize(),
                self._source_directory_string(dead))
            arguments = self._arguments
            arguments.terminal = False
            arguments.no_watchman = False
            Start(arguments, self._configuration, dead).run()

        flags = self._flags()
        flags.extend([
            '-stub-roots',
            ','.join(self._configuration.get_stub_roots()),
        ])

        if dead:
            LOG.info("Server initializing...")
        else:
            LOG.info("Waiting for server...")

        results = self._call_client(
            command=INCREMENTAL,
            source_directories=self._source_directories,
            flags=flags)

        errors = self._get_errors(results)
        self._print(errors)


class Rage(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Rage, self).__init__(arguments, configuration, source_directories)
        self._arguments.command = RAGE

    def run(self) -> None:
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
            source_directories)
        self._terminal = arguments.terminal
        self._no_watchman = arguments.no_watchman

    def run(self) -> None:
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
            ','.join(self._configuration.get_stub_roots()),
        ])

        results = self._call_client(
            command=START,
            source_directories=self._source_directories,
            flags=flags)
        self._check_results(results)


State = namedtuple('State', ['running', 'dead'])


class Stop(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Stop, self).__init__(arguments, configuration, source_directories)

    def run(self) -> None:
        running = self._state().running
        if running:
            results = self._call_client(
                command=STOP,
                source_directories=running)
            self._check_results(results)
            LOG.info(
                'Stopped %s at %s',
                self._server_string(running),
                self._source_directory_string(running))
        else:
            LOG.info('No %s running', self._server_string())


class Restart(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Restart, self).__init__(
            arguments,
            configuration,
            source_directories)

    def run(self) -> None:
        Stop(
            self._arguments,
            self._configuration,
            self._source_directories).run()
        Start(
            self._arguments,
            self._configuration,
            self._source_directories).run()


class Kill(Command):
    def __init__(self, arguments, configuration, source_directories) -> None:
        super(Kill, self).__init__(arguments, configuration, source_directories)

    def run(self) -> None:
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
                "Terminated %s at %s",
                self._server_string(running),
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

    def _remove_if_exists(self, path) -> None:
        try:
            os.remove(path)
        except OSError:
            pass
