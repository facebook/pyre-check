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

from . import (
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
    def __init__(self, link_tree, code, output):
        self.link_tree = link_tree
        self.code = code
        self.output = output

    def check(self):
        if self.code != SUCCESS:
            raise ClientException(
                'Client exited with error code {}'.format(self.code))


class Command:
    def __init__(self, arguments, configuration, link_trees):
        self._arguments = arguments
        self._configuration = configuration

        self._link_trees = link_trees
        self._debug = arguments.debug
        self._show_error_traces = arguments.show_error_traces
        self._verbose = arguments.verbose
        self._logging_sections = arguments.logging_sections
        self._check_unannotated = arguments.check_unannotated

    def run(self):
        pass

    def _flags(self):
        flags = []
        if self._debug:
            flags.append('-debug')
        if self._show_error_traces:
            flags.append('-show-error-traces')
        if self._verbose:
            flags.append('-verbose')
        if self._logging_sections:
            flags.extend(['-logging-sections', self._logging_sections])
        if self._check_unannotated:
            flags.append("-check-unannotated")
        return flags

    def _read_stdout(self, stdout):
        self._buffer = []
        for line in stdout:
            self._buffer.append(line.decode())

    def _call_client(
            self,
            command,
            link_trees,
            flags=None,
            capture_output=True):
        if not flags:
            flags = []

        results = []
        for link_tree in link_trees:
            if not os.path.isdir(link_tree):
                raise EnvironmentException(
                    '`{}` is not a link tree.'.format(link_tree))

            client_command = [
                self._configuration.get_binary(),
                command,
            ]
            client_command.extend(flags)
            client_command.append(link_tree)

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
                        link_tree=link_tree,
                        code=process.returncode,
                        output=output))
        return results

    def _check_results(self, results):
        for result in results:
            result.check()

    def on_client_exception(self):
        pass


class Persistent(Command):
    def __init__(self, arguments, configuration, link_trees):
        super(Persistent, self).__init__(arguments, configuration, link_trees)

    def run(self):
        arguments = self._arguments
        try:
            results = self._call_client(
                command=PERSISTENT,
                link_trees=self._link_trees,
                capture_output=False)
            self._check_results(results)
        except (ClientException, subprocess.CalledProcessError):
            arguments.terminal = False
            Restart(arguments, self._configuration, self._link_trees).run()

            results = self._call_client(
                command=PERSISTENT,
                link_trees=self._link_trees,
                capture_output=False)
            self._check_results(results)

    def on_client_exception(self):
        self._run_null_server()

    def _run_null_server(self):
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
    def __init__(self, arguments, configuration, link_trees):
        super(ErrorHandling, self).__init__(
            arguments,
            configuration,
            link_trees)
        self._verbose = arguments.verbose
        self._output = arguments.output
        self._original_directory = arguments.original_directory

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
            sys.stdout.write("\n")
            sys.stdout.write(
                '\n'.join([repr(error) for error in sorted(list(errors))]))
        else:
            sys.stdout.write(json.dumps([error.__dict__ for error in errors]))

    def _get_errors(self, results):
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
                    os.path.join(result.link_tree, error['path']))
                external = True
                if path.startswith(self._original_directory):
                    external = False
                    # Relativize path.
                    error['path'] = os.path.relpath(
                        path,
                        self._original_directory)
                errors.add(Error(external, **error))

        return errors


class Check(ErrorHandling):
    def __init__(self, arguments, configuration, link_trees):
        super(Check, self).__init__(arguments, configuration, link_trees)

    def run(self, retries=1):
        flags = self._flags()
        flags.extend([
            '-stub-roots',
            ','.join(self._configuration.get_stub_roots()),
        ])

        results = self._call_client(
            command=CHECK,
            link_trees=self._link_trees,
            flags=flags)
        errors = self._get_errors(results)
        self._print(errors)


class Incremental(ErrorHandling):
    NOT_RUNNING = 2

    def __init__(self, arguments, configuration, link_trees):
        super(Incremental, self).__init__(arguments, configuration, link_trees)

    def run(self, retries=1):
        arguments = self._arguments
        flags = self._flags()
        flags.extend([
            '-stub-roots',
            ','.join(self._configuration.get_stub_roots()),
        ])

        LOG.info("Waiting for server...")
        results = self._call_client(
            command=INCREMENTAL,
            link_trees=self._link_trees,
            flags=flags)

        if any([result.code == Incremental.NOT_RUNNING for result in results]):
            LOG.error('Not all servers running')
            arguments.no_watchman = False
            arguments.terminal = False
            Start(arguments, self._configuration, self._link_trees).run()

            if retries > 0:
                self.run(retries - 1)
                return
            else:
                raise ClientException('Unable to start servers')

        errors = self._get_errors(results)
        self._print(errors)


class Rage(Command):
    def __init__(self, arguments, configuration, link_trees):
        super(Rage, self).__init__(arguments, configuration, link_trees)
        self._arguments.command = RAGE

    def run(self):
        results = self._call_client(
            command=RAGE,
            link_trees=self._link_trees,
            capture_output=False)
        self._check_results(results)


class Start(Command):
    def __init__(self, arguments, configuration, link_trees):
        super(Start, self).__init__(arguments, configuration, link_trees)
        self._terminal = arguments.terminal
        self._no_watchman = arguments.no_watchman

    def run(self):
        flags = self._flags()
        flags.append('-daemonize')
        if not self._no_watchman:
            results = self._call_client(
                command=WATCHMAN,
                link_trees=self._link_trees,
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
            link_trees=self._link_trees,
            flags=flags)
        self._check_results(results)


class Stop(Command):
    def __init__(self, arguments, configuration, link_trees):
        super(Stop, self).__init__(arguments, configuration, link_trees)

    def run(self):
        results = self._call_client(
            command=STOP,
            link_trees=self._link_trees)
        self._check_results(results)


class Restart(Command):
    def __init__(self, arguments, configuration, link_trees):
        super(Restart, self).__init__(arguments, configuration, link_trees)

    def run(self):
        Stop(self._arguments, self._configuration, self._link_trees).run()
        Kill(self._arguments, self._configuration, self._link_trees).run()
        Start(self._arguments, self._configuration, self._link_trees).run()


class Kill(Command):
    def __init__(self, arguments, configuration, link_trees):
        super(Kill, self).__init__(arguments, configuration, link_trees)

    def run(self):
        running = []
        for link_tree in self._link_trees:
            pid_file = os.path.join(link_tree, '.pyre/server/server.pid')
            if os.path.isfile(pid_file):
                running.append(link_tree)
            self._kill(pid_file)
            self._remove_if_exists(
                os.path.join(link_tree, '.pyre/server/server.lock'))
            self._remove_if_exists(
                os.path.join(link_tree, '.pyre/server/server.sock'))
            self._kill(
                os.path.join(link_tree, '.pyre/watchman/watchman.pid'))
            self._remove_if_exists(
                os.path.join(link_tree, '.pyre/watchman/watchman.lock'))

        if not running:
            LOG.warning("No servers running")
        else:
            LOG.info(
                "Terminated server%s at %s",
                's' if len(running) > 1 else '',
                ', '.join('`{}`'.format(link_tree) for link_tree in running))

    def _kill(self, path):
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

    def _remove_if_exists(self, path):
        try:
            os.remove(path)
        except OSError:
            pass
