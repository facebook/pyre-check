# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
import fnmatch
import functools
import json
import logging
import os
import re
import subprocess
import threading
from typing import List  # noqa

from .. import SUCCESS, TEXT, EnvironmentException, log
from ..error import Error


LOG = logging.getLogger(__name__)


class ClientException(Exception):
    pass


class State(enum.IntEnum):
    DEAD = 0
    RUNNING = 1


class Result:
    def __init__(self, code, output) -> None:
        self.code = code
        self.output = output

    def check(self) -> None:
        if self.code != SUCCESS:
            raise ClientException(
                "Client exited with error code {}:"
                "\n{}".format(self.code, self.output)
            )


class Command:
    _buffer = []  # type: List[str]
    _call_client_terminated = False  # type: bool

    def __init__(self, arguments, configuration, source_directory) -> None:
        self._arguments = arguments
        self._configuration = configuration

        self._source_directory = source_directory
        self._debug = arguments.debug
        self._sequential = arguments.sequential
        self._strict = arguments.strict
        self._show_error_traces = arguments.show_error_traces
        self._verbose = arguments.verbose
        self._show_parse_errors = arguments.show_parse_errors
        self._logging_sections = arguments.logging_sections
        self._capable_terminal = arguments.capable_terminal

        self._original_directory = arguments.original_directory
        self._current_directory = arguments.current_directory
        if arguments.local_configuration:
            self._source_root = (
                arguments.local_configuration
                if os.path.isdir(arguments.local_configuration)
                else os.path.dirname(arguments.local_configuration)
            )
        else:
            self._source_root = arguments.original_directory

    def _run(self) -> int:
        pass

    def run(self) -> int:
        return self._run()

    def _flags(self):
        flags = []
        if self._debug:
            flags.extend(["-debug"])
        if self._sequential:
            flags.extend(["-sequential"])
        if self._strict:
            flags.extend(["-strict"])
        if self._show_error_traces:
            flags.append("-show-error-traces")
        if self._verbose:
            flags.append("-verbose")
        if self._show_parse_errors:
            if self._logging_sections:
                self._logging_sections = self._logging_sections + ",parser"
            else:
                self._logging_sections = "parser"
        if not self._capable_terminal:
            # Disable progress reporting for non-capable terminals.
            # This helps in reducing clutter.
            if self._logging_sections:
                self._logging_sections = self._logging_sections + ",-progress"
            else:
                self._logging_sections = "-progress"
        if self._logging_sections:
            flags.extend(["-logging-sections", self._logging_sections])
        if self._current_directory:
            flags.extend(["-project-root", self._current_directory])
        return flags

    def _read_stdout(self, stdout) -> None:
        self._buffer = []
        for line in stdout:
            self._buffer.append(line.decode())

    def _read_stderr(self, stream, _source_directory) -> None:
        buffer = None
        log_pattern = re.compile(r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} (\w+) (.*)")
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
                        section=match.groups()[0], data=[match.groups()[1]]
                    )
                elif buffer:
                    buffer.append(line)
            if buffer:
                buffer.flush()
        except Exception:
            pass

    def _call_client(self, command, flags=None, capture_output: bool = True):
        if not flags:
            flags = []

        if not os.path.isdir(self._source_directory):
            raise EnvironmentException(
                "`{}` is not a link tree.".format(self._source_directory)
            )

        client_command = [self._configuration.get_binary(), command]
        client_command.extend(flags)
        client_command.append(self._source_directory)

        LOG.debug("Running `%s`", " ".join(client_command))
        with subprocess.Popen(
            client_command,
            stdout=subprocess.PIPE if capture_output else None,
            stderr=subprocess.PIPE,
        ) as process:

            # Read stdout output
            if capture_output:
                stdout_reader = threading.Thread(
                    target=self._read_stdout, args=(process.stdout,)
                )
                stdout_reader.daemon = True
                stdout_reader.start()

            # Read the error output and print it.
            self._call_client_terminated = False
            stderr_reader = threading.Thread(
                target=self._read_stderr, args=(process.stderr, self._source_directory)
            )
            stderr_reader.daemon = True
            stderr_reader.start()

            # Wait for the process to finish and clean up.
            process.wait()
            self._call_client_terminated = True
            if capture_output:
                stdout_reader.join()

            output = ""
            if capture_output:
                output = "\n".join(self._buffer)
            if "[" in output:
                output = output[output.index("[") :]
            if process.returncode != 0 and capture_output:
                output = "".join(self._buffer)

            return Result(code=process.returncode, output=output)

    def _relative_path(self, path) -> str:
        return os.path.relpath(path, self._original_directory)

    def _state(self):
        pid_path = os.path.join(self._source_directory, ".pyre/server/server.pid")
        try:
            with open(pid_path) as file:
                pid = int(file.read())
                os.kill(pid, 0)  # throws if process is not running
            return State.RUNNING
        except Exception:
            return State.DEAD

    def _server_string(self, source_directory=None):
        if not source_directory:
            source_directory = self._source_directory
        return "server{}".format("" if len(source_directory) < 2 else "s")

    def _source_directory_string(self):
        return "`{}`".format(self._source_directory)

    def on_client_exception(self) -> None:
        pass


class ErrorHandling(Command):
    def __init__(self, arguments, configuration, source_directory) -> None:
        super(ErrorHandling, self).__init__(arguments, configuration, source_directory)
        self._verbose = arguments.verbose
        self._output = arguments.output
        self._do_not_check_paths = configuration.do_not_check
        self._discovered_source_directories = [self._source_root]

    def _print(self, errors):
        errors = [
            error
            for error in errors
            if (
                not error.is_do_not_check()
                and (
                    self._verbose
                    or not (
                        error.is_external_to_project_root()
                        or error.is_external_to_source_root()
                    )
                )
            )
        ]
        errors = sorted(
            errors, key=lambda error: (error.path, error.line, error.column)
        )

        if errors:
            length = len(errors)
            LOG.error("Found %d type error%s!", length, "s" if length > 1 else "")
        else:
            LOG.log(log.SUCCESS, "No type errors found")

        if self._output == TEXT:
            log.stdout.write("\n".join([repr(error) for error in errors]))
        else:
            log.stdout.write(json.dumps([error.__dict__ for error in errors]))

    @functools.lru_cache(maxsize=128)
    def _is_under_push_blocking_configuration(self, path: str) -> bool:
        for source_path in self._discovered_source_directories:
            if path.startswith(os.path.join(self._current_directory, source_path)):
                return True
        configuration_file = os.path.join(path, ".pyre_configuration.local")
        if os.path.isfile(configuration_file):
            with open(configuration_file) as file:
                configuration = json.loads(file.read())
                return bool(configuration.get("push_blocking", False))
        else:
            directory_path = os.path.dirname(path)
            under_configuration = (
                directory_path != path
                and self._is_under_push_blocking_configuration(directory_path)
            )
            if under_configuration:
                self._discovered_source_directories += directory_path
            return under_configuration

    def _get_errors(self, result):
        result.check()

        errors = set()
        try:
            results = json.loads(result.output)
        except (json.JSONDecodeError, ValueError):
            raise ClientException("Invalid output: `{}`.".format(result.output))

        for error in results:
            full_path = os.path.realpath(
                os.path.join(self._source_directory, error["path"])
            )
            # Relativize path to user's cwd.
            relative_path = self._relative_path(full_path)
            error["path"] = relative_path
            do_not_check = False
            external_to_project_root = True
            external_to_source_root = True
            if full_path.startswith(self._current_directory):
                external_to_project_root = False
            external_to_source_root = not (
                self._is_under_push_blocking_configuration(full_path)
            )
            for do_not_check_path in self._do_not_check_paths:
                if fnmatch.fnmatch(relative_path, (do_not_check_path + "*")):
                    do_not_check = True
                    break
            errors.add(
                Error(
                    do_not_check,
                    external_to_project_root,
                    external_to_source_root,
                    **error,
                )
            )

        return errors
