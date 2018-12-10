# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
import logging
import os
import re
import resource
import signal
import subprocess
import threading
from abc import abstractmethod
from typing import List, Set  # noqa

from .. import EnvironmentException, log
from ..filesystem import AnalysisDirectory


LOG = logging.getLogger(__name__)


class ClientException(Exception):
    pass


class State(enum.IntEnum):
    DEAD = 0
    RUNNING = 1


class ExitCode(enum.IntEnum):
    SUCCESS = 0
    FOUND_ERRORS = 1
    FAILURE = 2
    # If the process exited due to a signal, this will be the negative signal number.
    SIGSEGV = -signal.SIGSEGV


class Result:
    def __init__(self, code, output) -> None:
        self.code = code
        self.output = output

    def check(self) -> None:
        if self.code != ExitCode.SUCCESS:
            description = ":\n{}".format(self.output) if self.output else ""
            if self.code == ExitCode.SIGSEGV:
                description += (
                    "\nThis is a Pyre bug. Please re-run Pyre with --debug "
                    "and provide the output to the developers."
                )
            raise ClientException(
                "Client exited with error code {}{}".format(self.code, description)
            )


class Command:
    _buffer = []  # type: List[str]
    _call_client_terminated = False  # type: bool

    _exit_code = ExitCode.SUCCESS  # type: ExitCode

    def __init__(
        self, arguments, configuration, analysis_directory: AnalysisDirectory
    ) -> None:
        self._arguments = arguments
        self._configuration = configuration

        self._analysis_directory = analysis_directory
        self._debug = arguments.debug
        self._sequential = arguments.sequential
        self._strict = arguments.strict
        self._run_additional_checks = arguments.run_additional_checks
        self._show_error_traces = arguments.show_error_traces
        self._verbose = arguments.verbose
        self._show_parse_errors = arguments.show_parse_errors
        self._logging_sections = arguments.logging_sections
        self._capable_terminal = arguments.capable_terminal
        self._log_identifier = arguments.log_identifier
        self._logger = arguments.logger or (configuration and configuration.logger)

        self._original_directory = arguments.original_directory
        self._current_directory = arguments.current_directory
        if arguments.local_configuration:
            self._local_root = (
                arguments.local_configuration
                if os.path.isdir(arguments.local_configuration)
                else os.path.dirname(arguments.local_configuration)
            )
        else:
            self._local_root = arguments.original_directory

    def run(self) -> "Command":
        self._run()
        return self

    def exit_code(self) -> int:
        return self._exit_code

    @abstractmethod
    def _run(self) -> None:
        """ Abstract method expected to be overridden by subclasses. """
        pass

    def _flags(self) -> List[str]:
        flags = []
        if self._debug:
            flags.extend(["-debug"])
        if self._sequential:
            flags.extend(["-sequential"])
        if self._strict:
            flags.extend(["-strict"])
        if self._run_additional_checks:
            flags.extend(["-run-additional-checks"])
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
        if self._log_identifier:
            flags.extend(["-log-identifier", self._log_identifier])
        if self._logger:
            flags.extend(["-logger", self._logger])
        return flags

    def _read_stdout(self, stdout) -> None:
        self._buffer = []
        for line in stdout:
            self._buffer.append(line.decode())

    def _read_stderr(self, stream, _analysis_directory) -> None:
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

    def _call_client(self, command, capture_output: bool = True) -> Result:
        if not os.path.isdir(self._analysis_directory.get_root()):
            raise EnvironmentException(
                "`{}` is not a link tree.".format(self._analysis_directory.get_root())
            )

        client_command = [self._configuration.binary, command]
        client_command.extend(self._flags())
        client_command.append(self._analysis_directory.get_root())

        def limit_memory_usage():
            try:
                limit = 20 * 1024 * 1024 * 1024  # 20 GB
                resource.setrlimit(resource.RLIMIT_AS, (limit, limit))
            except OSError:
                # Run the process with unlimited memory if the underlying syscall fails.
                pass

        LOG.debug("Running `%s`", " ".join(client_command))
        with subprocess.Popen(
            client_command,
            stdout=subprocess.PIPE if capture_output else None,
            stderr=subprocess.PIPE,
            preexec_fn=limit_memory_usage,
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
                target=self._read_stderr,
                args=(process.stderr, self._analysis_directory.get_root()),
            )
            stderr_reader.daemon = True
            stderr_reader.start()

            # Wait for the process to finish and clean up.
            process.wait()
            self._call_client_terminated = True
            if capture_output:
                # pyre-fixme: stdout_reader is not always declared!
                stdout_reader.join()

            output = ""
            if capture_output:
                output = "".join(self._buffer)
            if process.returncode != 0 and capture_output:
                output = "".join(self._buffer)

            return Result(code=process.returncode, output=output)

    def _relative_path(self, path) -> str:
        # pyre-fixme: Expected str, got bytes
        return os.path.relpath(path, self._original_directory)

    def _state(self) -> State:
        pid_path = os.path.join(
            self._analysis_directory.get_root(), ".pyre/server/server.pid"
        )
        try:
            with open(pid_path) as file:
                pid = int(file.read())
                os.kill(pid, 0)  # throws if process is not running
            return State.RUNNING
        except Exception:
            return State.DEAD

    def _server_string(self, analysis_directory=None) -> str:
        if not analysis_directory:
            analysis_directory = self._analysis_directory.get_root()
        return "server{}".format("" if len(analysis_directory) < 2 else "s")

    def _analysis_directory_string(self) -> str:
        return "`{}`".format(self._analysis_directory.get_root())
