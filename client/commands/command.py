# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import enum
import logging
import os
import re
import resource
import signal
import subprocess
import threading
from abc import abstractmethod
from typing import Iterable, List, Optional, Set  # noqa

from .. import EnvironmentException, log
from ..configuration import Configuration
from ..filesystem import AnalysisDirectory, make_pyre_directory, remove_if_exists


LOG = logging.getLogger(__name__)  # type: logging.Logger


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
    def __init__(self, code: int, output: str) -> None:
        self.code = code
        self.output = output  # type: str

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

    _local_root = ""  # type: str

    def __init__(
        self,
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        self._arguments = arguments
        self._configuration = configuration

        self._analysis_directory = analysis_directory
        self._debug = arguments.debug  # type: bool
        self._enable_profiling = arguments.enable_profiling  # type: bool
        self._sequential = arguments.sequential  # type: bool
        self._strict = arguments.strict or (
            configuration and configuration.strict
        )  # type: bool
        self._additional_checks = arguments.additional_check  # type: List[str]
        self._show_error_traces = arguments.show_error_traces  # type: bool
        self._verbose = arguments.verbose  # type: bool
        self._hide_parse_errors = arguments.hide_parse_errors  # type: bool
        self._logging_sections = arguments.logging_sections  # type: str
        self._capable_terminal = arguments.capable_terminal  # type: bool
        self._log_identifier = arguments.log_identifier  # type: str
        self._logger = arguments.logger or (
            configuration and configuration.logger
        )  # type: str

        self._original_directory = arguments.original_directory  # type: str
        self._current_directory = arguments.current_directory  # type: str
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
        if self._additional_checks:
            flags.append("-additional-checks")
            flags.append(",".join(self._additional_checks))
        if self._show_error_traces:
            flags.append("-show-error-traces")
        if self._verbose:
            flags.append("-verbose")
        if not self._hide_parse_errors:
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
        if self._enable_profiling:
            pyre_directory = make_pyre_directory()
            profiling_output = os.path.join(pyre_directory, "profiling.log")
            # Clear the profiling log first since in pyre binary it's append-only
            remove_if_exists(profiling_output)
            flags.extend(["-profiling-output", profiling_output])
        if self._current_directory:
            flags.extend(["-project-root", self._current_directory])
        if self._log_identifier:
            flags.extend(["-log-identifier", self._log_identifier])
        if self._logger:
            flags.extend(["-logger", self._logger])
        return flags

    def _read_stdout(self, stdout: Iterable[bytes]) -> None:
        self._buffer = []
        for line in stdout:
            self._buffer.append(line.decode())

    def _read_stderr(self, stream: Iterable[bytes]) -> None:
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

    def _call_client(self, command: str, capture_output: bool = True) -> Result:
        if not os.path.isdir(self._analysis_directory.get_root()):
            raise EnvironmentException(
                "`{}` is not a link tree.".format(self._analysis_directory.get_root())
            )

        client_command = [self._configuration.binary, command]
        client_command.extend(self._flags())
        client_command.append(self._analysis_directory.get_root())

        def limit_memory_usage() -> None:
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
                target=self._read_stderr, args=(process.stderr,)
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

    def _relative_path(self, path: str) -> str:
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

    def _analysis_directory_string(self) -> str:
        return "`{}`".format(self._analysis_directory.get_root())
