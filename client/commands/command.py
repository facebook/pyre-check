# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import enum
import json
import logging
import os
import resource
import signal
import subprocess
import threading
from abc import ABC, abstractmethod
from pathlib import Path
from typing import IO, Iterable, List, Optional

from .. import command_arguments, json_rpc, log, recently_used_configurations, terminal
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration, InvalidConfiguration
from ..exceptions import EnvironmentException
from ..filesystem import remove_if_exists
from ..log import StreamLogger, file_tailer
from ..process import Process
from ..socket_connection import SocketConnection, SocketException


LOG: logging.Logger = logging.getLogger(__name__)


class ClientException(Exception):
    pass


class State(enum.IntEnum):
    DEAD = 0
    RUNNING = 1


class ExitCode(enum.IntEnum):
    SUCCESS = 0
    FOUND_ERRORS = 1
    FAILURE = 2
    BUCK_INTERNAL_ERROR = 3
    SERVER_NOT_FOUND = 4
    INCONSISTENT_SERVER = 5
    CONFIGURATION_ERROR = 6
    BUCK_USER_ERROR = 7
    # If the process exited due to a signal, this will be the negative signal number.
    SIGSEGV = -signal.SIGSEGV


class IncrementalStyle(enum.Enum):
    SHALLOW = "shallow"
    FINE_GRAINED = "fine_grained"

    def __str__(self) -> str:
        return self.value


class ProfileOutput(enum.Enum):
    TRACE_EVENT: str = "trace_event"
    COLD_START_PHASES: str = "cold_start_phases"
    INCREMENTAL_UPDATES: str = "incremental_updates"
    TAINT: str = "taint"
    INDIVIDUAL_TABLE_SIZES: str = "individual_table_sizes"
    TOTAL_SHARED_MEMORY_SIZE_OVER_TIME: str = "total_shared_memory_size_over_time"
    TOTAL_SHARED_MEMORY_SIZE_OVER_TIME_GRAPH: str = (
        "total_shared_memory_size_over_time_graph"  # noqa B950
    )

    def __str__(self) -> str:
        return self.value


class Result:
    def __init__(self, code: int, output: str, error: Optional[str] = None) -> None:
        self.code: int = code
        self.output: str = output
        self.error: Optional[str] = error

    def check(self) -> None:
        if self.code != ExitCode.SUCCESS:
            description = f":\n{self.output}" if self.output else ""
            if self.code == ExitCode.SIGSEGV:
                description += (
                    "\nThis is a Pyre bug. Please re-run Pyre with --debug "
                    + "and provide the output to the developers."
                )
            raise ClientException(
                f"Client exited with error code {self.code}{description}"
            )


def _convert_json_response_to_result(response: json_rpc.Response) -> Result:
    if isinstance(response, json_rpc.ErrorResponse):
        return Result(output="", code=ExitCode.FAILURE, error=response.serialize())
    elif isinstance(response, json_rpc.SuccessResponse):
        return Result(
            output=json.dumps(response.result), code=ExitCode.SUCCESS, error=None
        )
    else:
        raise RuntimeError("Response should be either success or error.")


def executable_file(file_path: str) -> str:
    if not os.path.isfile(file_path):
        raise EnvironmentException(f"{file_path} is not a valid file")
    if not os.access(file_path, os.X_OK):
        raise EnvironmentException(f"{file_path} is not an executable file")
    return file_path


class CommandParser(ABC):
    NAME = ""
    HIDDEN = False
    _exit_code: ExitCode = ExitCode.SUCCESS

    def __init__(self) -> None:
        pass

    @abstractmethod
    def _run(self) -> None:
        """Abstract method expected to be overridden by subclasses."""
        pass

    def run(self) -> "CommandParser":
        self._run()
        return self

    def exit_code(self) -> ExitCode:
        return self._exit_code

    @property
    def configuration(self) -> Optional[Configuration]:
        return None


class Command(CommandParser, ABC):
    _buffer: List[str] = []

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super().__init__()
        self._command_arguments = command_arguments

        self._show_error_traces: bool = self._command_arguments.show_error_traces
        self._output: str = self._command_arguments.output

        # Derived arguments
        self._capable_terminal: bool = terminal.is_capable()
        self._original_directory: str = original_directory

        self._logging_sections: Optional[str] = self._command_arguments.logging_sections
        self._noninteractive: bool = self._command_arguments.noninteractive
        if self._command_arguments.debug or not self._capable_terminal:
            self._noninteractive = True

        self._configuration: Configuration = configuration
        self._version_hash: str = (
            self._configuration.get_version_hash_respecting_override() or "unversioned"
        )
        self._taint_models_path: List[str] = list(self._configuration.taint_models_path)

        self._analysis_directory: AnalysisDirectory = (
            analysis_directory or self.generate_analysis_directory()
        )

        if self._command_arguments.filter_directory is not None:
            LOG.warning(
                "Warning: `--filter-directory` is deprecated and will be removed soon."
            )

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        pass

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            filter_directory=self._command_arguments.filter_directory,
            relative_local_root=self._configuration.relative_local_root,
        )

    def run(self) -> "Command":
        if self._configuration.disabled:
            LOG.log(log.SUCCESS, "Pyre will not run due to being explicitly disabled")
        else:
            relative_local_root = self._configuration.relative_local_root
            if relative_local_root:
                recently_used_configurations.Cache(
                    self._configuration.dot_pyre_directory
                ).put(relative_local_root)
            Path(self._configuration.log_directory).mkdir(parents=True, exist_ok=True)
            self._run()
        return self

    def _run(self) -> None:
        pass

    def cleanup(self) -> None:
        self._analysis_directory.cleanup(delete_long_lasting_files=False)

    def _flags(self) -> List[str]:
        flags = []
        if self._command_arguments.debug:
            flags.extend(["-debug"])
        if self._command_arguments.sequential:
            flags.extend(["-sequential"])
        if self._configuration.strict:
            flags.extend(["-strict"])
        additional_checks = self._command_arguments.additional_checks
        if additional_checks:
            flags.append("-additional-checks")
            flags.append(",".join(additional_checks))
        if self._show_error_traces:
            flags.append("-show-error-traces")
        logging_sections = self._logging_sections
        if not self._capable_terminal or self._noninteractive:
            # Disable progress reporting for non-capable terminals.
            # This helps in reducing clutter.
            if logging_sections:
                logging_sections = logging_sections + ",-progress"
            else:
                logging_sections = "-progress"
        if logging_sections:
            flags.extend(["-logging-sections", logging_sections])
        if self._command_arguments.enable_profiling:
            flags.extend(["-profiling-output", self.profiling_log_path()])
        if self._command_arguments.enable_memory_profiling:
            flags.extend(["-memory-profiling-output", self.profiling_log_path()])
        if (
            self._command_arguments.enable_profiling
            or self._command_arguments.enable_memory_profiling
        ):
            # Clear the profiling log first since in pyre binary it's append-only
            remove_if_exists(self.profiling_log_path())
        flags.extend(["-project-root", self._configuration.project_root])
        log_identifier = self._command_arguments.log_identifier
        if log_identifier:
            flags.extend(["-log-identifier", log_identifier])
        logger = self._configuration.logger
        if logger:
            flags.extend(["-logger", logger])
        flags.extend(["-log-directory", self._configuration.log_directory])
        python_version = self._configuration.get_python_version()
        flags.extend(
            [
                "-python-major-version",
                str(python_version.major),
                "-python-minor-version",
                str(python_version.minor),
                "-python-micro-version",
                str(python_version.micro),
            ]
        )
        return flags

    # temporarily always return empty list to unblock client release
    def _feature_flags(self) -> List[str]:
        features = self._command_arguments.features
        if features:
            lsp_features = ["click_to_fix", "hover", "go_to_definition"]
            filtered = {
                key: value
                for key, value in json.loads(features).items()
                if key in lsp_features
            }
            return ["-features", json.dumps(filtered)]
        return []

    def _read_stdout(self, stdout: Iterable[str]) -> None:
        self._buffer = []
        for line in stdout:
            self._buffer.append(line)

    def _call_client(
        self,
        command: str,
        capture_output: bool = True,
        stdout: Optional[IO[str]] = None,
        check_analysis_directory: bool = True,
    ) -> Result:
        if check_analysis_directory:
            if not os.path.isdir(self._analysis_directory.get_root()):
                raise EnvironmentException(
                    f"`{self._analysis_directory.get_root()}` is not a link tree."
                )

        binary = self._configuration.get_binary_respecting_override()
        if binary is None:
            raise InvalidConfiguration("Cannot find any pyre binary.")
        client_command = [binary, command]
        client_command.extend(self._flags())
        client_command.append(self._analysis_directory.get_command_line_root())

        def limit_memory_usage() -> None:
            try:
                limit = 30 * 1024 * 1024 * 1024  # 30 GB
                resource.setrlimit(resource.RLIMIT_DATA, (limit, limit))
            except OSError:
                # Run the process with unlimited memory if the underlying syscall fails.
                pass

        if capture_output:
            assert stdout is None, "capture_output and stdout are mutually exclusive"
            stdout = subprocess.PIPE

        LOG.debug("Running `%s`", " ".join(client_command))
        with subprocess.Popen(
            client_command,
            stdout=stdout,
            stderr=subprocess.PIPE,
            preexec_fn=limit_memory_usage,
            universal_newlines=True,
        ) as process:

            # Read stdout output
            if capture_output:
                stdout_reader = threading.Thread(
                    target=self._read_stdout, args=(process.stdout,)
                )
                stdout_reader.daemon = True
                stdout_reader.start()

            # Read the error output and print it.
            # pyre-fixme[6]: Expected `Iterable[str]` for 1st param but got
            #  `Optional[IO[typing.Any]]`.
            with StreamLogger(process.stderr) as stream_logger:
                with Process.register_non_unique_process(
                    process.pid, self.NAME, self._configuration.log_directory
                ):
                    # Wait for the process to finish and clean up.
                    process.wait()
                # In the exceptional case, make sure that we don't stop early
                if process.returncode != 0:
                    stream_logger.join()

            if capture_output:
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
        pid_path = Path(self._configuration.log_directory, "server/server.pid")
        return State.RUNNING if Process.is_alive(pid_path) else State.DEAD

    # will open a socket, send a request, read the response and close the socket.
    def _send_and_handle_socket_request(
        self, request: json_rpc.Request, version_hash: str
    ) -> None:
        try:
            stderr_file = os.path.join(
                self._configuration.log_directory, "server/server.stdout"
            )
            with file_tailer(Path(stderr_file)) as stderr_tail:
                with SocketConnection(
                    self._configuration.log_directory
                ) as socket_connection:
                    socket_connection.perform_handshake(version_hash)
                    with StreamLogger(stderr_tail) as stderr_logger:
                        socket_connection.send(request)
                        response = socket_connection.read()
                    result = _convert_json_response_to_result(response)
                    result.check()
                    self._socket_result_handler(result)
            stderr_logger.join()
        except (
            OSError,
            SocketException,
            ResourceWarning,
            ClientException,
            json_rpc.JSONRPCException,
        ) as exception:
            LOG.error("Error while waiting for server: %s", str(exception))
            LOG.error("Run `pyre restart` in order to restart the server.")
            self._exit_code = ExitCode.FAILURE

    # Will be overwritten in subclasses to specialize how json socket
    # responses are handled.
    def _socket_result_handler(self, result: Result) -> None:
        log.stdout.write(result.output)

    def profiling_log_path(self) -> str:
        return os.path.join(self._configuration.log_directory, "profiling.log")

    @property
    def analysis_directory(self) -> AnalysisDirectory:
        return self._analysis_directory

    @property
    def configuration(self) -> Configuration:
        return self._configuration

    def _enable_logging_section(self, section: str) -> None:
        logging_sections = self._logging_sections
        if logging_sections:
            self._logging_sections = logging_sections + "," + section
        else:
            self._logging_sections = section

    def result(self) -> Optional[Result]:
        return None

    @property
    def noninteractive(self) -> bool:
        return self._noninteractive
