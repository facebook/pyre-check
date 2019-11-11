# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import enum
import json
import logging
import os
import re
import resource
import signal
import subprocess
import threading
from abc import abstractmethod
from typing import Iterable, List, Optional, Set  # noqa

from .. import json_rpc, log, readable_directory
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..exceptions import EnvironmentException
from ..filesystem import remove_if_exists
from ..socket_connection import SocketConnection, SocketException


TEXT = "text"  # type: str
JSON = "json"  # type: str


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
    BUCK_ERROR = 3
    # If the process exited due to a signal, this will be the negative signal number.
    SIGSEGV = -signal.SIGSEGV


class IncrementalStyle(enum.Enum):
    SHALLOW = "shallow"
    TRANSITIVE = "transitive"
    FINE_GRAINED = "fine_grained"

    def __str__(self) -> str:
        return self.value


class ProfileOutput(enum.Enum):
    TRACE_EVENT: str = "trace_event"
    COLD_START_PHASES: str = "cold_start_phases"
    INCREMENTAL_UPDATES: str = "incremental_updates"

    def __str__(self) -> str:
        return self.value


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


def typeshed_search_path(typeshed_root: str) -> List[str]:
    search_path = []
    typeshed_subdirectories = ["stdlib", "third_party"]
    for typeshed_subdirectory_name in typeshed_subdirectories:
        typeshed_subdirectory = os.path.join(typeshed_root, typeshed_subdirectory_name)
        if (
            not os.path.isdir(typeshed_subdirectory)
            or typeshed_subdirectory_name == "tests"
            or typeshed_subdirectory_name[0] == "."
        ):
            continue

        # Always prefer newer version over older version
        version_names = reversed(sorted(os.listdir(typeshed_subdirectory)))
        for version_name in version_names:
            # Anything under 2/ or 2.x is unusable for Pyre
            if version_name.startswith("2") and version_name != "2and3":
                continue
            search_path.append(os.path.join(typeshed_subdirectory, version_name))
    return search_path


def _convert_json_response_to_result(response: json_rpc.Response) -> Result:
    if response.error:
        error_code = ExitCode.FAILURE
    else:
        error_code = ExitCode.SUCCESS
    return Result(output=json.dumps(response.result), code=error_code)


def executable_file(file_path: str) -> str:
    if not os.path.isfile(file_path):
        raise EnvironmentException("%s is not a valid file" % file_path)
    if not os.access(file_path, os.X_OK):
        raise EnvironmentException("%s is not an executable file" % file_path)
    return file_path


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
        self._enable_memory_profiling = arguments.enable_memory_profiling  # type: bool
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
        self._log_directory = arguments.log_directory  # type: str

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

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "-l", "--local-configuration", type=str, help="Use a local configuration"
        )

        parser.add_argument(
            "--version",
            action="store_true",
            help="Print the client and binary versions of Pyre.",
        )

        parser.add_argument("--debug", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument("--sequential", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument("--strict", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument(
            "--additional-check", action="append", help=argparse.SUPPRESS
        )

        parser.add_argument(
            "--show-error-traces",
            action="store_true",
            help="Display errors trace information",
        )

        # Logging.
        parser.add_argument(
            "--output", choices=[TEXT, JSON], default=TEXT, help="How to format output"
        )
        parser.add_argument(
            "--verbose", action="store_true", help="Enable verbose logging"
        )
        parser.add_argument(
            "--enable-profiling", action="store_true", help=argparse.SUPPRESS
        )
        parser.add_argument(
            "--enable-memory-profiling", action="store_true", help=argparse.SUPPRESS
        )
        parser.add_argument(
            "-n",
            "--noninteractive",
            action="store_true",
            help="Disable interactive logging",
        )
        parser.add_argument(
            "--hide-parse-errors",
            action="store_true",
            help="Hide detailed information about parse errors",
        )
        parser.add_argument(
            "--show-parse-errors",
            action="store_true",
            help="[DEPRECATED] Show detailed information about parse errors",
        )
        parser.add_argument(
            "--logging-sections", help=argparse.SUPPRESS  # Enable sectional logging.
        )
        parser.add_argument(
            "--log-identifier",
            default="",
            help=argparse.SUPPRESS,  # Add given identifier to logged samples.
        )
        parser.add_argument(
            "--log-directory",
            help=argparse.SUPPRESS,  # Override default location for logs
        )
        parser.add_argument(
            "--logger", help=argparse.SUPPRESS  # Specify custom logging binary.
        )
        parser.add_argument("--formatter", help=argparse.SUPPRESS)

        # Link tree determination.
        buck_arguments = parser.add_argument_group("buck")
        buck_arguments.add_argument(
            "--target", action="append", dest="targets", help="The buck target to check"
        )
        buck_arguments.add_argument(
            "--build",
            action="store_true",
            help="Freshly build all the necessary artifacts.",
        )
        buck_arguments.add_argument(
            "--use-buck-builder",
            action="store_true",
            help="Use Pyre's experimental builder for Buck projects.",
        )
        buck_arguments.add_argument(
            "--use-legacy-builder",
            action="store_true",
            help="Use Pyre's legacy builder for Buck projects.",
        )
        buck_arguments.add_argument(
            "--buck-builder-debug", action="store_true", help=argparse.SUPPRESS
        )

        source_directories = parser.add_argument_group("source-directories")
        source_directories.add_argument(
            "--source-directory",
            action="append",
            dest="source_directories",
            help="The source directory to check",
            type=os.path.abspath,
        )
        source_directories.add_argument(
            "--filter-directory", help=argparse.SUPPRESS  # override filter directory
        )

        parser.add_argument(
            "--use-global-shared-analysis-directory",
            action="store_true",
            help=argparse.SUPPRESS,
        )
        parser.add_argument(
            "--no-saved-state",
            action="store_true",
            help="Don't attempt to load Pyre from a saved state.",
        )

        # Handling of search path
        parser.add_argument(
            "--search-path",
            action="append",
            default=[],
            type=readable_directory,
            help="Add an additional directory of modules and stubs to include"
            " in the type environment",
        )
        parser.add_argument(
            "--preserve-pythonpath",
            action="store_true",
            default=False,
            help="Preserve the value of the PYTHONPATH environment variable and "
            "inherit the current python environment's search path",
        )

        parser.add_argument(
            "--binary",
            default=None,
            type=executable_file,
            help="Location of the pyre binary",
        )

        parser.add_argument(
            "--buck-builder-binary",
            default=None,
            help="Location of the buck builder binary",
        )
        parser.add_argument(
            "--buck-builder-target", default=None, help=argparse.SUPPRESS
        )

        parser.add_argument(
            "--exclude",
            action="append",
            default=[],
            help="Exclude files and directories matching this regexp from parsing",
        )

        # Typeshed stubs location
        parser.add_argument(
            "--typeshed",
            default=None,
            type=readable_directory,
            help="Location of the typeshed stubs",
        )
        parser.add_argument(
            "--save-initial-state-to",
            default=None,
            help="Path to serialize pyre's initial state to.",
        )
        parser.add_argument(
            "--load-initial-state-from", default=None, type=str, help=argparse.SUPPRESS
        )
        parser.add_argument(
            "--changed-files-path", default=None, type=str, help=argparse.SUPPRESS
        )
        parser.add_argument(
            "--saved-state-project", default=None, type=str, help=argparse.SUPPRESS
        )
        # Temporary flag to help migrate to json sockets for incremental and query commands.
        parser.add_argument(
            "--use-json-sockets",
            action="store_true",
            default=False,
            help=argparse.SUPPRESS,
        )

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        pass

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
            flags.extend(["-profiling-output", self.profiling_log_path()])
        if self._enable_memory_profiling:
            flags.extend(["-memory-profiling-output", self.profiling_log_path()])
        if self._enable_profiling or self._enable_memory_profiling:
            # Clear the profiling log first since in pyre binary it's append-only
            remove_if_exists(self.profiling_log_path())
        if self._current_directory:
            flags.extend(["-project-root", self._current_directory])
        if self._log_identifier:
            flags.extend(["-log-identifier", self._log_identifier])
        if self._logger:
            flags.extend(["-logger", self._logger])
        if self._log_directory:
            flags.extend(["-log-directory", self._log_directory])
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
                resource.setrlimit(resource.RLIMIT_DATA, (limit, limit))
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
            # In the exceptional case, make sure that we print the error messages.
            if process.returncode != 0:
                stderr_reader.join()
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
        pid_path = os.path.join(self._log_directory, "server/server.pid")
        try:
            with open(pid_path) as file:
                pid = int(file.read())
                os.kill(pid, 0)  # throws if process is not running
            return State.RUNNING
        except Exception:
            return State.DEAD

    # will open a socket, send a request, read the response and close the socket.
    def _send_and_handle_socket_request(
        self, request: json_rpc.Request, version_hash: str
    ) -> None:
        try:
            with SocketConnection(
                self._configuration.log_directory
            ) as socket_connection:
                socket_connection.perform_handshake(version_hash)
                socket_connection.send_request(request)
                response = json_rpc.read_response(socket_connection.input)
                result = _convert_json_response_to_result(response)
                result.check()
                self._socket_result_handler(result)
        except (
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
        return os.path.join(self._log_directory, "profiling.log")
