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
from ..configuration import Configuration
from ..exceptions import EnvironmentException
from ..filesystem import readable_directory, remove_if_exists
from ..log import StreamLogger
from ..process import Process
from ..socket_connection import SocketConnection, SocketException


TEXT: str = "text"
JSON: str = "json"


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
    BUCK_ERROR = 3
    SERVER_NOT_FOUND = 4
    INCONSISTENT_SERVER = 5
    CONFIGURATION_ERROR = 6
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
    INDIVIDUAL_TABLE_SIZES: str = "individual_table_sizes"
    TOTAL_SHARED_MEMORY_SIZE_OVER_TIME: str = "total_shared_memory_size_over_time"
    TOTAL_SHARED_MEMORY_SIZE_OVER_TIME_GRAPH: str = "total_shared_memory_size_over_time_graph"  # noqa B950

    def __str__(self) -> str:
        return self.value


class Result:
    def __init__(self, code: int, output: str, error: Optional[str] = None) -> None:
        self.code: int = code
        self.output: str = output
        self.error: Optional[str] = error

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
        version_names = sorted(os.listdir(typeshed_subdirectory), reverse=True)
        for version_name in version_names:
            # Anything under 2/ or 2.x is unusable for Pyre
            if version_name.startswith("2") and version_name != "2and3":
                continue
            search_path.append(os.path.join(typeshed_subdirectory, version_name))
    return search_path


def _convert_json_response_to_result(response: json_rpc.Response) -> Result:
    error = response.error
    error_message = None
    if error:
        error_code = ExitCode.FAILURE
        error_message = json.dumps(error)
    else:
        error_code = ExitCode.SUCCESS
    return Result(
        output=json.dumps(response.result), code=error_code, error=error_message
    )


def executable_file(file_path: str) -> str:
    if not os.path.isfile(file_path):
        raise EnvironmentException("%s is not a valid file" % file_path)
    if not os.access(file_path, os.X_OK):
        raise EnvironmentException("%s is not an executable file" % file_path)
    return file_path


class CommandParser(ABC):
    NAME = ""
    HIDDEN = False
    _exit_code: ExitCode = ExitCode.SUCCESS

    def __init__(self) -> None:
        pass

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
            "--logging-sections", help=argparse.SUPPRESS  # Enable sectional logging.
        )
        parser.add_argument(
            "--log-identifier",
            default="",
            help=argparse.SUPPRESS,  # Add given identifier to logged samples.
        )

        # Directory where Pyre places its log files and artifacts.
        parser.add_argument("--dot-pyre-directory", type=Path, help=argparse.SUPPRESS)
        parser.add_argument(
            "--logger", help=argparse.SUPPRESS  # Specify custom logging binary.
        )
        parser.add_argument("--formatter", help=argparse.SUPPRESS)

        # Link tree determination.
        buck_arguments = parser.add_argument_group("buck")
        buck_arguments.add_argument(
            "--target",
            action="append",
            dest="targets",
            default=[],
            help="The buck target to check",
        )

        use_buck_builder_group = buck_arguments.add_mutually_exclusive_group()
        use_buck_builder_group.add_argument(
            "--use-buck-builder",
            action="store_const",
            const=True,
            help="Use Pyre's experimental builder for Buck projects.",
        )
        use_buck_builder_group.add_argument(
            "--use-legacy-buck-builder",
            dest="use_buck_builder",
            action="store_const",
            const=False,
            help="Use the legacy builder for Buck projects.",
        )

        buck_arguments.add_argument(
            "--buck-mode", type=str, help="Mode to pass to `buck query`"
        )
        buck_arguments.add_argument(
            "--use-buck-source-database",
            action="store_const",
            const=True,
            help=argparse.SUPPRESS,
        )

        source_directories = parser.add_argument_group("source-directories")
        source_directories.add_argument(
            "--source-directory",
            action="append",
            dest="source_directories",
            default=[],
            help="The source directory to check",
            type=os.path.abspath,
        )
        source_directories.add_argument(
            "--filter-directory", help=argparse.SUPPRESS  # override filter directory
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
        parser.add_argument(
            "--features", default=None, type=str, help=argparse.SUPPRESS
        )
        # Temporary flag to help migrate to json sockets for incremental and query
        # commands.
        parser.add_argument(
            "--use-json-sockets",
            action="store_true",
            default=False,
            help=argparse.SUPPRESS,
        )

    @classmethod
    @abstractmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        pass

    @abstractmethod
    def _run(self) -> None:
        """ Abstract method expected to be overridden by subclasses. """
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
        self._version_hash: str = self._configuration.version_hash
        self._taint_models_path: List[str] = self._configuration.taint_models_path

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
            self._configuration.project_root,
            filter_directory=self._command_arguments.filter_directory,
            buck_mode=self._command_arguments.buck_mode,
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
        self._analysis_directory.cleanup()

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
        if not self._capable_terminal:
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
    ) -> Result:
        if not os.path.isdir(self._analysis_directory.get_root()):
            raise EnvironmentException(
                "`{}` is not a link tree.".format(self._analysis_directory.get_root())
            )

        client_command = [self._configuration.binary, command]
        client_command.extend(self._flags())
        client_command.append(self._analysis_directory.get_root())

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
            with subprocess.Popen(
                ["tail", "--follow", "--lines=0", stderr_file],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                universal_newlines=True,
            ) as stderr_tail:
                try:
                    with SocketConnection(
                        self._configuration.log_directory
                    ) as socket_connection:
                        socket_connection.perform_handshake(version_hash)
                        # pyre-fixme[6]: Expected `Iterable[str]` for 1st param but
                        #  got `Optional[IO[typing.Any]]`.
                        with StreamLogger(stderr_tail.stdout):
                            socket_connection.send(request)
                            response = socket_connection.read()
                        result = _convert_json_response_to_result(response)
                        result.check()
                        self._socket_result_handler(result)
                finally:
                    stderr_tail.terminate()
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
