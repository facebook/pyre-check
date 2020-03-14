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
import sys
import threading
from abc import ABC, abstractmethod
from typing import IO, Iterable, List, Optional

from typing_extensions import Final

from .. import (
    find_local_root,
    find_log_directory,
    find_project_root,
    is_capable_terminal,
    json_rpc,
    log,
    readable_directory,
)
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from ..exceptions import EnvironmentException
from ..filesystem import remove_if_exists, translate_path
from ..log import StreamLogger
from ..process import register_non_unique_process
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

    def __str__(self) -> str:
        return self.value


class Result:
    def __init__(self, code: int, output: str) -> None:
        self.code: int = code
        self.output: str = output

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


class CommandParser(ABC):
    NAME = ""
    HIDDEN = False
    _exit_code: ExitCode = ExitCode.SUCCESS

    def __init__(self, arguments: argparse.Namespace, original_directory: str) -> None:
        self._arguments = arguments
        self._local_configuration: Final[Optional[str]] = arguments.local_configuration
        self._version: bool = arguments.version
        self._debug: bool = arguments.debug
        self._sequential: bool = arguments.sequential
        self._strict: bool = arguments.strict
        self._additional_checks: List[str] = arguments.additional_check
        self._show_error_traces: bool = arguments.show_error_traces
        self._output: str = arguments.output
        self._verbose: bool = arguments.verbose
        self._enable_profiling: bool = arguments.enable_profiling
        self._enable_memory_profiling: bool = arguments.enable_memory_profiling
        self._noninteractive: bool = arguments.noninteractive
        self._hide_parse_errors: bool = arguments.hide_parse_errors
        self._logging_sections: str = arguments.logging_sections
        self._log_identifier: str = arguments.log_identifier
        self._log_directory: str = arguments.log_directory
        self._logger: str = arguments.logger
        self._formatter: List[str] = arguments.formatter

        self._targets: List[str] = arguments.targets
        self._build: bool = arguments.build
        self._use_buck_builder: bool = arguments.use_buck_builder
        self._use_legacy_builder: bool = arguments.use_legacy_builder
        self._buck_builder_debug: bool = arguments.buck_builder_debug

        self._source_directories: List[str] = arguments.source_directories
        self._filter_directory: List[str] = arguments.filter_directory
        self._use_global_shared_analysis_directory: bool = arguments.use_global_shared_analysis_directory
        self._no_saved_state: bool = arguments.no_saved_state

        self._search_path: List[str] = arguments.search_path
        self._preserve_pythonpath: bool = arguments.preserve_pythonpath
        self._binary: str = arguments.binary
        self._buck_builder_binary: Final[Optional[str]] = arguments.buck_builder_binary
        self._buck_builder_target: Final[Optional[str]] = arguments.buck_builder_target
        self._exclude: List[str] = arguments.exclude
        self._typeshed: str = arguments.typeshed
        self._save_initial_state_to: Final[
            Optional[str]
        ] = arguments.save_initial_state_to
        self._load_initial_state_from: Final[
            Optional[str]
        ] = arguments.load_initial_state_from
        self._changed_files_path: Final[Optional[str]] = arguments.changed_files_path
        self._saved_state_project: Final[Optional[str]] = arguments.saved_state_project

        # Derived arguments
        self._capable_terminal: bool = is_capable_terminal()
        self._original_directory: str = original_directory
        self._current_directory: str = find_project_root(self._original_directory)
        self._local_configuration = find_local_root(
            self._original_directory, self._local_configuration
        )
        self._log_directory: str = find_log_directory(
            self._log_directory, self._current_directory, self._local_configuration
        )
        logger = self._logger
        if logger:
            self._logger = translate_path(self._original_directory, logger)
        if self._debug or not self._capable_terminal:
            self._noninteractive = True

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
        buck_arguments.add_argument(
            "--buck-mode", type=str, help="Mode to pass to `buck query`"
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

    def cleanup(self) -> None:
        pass

    def exit_code(self) -> ExitCode:
        return self._exit_code

    @property
    def configuration(self) -> Optional[Configuration]:
        return None

    @property
    def current_directory(self) -> Optional[str]:
        return self._current_directory

    @property
    def local_configuration(self) -> Optional[str]:
        return self._local_configuration

    @property
    def log_directory(self) -> str:
        return self._log_directory

    @property
    def noninteractive(self) -> bool:
        return self._noninteractive


class Command(CommandParser, ABC):
    _buffer: List[str] = []

    _local_root: str = ""

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Command, self).__init__(arguments, original_directory)
        local_configuration = self._local_configuration
        if local_configuration:
            self._local_root = (
                local_configuration
                if os.path.isdir(local_configuration)
                else os.path.dirname(local_configuration)
            )
        else:
            self._local_root = self._original_directory

        self._configuration: Configuration = (
            configuration or self.generate_configuration()
        )
        self._strict: bool = arguments.strict or self._configuration.strict
        self._logger: str = arguments.logger or (configuration and configuration.logger)
        self._ignore_all_errors_paths: Iterable[str] = (
            self._configuration.ignore_all_errors
        )
        self._number_of_workers: int = self._configuration.number_of_workers
        self._version_hash: str = self._configuration.version_hash
        self._formatter: Final[Optional[str]] = self._configuration.formatter
        self._taint_models_path: List[str] = [
            translate_path(self._original_directory, path)
            for path in self._configuration.taint_models_path
        ]

        self._analysis_directory: AnalysisDirectory = (
            analysis_directory or self.generate_analysis_directory()
        )
        self._features: Final[Optional[str]] = arguments.features

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        pass

    def generate_configuration(self) -> Configuration:
        return Configuration(
            local_configuration=self._local_configuration,
            search_path=self._search_path,
            binary=self._binary,
            typeshed=self._typeshed,
            preserve_pythonpath=self._preserve_pythonpath,
            excludes=self._exclude,
            logger=self._logger,
            formatter=self._formatter,
            log_directory=self._log_directory,
        )

    def generate_analysis_directory(self) -> AnalysisDirectory:
        configuration = self._configuration
        if not configuration:
            return AnalysisDirectory(".")
        else:
            return resolve_analysis_directory(
                self._arguments,
                configuration,
                self._original_directory,
                self._current_directory,
            )

    def run(self) -> "Command":
        configuration = self._configuration
        if configuration and configuration.disabled:
            LOG.log(log.SUCCESS, "Pyre will not run due to being explicitly disabled")
        else:
            self._run()
        return self

    def _run(self) -> None:
        pass

    def cleanup(self) -> None:
        self._analysis_directory.cleanup()

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

    # temporarily always return empty list to unblock client release
    def _feature_flags(self) -> List[str]:
        features = self._features
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
            text=True,
        ) as process:

            # Read stdout output
            if capture_output:
                stdout_reader = threading.Thread(
                    target=self._read_stdout, args=(process.stdout,)
                )
                stdout_reader.daemon = True
                stdout_reader.start()

            # Read the error output and print it.
            with StreamLogger(process.stderr) as stream_logger:
                with register_non_unique_process(
                    process.pid, self.NAME, self.log_directory
                ):
                    # Wait for the process to finish and clean up.
                    process.wait()
                # In the exceptional case, make sure that we don't stop early
                if process.returncode != 0:
                    stream_logger.join()

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
            stderr_file = os.path.join(self._log_directory, "server/server.stdout")
            with subprocess.Popen(
                ["tail", "--follow", "--lines=0", stderr_file],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                text=True,
            ) as stderr_tail:
                try:
                    with SocketConnection(self._log_directory) as socket_connection:
                        socket_connection.perform_handshake(version_hash)
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
        return os.path.join(self._log_directory, "profiling.log")

    @property
    def analysis_directory(self) -> AnalysisDirectory:
        return self._analysis_directory

    @property
    def configuration(self) -> Optional[Configuration]:
        return self._configuration
