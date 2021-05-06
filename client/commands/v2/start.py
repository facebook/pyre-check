# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import contextlib
import dataclasses
import datetime
import enum
import json
import logging
import os
import re
import subprocess
import tempfile
from pathlib import Path
from typing import (
    IO,
    TextIO,
    Any,
    Dict,
    Iterator,
    List,
    Mapping,
    Optional,
    Sequence,
    Set,
    Tuple,
    Union,
)

from ... import (
    command_arguments,
    commands,
    configuration as configuration_module,
    find_directories,
    log,
)
from . import server_connection, server_event, stop, remote_logging


LOG: logging.Logger = logging.getLogger(__name__)

ARTIFACT_ROOT_NAME: str = "link_trees"
SERVER_LOG_FILE_FORMAT: str = "server.stderr.%Y_%m_%d_%H_%M_%S_%f"

# NOTE(grievejia): This is a very restricted form of target specification used
# for a hacky heuristic. We should consider moving away from it in the future.
# Do NOT use it for general-purpose target parsing.
BUCK_TARGET_PATTERN: str = (
    r"[A-Za-z0-9._-]*//[A-Za-z0-9/._-]+((:[A-Za-z0-9_/.=,@~+-]+)|(/\.\.\.))"
)


class MatchPolicy(enum.Enum):
    BASE_NAME = "base_name"
    EXTENSION = "extension"
    FULL_PATH = "full_path"

    def __str__(self) -> str:
        return self.value


@dataclasses.dataclass(frozen=True)
class CriticalFile:
    policy: MatchPolicy
    path: str

    def serialize(self) -> Dict[str, str]:
        return {str(self.policy): self.path}


@dataclasses.dataclass(frozen=True)
class LoadSavedStateFromFile:
    shared_memory_path: str
    changed_files_path: Optional[str] = None

    def serialize(self) -> Tuple[str, Dict[str, str]]:
        return (
            "load_from_file",
            {
                "shared_memory_path": self.shared_memory_path,
                **(
                    {}
                    if self.changed_files_path is None
                    else {"changed_files_path": self.changed_files_path}
                ),
            },
        )


@dataclasses.dataclass(frozen=True)
class LoadSavedStateFromProject:
    project_name: str
    project_metadata: Optional[str] = None

    def serialize(self) -> Tuple[str, Dict[str, str]]:
        return (
            "load_from_project",
            {
                "project_name": self.project_name,
                **(
                    {}
                    if self.project_metadata is None
                    else {"project_metadata": self.project_metadata}
                ),
            },
        )


@dataclasses.dataclass(frozen=True)
class StoreSavedStateToFile:
    shared_memory_path: str

    def serialize(self) -> Tuple[str, Dict[str, str]]:
        return (
            "save_to_file",
            {
                "shared_memory_path": self.shared_memory_path,
            },
        )


SavedStateAction = Union[
    LoadSavedStateFromFile, LoadSavedStateFromProject, StoreSavedStateToFile
]


@dataclasses.dataclass(frozen=True)
class RemoteLogging:
    logger: str
    identifier: str = ""

    def serialize(self) -> Dict[str, str]:
        return {"logger": self.logger, "identifier": self.identifier}


@dataclasses.dataclass(frozen=True)
class SimpleSourcePath:
    elements: Sequence[configuration_module.SearchPathElement] = dataclasses.field(
        default_factory=list
    )

    def serialize(self) -> Dict[str, object]:
        return {
            "kind": "simple",
            "paths": [element.command_line_argument() for element in self.elements],
        }

    def get_checked_directory_allowlist(self) -> Set[str]:
        return {element.path() for element in self.elements}


def get_checked_directory_for_target(target: str) -> Optional[str]:
    match = re.search(BUCK_TARGET_PATTERN, target)
    if match is None:
        return None

    result = match[0]
    root_index = result.find("//")
    if root_index != -1:
        result = result[root_index + 2 :]
    result = result.replace("/...", "")
    result = result.split(":")[0]
    return result


@dataclasses.dataclass(frozen=True)
class BuckSourcePath:
    source_root: Path
    artifact_root: Path
    targets: Sequence[str] = dataclasses.field(default_factory=list)
    mode: Optional[str] = None
    isolation_prefix: Optional[str] = None

    def serialize(self) -> Dict[str, object]:
        mode = self.mode
        isolation_prefix = self.isolation_prefix
        return {
            "kind": "buck",
            "targets": self.targets,
            **({} if mode is None else {"mode": mode}),
            **(
                {}
                if isolation_prefix is None
                else {"isolation_prefix": isolation_prefix}
            ),
            "source_root": str(self.source_root),
            "artifact_root": str(self.artifact_root),
        }

    def get_checked_directory_allowlist(self) -> Set[str]:
        return {
            str(self.source_root / directory)
            for directory in (
                get_checked_directory_for_target(target) for target in self.targets
            )
            if directory is not None
        }


SourcePath = Union[SimpleSourcePath, BuckSourcePath]


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend server can recognize.
    Need to keep in sync with `pyre/new_server/serverConfiguration.mli`
    """

    log_path: str
    global_root: str
    source_paths: SourcePath

    additional_logging_sections: Sequence[str] = dataclasses.field(default_factory=list)
    checked_directory_allowlist: Sequence[str] = dataclasses.field(default_factory=list)
    checked_directory_blocklist: Sequence[str] = dataclasses.field(default_factory=list)
    critical_files: Sequence[CriticalFile] = dataclasses.field(default_factory=list)
    debug: bool = False
    excludes: Sequence[str] = dataclasses.field(default_factory=list)
    extensions: Sequence[str] = dataclasses.field(default_factory=list)
    relative_local_root: Optional[str] = None
    memory_profiling_output: Optional[Path] = None
    number_of_workers: int = 1
    parallel: bool = True
    profiling_output: Optional[Path] = None
    python_version: configuration_module.PythonVersion = (
        configuration_module.PythonVersion(major=3)
    )
    remote_logging: Optional[RemoteLogging] = None
    saved_state_action: Optional[SavedStateAction] = None
    search_paths: Sequence[configuration_module.SearchPathElement] = dataclasses.field(
        default_factory=list
    )
    show_error_traces: bool = False
    store_type_check_resolution: bool = False
    strict: bool = False
    taint_models_path: Sequence[str] = dataclasses.field(default_factory=list)
    watchman_root: Optional[Path] = None

    @property
    def local_root(self) -> Optional[str]:
        if self.relative_local_root is None:
            return None
        return os.path.join(self.global_root, self.relative_local_root)

    def serialize(self) -> Dict[str, Any]:
        local_root = self.local_root
        return {
            "source_paths": self.source_paths.serialize(),
            "search_paths": [
                element.command_line_argument() for element in self.search_paths
            ],
            "excludes": self.excludes,
            "checked_directory_allowlist": self.checked_directory_allowlist,
            "checked_directory_blocklist": self.checked_directory_blocklist,
            "extensions": self.extensions,
            "log_path": self.log_path,
            "global_root": self.global_root,
            **({} if local_root is None else {"local_root": local_root}),
            **(
                {}
                if self.watchman_root is None
                else {"watchman_root": str(self.watchman_root)}
            ),
            "taint_model_paths": self.taint_models_path,
            "debug": self.debug,
            "strict": self.strict,
            "python_version": {
                "major": self.python_version.major,
                "minor": self.python_version.minor,
                "micro": self.python_version.micro,
            },
            "show_error_traces": self.show_error_traces,
            "critical_files": [
                critical_file.serialize() for critical_file in self.critical_files
            ],
            **(
                {}
                if self.saved_state_action is None
                else {"saved_state_action": self.saved_state_action.serialize()}
            ),
            "store_type_check_resolution": self.store_type_check_resolution,
            "parallel": self.parallel,
            "number_of_workers": self.number_of_workers,
            "additional_logging_sections": self.additional_logging_sections,
            **(
                {}
                if self.remote_logging is None
                else {"remote_logging": self.remote_logging.serialize()}
            ),
            **(
                {}
                if self.profiling_output is None
                else {"profiling_output": str(self.profiling_output)}
            ),
            **(
                {}
                if self.memory_profiling_output is None
                else {"memory_profiling_output": str(self.memory_profiling_output)}
            ),
        }


def get_critical_files(
    configuration: configuration_module.Configuration,
) -> List[CriticalFile]:
    def get_full_path(root: str, relative: str) -> str:
        full_path = (Path(root) / relative).resolve(strict=False)
        if not full_path.exists():
            LOG.warning(f"Critical file does not exist: {full_path}")
        return str(full_path)

    local_root = configuration.local_root
    return [
        CriticalFile(
            policy=MatchPolicy.FULL_PATH,
            path=get_full_path(
                root=configuration.project_root,
                relative=find_directories.CONFIGURATION_FILE,
            ),
        ),
        *(
            []
            if local_root is None
            else [
                CriticalFile(
                    policy=MatchPolicy.FULL_PATH,
                    path=get_full_path(
                        root=local_root,
                        relative=find_directories.LOCAL_CONFIGURATION_FILE,
                    ),
                )
            ]
        ),
        *(
            [
                CriticalFile(
                    policy=MatchPolicy.FULL_PATH,
                    path=get_full_path(root=path, relative=""),
                )
                for path in configuration.other_critical_files
            ]
        ),
    ]


def get_saved_state_action(
    start_arguments: command_arguments.StartArguments,
    relative_local_root: Optional[str] = None,
) -> Optional[SavedStateAction]:
    saved_state_output_path = start_arguments.save_initial_state_to
    if saved_state_output_path is not None:
        return StoreSavedStateToFile(shared_memory_path=saved_state_output_path)

    saved_state_input_path = start_arguments.load_initial_state_from
    if saved_state_input_path is not None:
        return LoadSavedStateFromFile(
            shared_memory_path=saved_state_input_path,
            changed_files_path=start_arguments.changed_files_path,
        )

    saved_state_project = start_arguments.saved_state_project
    if saved_state_project is not None:
        return LoadSavedStateFromProject(
            project_name=saved_state_project,
            project_metadata=relative_local_root.replace("/", "$")
            if relative_local_root is not None
            else None,
        )

    return None


def find_watchman_root(base: Path) -> Optional[Path]:
    return find_directories.find_parent_directory_containing_file(
        base, ".watchmanconfig"
    )


def find_buck_root(base: Path) -> Optional[Path]:
    return find_directories.find_parent_directory_containing_file(base, ".buckconfig")


def get_source_path(configuration: configuration_module.Configuration) -> SourcePath:
    source_directories = configuration.source_directories
    targets = configuration.targets

    if source_directories is not None and targets is None:
        elements: Sequence[
            configuration_module.SearchPathElement
        ] = configuration.get_source_directories()
        if len(elements) == 0:
            LOG.warning("Pyre did not find an existent source directory.")
        return SimpleSourcePath(elements)

    if targets is not None and source_directories is None:
        if len(targets) == 0:
            LOG.warning("Pyre did not find any targets to check.")

        search_base = Path(configuration.project_root)
        artifact_root = configuration.dot_pyre_directory / ARTIFACT_ROOT_NAME

        relative_local_root = configuration.relative_local_root
        if relative_local_root is not None:
            search_base = search_base / relative_local_root
            artifact_root = artifact_root / relative_local_root

        source_root = find_buck_root(search_base)
        if source_root is None:
            raise configuration_module.InvalidConfiguration(
                "Cannot find a buck root for the specified targets. "
                + "Make sure the project is covered by a `.buckconfig` file."
            )

        return BuckSourcePath(
            source_root=source_root,
            artifact_root=artifact_root,
            targets=targets,
            mode=configuration.buck_mode,
            isolation_prefix=configuration.isolation_prefix,
        )

    if source_directories is None and targets is not None:
        raise configuration_module.InvalidConfiguration(
            "`source_directory` and `targets` are mutually exclusive"
        )

    raise configuration_module.InvalidConfiguration(
        "Cannot find any source files to analyze. "
        + "Either `source_directory` or `targets` must be specified."
    )


def get_profiling_log_path(log_directory: Path) -> Path:
    return log_directory / "profiling.log"


def create_server_arguments(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> Arguments:
    """
    Translate client configurations and command-line flags to server
    configurations.

    This API is not pure since it needs to access filesystem to filter out
    nonexistent directories. It is idempotent though, since it does not alter
    any filesystem state.
    """
    source_paths = get_source_path(configuration)

    logging_sections = start_arguments.logging_sections
    additional_logging_sections = (
        [] if logging_sections is None else logging_sections.split(",")
    )
    if start_arguments.noninteractive:
        additional_logging_sections.append("-progress")
    # Server section is usually useful when Pyre server is involved
    additional_logging_sections.append("server")

    profiling_output = (
        get_profiling_log_path(Path(configuration.log_directory))
        if start_arguments.enable_profiling
        else None
    )
    memory_profiling_output = (
        get_profiling_log_path(Path(configuration.log_directory))
        if start_arguments.enable_memory_profiling
        else None
    )

    logger = configuration.logger
    remote_logging = (
        RemoteLogging(logger=logger, identifier=start_arguments.log_identifier or "")
        if logger is not None
        else None
    )

    return Arguments(
        log_path=configuration.log_directory,
        global_root=configuration.project_root,
        additional_logging_sections=additional_logging_sections,
        checked_directory_allowlist=(
            list(source_paths.get_checked_directory_allowlist())
            + configuration.get_existent_do_not_ignore_errors_in_paths()
        ),
        checked_directory_blocklist=(
            configuration.get_existent_ignore_all_errors_paths()
        ),
        critical_files=get_critical_files(configuration),
        debug=start_arguments.debug,
        excludes=configuration.excludes,
        extensions=configuration.get_valid_extension_suffixes(),
        relative_local_root=configuration.relative_local_root,
        memory_profiling_output=memory_profiling_output,
        number_of_workers=configuration.get_number_of_workers(),
        parallel=not start_arguments.sequential,
        profiling_output=profiling_output,
        python_version=configuration.get_python_version(),
        remote_logging=remote_logging,
        saved_state_action=None
        if start_arguments.no_saved_state
        else get_saved_state_action(
            start_arguments, relative_local_root=configuration.relative_local_root
        ),
        search_paths=configuration.get_existent_search_paths(),
        show_error_traces=start_arguments.show_error_traces,
        source_paths=source_paths,
        store_type_check_resolution=start_arguments.store_type_check_resolution,
        strict=configuration.strict,
        taint_models_path=configuration.taint_models_path,
        watchman_root=None
        if start_arguments.no_watchman
        else find_watchman_root(Path(configuration.project_root)),
    )


def get_server_identifier(configuration: configuration_module.Configuration) -> str:
    global_identifier = Path(configuration.project_root).name
    relative_local_root = configuration.relative_local_root
    if relative_local_root is None:
        return global_identifier
    return f"{global_identifier}/{relative_local_root}"


def _write_argument_file(output_file: IO[str], arguments: Arguments) -> None:
    LOG.info(f"Writing server startup configurations into {output_file.name}...")
    serialized_arguments = arguments.serialize()
    LOG.debug(f"Arguments:\n{json.dumps(serialized_arguments, indent=2)}")
    output_file.write(json.dumps(serialized_arguments))
    output_file.flush()


def _run_in_foreground(
    command: Sequence[str], environment: Mapping[str, str]
) -> commands.ExitCode:
    # In foreground mode, we shell out to the backend server and block on it.
    # Server stdout/stderr will be forwarded to the current terminal.
    return_code = 0
    try:
        LOG.warning("Starting server in the foreground...")
        result = subprocess.run(
            command,
            env=environment,
            stdout=subprocess.DEVNULL,
            stderr=None,
            universal_newlines=True,
        )
        return_code = result.returncode
    except KeyboardInterrupt:
        # Backend server will exit cleanly when receiving SIGINT.
        pass

    if return_code == 0:
        return commands.ExitCode.SUCCESS
    else:
        LOG.error(f"Server exited with non-zero return code: {return_code}")
        return commands.ExitCode.FAILURE


@contextlib.contextmanager
def background_logging(log_file: Path) -> Iterator[None]:
    with log.file_tailer(log_file) as log_stream:
        with log.StreamLogger(log_stream) as logger:
            yield
    logger.join()


def _create_symbolic_link(source: Path, target: Path) -> None:
    try:
        source.unlink()
    except FileNotFoundError:
        pass
    source.symlink_to(target)


@contextlib.contextmanager
def background_server_log_file(log_directory: Path) -> Iterator[TextIO]:
    new_server_log_directory = log_directory / "new_server"
    new_server_log_directory.mkdir(parents=True, exist_ok=True)
    log_file_path = new_server_log_directory / datetime.datetime.now().strftime(
        SERVER_LOG_FILE_FORMAT
    )
    with open(str(log_file_path), "a") as log_file:
        yield log_file
    # Symlink the log file to a known location for subsequent `pyre incremental`
    # to find.
    _create_symbolic_link(new_server_log_directory / "server.stderr", log_file_path)


def _run_in_background(
    command: Sequence[str],
    environment: Mapping[str, str],
    log_directory: Path,
    event_waiter: server_event.Waiter,
) -> commands.ExitCode:
    # In background mode, we asynchronously start the server with `Popen` and
    # detach it from the current process immediately with `start_new_session`.
    # Do not call `wait()` on the Popen object to avoid blocking.
    # Server stderr will be forwarded to dedicated log files.
    # Server stdout will be used as additional communication channel for status
    # updates.
    with background_server_log_file(log_directory) as server_stderr:
        log_file = Path(server_stderr.name)
        server_process = subprocess.Popen(
            command,
            stdout=subprocess.PIPE,
            stderr=server_stderr,
            env=environment,
            start_new_session=True,
            universal_newlines=True,
        )

    LOG.info("Server is starting in the background.\n")
    server_stdout = server_process.stdout
    if server_stdout is None:
        raise RuntimeError("subprocess.Popen failed to set up a pipe for server stdout")

    try:
        # Block until an expected server event is obtained from stdout
        with background_logging(log_file):
            event_waiter.wait_on(server_stdout)
            server_stdout.close()
            return commands.ExitCode.SUCCESS
    except KeyboardInterrupt:
        LOG.info("SIGINT received. Terminating background server...")

        # If a background server has spawned and the user hits Ctrl-C, bring down
        # the spwaned server as well.
        server_stdout.close()
        server_process.terminate()
        server_process.wait()

        # Since we abruptly terminate the background server, it may not have the
        # chance to clean up the socket file properly. Make sure the file is
        # removed on our side.
        stop.remove_socket_if_exists(
            server_connection.get_default_socket_path(log_directory)
        )

        raise commands.ClientException("Interrupted by user. No server is spawned.")


@contextlib.contextmanager
def server_argument_file(server_arguments: Arguments) -> Iterator[Path]:
    with tempfile.NamedTemporaryFile(
        mode="w", prefix="pyre_arguments_", suffix=".json"
    ) as argument_file:
        _write_argument_file(argument_file, server_arguments)
        yield Path(argument_file.name)


def run_start(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> commands.ExitCode:
    binary_location = configuration.get_binary_respecting_override()
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    log_directory = Path(configuration.log_directory)

    server_arguments = create_server_arguments(configuration, start_arguments)
    if not start_arguments.no_watchman and server_arguments.watchman_root is None:
        LOG.warning(
            "Cannot find a watchman root. Incremental check will not be functional"
            " since no filesystem updates will be sent to the Pyre server."
        )

    LOG.info(f"Starting server at `{get_server_identifier(configuration)}`...")
    with server_argument_file(server_arguments) as argument_file_path:
        server_command = [binary_location, "newserver", str(argument_file_path)]
        server_environment = {
            **os.environ,
            # This is to make sure that backend server shares the socket root
            # directory with the client.
            # TODO(T77556312): It might be cleaner to turn this into a
            # configuration option instead.
            "TMPDIR": tempfile.gettempdir(),
        }
        if start_arguments.terminal:
            return _run_in_foreground(server_command, server_environment)
        else:
            return _run_in_background(
                server_command,
                server_environment,
                log_directory,
                server_event.Waiter(
                    wait_on_initialization=start_arguments.wait_on_initialization
                ),
            )


@remote_logging.log_usage(command_name="start")
def run(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> commands.ExitCode:
    return run_start(configuration, start_arguments)
