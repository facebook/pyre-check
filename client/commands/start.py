# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import contextlib
import dataclasses
import datetime
import enum
import logging
import os
import subprocess
import tempfile
from pathlib import Path
from typing import (
    Any,
    Dict,
    Iterator,
    List,
    Mapping,
    Optional,
    Sequence,
    TextIO,
    Tuple,
    Union,
)

from .. import (
    command_arguments,
    configuration as configuration_module,
    find_directories,
    identifiers,
    log,
)
from . import (
    backend_arguments,
    commands,
    daemon_socket,
    frontend_configuration,
    server_event,
    stop,
)


LOG: logging.Logger = logging.getLogger(__name__)

DAEMON_LOG_PATH_FORMAT: str = "server{flavor_suffix}.stderr.{time}"
DAEMON_LOG_TIME_FORMAT: str = "%Y_%m_%d_%H_%M_%S_%f"
DAEMON_CURRENT_LOG_PATH_FORMAT: str = "server{flavor_suffix}.stderr"


def deamon_current_log_path(
    log_directory: Path,
    flavor: identifiers.PyreFlavor,
) -> Path:
    return log_directory / DAEMON_CURRENT_LOG_PATH_FORMAT.format(
        flavor_suffix=flavor.path_suffix()
    )


def daemon_log_path(
    log_directory: Path,
    flavor: identifiers.PyreFlavor,
    now: datetime.datetime,
) -> Path:
    return log_directory / DAEMON_LOG_PATH_FORMAT.format(
        flavor_suffix=flavor.path_suffix(),
        time=now.strftime(DAEMON_LOG_TIME_FORMAT),
    )


def datetime_from_log_path(path: Path) -> Optional[datetime.datetime]:
    try:
        time_portion = path.name.split(".")[-1]
        return datetime.datetime.strptime(time_portion, DAEMON_LOG_TIME_FORMAT)
    except (IndexError, ValueError):
        return None


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
class Arguments:
    """
    Data structure for configuration options the backend server can recognize.
    Need to keep in sync with `source/command/serverCommand.ml`
    """

    base_arguments: backend_arguments.BaseArguments

    socket_path: Path
    strict: bool = False
    show_error_traces: bool = False
    additional_logging_sections: Sequence[str] = dataclasses.field(default_factory=list)
    watchman_root: Optional[Path] = None
    taint_models_path: Sequence[str] = dataclasses.field(default_factory=list)
    store_type_check_resolution: bool = False
    critical_files: Sequence[CriticalFile] = dataclasses.field(default_factory=list)
    saved_state_action: Optional[SavedStateAction] = None
    skip_initial_type_check: bool = False
    use_lazy_module_tracking: bool = False

    def serialize(self) -> Dict[str, Any]:
        return {
            **self.base_arguments.serialize(),
            "strict": self.strict,
            "socket_path": str(self.socket_path),
            "show_error_traces": self.show_error_traces,
            "additional_logging_sections": self.additional_logging_sections,
            **(
                {}
                if self.watchman_root is None
                else {"watchman_root": str(self.watchman_root)}
            ),
            "taint_model_paths": self.taint_models_path,
            "store_type_check_resolution": self.store_type_check_resolution,
            "critical_files": [
                critical_file.serialize() for critical_file in self.critical_files
            ],
            **(
                {}
                if self.saved_state_action is None
                else {"saved_state_action": self.saved_state_action.serialize()}
            ),
            "skip_initial_type_check": self.skip_initial_type_check,
            "use_lazy_module_tracking": self.use_lazy_module_tracking,
        }


def get_critical_files(
    configuration: frontend_configuration.Base,
) -> List[CriticalFile]:
    def get_full_path(root: str, relative: str) -> str:
        full_path = (Path(root) / relative).resolve(strict=False)
        if not full_path.exists():
            LOG.warning(f"Critical file does not exist: {full_path}")
        return str(full_path)

    local_root = configuration.get_local_root()
    return [
        CriticalFile(
            policy=MatchPolicy.FULL_PATH,
            path=get_full_path(
                root=str(configuration.get_global_root()),
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
                        root=str(local_root),
                        relative=find_directories.LOCAL_CONFIGURATION_FILE,
                    ),
                )
            ]
        ),
        *(
            # TODO(T92070475): This is a temporary hack until generated code can be
            # fully supported.
            []
            if configuration.get_buck_targets() is None
            else [CriticalFile(policy=MatchPolicy.EXTENSION, path="thrift")]
        ),
        *(
            [
                CriticalFile(
                    policy=MatchPolicy.FULL_PATH,
                    path=get_full_path(root=path, relative=""),
                )
                for path in configuration.get_other_critical_files()
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


def create_server_arguments(
    configuration: frontend_configuration.Base,
    start_arguments: command_arguments.StartArguments,
) -> Arguments:
    """
    Translate client configurations and command-line flags to server
    configurations.

    This API is not pure since it needs to access filesystem to filter out
    nonexistent directories. It is idempotent though, since it does not alter
    any filesystem state.
    """
    source_paths = backend_arguments.get_source_path_for_server(configuration)

    logging_sections = start_arguments.logging_sections
    additional_logging_sections = (
        [] if logging_sections is None else logging_sections.split(",")
    )
    if start_arguments.noninteractive:
        additional_logging_sections.append("-progress")
    # Server section is usually useful when Pyre server is involved
    additional_logging_sections.append("server")

    log_directory = configuration.get_log_directory()
    profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if start_arguments.enable_profiling
        else None
    )
    memory_profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if start_arguments.enable_memory_profiling
        else None
    )

    global_root = configuration.get_global_root()
    relative_local_root = configuration.get_relative_local_root()
    return Arguments(
        base_arguments=backend_arguments.BaseArguments(
            log_path=str(log_directory),
            global_root=str(global_root),
            checked_directory_allowlist=backend_arguments.get_checked_directory_allowlist(
                configuration, source_paths
            ),
            checked_directory_blocklist=(configuration.get_ignore_all_errors()),
            debug=start_arguments.debug,
            excludes=configuration.get_excludes(),
            extensions=configuration.get_valid_extension_suffixes(),
            relative_local_root=relative_local_root,
            memory_profiling_output=memory_profiling_output,
            number_of_workers=configuration.get_number_of_workers(),
            parallel=not start_arguments.sequential,
            profiling_output=profiling_output,
            python_version=configuration.get_python_version(),
            shared_memory=configuration.get_shared_memory(),
            remote_logging=backend_arguments.RemoteLogging.create(
                configuration.get_remote_logger(),
                start_arguments.get_log_identifier(),
            ),
            search_paths=configuration.get_existent_search_paths(),
            source_paths=source_paths,
        ),
        strict=configuration.is_strict(),
        show_error_traces=start_arguments.show_error_traces,
        additional_logging_sections=additional_logging_sections,
        watchman_root=None
        if start_arguments.no_watchman
        else backend_arguments.find_watchman_root(global_root),
        taint_models_path=configuration.get_taint_models_path(),
        store_type_check_resolution=start_arguments.store_type_check_resolution,
        critical_files=get_critical_files(configuration),
        saved_state_action=None
        if start_arguments.no_saved_state
        else get_saved_state_action(
            start_arguments, relative_local_root=relative_local_root
        ),
        skip_initial_type_check=start_arguments.skip_initial_type_check,
        use_lazy_module_tracking=start_arguments.use_lazy_module_tracking,
        socket_path=daemon_socket.get_socket_path(
            configuration.get_project_identifier(),
            flavor=start_arguments.flavor,
        ),
    )


def _run_in_foreground(
    command: Sequence[str], environment: Mapping[str, str]
) -> commands.ExitCode:
    # In foreground mode, we shell out to the backend server and block on it.
    # Server stdout/stderr will be forwarded to the current terminal.
    return_code = 0
    try:
        LOG.warning("Starting server in the foreground...")
        # lint-ignore: NoUnsafeExecRule
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
def background_server_log_file(
    log_directory: Path,
    flavor: identifiers.PyreFlavor,
) -> Iterator[TextIO]:
    new_server_log_directory = log_directory / "new_server"
    new_server_log_directory.mkdir(parents=True, exist_ok=True)
    log_file_path = daemon_log_path(
        log_directory=new_server_log_directory,
        flavor=flavor,
        now=datetime.datetime.now(),
    )
    # lint-ignore: NoUnsafeFilesystemRule
    with open(str(log_file_path), "a") as log_file:
        yield log_file
    # Symlink the log file to a known location for subsequent `pyre incremental`
    # to find.
    _create_symbolic_link(
        deamon_current_log_path(
            log_directory=new_server_log_directory,
            flavor=flavor,
        ),
        log_file_path,
    )


def _run_in_background(
    command: Sequence[str],
    environment: Mapping[str, str],
    log_directory: Path,
    socket_path: Path,
    flavor: identifiers.PyreFlavor,
    event_waiter: server_event.Waiter,
) -> commands.ExitCode:
    # In background mode, we asynchronously start the server with `Popen` and
    # detach it from the current process immediately with `start_new_session`.
    # Do not call `wait()` on the Popen object to avoid blocking.
    # Server stderr will be forwarded to dedicated log files.
    # Server stdout will be used as additional communication channel for status
    # updates.
    with background_server_log_file(log_directory, flavor=flavor) as server_stderr:
        log_file = Path(server_stderr.name)
        # lint-ignore: NoUnsafeExecRule
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
        stop.remove_socket_if_exists(socket_path)

        raise commands.ClientException("Interrupted by user. No server is spawned.")


def run_start(
    configuration: frontend_configuration.Base,
    start_arguments: command_arguments.StartArguments,
) -> commands.ExitCode:
    binary_location = configuration.get_binary_location(download_if_needed=True)
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    log_directory = configuration.get_log_directory()
    server_arguments = create_server_arguments(configuration, start_arguments)
    if not start_arguments.no_watchman and server_arguments.watchman_root is None:
        LOG.warning(
            "Cannot find a watchman root. Incremental check will not be functional"
            " since no filesystem updates will be sent to the Pyre server."
        )

    LOG.info(f"Starting server at `{configuration.get_project_identifier()}`...")
    with backend_arguments.temporary_argument_file(
        server_arguments
    ) as argument_file_path:
        server_command = [str(binary_location), "newserver", str(argument_file_path)]
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
            socket_path = daemon_socket.get_socket_path(
                configuration.get_project_identifier(),
                flavor=start_arguments.flavor,
            )
            return _run_in_background(
                server_command,
                server_environment,
                log_directory,
                socket_path,
                flavor=start_arguments.flavor,
                event_waiter=server_event.Waiter(
                    wait_on_initialization=start_arguments.wait_on_initialization
                ),
            )


def run(
    configuration: configuration_module.Configuration,
    start_arguments: command_arguments.StartArguments,
) -> commands.ExitCode:
    return run_start(frontend_configuration.OpenSource(configuration), start_arguments)
