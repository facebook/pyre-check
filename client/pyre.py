# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import json
import logging
import os
import shutil
import sys
import time
import traceback
from dataclasses import replace
from pathlib import Path
from typing import Iterable, List, Optional

import click

from . import (
    buck,
    command_arguments,
    commands,
    configuration as configuration_module,
    filesystem,
    log,
    recently_used_configurations,
    statistics as statistics_module,
)
from .commands import Command, ExitCode, v2
from .commands.analyze import MissingFlowsKind
from .exceptions import EnvironmentException
from .version import __version__


LOG: logging.Logger = logging.getLogger(__name__)


def _log_statistics(
    command: Command,
    start_time: float,
    client_exception_message: str,
    error_message: Optional[str],
    exit_code: int,
) -> None:
    configuration = command.configuration
    if configuration is not None:
        statistics_module.log_with_configuration(
            category=statistics_module.LoggerCategory.USAGE,
            configuration=configuration,
            integers={
                "exit_code": exit_code,
                "runtime": int((time.time() - start_time) * 1000),
            },
            normals={
                "cwd": os.getcwd(),
                "client_version": __version__,
                "command_line": " ".join(sys.argv),
                "command": command.NAME,
                "client_exception": client_exception_message,
                "error_message": error_message,
            },
        )


def _show_pyre_version_as_text(
    binary_version: Optional[str], client_version: str
) -> None:
    if binary_version:
        log.stdout.write(f"Binary version: {binary_version}\n")
    log.stdout.write(f"Client version: {__version__}\n")


def _show_pyre_version_as_json(
    binary_version: Optional[str], client_version: str
) -> None:
    version_json = {
        **({} if binary_version is None else {"binary": binary_version}),
        "client": client_version,
    }
    log.stdout.write(f"{json.dumps(version_json)}\n")


def _show_pyre_version(arguments: command_arguments.CommandArguments) -> None:
    binary_version: Optional[str] = None
    client_version: str = __version__
    try:
        configuration = configuration_module.create_configuration(arguments, Path("."))
        binary_version = configuration.get_binary_version()
    except Exception:
        pass
    if arguments.output == command_arguments.JSON:
        _show_pyre_version_as_json(binary_version, client_version)
    else:
        _show_pyre_version_as_text(binary_version, client_version)


def run_pyre_command(
    command: Command,
    configuration: configuration_module.Configuration,
    noninteractive: bool,
) -> ExitCode:
    start_time = time.time()

    client_exception_message = ""
    # Having this as a fails-by-default helps flag unexpected exit
    # from exception flows.
    exit_code = ExitCode.FAILURE
    try:
        _check_configuration(configuration)
        log.start_logging_to_directory(noninteractive, configuration.log_directory)
        LOG.debug(f"Running cli command `{' '.join(sys.argv)}`...")
        exit_code = command.run().exit_code()
    except (buck.BuckException, EnvironmentException) as error:
        client_exception_message = str(error)
        exit_code = ExitCode.FAILURE
        if isinstance(error, buck.BuckUserError):
            exit_code = ExitCode.BUCK_USER_ERROR
        elif isinstance(error, buck.BuckException):
            exit_code = ExitCode.BUCK_INTERNAL_ERROR
    except (configuration_module.InvalidConfiguration) as error:
        client_exception_message = str(error)
        exit_code = ExitCode.CONFIGURATION_ERROR
    except commands.ClientException as error:
        client_exception_message = str(error)
        exit_code = ExitCode.FAILURE
    except Exception:
        client_exception_message = traceback.format_exc()
        exit_code = ExitCode.FAILURE
    except KeyboardInterrupt:
        LOG.warning("Interrupted by user")
        LOG.debug(traceback.format_exc())
        exit_code = ExitCode.SUCCESS
    finally:
        if len(client_exception_message) > 0:
            LOG.error(client_exception_message)
        result = command.result()
        error_message = result.error if result else None
        command.cleanup()
        _log_statistics(
            command,
            start_time,
            client_exception_message,
            error_message,
            exit_code,
        )
    return exit_code


def _run_check_command(arguments: command_arguments.CommandArguments) -> ExitCode:
    configuration = _create_configuration_with_retry(arguments, Path("."))
    return run_pyre_command(
        commands.Check(
            arguments, original_directory=os.getcwd(), configuration=configuration
        ),
        configuration,
        arguments.noninteractive,
    )


def _run_incremental_command(
    arguments: command_arguments.CommandArguments,
    nonblocking: bool,
    incremental_style: commands.IncrementalStyle,
    no_start_server: bool,
    no_watchman: bool,
) -> ExitCode:
    configuration = _create_configuration_with_retry(arguments, Path("."))
    if configuration.use_command_v2:
        _check_configuration(configuration)
        log.start_logging_to_directory(
            arguments.noninteractive, configuration.log_directory
        )
        start_arguments = command_arguments.StartArguments(
            changed_files_path=arguments.changed_files_path,
            debug=arguments.debug,
            enable_memory_profiling=arguments.enable_memory_profiling,
            enable_profiling=arguments.enable_profiling,
            load_initial_state_from=arguments.load_initial_state_from,
            log_identifier=arguments.log_identifier,
            logging_sections=arguments.logging_sections,
            no_saved_state=arguments.no_saved_state,
            no_watchman=no_watchman,
            noninteractive=arguments.noninteractive,
            save_initial_state_to=arguments.save_initial_state_to,
            saved_state_project=arguments.saved_state_project,
            sequential=arguments.sequential,
            show_error_traces=arguments.show_error_traces,
            store_type_check_resolution=False,
            terminal=False,
            wait_on_initialization=True,
        )
        return v2.incremental.run(
            configuration,
            command_arguments.IncrementalArguments(
                output=arguments.output,
                no_start=no_start_server,
                start_arguments=start_arguments,
            ),
        )
    else:
        return run_pyre_command(
            commands.Incremental(
                arguments,
                original_directory=os.getcwd(),
                configuration=configuration,
                nonblocking=nonblocking,
                incremental_style=incremental_style,
                no_start_server=no_start_server,
                no_watchman=no_watchman,
            ),
            configuration,
            arguments.noninteractive,
        )


def _run_default_command(arguments: command_arguments.CommandArguments) -> ExitCode:
    if shutil.which("watchman"):
        return _run_incremental_command(
            arguments=arguments,
            nonblocking=False,
            incremental_style=commands.IncrementalStyle.FINE_GRAINED,
            no_start_server=False,
            no_watchman=False,
        )
    else:
        watchman_link = "https://facebook.github.io/watchman/docs/install"
        LOG.warning(
            "No watchman binary found. \n"
            + "To enable pyre incremental, "
            + f"you can install watchman: {watchman_link}"
        )
        LOG.warning("Defaulting to non-incremental check.")
        return _run_check_command(arguments)


def _run_servers_list_command(
    arguments: command_arguments.CommandArguments,
) -> ExitCode:
    if arguments.use_command_v2:
        return v2.servers.run_list()
    else:
        configuration = configuration_module.create_configuration(arguments, Path("."))
        return run_pyre_command(
            commands.Servers(
                arguments,
                original_directory=os.getcwd(),
                configuration=configuration,
                subcommand="list",
            ),
            configuration,
            arguments.noninteractive,
        )


def _create_configuration_with_retry(
    arguments: command_arguments.CommandArguments, base_directory: Path
) -> configuration_module.Configuration:
    configuration = configuration_module.create_configuration(arguments, base_directory)
    if (
        configuration.source_directories is not None
        or configuration.targets is not None
    ):
        return configuration

    if arguments.local_configuration is not None:
        raise configuration_module.InvalidConfiguration(
            "No buck targets or source directories to analyze.\nHint: Include"
            + ' a "source_directories" or "targets" entry in your local'
            + " configuration file."
        )

    # Heuristic: If neither `source_directories` nor `targets` is specified,
    # and if there exists recently-used local configurations, we guess that
    # the user may have forgotten to specifiy `-l`.
    error_message = "No buck targets or source directories to analyze."
    recently_used_local_roots = recently_used_configurations.Cache(
        configuration.dot_pyre_directory
    ).get_all_items()
    if len(recently_used_local_roots) == 0:
        raise configuration_module.InvalidConfiguration(error_message)

    LOG.warning(error_message)
    local_root_for_rerun = recently_used_configurations.prompt_user_for_local_root(
        recently_used_local_roots
    )
    if local_root_for_rerun is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot determine which recent local root to rerun. "
        )

    LOG.warning(f"Running pyre under local root `{local_root_for_rerun}`...")
    LOG.warning(
        f"Hint: To avoid this prompt, run `pyre -l {local_root_for_rerun}` "
        + f"or `cd {local_root_for_rerun} && pyre`."
    )
    new_configuration = configuration_module.create_configuration(
        replace(arguments, local_configuration=local_root_for_rerun), base_directory
    )
    if (
        new_configuration.source_directories is not None
        or new_configuration.targets is not None
    ):
        return new_configuration
    raise configuration_module.InvalidConfiguration(error_message)


def _check_configuration(configuration: configuration_module.Configuration) -> None:
    configuration_module.check_nested_local_configuration(configuration)
    configuration_module.check_open_source_version(configuration)


@click.group(
    invoke_without_command=True,
    context_settings={"help_option_names": ["-h", "--help"]},
)
@click.pass_context
@click.option(
    "-l",
    "--local-configuration",
    type=str,
    help="Specify a path where Pyre could find a local configuration.",
)
@click.option(
    "--version",
    is_flag=True,
    default=False,
    help="Print the client and binary versions of Pyre.",
)
@click.option("--debug/--no-debug", default=False, hidden=True)
@click.option(
    "--sequential/--no-sequential",
    default=None,
    help="Run Pyre in single-threaded mode.",
)
@click.option(
    "--strict/--no-strict",
    default=None,
    help="Check all file in strict mode by default.",
)
@click.option("--additional-check", type=str, multiple=True, hidden=True)
@click.option("--show-error-traces/--no-show-error-traces", default=False, hidden=True)
@click.option(
    "--output",
    type=click.Choice(
        [command_arguments.TEXT, command_arguments.JSON], case_sensitive=False
    ),
    default=command_arguments.TEXT,
    help="How to format output.",
)
@click.option("--enable-profiling/--no-enable-profiling", default=False, hidden=True)
@click.option(
    "--enable-memory-profiling/--no-enable-memory-profiling", default=False, hidden=True
)
@click.option(
    "-n",
    "--noninteractive",
    is_flag=True,
    help="Enable verbose non-interactive logging.",
)
@click.option("--logging-sections", type=str, hidden=True)
@click.option("--log-identifier", type=str, default=None, hidden=True)
@click.option("--dot-pyre-directory", type=str, hidden=True)
@click.option("--logger", type=str, hidden=True)
@click.option("--formatter", type=str, hidden=True)
@click.option(
    "--target",
    type=str,
    multiple=True,
    help=(
        "The buck target to check. "
        "Can be specified multiple times to include multiple directories."
    ),
)
@click.option(
    "--use-buck-builder/--use-legacy-buck-builder",
    default=None,
    help="Use Pyre's own Java builder for Buck projects.",
)
@click.option("--buck-mode", type=str, help="Mode to pass to `buck query`")
@click.option(
    "--use-buck-source-database/--no-use-buck-source-database",
    default=None,
    hidden=True,
)
@click.option(
    "--source-directory",
    type=str,
    multiple=True,
    help=(
        "The source directory to check. "
        "Can be specified multiple times to include multiple directories."
    ),
)
@click.option("--filter-directory", type=str, hidden=True)
@click.option(
    "--no-saved-state",
    is_flag=True,
    hidden=True,
    help="Do not attempt loading Pyre from saved state.",
)
@click.option(
    "--search-path",
    type=str,
    multiple=True,
    help=(
        "Additional directory of modules and stubs to include in the type environment. "
        "Can be specified multiple times to include multiple directories."
    ),
)
@click.option(
    "--binary", type=str, show_envvar=True, help="Override location of the Pyre binary."
)
@click.option(
    "--buck-builder-binary",
    type=str,
    show_envvar=True,
    help="Override location of the buck builder binary.",
)
@click.option("--exclude", type=str, multiple=True, hidden=True)
@click.option(
    "--typeshed",
    type=str,
    show_envvar=True,
    help="Override location of the typeshed stubs.",
)
@click.option("--save-initial-state-to", type=str, hidden=True)
@click.option("--load-initial-state-from", type=str, hidden=True)
@click.option("--changed-files-path", type=str, hidden=True)
@click.option("--saved-state-project", type=str, hidden=True)
@click.option("--features", type=str, hidden=True)
@click.option("--use-command-v2", is_flag=True, default=None, hidden=True)
@click.option("--isolation-prefix", type=str, hidden=True)
@click.option(
    "--python-version",
    type=str,
    help=(
        "Specify the version of Python in which the codebase is written."
        " Pyre recognizes version string in the form of 'X.Y.Z'."
        " By default, the Python version used to run `pyre` itself is used."
    ),
    hidden=True,
)
def pyre(
    context: click.Context,
    local_configuration: Optional[str],
    version: bool,
    debug: bool,
    sequential: Optional[bool],
    strict: Optional[bool],
    additional_check: Iterable[str],
    show_error_traces: bool,
    output: str,
    enable_profiling: bool,
    enable_memory_profiling: bool,
    noninteractive: bool,
    logging_sections: Optional[str],
    log_identifier: Optional[str],
    dot_pyre_directory: Optional[str],
    logger: Optional[str],
    formatter: Optional[str],
    target: Iterable[str],
    use_buck_builder: Optional[bool],
    buck_mode: Optional[str],
    use_buck_source_database: Optional[bool],
    source_directory: Iterable[str],
    filter_directory: Optional[str],
    no_saved_state: bool,
    search_path: Iterable[str],
    binary: Optional[str],
    buck_builder_binary: Optional[str],
    exclude: Iterable[str],
    typeshed: Optional[str],
    save_initial_state_to: Optional[str],
    load_initial_state_from: Optional[str],
    changed_files_path: Optional[str],
    saved_state_project: Optional[str],
    features: Optional[str],
    use_command_v2: Optional[bool],
    isolation_prefix: Optional[str],
    python_version: Optional[str],
) -> int:
    arguments = command_arguments.CommandArguments(
        local_configuration=local_configuration,
        version=version,
        debug=debug,
        sequential=sequential or False,
        strict=strict or False,
        additional_checks=list(additional_check),
        show_error_traces=show_error_traces,
        output=output,
        enable_profiling=enable_profiling,
        enable_memory_profiling=enable_memory_profiling,
        noninteractive=noninteractive,
        logging_sections=logging_sections,
        log_identifier=log_identifier,
        logger=logger,
        formatter=formatter,
        targets=list(target),
        use_buck_builder=use_buck_builder,
        use_buck_source_database=use_buck_source_database,
        source_directories=list(source_directory),
        filter_directory=filter_directory,
        buck_mode=buck_mode,
        no_saved_state=no_saved_state,
        search_path=list(search_path),
        binary=binary,
        buck_builder_binary=buck_builder_binary,
        exclude=list(exclude),
        typeshed=typeshed,
        save_initial_state_to=save_initial_state_to,
        load_initial_state_from=load_initial_state_from,
        changed_files_path=changed_files_path,
        saved_state_project=saved_state_project,
        dot_pyre_directory=Path(dot_pyre_directory)
        if dot_pyre_directory is not None
        else None,
        features=features,
        use_command_v2=use_command_v2,
        isolation_prefix=isolation_prefix,
        python_version=python_version,
    )
    if arguments.version:
        _show_pyre_version(arguments)
        return ExitCode.SUCCESS

    context.ensure_object(dict)
    context.obj["arguments"] = arguments

    if context.invoked_subcommand is None:
        return _run_default_command(arguments)

    # This return value is not used anywhere.
    return ExitCode.SUCCESS


@pyre.command()
@click.argument("analysis", type=str, default="taint")
@click.option(
    "--taint-models-path",
    type=filesystem.readable_directory,
    multiple=True,
    help="Location of taint models.",
)
@click.option(
    "--no-verify",
    is_flag=True,
    default=False,
    help="Do not verify models for the taint analysis.",
)
@click.option(
    "--save-results-to",
    type=filesystem.writable_directory,
    help="Directory to write analysis results to.",
)
@click.option("--dump-call-graph", is_flag=True, default=False, hidden=True)
@click.option("--repository-root", type=os.path.abspath)
@click.option(
    "--rule",
    type=int,
    multiple=True,
    help="Only track taint flows for the given rule.",
)
@click.option(
    "--find-missing-flows",
    type=click.Choice([kind.value for kind in MissingFlowsKind]),
    help="Perform a taint analysis to find flows through obscure models.",
)
@click.option(
    "--dump-model-query-results",
    is_flag=True,
    default=False,
    help="Provide model query debugging output.",
)
@click.option(
    "--use-cache",
    is_flag=True,
    default=False,
    help="Store information in .pyre/pysa.cache for faster runs.",
)
@click.option(
    "--inline-decorators",
    is_flag=True,
    default=False,
    help="Inline decorators at use sites to catch flows through decorators.",
)
@click.pass_context
def analyze(
    context: click.Context,
    analysis: str,
    taint_models_path: Iterable[str],
    no_verify: bool,
    save_results_to: Optional[str],
    dump_call_graph: bool,
    repository_root: Optional[str],
    rule: Iterable[int],
    find_missing_flows: Optional[str],
    dump_model_query_results: bool,
    use_cache: bool,
    inline_decorators: bool,
) -> int:
    """
    Run Pysa, the inter-procedural static analysis tool.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))
    rules = list(rule)
    return run_pyre_command(
        commands.Analyze(
            command_argument,
            original_directory=os.getcwd(),
            configuration=configuration,
            analysis=analysis,
            taint_models_path=list(taint_models_path),
            no_verify=no_verify,
            save_results_to=save_results_to,
            dump_call_graph=dump_call_graph,
            repository_root=repository_root,
            rules=list(rules) if len(rules) > 0 else None,
            find_missing_flows=(
                MissingFlowsKind(find_missing_flows)
                if find_missing_flows is not None
                else None
            ),
            dump_model_query_results=dump_model_query_results,
            use_cache=use_cache,
            inline_decorators=inline_decorators,
        ),
        configuration,
        command_argument.noninteractive,
    )


@pyre.command()
@click.pass_context
def check(context: click.Context) -> int:
    """
    Runs a one-time type check of a Python project.
    """
    return _run_check_command(context.obj["arguments"])


@pyre.command()
@click.option(
    "--nonblocking",
    is_flag=True,
    default=False,
    help=(
        "[DEPRECATED] Ask the server to return partial results immediately, "
        "even if analysis is still in progress."
    ),
)
@click.option(
    "--incremental-style",
    type=click.Choice(
        [
            str(commands.IncrementalStyle.SHALLOW),
            str(commands.IncrementalStyle.FINE_GRAINED),
        ]
    ),
    default=str(commands.IncrementalStyle.FINE_GRAINED),
    help="[DEPRECATED] How to approach doing incremental checks.",
)
@click.option("--no-start", is_flag=True, default=False, hidden=True)
# This is mostly to allow `restart` to pass on the flag to `start`.
@click.option("--no-watchman", is_flag=True, default=False, hidden=True)
@click.pass_context
def incremental(
    context: click.Context,
    nonblocking: bool,
    incremental_style: str,
    no_start: bool,
    no_watchman: bool,
) -> int:
    """
    Connects to a running Pyre server and returns the current type errors for your
    project. If no server exists for your projects, starts a new one. Running `pyre`
    implicitly runs `pyre incremental`.

    By default, incremental checks ensure that all dependencies of changed files are
    analyzed before returning results. If you'd like to get partial type checking
    results eagerly, you can run `pyre incremental --nonblocking`.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    return _run_incremental_command(
        arguments=command_argument,
        nonblocking=nonblocking,
        incremental_style=commands.IncrementalStyle.SHALLOW
        if incremental_style == str(commands.IncrementalStyle.SHALLOW)
        else commands.IncrementalStyle.FINE_GRAINED,
        no_start_server=no_start,
        no_watchman=no_watchman,
    )


@pyre.command()
@click.argument("modify_paths", type=filesystem.file_or_directory_exists, nargs=-1)
@click.option(
    "-p",
    "--print-only",
    is_flag=True,
    default=False,
    help=(
        "Print raw JSON errors to standard output, without converting to stubs "
        "or annnotating."
    ),
)
@click.option(
    "-f",
    "--full-only",
    is_flag=True,
    default=False,
    help="Only output fully annotated functions.",
)
@click.option(
    "-r",
    "--recursive",
    is_flag=True,
    default=False,
    help="Recursively run infer until no new annotations are generated.",
)
@click.option(
    "-i",
    "--in-place",
    is_flag=True,
    default=False,
    help="Modifies original files and add inferred annotations to all functions.",
)
@click.option(
    "--json",
    is_flag=True,
    default=False,
    help="Accept JSON input instead of running full check.",
)
@click.option(
    "--annotate-from-existing-stubs",
    is_flag=True,
    default=False,
    help="Add annotations from existing stubs.",
)
@click.option(
    "--debug-infer",
    is_flag=True,
    default=False,
    help="Print error message when file fails to annotate.",
)
@click.option(
    "--full-stubs",
    is_flag=True,
    default=False,
    help="Include pre-existing annotations in the generated stubs.",
)
@click.pass_context
def infer(
    context: click.Context,
    modify_paths: Iterable[str],
    print_only: bool,
    full_only: bool,
    recursive: bool,
    in_place: bool,
    json: bool,
    annotate_from_existing_stubs: bool,
    debug_infer: bool,
    full_stubs: bool,
) -> int:
    """
    Try adding type annotations to untyped codebase.
    """
    in_place_paths = list(modify_paths) if in_place else None
    full_stub_paths = list(modify_paths) if full_stubs else None
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))
    return run_pyre_command(
        commands.Infer(
            command_argument,
            original_directory=os.getcwd(),
            configuration=configuration,
            print_errors=print_only,
            full_only=full_only,
            recursive=recursive,
            in_place=in_place_paths,
            errors_from_stdin=json,
            annotate_from_existing_stubs=annotate_from_existing_stubs,
            debug_infer=debug_infer,
            full_stub_paths=full_stub_paths,
        ),
        configuration,
        command_argument.noninteractive,
    )


@pyre.command()
@click.option(
    "--local",
    is_flag=True,
    default=False,
    help="[DEPRECATED] Initializes a local configuration.",
)
@click.pass_context
def init(context: click.Context, local: bool) -> int:
    """
    Create a pyre configuration file at the current directory.
    """
    return commands.Initialize().run().exit_code()


@pyre.command()
@click.option(
    "--with-fire", is_flag=True, default=False, help="A no-op flag that adds emphasis."
)
@click.pass_context
def kill(context: click.Context, with_fire: bool) -> int:
    """
    Force all running Pyre servers to terminate.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = configuration_module.create_configuration(
        command_argument, Path(".")
    )
    if configuration.use_command_v2:
        return v2.kill.run(configuration, with_fire)
    else:
        return run_pyre_command(
            commands.Kill(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
                with_fire=with_fire,
            ),
            configuration,
            command_argument.noninteractive,
        )


@pyre.command()
@click.option("--no-watchman", is_flag=True, default=False, hidden=True)
@click.pass_context
def persistent(context: click.Context, no_watchman: bool) -> int:
    """
    Entry point for IDE integration to Pyre. Communicates with a Pyre server using
    the Language Server Protocol, accepts input from stdin and writing diagnostics
    and responses from the Pyre server to stdout.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = configuration_module.create_configuration(
        command_argument, Path(".")
    )
    if configuration.use_command_v2:
        log.start_logging_to_directory(
            # Always log to file regardless of whether `-n` is given
            noninteractive=False,
            log_directory=configuration.log_directory,
        )
        return v2.persistent.run(
            configuration,
            command_arguments.StartArguments(
                changed_files_path=command_argument.changed_files_path,
                debug=command_argument.debug,
                enable_memory_profiling=command_argument.enable_memory_profiling,
                enable_profiling=command_argument.enable_profiling,
                load_initial_state_from=command_argument.load_initial_state_from,
                log_identifier=command_argument.log_identifier,
                logging_sections=command_argument.logging_sections,
                no_saved_state=command_argument.no_saved_state,
                no_watchman=no_watchman,
                noninteractive=command_argument.noninteractive,
                save_initial_state_to=command_argument.save_initial_state_to,
                saved_state_project=command_argument.saved_state_project,
                sequential=command_argument.sequential,
                show_error_traces=command_argument.show_error_traces,
                store_type_check_resolution=False,
                terminal=False,
                wait_on_initialization=True,
            ),
        )
    else:
        return run_pyre_command(
            commands.Persistent(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
                no_watchman=no_watchman,
            ),
            configuration,
            True,
        )


@pyre.command()
@click.option("--no-watchman", is_flag=True, default=False, hidden=True)
@click.pass_context
def pysa_language_server(context: click.Context, no_watchman: bool) -> int:
    """
    Entry point for IDE integration to Pysa. Communicates with a Pysa server using
    the Language Server Protocol, accepts input from stdin and writing diagnostics
    and responses from the Pysa server to stdout.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = configuration_module.create_configuration(
        command_argument, Path(".")
    )
    log.start_logging_to_directory(
        # Always log to file regardless of whether `-n` is given
        noninteractive=False,
        log_directory=configuration.log_directory,
    )
    return v2.pysa_server.run(
        configuration,
        command_arguments.StartArguments(
            changed_files_path=command_argument.changed_files_path,
            debug=command_argument.debug,
            enable_memory_profiling=command_argument.enable_memory_profiling,
            enable_profiling=command_argument.enable_profiling,
            load_initial_state_from=command_argument.load_initial_state_from,
            log_identifier=command_argument.log_identifier,
            logging_sections=command_argument.logging_sections,
            no_saved_state=command_argument.no_saved_state,
            no_watchman=no_watchman,
            noninteractive=command_argument.noninteractive,
            save_initial_state_to=command_argument.save_initial_state_to,
            saved_state_project=command_argument.saved_state_project,
            sequential=command_argument.sequential,
            show_error_traces=command_argument.show_error_traces,
            store_type_check_resolution=False,
            terminal=False,
            wait_on_initialization=True,
        ),
    )


@pyre.command()
@click.option(
    "--profile-output",
    type=click.Choice([str(x) for x in commands.ProfileOutput]),
    default=str(commands.ProfileOutput.COLD_START_PHASES),
    help="Specify what to output.",
)
@click.pass_context
def profile(context: click.Context, profile_output: str) -> int:
    """
    Display profiling output.
    """

    def get_profile_output(profile_output: str) -> commands.ProfileOutput:
        for item in commands.ProfileOutput:
            if str(item) == profile_output:
                return item
        raise ValueError(f"Unrecognized value for --profile-output: {profile_output}")

    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))
    return run_pyre_command(
        commands.Profile(
            command_argument,
            original_directory=os.getcwd(),
            configuration=configuration,
            profile_output=get_profile_output(profile_output),
        ),
        configuration,
        command_argument.noninteractive,
    )


@pyre.command()
@click.argument("query", type=str)
@click.pass_context
def query(context: click.Context, query: str) -> int:
    """
    Query a running Pyre server for type, function, and attribute information.

    `https://pyre-check.org/docs/querying-pyre.html` contains examples and
    documentation for this command.

    To get a full list of queries, you can run `pyre query help`.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))

    if configuration.use_command_v2:
        log.start_logging_to_directory(
            command_argument.noninteractive, configuration.log_directory
        )
        return v2.query.run(configuration, query)
    else:
        return run_pyre_command(
            commands.Query(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
                query=query,
            ),
            configuration,
            command_argument.noninteractive,
        )


@pyre.command()
@click.option(
    "--output-file",
    type=os.path.abspath,
    help="The path to the output file (defaults to stdout)",
)
@click.option(
    "--server-log-count",
    type=int,
    default=3,
    help="Number of server logs to include in the diagnositics. Default to 3.",
)
@click.pass_context
def rage(
    context: click.Context, output_file: Optional[str], server_log_count: int
) -> int:
    """
    Collects troubleshooting diagnostics for Pyre, and writes this information
    to the terminal or to a file.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = configuration_module.create_configuration(
        command_argument, Path(".")
    )

    if configuration.use_command_v2:
        return v2.rage.run(
            configuration,
            command_arguments.RageArguments(
                output=Path(output_file) if output_file is not None else None,
                server_log_count=server_log_count,
            ),
        )
    else:
        return run_pyre_command(
            commands.Rage(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
                output_path=output_file,
            ),
            configuration,
            command_argument.noninteractive,
        )


@pyre.command()
@click.option(
    "--terminal", is_flag=True, default=False, help="Run the server in the terminal."
)
@click.option(
    "--store-type-check-resolution",
    is_flag=True,
    default=False,
    help="Store extra information for `types` queries.",
)
@click.option(
    "--no-watchman",
    is_flag=True,
    default=False,
    help="Do not spawn a watchman client in the background.",
)
@click.option(
    "--incremental-style",
    type=click.Choice(
        [
            str(commands.IncrementalStyle.SHALLOW),
            str(commands.IncrementalStyle.FINE_GRAINED),
        ]
    ),
    default=str(commands.IncrementalStyle.FINE_GRAINED),
    help="[DEPRECATED] How to approach doing incremental checks.",
)
@click.pass_context
def restart(
    context: click.Context,
    terminal: bool,
    store_type_check_resolution: bool,
    no_watchman: bool,
    incremental_style: str,
) -> int:
    """
    Restarts a server. Equivalent to `pyre stop && pyre`.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))
    if configuration.use_command_v2:
        _check_configuration(configuration)
        log.start_logging_to_directory(
            command_argument.noninteractive, configuration.log_directory
        )
        start_arguments = command_arguments.StartArguments(
            changed_files_path=command_argument.changed_files_path,
            debug=command_argument.debug,
            enable_memory_profiling=command_argument.enable_memory_profiling,
            enable_profiling=command_argument.enable_profiling,
            load_initial_state_from=command_argument.load_initial_state_from,
            log_identifier=command_argument.log_identifier,
            logging_sections=command_argument.logging_sections,
            no_saved_state=command_argument.no_saved_state,
            no_watchman=no_watchman,
            noninteractive=command_argument.noninteractive,
            save_initial_state_to=command_argument.save_initial_state_to,
            saved_state_project=command_argument.saved_state_project,
            sequential=command_argument.sequential,
            show_error_traces=command_argument.show_error_traces,
            store_type_check_resolution=store_type_check_resolution,
            terminal=terminal,
            wait_on_initialization=True,
        )
        return v2.restart.run(
            configuration,
            command_arguments.IncrementalArguments(
                output=command_argument.output,
                no_start=False,
                start_arguments=start_arguments,
            ),
        )
    else:
        return run_pyre_command(
            commands.Restart(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
                terminal=terminal,
                store_type_check_resolution=store_type_check_resolution,
                use_watchman=not no_watchman,
                incremental_style=commands.IncrementalStyle.SHALLOW
                if incremental_style == str(commands.IncrementalStyle.SHALLOW)
                else commands.IncrementalStyle.FINE_GRAINED,
            ),
            configuration,
            command_argument.noninteractive,
        )


@pyre.group(
    invoke_without_command=True,
)
@click.pass_context
def servers(context: click.Context) -> int:
    """
    Commands to manipulate multiple Pyre servers.
    """
    if context.invoked_subcommand is None:
        return _run_servers_list_command(context.obj["arguments"])
    # This return value is not used anywhere.
    return ExitCode.SUCCESS


@servers.command(name="list")
@click.pass_context
def servers_list(context: click.Context) -> int:
    """
    List all running servers.
    """
    return _run_servers_list_command(context.obj["arguments"])


@servers.command(name="stop")
@click.pass_context
def servers_stop(context: click.Context) -> int:
    """
    Stop all running servers.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    if command_argument.use_command_v2:
        return v2.servers.run_stop()
    else:
        configuration = configuration_module.create_configuration(
            command_argument, Path(".")
        )
        return run_pyre_command(
            commands.Servers(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
                subcommand="stop",
            ),
            configuration,
            command_argument.noninteractive,
        )


@pyre.command()
@click.option(
    "--terminal", is_flag=True, default=False, help="Run the server in the terminal."
)
@click.option(
    "--store-type-check-resolution",
    is_flag=True,
    default=False,
    help="Store extra information for `types` queries.",
)
@click.option(
    "--no-watchman",
    is_flag=True,
    default=False,
    help="Do not spawn a watchman client in the background.",
)
@click.option(
    "--incremental-style",
    type=click.Choice(
        [
            str(commands.IncrementalStyle.SHALLOW),
            str(commands.IncrementalStyle.FINE_GRAINED),
        ]
    ),
    default=str(commands.IncrementalStyle.FINE_GRAINED),
    help="[DEPRECATED] How to approach doing incremental checks.",
)
@click.option(
    "--wait-on-initialization/--no-wait-on-initialization",
    default=False,
    hidden=True,
    help="When `--terminal` is unset, wait for server initialization to finish.",
)
@click.pass_context
def start(
    context: click.Context,
    terminal: bool,
    store_type_check_resolution: bool,
    no_watchman: bool,
    incremental_style: str,
    wait_on_initialization: bool,
) -> int:
    """
    Starts a pyre server as a daemon.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))
    if configuration.use_command_v2:
        _check_configuration(configuration)
        log.start_logging_to_directory(
            command_argument.noninteractive, configuration.log_directory
        )
        return v2.start.run(
            configuration,
            command_arguments.StartArguments(
                changed_files_path=command_argument.changed_files_path,
                debug=command_argument.debug,
                enable_memory_profiling=command_argument.enable_memory_profiling,
                enable_profiling=command_argument.enable_profiling,
                load_initial_state_from=command_argument.load_initial_state_from,
                log_identifier=command_argument.log_identifier,
                logging_sections=command_argument.logging_sections,
                no_saved_state=command_argument.no_saved_state,
                no_watchman=no_watchman,
                noninteractive=command_argument.noninteractive,
                save_initial_state_to=command_argument.save_initial_state_to,
                saved_state_project=command_argument.saved_state_project,
                sequential=command_argument.sequential,
                show_error_traces=command_argument.show_error_traces,
                store_type_check_resolution=store_type_check_resolution,
                terminal=terminal,
                wait_on_initialization=wait_on_initialization,
            ),
        )
    else:
        return run_pyre_command(
            commands.Start(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
                terminal=terminal,
                store_type_check_resolution=store_type_check_resolution,
                use_watchman=not no_watchman,
                incremental_style=commands.IncrementalStyle.SHALLOW
                if incremental_style == str(commands.IncrementalStyle.SHALLOW)
                else commands.IncrementalStyle.FINE_GRAINED,
            ),
            configuration,
            command_argument.noninteractive,
        )


@pyre.command()
# TODO[T60916205]: Rename this argument, it doesn't make sense anymore
@click.argument("filter_paths", type=filesystem.exists, nargs=-1)
@click.option(
    "--log-results",
    is_flag=True,
    default=False,
    help="Log the statistics results to external tables.",
)
@click.pass_context
def statistics(
    context: click.Context, filter_paths: Iterable[str], log_results: bool
) -> int:
    """
    Collect various syntactic metrics on type coverage.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))
    return run_pyre_command(
        commands.Statistics(
            command_argument,
            original_directory=os.getcwd(),
            configuration=configuration,
            filter_paths=list(filter_paths),
            log_results=log_results,
        ),
        configuration,
        command_argument.noninteractive,
    )


@pyre.command()
@click.pass_context
def stop(context: click.Context) -> int:
    """
    Signals the Pyre server to stop.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = configuration_module.create_configuration(
        command_argument, Path(".")
    )
    if configuration.use_command_v2:
        log.start_logging_to_directory(
            command_argument.noninteractive, configuration.log_directory
        )
        return v2.stop.run(configuration)
    else:
        return run_pyre_command(
            commands.Stop(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
            ),
            configuration,
            command_argument.noninteractive,
        )


@pyre.command()
@click.pass_context
def validate_models(context: click.Context) -> int:
    """
    Validate the taint models for the given project by querying the Pyre server.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration_with_retry(command_argument, Path("."))

    if configuration.use_command_v2:
        log.start_logging_to_directory(
            command_argument.noninteractive, configuration.log_directory
        )
        return v2.validate_models.run(configuration, output=command_argument.output)
    else:
        return run_pyre_command(
            commands.ValidateModels(
                command_argument,
                original_directory=os.getcwd(),
                configuration=configuration,
            ),
            configuration,
            command_argument.noninteractive,
        )


# Need the default argument here since this is our entry point in setup.py
def main(argv: List[str] = sys.argv[1:]) -> int:
    noninteractive = ("-n" in argv) or ("--noninteractive" in argv)
    with log.configured_logger(noninteractive):
        try:
            return_code = pyre(argv, auto_envvar_prefix="PYRE", standalone_mode=False)
        except configuration_module.InvalidConfiguration as error:
            LOG.error(str(error))
            return ExitCode.CONFIGURATION_ERROR
        except click.ClickException as error:
            error.show()
            return_code = ExitCode.FAILURE
        except Exception as error:
            LOG.error(str(error))
            return_code = ExitCode.FAILURE
    return return_code


if __name__ == "__main__":
    try:
        os.getcwd()
    except FileNotFoundError:
        LOG.error(
            "Pyre could not determine the current working directory. "
            "Has it been removed?\nExiting."
        )
        sys.exit(ExitCode.FAILURE)
    sys.exit(main(sys.argv[1:]))
