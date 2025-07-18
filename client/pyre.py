# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Entry point for the `pyre` command in open-source Pyre.

The interface is defined using click decorators. See
https://click.palletsprojects.com/ for documentation, in
particular the "Commands and Groups" subsection to understand
how click works with subcommands.
"""

import json
import logging
import os
import re
import shutil
import sys
import textwrap
import traceback
from pathlib import Path
from typing import Iterable, List, Optional, Sequence

import click

from . import (
    backend_arguments,
    command_arguments,
    commands,
    configuration as configuration_module,
    frontend_configuration,
    identifiers,
    log,
    version,
)
from .language_server import features as language_server_features

LOG: logging.Logger = logging.getLogger(__name__)
CLASSIC_FLAVOR: identifiers.PyreFlavor = identifiers.PyreFlavor.CLASSIC


def show_pyre_version_as_text(
    binary_version: Optional[str], client_version: str
) -> None:
    if binary_version:
        log.stdout.write(f"Binary version: {binary_version}\n")
    log.stdout.write(f"Client version: {client_version}\n")


def show_pyre_version_as_json(
    binary_version: Optional[str], client_version: str
) -> None:
    version_json = {
        **({} if binary_version is None else {"binary": binary_version}),
        "client": client_version,
    }
    log.stdout.write(f"{json.dumps(version_json)}\n")


def _show_pyre_version(arguments: command_arguments.CommandArguments) -> None:
    # Open-source binary version is usually not useful to show to the user
    if arguments.output == command_arguments.JSON:
        show_pyre_version_as_json(
            binary_version=None, client_version=version.__version__
        )
    else:
        show_pyre_version_as_text(
            binary_version=None, client_version=version.__version__
        )


def start_logging_to_directory(
    log_directory_path: Path,
    flavor: identifiers.PyreFlavor,
) -> None:
    log_directory_path.mkdir(parents=True, exist_ok=True)
    log.enable_file_logging(log_directory_path / f"pyre{flavor.path_suffix()}.stderr")


def _run_check_command(
    arguments: command_arguments.CommandArguments,
) -> commands.ExitCode:
    configuration = _create_and_check_configuration(arguments, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), CLASSIC_FLAVOR)
    check_arguments = command_arguments.CheckArguments.create(arguments)
    return commands.check.run(configuration, check_arguments)


def _run_incremental_command(
    arguments: command_arguments.CommandArguments,
    no_start_server: bool,
    no_watchman: bool,
) -> commands.ExitCode:
    configuration = _create_and_check_configuration(arguments, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), CLASSIC_FLAVOR)
    start_arguments = command_arguments.StartArguments.create(
        arguments,
        no_watchman=no_watchman,
        store_type_check_resolution=False,
        terminal=False,
        wait_on_initialization=True,
    )
    return commands.incremental.run(
        configuration,
        command_arguments.IncrementalArguments(
            output=arguments.output,
            no_start=no_start_server,
            start_arguments=start_arguments,
        ),
    ).exit_code


def _run_default_command(
    arguments: command_arguments.CommandArguments,
) -> commands.ExitCode:
    if shutil.which("watchman"):
        return _run_incremental_command(
            arguments=arguments,
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


def _check_open_source_version(
    configuration: configuration_module.Configuration,
) -> None:
    """
    Check if version specified in configuration matches running version and warn
    if it does not.
    """
    expected_version = configuration.version_hash
    if expected_version is None or not re.match(r"\d+\.\d+\.\d+", expected_version):
        return

    try:
        from .version import __version__ as actual_version

        if expected_version != actual_version:
            LOG.warning(
                textwrap.dedent(
                    f"""\
                    Your running version does not match the configured version for this
                    project (running {actual_version}, expected {expected_version})."""
                )
            )
    except ImportError:
        pass


def _create_configuration(
    arguments: command_arguments.CommandArguments,
    base_directory: Path,
) -> frontend_configuration.OpenSource:
    configuration = configuration_module.create_configuration(arguments, base_directory)
    return frontend_configuration.OpenSource(configuration)


def _create_and_check_configuration(
    arguments: command_arguments.CommandArguments,
    base_directory: Path,
) -> frontend_configuration.OpenSource:
    configuration = configuration_module.create_configuration(arguments, base_directory)
    _check_open_source_version(configuration)
    return frontend_configuration.OpenSource(configuration)


@click.group(
    invoke_without_command=True,
    context_settings={"help_option_names": ["-h", "--help"]},
)
@click.pass_context
@click.option(
    "--version",
    is_flag=False,
    type=click.Choice([kind.value for kind in command_arguments.VersionKind]),
    flag_value=command_arguments.VersionKind.CLIENT_AND_BINARY.value,
    default=command_arguments.VersionKind.NONE.value,
    help="Print the versions of Pyre.",
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
@click.option("--show-error-traces/--no-show-error-traces", default=False, hidden=True)
@click.option(
    "--output",
    type=click.Choice(
        [command_arguments.TEXT, command_arguments.JSON, command_arguments.SARIF],
        case_sensitive=False,
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
@click.option(
    "--log-level",
    type=click.Choice(
        ["CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG"], case_sensitive=True
    ),
    default="DEBUG",
    help="The granularity of the messages the Pyre client will log. Default is 'DEBUG'.",
)
@click.option("--logging-sections", type=str, hidden=True)
@click.option(
    "--dot-pyre-directory",
    type=str,
    help="The directory Pyre will use for log output and cache.",
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
@click.option(
    "--only-check-paths",
    type=str,
    multiple=True,
    help=(
        "Report type errors for the given locations, rather than the default "
        "directories."
    ),
    hidden=True,
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
    "--optional-search-path",
    type=str,
    multiple=True,
    help=(
        "Additional directory of modules and stubs to include in the type environment. Existence on disk is unnecessary. "
        "Can be specified multiple times to include multiple directories."
    ),
)
@click.option(
    "--binary", type=str, show_envvar=True, help="Override location of the Pyre binary."
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
@click.option(
    "--shared-memory-heap-size",
    type=int,
    help="Size of the shared memory heap, in bytes.",
    hidden=True,
)
@click.option(
    "--shared-memory-dependency-table-power",
    type=int,
    help="Power of the dependency table in shared memory.",
    hidden=True,
)
@click.option(
    "--shared-memory-hash-table-power",
    type=int,
    help="Power of the hash table in shared memory.",
    hidden=True,
)
@click.option("--number-of-workers", type=int, help="Number of parallel workers to use")
@click.option(
    "--max-number-of-workers",
    type=int,
    help="When `--number-of-workers` is not set, this is the maximum number when calculating the default.",
)
@click.option("--no-logger", is_flag=True, default=False, hidden=True)
def pyre(
    context: click.Context,
    version: command_arguments.VersionKind,
    debug: bool,
    sequential: Optional[bool],
    strict: Optional[bool],
    show_error_traces: bool,
    output: str,
    enable_profiling: bool,
    enable_memory_profiling: bool,
    noninteractive: bool,
    log_level: str,
    logging_sections: Optional[str],
    dot_pyre_directory: Optional[str],
    source_directory: Iterable[str],
    only_check_paths: Iterable[str],
    search_path: Iterable[str],
    optional_search_path: Iterable[str],
    binary: Optional[str],
    exclude: Iterable[str],
    typeshed: Optional[str],
    save_initial_state_to: Optional[str],
    load_initial_state_from: Optional[str],
    changed_files_path: Optional[str],
    python_version: Optional[str],
    shared_memory_heap_size: Optional[int],
    shared_memory_dependency_table_power: Optional[int],
    shared_memory_hash_table_power: Optional[int],
    number_of_workers: Optional[int],
    max_number_of_workers: Optional[int],
    no_logger: bool,
) -> None:
    arguments = command_arguments.CommandArguments(
        local_configuration=None,
        version=version,
        debug=debug,
        sequential=sequential or False,
        strict=strict or False,
        show_error_traces=show_error_traces,
        output=output,
        enable_profiling=enable_profiling,
        enable_memory_profiling=enable_memory_profiling,
        noninteractive=noninteractive,
        logging_sections=logging_sections,
        log_identifier=None,
        logger=None,
        no_logger=no_logger,
        targets=[],
        source_directories=list(source_directory),
        only_check_paths=list(only_check_paths),
        buck_mode=None,
        no_saved_state=True,
        search_path=list(search_path),
        optional_search_path=list(optional_search_path),
        binary=binary,
        exclude=list(exclude),
        typeshed=typeshed,
        save_initial_state_to=save_initial_state_to,
        load_initial_state_from=load_initial_state_from,
        changed_files_path=changed_files_path,
        dot_pyre_directory=(
            Path(dot_pyre_directory) if dot_pyre_directory is not None else None
        ),
        isolation_prefix=None,
        python_version=python_version,
        shared_memory_heap_size=shared_memory_heap_size,
        shared_memory_dependency_table_power=shared_memory_dependency_table_power,
        shared_memory_hash_table_power=shared_memory_hash_table_power,
        number_of_workers=number_of_workers,
        max_number_of_workers=max_number_of_workers,
        use_buck2=None,
    )
    context.ensure_object(dict)
    context.obj["arguments"] = arguments


@pyre.command()
@click.argument("analysis", type=str, default="taint")
@click.option(
    "--taint-models-path",
    type=click.Path(exists=True, file_okay=False, dir_okay=True, readable=True),
    multiple=True,
    help="Location of taint models.",
)
@click.option(
    "--no-verify",
    is_flag=True,
    default=False,
    help="Do not verify models or DSL model queries for the taint analysis.",
)
@click.option(
    "--verify-dsl",
    is_flag=True,
    default=False,
    help="Verify DSL model queries for the taint analysis.",
)
@click.option(
    "--verify-taint-config-only",
    is_flag=True,
    default=False,
    help="Verify taint.config files. Skips analysis.",
)
@click.option(
    "--version",
    is_flag=False,
    type=click.Choice([kind.value for kind in command_arguments.VersionKind]),
    flag_value=command_arguments.VersionKind.CLIENT_AND_BINARY.value,
    default=command_arguments.VersionKind.NONE.value,
    help="Print the versions of Pysa.",
)
@click.option(
    "--save-results-to",
    type=click.Path(),
    help="Directory to write analysis results to.",
)
@click.option(
    "--output-format",
    type=click.Choice([kind.value for kind in command_arguments.TaintOutputFormat]),
    help="Format of the taint output file(s).",
)
@click.option(
    "--pyrefly-results",
    type=click.Path(),
    help="Directory to pyrefly results (pyrefly --report-pysa).",
)
@click.option(
    "--dump-call-graph",
    type=str,
    help="Dump the call graph in the given file.",
)
@click.option("--repository-root", type=os.path.abspath)
@click.option(
    "--rule",
    type=int,
    multiple=True,
    help="Only track taint flows for the given rule(s).",
)
@click.option(
    "--source",
    type=str,
    multiple=True,
    help="Only track taint flows for the given source(s).",
)
@click.option(
    "--sink",
    type=str,
    multiple=True,
    help="Only track taint flows for the given sink(s).",
)
@click.option(
    "--transform",
    type=str,
    multiple=True,
    help="Only track taint flows for the given transform(s).",
)
@click.option(
    "--find-missing-flows",
    type=click.Choice([kind.value for kind in command_arguments.MissingFlowsKind]),
    help="Perform a taint analysis to find flows through obscure models.",
)
@click.option(
    "--dump-model-query-results",
    type=str,
    help="Dump model query results in the given file.",
)
@click.option(
    "--use-cache",
    is_flag=True,
    default=False,
    help="Store information in .pyre/pysa.cache for faster runs.",
)
@click.option(
    "--build-cache-only",
    is_flag=True,
    default=False,
    help="Build the cache and exit without computing results..",
)
@click.option(
    "--infer-self-tito/--no-infer-self-tito",
    is_flag=True,
    default=True,
    help="Infer taint propagations (tito) from arguments to `self` for all methods.",
)
@click.option(
    "--infer-argument-tito/--no-infer-argument-tito",
    is_flag=True,
    default=False,
    help="Infer taint propagations (tito) from arguments to other arguments.",
)
@click.option(
    "--maximum-model-source-tree-width",
    type=int,
    help="Limits the width of the source tree in the model for a callable.",
)
@click.option(
    "--maximum-model-sink-tree-width",
    type=int,
    help="Limits the width of the sink tree in the model for a callable.",
)
@click.option(
    "--maximum-model-tito-tree-width",
    type=int,
    help="Limits the width of the tito tree in the model for a callable.",
)
@click.option(
    "--maximum-tree-depth-after-widening",
    type=int,
    help="Limits the depth of source, sink and tito trees within loops.",
)
@click.option(
    "--maximum-return-access-path-width",
    type=int,
    help="Limits the width of the return access path tree in the model for a callable.",
)
@click.option(
    "--maximum-return-access-path-depth-after-widening",
    type=int,
    help="Limits the depth of the return access path tree within loops.",
)
@click.option(
    "--maximum-tito-collapse-depth",
    type=int,
    help="Limits the depth of taint trees after applying taint-in-taitn-out.",
)
@click.option(
    "--maximum-tito-positions",
    type=int,
    help="Limits the number of tito positions.",
)
@click.option(
    "--maximum-overrides-to-analyze",
    type=int,
    help="Limits the number of overrides to consider at a call site.",
)
@click.option(
    "--maximum-trace-length",
    type=int,
    help="Limit the trace length of taint flows.",
)
@click.option(
    "--maximum-tito-depth",
    type=int,
    help="Limit the depth of inferred taint-in-taint-out in taint flows.",
)
@click.option(
    "--check-invariants",
    is_flag=True,
    default=False,
    help="Perform additional assertions about analysis invariants.",
)
@click.option(
    "--limit-entrypoints",
    is_flag=True,
    default=False,
    help="Only analyze functions within the call graph of entrypoint models.",
)
@click.option(
    "--compact-ocaml-heap",
    is_flag=True,
    default=False,
    help="Compact OCaml heap during the analysis to save memory.",
)
@click.option(
    "--compute-coverage",
    is_flag=True,
    default=False,
    help="Whether to compute the file, kind, and rule coverage.",
)
@click.option(
    "--scheduler-policies",
    type=click.Path(exists=True, file_okay=True, dir_okay=False, readable=True),
    help="Path to a configuration file containing scheduler policies.",
    hidden=True,
)
@click.option(
    "--kill-buck-after-build",
    is_flag=True,
    default=False,
    help="Kill Buck after building the project.",
)
@click.option(
    "--number-of-buck-threads",
    type=int,
    help="Limits the number of threads during Buck build.",
)
@click.option(
    "--higher-order-call-graph-max-iterations",
    type=int,
    help="Limits the number of fixpoint iterations when building higher order call graphs.",
)
@click.option(
    "--maximum-target-depth",
    type=int,
    help="Limits the depth of parameterized targets that are created during higher order call graph building.",
)
@click.option(
    "--maximum-parameterized-targets-at-call-site",
    type=int,
    help="Limits the number of parameterized targets that can be created at any call site, during higher order call graph building.",
)
@click.pass_context
def analyze(
    context: click.Context,
    analysis: str,
    taint_models_path: Iterable[str],
    no_verify: bool,
    verify_dsl: bool,
    verify_taint_config_only: bool,
    version: command_arguments.VersionKind,
    save_results_to: Optional[str],
    output_format: Optional[str],
    pyrefly_results: Optional[str],
    dump_call_graph: Optional[str],
    repository_root: Optional[str],
    rule: Iterable[int],
    source: Iterable[str],
    sink: Iterable[str],
    transform: Iterable[str],
    find_missing_flows: Optional[str],
    dump_model_query_results: Optional[str],
    use_cache: bool,
    build_cache_only: bool,
    infer_self_tito: bool,
    infer_argument_tito: bool,
    maximum_model_source_tree_width: Optional[int],
    maximum_model_sink_tree_width: Optional[int],
    maximum_model_tito_tree_width: Optional[int],
    maximum_tree_depth_after_widening: Optional[int],
    maximum_return_access_path_width: Optional[int],
    maximum_return_access_path_depth_after_widening: Optional[int],
    maximum_tito_collapse_depth: Optional[int],
    maximum_tito_positions: Optional[int],
    maximum_overrides_to_analyze: Optional[int],
    maximum_trace_length: Optional[int],
    maximum_tito_depth: Optional[int],
    check_invariants: bool,
    limit_entrypoints: bool,
    compact_ocaml_heap: bool,
    compute_coverage: bool,
    scheduler_policies: Optional[str],
    kill_buck_after_build: bool,
    number_of_buck_threads: Optional[int],
    higher_order_call_graph_max_iterations: Optional[int],
    maximum_target_depth: Optional[int],
    maximum_parameterized_targets_at_call_site: Optional[int],
) -> int:
    """
    Run Pysa, the inter-procedural static analysis tool.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    if version != command_arguments.VersionKind.NONE:
        _show_pyre_version(command_argument)
        return commands.ExitCode.SUCCESS

    configuration = _create_and_check_configuration(command_argument, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), CLASSIC_FLAVOR)
    return commands.analyze.run(
        configuration,
        command_arguments.AnalyzeArguments(
            debug=command_argument.debug,
            dump_call_graph=dump_call_graph,
            dump_model_query_results=dump_model_query_results,
            enable_memory_profiling=command_argument.enable_memory_profiling,
            enable_profiling=command_argument.enable_profiling,
            find_missing_flows=(
                command_arguments.MissingFlowsKind(find_missing_flows)
                if find_missing_flows is not None
                else None
            ),
            infer_self_tito=infer_self_tito,
            infer_argument_tito=infer_argument_tito,
            log_identifier=command_argument.log_identifier,
            maximum_model_source_tree_width=maximum_model_source_tree_width,
            maximum_model_sink_tree_width=maximum_model_sink_tree_width,
            maximum_model_tito_tree_width=maximum_model_tito_tree_width,
            maximum_tree_depth_after_widening=maximum_tree_depth_after_widening,
            maximum_return_access_path_width=maximum_return_access_path_width,
            maximum_return_access_path_depth_after_widening=maximum_return_access_path_depth_after_widening,
            maximum_tito_collapse_depth=maximum_tito_collapse_depth,
            maximum_tito_positions=maximum_tito_positions,
            maximum_overrides_to_analyze=maximum_overrides_to_analyze,
            maximum_tito_depth=maximum_tito_depth,
            maximum_trace_length=maximum_trace_length,
            no_verify=no_verify,
            verify_dsl=verify_dsl,
            verify_taint_config_only=verify_taint_config_only,
            output=command_argument.output,
            repository_root=repository_root,
            rule=list(rule),
            source=list(source),
            sink=list(sink),
            transform=list(transform),
            save_results_to=save_results_to,
            output_format=(
                command_arguments.TaintOutputFormat(output_format)
                if output_format is not None
                else None
            ),
            pyrefly_results=pyrefly_results,
            sequential=command_argument.sequential,
            taint_models_path=list(taint_models_path),
            use_cache=use_cache,
            build_cache_only=build_cache_only,
            check_invariants=check_invariants,
            limit_entrypoints=limit_entrypoints,
            compact_ocaml_heap=compact_ocaml_heap,
            saved_state_arguments=command_arguments.PysaSavedStateArguments(),
            compute_coverage=compute_coverage,
            scheduler_policies_path=(
                Path(scheduler_policies) if scheduler_policies is not None else None
            ),
            kill_buck_after_build=kill_buck_after_build,
            number_of_buck_threads=number_of_buck_threads,
            higher_order_call_graph_max_iterations=higher_order_call_graph_max_iterations,
            maximum_target_depth=maximum_target_depth,
            maximum_parameterized_targets_at_call_site=maximum_parameterized_targets_at_call_site,
        ),
    )


@pyre.command()
@click.pass_context
def check(context: click.Context) -> int:
    """
    Runs a one-time type check of a Python project.
    """
    return _run_check_command(context.obj["arguments"])


@pyre.command()
@click.option("--no-start", is_flag=True, default=False, hidden=True)
# This is mostly to allow `restart` to pass on the flag to `start`.
@click.option("--no-watchman", is_flag=True, default=False, hidden=True)
@click.pass_context
def incremental(
    context: click.Context,
    no_start: bool,
    no_watchman: bool,
) -> int:
    """
    Connects to a running Pyre server and returns the current type errors for your
    project. If no server exists for your projects, starts a new one. Running `pyre`
    implicitly runs `pyre incremental`.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    return _run_incremental_command(
        arguments=command_argument,
        no_start_server=no_start,
        no_watchman=no_watchman,
    )


@pyre.command()
@click.argument(
    "paths_to_modify",
    type=click.Path(exists=True),
    nargs=-1,
)
@click.option(
    "-p",
    "--print-only",
    is_flag=True,
    default=False,
    help=(
        "Print raw JSON inference data to standard output, without "
        "converting to stubs or annnotating."
    ),
)
@click.option(
    "-i",
    "--in-place",
    is_flag=True,
    default=False,
    help="Modifies original files and add inferred annotations to all functions.",
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
    "--read-stdin",
    is_flag=True,
    default=False,
    help="Read input from stdin instead of running a full infer.",
)
@click.option(
    "--annotate-attributes",
    is_flag=True,
    default=False,
    help=(
        "Allow infer to attempt to annotate class attributes? "
        "The code-generation logic for this is incomplete, so the "
        "default is False but you may manually enable it."
    ),
)
@click.option(
    "--use-future-annotations/--no-future-annotations",
    is_flag=True,
    default=False,
    help=(
        "Should pyre infer inject `from __future__ import annotations`? "
        "By default, we won't because it may not work depending on the "
        "version of python and libcst we ware using."
    ),
)
@click.option(
    "--quote-annotations",
    is_flag=True,
    default=False,
    help=(
        "Quote all added type annotations? "
        "This is recommended when using pyre infer prior to pysa "
        "because it allows us to avoid introducing imports, which "
        "is important because then the line numbers match checked-in "
        "code."
    ),
)
@click.option(
    "--simple-annotations",
    is_flag=True,
    default=False,
    help=(
        "Only infer the simplest annotations that are guaranteed to "
        "codemod with --in-place cleanly."
    ),
)
@click.option(
    "--dequalify",
    is_flag=True,
    default=False,
    help=(
        "Dequalify all annotations? This is a temporary flag, used to "
        "force fully-qualified names (e.g. sqlalchemy.sql.schema.Column) "
        "to be dequalified (e.g. Column). It is needed now because pyre "
        "infer doesn't yet know how to handle imports and qualified names "
        "in a principled way."
    ),
)
@click.option(
    "--kill-buck-after-build",
    is_flag=True,
    default=False,
    help="Kill Buck after building the project.",
)
@click.option(
    "--number-of-buck-threads",
    type=int,
    help="Limits the number of threads during Buck build.",
)
@click.pass_context
def infer(
    context: click.Context,
    paths_to_modify: List[str],
    print_only: bool,
    in_place: bool,
    annotate_from_existing_stubs: bool,
    debug_infer: bool,
    read_stdin: bool,
    annotate_attributes: bool,
    use_future_annotations: bool,
    quote_annotations: bool,
    simple_annotations: bool,
    dequalify: bool,
    kill_buck_after_build: bool,
    number_of_buck_threads: Optional[int],
) -> int:
    """
    Run pyre infer.

    The optional PATHS_TO_MODIFY argument is a list of directory or file
    paths to include when annotating in-place.

    If empty, then we'll annotate all relevant modules in-place, and it is
    ignored unless the `--in-place` flag is set.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration(command_argument, Path("."))
    working_directory = Path.cwd()
    modify_paths = (
        None
        if len(paths_to_modify) == 0
        else {working_directory / Path(path) for path in paths_to_modify}
    )
    return commands.infer.run(
        configuration,
        command_arguments.InferArguments(
            working_directory=working_directory,
            annotate_attributes=annotate_attributes,
            annotate_from_existing_stubs=annotate_from_existing_stubs,
            enable_memory_profiling=command_argument.enable_memory_profiling,
            enable_profiling=command_argument.enable_profiling,
            debug_infer=debug_infer,
            quote_annotations=quote_annotations,
            simple_annotations=simple_annotations,
            dequalify=dequalify,
            log_identifier=command_argument.log_identifier,
            logging_sections=command_argument.logging_sections,
            use_future_annotations=use_future_annotations,
            in_place=in_place,
            paths_to_modify=modify_paths,
            print_only=print_only,
            read_stdin=read_stdin,
            sequential=command_argument.sequential,
            kill_buck_after_build=kill_buck_after_build,
            number_of_buck_threads=number_of_buck_threads,
        ),
    )


@pyre.command()
@click.pass_context
def init(context: click.Context) -> int:
    """
    Create a pyre configuration file at the current directory.
    """
    return commands.initialize.run()


@pyre.command()
@click.option(
    "--skip-environment-setup",
    is_flag=True,
    default=False,
    help="Skip setting up an environment to run Pysa",
)
@click.pass_context
def init_pysa(context: click.Context, skip_environment_setup: bool) -> int:
    """
    Creates a suitable environment for running pyre analyze.
    """
    return commands.initialize_pysa.run(skip_environment_setup)


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
    configuration = _create_configuration(command_argument, Path("."))
    return commands.kill.run(configuration, with_fire)


@pyre.command()
@click.option(
    "--flavor",
    type=click.Choice(identifiers.PyreFlavor.persistent_choices()),
    help=(
        "Flavor of the pyre server. "
        "This is used to disambiguate paths and log handling."
    ),
    hidden=True,
)
@click.option(
    "--skip-initial-type-check/--no-skip-initial-type-check",
    default=False,
    hidden=True,
    help="Skip the initial type check of all in-project modules.",
)
@click.option(
    "--use-lazy-module-tracking/--no-use-lazy-module-tracking",
    default=False,
    hidden=True,
    help="Use lazy module tracking. This is experimental and cannot power full checks.",
)
@click.option(
    "--status-updates",
    type=click.Choice(
        [kind.value for kind in language_server_features.StatusUpdatesAvailability]
    ),
    default=language_server_features.LanguageServerFeatures.status_updates.value,
    help="Availability of the status updates language server feature",
    hidden=True,
)
@click.option(
    "--type-coverage",
    type=click.Choice(
        [kind.value for kind in language_server_features.TypeCoverageAvailability]
    ),
    default=language_server_features.LanguageServerFeatures.type_coverage.value,
    help="Availability of the type coverage langauge server feature",
    hidden=True,
)
@click.option(
    "--type-errors",
    type=click.Choice(
        [kind.value for kind in language_server_features.TypeErrorsAvailability]
    ),
    default=language_server_features.LanguageServerFeatures.type_errors.value,
    help="Availability of the type errors langauge server feature",
    hidden=True,
)
@click.option(
    "--unsaved-changes",
    type=click.Choice(
        [kind.value for kind in language_server_features.UnsavedChangesAvailability]
    ),
    default=language_server_features.LanguageServerFeatures.unsaved_changes.value,
    help="Availability support for Pyre analyzing unsaved editor buffers",
    hidden=True,
)
@click.pass_context
def persistent(
    context: click.Context,
    flavor: Optional[str],
    skip_initial_type_check: bool,
    use_lazy_module_tracking: bool,
    status_updates: str,
    type_coverage: str,
    type_errors: str,
    unsaved_changes: str,
) -> int:
    """
    Entry point for IDE integration to Pyre. Communicates with a Pyre server using
    the Language Server Protocol, accepts input from stdin and writing diagnostics
    and responses from the Pyre server to stdout.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    start_command_argument = command_arguments.StartArguments.create(
        command_argument=command_argument,
        flavor=CLASSIC_FLAVOR if flavor is None else identifiers.PyreFlavor(flavor),
        skip_initial_type_check=skip_initial_type_check,
        use_lazy_module_tracking=use_lazy_module_tracking,
    )
    base_directory: Path = Path(".").resolve()
    configuration = _create_configuration(command_argument, base_directory)
    start_logging_to_directory(
        configuration.get_log_directory(),
        flavor=start_command_argument.flavor,
    )
    return commands.persistent.run(
        read_server_options=commands.pyre_server_options.PyreServerOptions.create_reader(
            start_command_argument=start_command_argument,
            read_frontend_configuration=lambda: frontend_configuration.OpenSource(
                configuration_module.create_configuration(
                    command_argument, base_directory
                )
            ),
            language_server_features=language_server_features.LanguageServerFeatures(
                status_updates=language_server_features.StatusUpdatesAvailability(
                    status_updates
                ),
                type_coverage=language_server_features.TypeCoverageAvailability(
                    type_coverage
                ),
                type_errors=language_server_features.TypeErrorsAvailability(
                    type_errors
                ),
                unsaved_changes=language_server_features.UnsavedChangesAvailability(
                    unsaved_changes
                ),
                telemetry=language_server_features.TelemetryAvailability.DISABLED,
            ),
        ),
        remote_logging=backend_arguments.RemoteLogging.create(
            configuration.get_remote_logger(),
            start_command_argument.get_log_identifier(),
        ),
    )


@pyre.command()
@click.option(
    "--profile-output",
    type=click.Choice([str(x) for x in command_arguments.ProfileOutput]),
    default=str(command_arguments.ProfileOutput.COLD_START_PHASES),
    help="Specify what to output.",
)
@click.pass_context
def profile(context: click.Context, profile_output: str) -> int:
    """
    Display profiling output.
    """

    def get_profile_output(profile_output: str) -> command_arguments.ProfileOutput:
        for item in command_arguments.ProfileOutput:
            if str(item) == profile_output:
                return item
        raise ValueError(f"Unrecognized value for --profile-output: {profile_output}")

    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration(command_argument, Path("."))
    return commands.profile.run(
        configuration,
        get_profile_output(profile_output),
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
    configuration = _create_configuration(command_argument, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), CLASSIC_FLAVOR)
    return commands.pysa_server.run(
        configuration,
        command_arguments.StartArguments.create(
            command_argument,
            no_watchman=no_watchman,
            store_type_check_resolution=False,
            terminal=False,
            wait_on_initialization=True,
        ),
    )


@pyre.command()
@click.argument("query", type=str)
@click.option("--no-daemon", is_flag=True, default=False)
@click.option("--no-validation-on-class-lookup-failure", is_flag=True, default=False)
@click.pass_context
def query(
    context: click.Context,
    query: str,
    no_daemon: bool,
    no_validation_on_class_lookup_failure: bool,
) -> int:
    """
    Query a running Pyre server for type, function, and attribute information.

    `https://pyre-check.org/docs/querying-pyre.html` contains examples and
    documentation for this command.

    To get a full list of queries, you can run `pyre query help`.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration(command_argument, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), CLASSIC_FLAVOR)
    return commands.query.run(
        configuration,
        command_arguments.QueryArguments(
            query=query,
            no_daemon=no_daemon,
            no_validation_on_class_lookup_failure=no_validation_on_class_lookup_failure,
            check_arguments=command_arguments.CheckArguments.create(command_argument),
        ),
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
    configuration = _create_configuration(command_argument, Path("."))
    return commands.rage.run(
        configuration,
        command_arguments.RageArguments(
            output=Path(output_file) if output_file is not None else None,
            server_log_count=server_log_count,
        ),
    )


@pyre.command()
@click.pass_context
def info(
    context: click.Context,
) -> int:
    """
    Collects troubleshooting diagnostics for Pyre, and writes this information
    to the terminal or to a file.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration(command_argument, Path("."))
    return commands.info.run(
        configuration,
        command_argument,
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
@click.pass_context
def restart(
    context: click.Context,
    terminal: bool,
    store_type_check_resolution: bool,
    no_watchman: bool,
) -> int:
    """
    Restarts a server. Equivalent to `pyre stop && pyre`.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_and_check_configuration(command_argument, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), CLASSIC_FLAVOR)
    start_arguments = command_arguments.StartArguments.create(
        command_argument=command_argument,
        no_watchman=no_watchman,
        store_type_check_resolution=store_type_check_resolution,
        terminal=terminal,
        wait_on_initialization=True,
    )
    return commands.restart.run(
        configuration,
        command_arguments.IncrementalArguments(
            output=command_argument.output,
            no_start=False,
            start_arguments=start_arguments,
        ),
    )


@pyre.group(
    invoke_without_command=True,
)
@click.pass_context
def servers(context: click.Context) -> None:
    """
    Commands to manipulate multiple Pyre servers.
    """
    pass


@servers.result_callback()
@click.pass_context
def run_default_servers_command(
    context: click.Context,
    value: Optional[commands.ExitCode],
    *args: object,
    **kwargs: object,
) -> commands.ExitCode:
    if context.invoked_subcommand is None:
        arguments: command_arguments.CommandArguments = context.obj["arguments"]
        return commands.servers.run_list(arguments.output)
    elif value is not None:
        return value
    else:
        raise commands.ClientException(
            "Non-default serevers subcommand did not return a value"
        )


@servers.command(name="list")
@click.pass_context
def servers_list(context: click.Context) -> int:
    """
    List all running servers.
    """
    arguments: command_arguments.CommandArguments = context.obj["arguments"]
    return commands.servers.run_list(arguments.output)


@servers.command(name="stop")
@click.pass_context
def servers_stop(context: click.Context) -> int:
    """
    Stop all running servers.
    """
    return commands.servers.run_stop()


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
    "--wait-on-initialization/--no-wait-on-initialization",
    default=False,
    hidden=True,
    help="When `--terminal` is unset, wait for server initialization to finish.",
)
@click.option(
    "--skip-initial-type-check/--no-skip-initial-type-check",
    default=False,
    hidden=True,
    help="Skip the initial type check of all in-project modules.",
)
@click.option(
    "--use-lazy-module-tracking/--no-use-lazy-module-tracking",
    default=False,
    hidden=True,
    help="Use lazy module tracking. This is experimental and cannot power full checks.",
)
@click.option(
    "--analyze-external-sources",
    is_flag=True,
    default=False,
    hidden=True,
    help="Include external sources for type checks and queries.",
)
@click.option(
    "--flavor",
    type=click.Choice(identifiers.PyreFlavor.server_flavor_choices()),
    help=(
        "Flavor of the pyre server to stop."
        "This is used to disambiguate paths and log handling."
    ),
)
@click.option(
    "--number-of-buck-threads",
    type=int,
    help="Limits the number of threads during Buck build.",
)
@click.pass_context
def start(
    context: click.Context,
    terminal: bool,
    store_type_check_resolution: bool,
    no_watchman: bool,
    wait_on_initialization: bool,
    skip_initial_type_check: bool,
    use_lazy_module_tracking: bool,
    analyze_external_sources: bool,
    flavor: Optional[str],
    number_of_buck_threads: Optional[int],
) -> int:
    """
    Starts a pyre server as a daemon_socket.
    """
    flavor_choice = (
        identifiers.PyreFlavor(flavor) if flavor is not None else CLASSIC_FLAVOR
    )
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_and_check_configuration(command_argument, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), flavor_choice)
    return commands.start.run(
        configuration,
        command_arguments.StartArguments.create(
            command_argument=command_argument,
            flavor=flavor_choice,
            no_watchman=no_watchman,
            store_type_check_resolution=store_type_check_resolution,
            terminal=terminal,
            wait_on_initialization=wait_on_initialization,
            skip_initial_type_check=skip_initial_type_check,
            use_lazy_module_tracking=use_lazy_module_tracking,
            analyze_external_sources=analyze_external_sources,
            number_of_buck_threads=number_of_buck_threads,
        ),
    )


@pyre.command()
@click.pass_context
@click.argument("files_and_directories", type=str, nargs=-1)
def report(
    context: click.Context,
    files_and_directories: Iterable[str],
) -> int:
    """
    Report per-module data on strictness, suppressions, and function annotations.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = configuration_module.create_configuration(
        command_argument, Path(".")
    )
    paths: Optional[Sequence[Path]] = [Path(d) for d in files_and_directories]
    paths = None if len(paths) == 0 else paths
    return commands.report.run(
        frontend_configuration.OpenSource(configuration),
        paths=paths,
    )


@pyre.command()
@click.pass_context
@click.argument("files_and_directories", type=str, nargs=-1)
def report_any_expressions(
    context: click.Context,
    files_and_directories: Iterable[str],
) -> int:
    """
    Report on expressions whose types contain `typing.Any`.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = configuration_module.create_configuration(
        command_argument, Path(".")
    )
    paths: Optional[Sequence[Path]] = [Path(d) for d in files_and_directories]
    paths = None if len(paths) == 0 else paths
    return commands.report_any_expressions.run(
        frontend_configuration.OpenSource(configuration),
        paths=paths,
    )


@pyre.command()
@click.argument("files_and_directories", type=str, nargs=-1)
@click.option(
    "--log-results",
    is_flag=True,
    default=False,
    help="Log the statistics results to external tables.",
)
@click.option(
    "--aggregate",
    is_flag=True,
    default=False,
    help="Print aggregate instead of per-path data.",
)
@click.option(
    "--print-summary",
    is_flag=True,
    default=False,
    help="Pretty print human-readable type coverage summary for project.",
)
@click.pass_context
def statistics(
    context: click.Context,
    files_and_directories: Iterable[str],
    log_results: bool,
    aggregate: bool,
    print_summary: bool,
) -> int:
    """
    Collect various syntactic metrics on type coverage.

    If no paths are specified, defaults to counting all sources in the project.

    NOTE: `pyre statistics` is now in maintenance mode. Use `pyre report`
    instead, which provides a more useful data format, for new use cases.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration(command_argument, Path("."))
    paths: Optional[Sequence[Path]] = [Path(d) for d in files_and_directories]
    paths = None if len(paths) == 0 else paths
    return commands.statistics.run(
        configuration,
        command_arguments.StatisticsArguments(
            paths=paths,
            log_identifier=command_argument.log_identifier,
            log_results=log_results,
            aggregate=aggregate,
            print_summary=print_summary,
        ),
    )


@pyre.command()
@click.option(
    "--flavor",
    type=click.Choice(identifiers.PyreFlavor.server_flavor_choices()),
    help=(
        "Flavor of the pyre server to stop."
        "This is used to disambiguate paths and log handling."
    ),
)
@click.pass_context
def stop(context: click.Context, flavor: Optional[str]) -> int:
    """
    Signals the Pyre server to stop.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    flavor_choice = (
        identifiers.PyreFlavor(flavor) if flavor is not None else CLASSIC_FLAVOR
    )
    configuration = _create_and_check_configuration(command_argument, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), flavor_choice)
    return commands.stop.run(configuration, flavor_choice)


@pyre.command()
@click.pass_context
def validate_models(context: click.Context) -> int:
    """
    Validate the taint models for the given project by querying the Pyre server.
    """
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    configuration = _create_configuration(command_argument, Path("."))
    start_logging_to_directory(configuration.get_log_directory(), CLASSIC_FLAVOR)
    return commands.validate_models.run(configuration, output=command_argument.output)


@pyre.result_callback()
@click.pass_context
def run_default_command(
    context: click.Context,
    value: Optional[commands.ExitCode],
    *args: object,
    **kwargs: object,
) -> commands.ExitCode:
    command_argument: command_arguments.CommandArguments = context.obj["arguments"]
    if command_argument.version != command_arguments.VersionKind.NONE:
        _show_pyre_version(command_argument)
        return commands.ExitCode.SUCCESS
    elif context.invoked_subcommand is None:
        return _run_default_command(command_argument)
    elif value is not None:
        return value
    else:
        raise commands.ClientException("Non-default command did not return a value")


# Need the default argument here since this is our entry point in setup.py
def main(argv: List[str] = sys.argv[1:]) -> int:
    noninteractive = ("-n" in argv) or ("--noninteractive" in argv)
    logging_level = logging.DEBUG
    if "--log-level" in argv:
        log_level_descriptor_index = argv.index("--log-level") + 1
        if (
            log_level_descriptor_index < len(argv)
            and argv[log_level_descriptor_index] in log.LOG_LEVELS
        ):
            logging_level = log.LOG_LEVELS[argv[log_level_descriptor_index]]
    with log.configured_logger(noninteractive, logging_level=logging_level):
        try:
            return_code = pyre(argv, auto_envvar_prefix="PYRE", standalone_mode=False)
        except configuration_module.InvalidConfiguration as error:
            LOG.error(str(error))
            return commands.ExitCode.CONFIGURATION_ERROR
        except click.ClickException as error:
            error.show()
            return_code = commands.ExitCode.CLICK_EXCEPTION
        except commands.ClientException as error:
            for line in str(error).split("\n"):
                LOG.error(line)
            return_code = error.exit_code
        except Exception as error:
            LOG.error(str(error))
            traceback.print_exc()
            return_code = commands.ExitCode.FAILURE
    return return_code


def invoke_main() -> None:
    sys.exit(main(sys.argv[1:]))


if __name__ == "__main__":
    invoke_main()  # pragma: no cover
