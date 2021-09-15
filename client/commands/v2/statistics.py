# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import itertools
import json
import logging
import re
import time
from pathlib import Path
from typing import Any, Callable, Dict, Mapping, Iterable, Optional, Union

import libcst as cst

from ... import (
    commands,
    command_arguments,
    configuration as configuration_module,
    statistics_collectors as collectors,
    statistics_logger,
    log,
)
from . import remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


def find_roots(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> Iterable[Path]:
    filter_paths = statistics_arguments.filter_paths
    if len(filter_paths) > 0:

        def to_absolute_path(given: str) -> Path:
            path = Path(given)
            return path if path.is_absolute() else Path.cwd() / path

        return {to_absolute_path(path) for path in filter_paths}

    local_root = configuration.local_root
    if local_root is not None:
        return [Path(local_root)]

    return [Path.cwd()]


def find_paths_to_parse(
    configuration: configuration_module.Configuration, paths: Iterable[Path]
) -> Iterable[Path]:
    def _is_excluded(path: Path) -> bool:
        try:
            return any(
                [
                    re.match(exclude_pattern, str(path))
                    for exclude_pattern in configuration.excludes
                ]
            )
        except re.error:
            LOG.warning("Could not parse `excludes`: %s", configuration.excludes)
            return False

    def _should_ignore(path: Path) -> bool:
        return (
            path.name.startswith("__")
            or path.name.startswith(".")
            or _is_excluded(path)
        )

    def _get_paths_for_file(target_file: Path) -> Iterable[Path]:
        return (
            [target_file]
            if target_file.suffix == ".py" and not _should_ignore(target_file)
            else []
        )

    def _get_paths_in_directory(target_directory: Path) -> Iterable[Path]:
        return (
            path
            for path in target_directory.glob("**/*.py")
            if not _should_ignore(path)
        )

    return itertools.chain.from_iterable(
        _get_paths_for_file(path)
        if not path.is_dir()
        else _get_paths_in_directory(path)
        for path in paths
    )


def parse_text_to_module(text: str) -> Optional[cst.Module]:
    try:
        return cst.parse_module(text)
    except cst.ParserSyntaxError:
        return None


def parse_path_to_module(path: Path) -> Optional[cst.Module]:
    try:
        return parse_text_to_module(path.read_text())
    except FileNotFoundError:
        return None


def _collect_statistics_for_modules(
    modules: Mapping[Path, Union[cst.Module, cst.MetadataWrapper]],
    collector_factory: Callable[[], collectors.StatisticsCollector],
) -> Dict[str, Any]:
    result: Dict[str, Any] = {}
    for path, module in modules.items():
        collector = collector_factory()
        try:
            module.visit(collector)
            result[str(path)] = collector.build_json()
        except RecursionError:
            LOG.warning(f"LibCST encountered recursion error in `{path}`")
    return result


def _collect_annotation_statistics(
    modules: Mapping[Path, cst.Module]
) -> Dict[str, Any]:
    return _collect_statistics_for_modules(
        {path: cst.MetadataWrapper(module) for path, module in modules.items()},
        collectors.AnnotationCountCollector,
    )


def _collect_fixme_statistics(modules: Mapping[Path, cst.Module]) -> Dict[str, Any]:
    return _collect_statistics_for_modules(modules, collectors.FixmeCountCollector)


def _collect_ignore_statistics(modules: Mapping[Path, cst.Module]) -> Dict[str, Any]:
    return _collect_statistics_for_modules(modules, collectors.IgnoreCountCollector)


def _collect_strict_file_statistics(
    modules: Mapping[Path, cst.Module], strict_default: bool
) -> Dict[str, Any]:
    def collector_factory() -> collectors.StrictCountCollector:
        return collectors.StrictCountCollector(strict_default)

    return _collect_statistics_for_modules(modules, collector_factory)


@dataclasses.dataclass(frozen=True)
class StatisticsData:
    annotations: Dict[str, Any] = dataclasses.field(default_factory=dict)
    fixmes: Dict[str, Any] = dataclasses.field(default_factory=dict)
    ignores: Dict[str, Any] = dataclasses.field(default_factory=dict)
    strict: Dict[str, Any] = dataclasses.field(default_factory=dict)


def collect_statistics(sources: Iterable[Path], strict_default: bool) -> StatisticsData:
    modules: Dict[Path, cst.Module] = {}
    for path in sources:
        module = parse_path_to_module(path)
        if module is not None:
            modules[path] = module

    annotation_statistics = _collect_annotation_statistics(modules)
    fixme_statistics = _collect_fixme_statistics(modules)
    ignore_statistics = _collect_ignore_statistics(modules)
    strict_file_statistics = _collect_strict_file_statistics(modules, strict_default)
    return StatisticsData(
        annotations=annotation_statistics,
        fixmes=fixme_statistics,
        ignores=ignore_statistics,
        strict=strict_file_statistics,
    )


@dataclasses.dataclass(frozen=True)
class AggregatedStatisticsData:
    annotations: Dict[str, int] = dataclasses.field(default_factory=dict)
    fixmes: int = 0
    ignores: int = 0
    strict: int = 0
    unsafe: int = 0


def aggregate_statistics(data: StatisticsData) -> AggregatedStatisticsData:
    aggregate_annotations = {
        "return_count": 0,
        "annotated_return_count": 0,
        "globals_count": 0,
        "annotated_globals_count": 0,
        "parameter_count": 0,
        "annotated_parameter_count": 0,
        "attribute_count": 0,
        "annotated_attribute_count": 0,
        "partially_annotated_function_count": 0,
        "fully_annotated_function_count": 0,
        "line_count": 0,
    }
    for annotation_data in data.annotations.values():
        for key in aggregate_annotations.keys():
            aggregate_annotations[key] += annotation_data[key]

    return AggregatedStatisticsData(
        annotations=aggregate_annotations,
        fixmes=sum(len(fixmes) for fixmes in data.fixmes.values()),
        ignores=sum(len(ignores) for ignores in data.ignores.values()),
        strict=sum(strictness["strict_count"] for strictness in data.strict.values()),
        unsafe=sum(strictness["unsafe_count"] for strictness in data.strict.values()),
    )


def log_to_remote(
    configuration: configuration_module.Configuration, run_id: str, data: Dict[str, Any]
) -> None:
    def _log_fixmes(fixme_type: str, data: Dict[str, int], path: str) -> None:
        for error_code, count in data.items():
            statistics_logger.log_with_configuration(
                statistics_logger.LoggerCategory.FIXME_COUNTS,
                configuration,
                integers={"count": count},
                normals={
                    "run_id": run_id,
                    "code": error_code,
                    "type": fixme_type,
                    "path": path,
                },
            )

    for path, counts in data["annotations"].items():
        statistics_logger.log_with_configuration(
            statistics_logger.LoggerCategory.ANNOTATION_COUNTS,
            configuration,
            integers=counts,
            normals={"run_id": run_id, "path": path},
        )
    for path, counts in data["fixmes"].items():
        _log_fixmes("fixme", counts, path)
    for path, counts in data["ignores"].items():
        _log_fixmes("ignore", counts, path)
    for path, counts in data["strict"].items():
        statistics_logger.log_with_configuration(
            statistics_logger.LoggerCategory.STRICT_ADOPTION,
            configuration,
            integers=counts,
            normals={"run_id": run_id, "path": path},
        )


def run_statistics(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    data = collect_statistics(
        find_paths_to_parse(
            configuration, find_roots(configuration, statistics_arguments)
        ),
        strict_default=configuration.strict,
    )

    if statistics_arguments.print_aggregates:
        aggregated_data = aggregate_statistics(data)
        log.stdout.write(json.dumps(dataclasses.asdict(aggregated_data), indent=4))
    else:
        log.stdout.write(json.dumps(dataclasses.asdict(data), indent=4))
        if statistics_arguments.log_results:
            logger = configuration.logger
            if logger is None:
                LOG.warning("Skip remote logging since no logger is specified.")
            else:
                log_identifier = statistics_arguments.log_identifier
                run_id = (
                    log_identifier
                    if log_identifier is not None
                    else str(time.time_ns())
                )
                log_to_remote(configuration, run_id, dataclasses.asdict(data))

    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="statistics")
def run(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    try:
        return run_statistics(configuration, statistics_arguments)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during statistics collection: {error}"
        ) from error
