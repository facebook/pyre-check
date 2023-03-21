# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Entrypoint for the `pyre statistics` command.

This command allows users to get some simple information about the number of
code objects (e.g. returns, parameters), how many of those code objects have
annotations, and some other information such as the number of strict files
and pyre-fixme and pyre-ignore directives.
"""


import dataclasses
import json
import logging
from pathlib import Path
from typing import Dict, Iterable, Mapping

from .. import (
    command_arguments,
    configuration as configuration_module,
    coverage_data,
    frontend_configuration,
    log,
)
from . import commands


LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class StatisticsData:
    annotations: coverage_data.ModuleAnnotationData
    fixmes: coverage_data.ModuleSuppressionData
    ignores: coverage_data.ModuleSuppressionData
    strict: coverage_data.ModuleStrictData


def collect_statistics(
    sources: Iterable[Path], strict_default: bool
) -> Dict[str, StatisticsData]:
    data: Dict[str, StatisticsData] = {}
    for path in sources:
        module = coverage_data.module_from_path(path)
        if module is None:
            continue
        try:
            annotations = coverage_data.AnnotationCountCollector().collect(module)
            fixmes = coverage_data.FixmeCountCollector().collect(module)
            ignores = coverage_data.IgnoreCountCollector().collect(module)
            modes = coverage_data.StrictCountCollector(strict_default).collect(module)
            statistics_data = StatisticsData(
                annotations,
                fixmes,
                ignores,
                modes,
            )
            data[str(path)] = statistics_data
        except RecursionError:
            LOG.warning(f"LibCST encountered recursion error in `{path}`")
    return data


def collect_all_statistics(
    configuration: frontend_configuration.Base,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> Dict[str, StatisticsData]:
    local_root = configuration.get_local_root()
    return collect_statistics(
        coverage_data.find_module_paths(
            coverage_data.get_paths_to_collect(
                statistics_arguments.paths,
                local_root=Path(local_root) if local_root is not None else None,
                global_root=Path(configuration.get_global_root()),
            ),
            excludes=configuration.get_excludes(),
        ),
        strict_default=configuration.is_strict(),
    )


@dataclasses.dataclass(frozen=True)
class AggregatedStatisticsData:
    annotations: Dict[str, int] = dataclasses.field(default_factory=dict)
    fixmes: int = 0
    ignores: int = 0
    strict: int = 0
    unsafe: int = 0


def aggregate_statistics(
    data: Mapping[str, StatisticsData]
) -> AggregatedStatisticsData:
    aggregate_annotations = {
        "return_count": 0,
        "annotated_return_count": 0,
        "globals_count": 0,
        "annotated_globals_count": 0,
        "parameter_count": 0,
        "annotated_parameter_count": 0,
        "attribute_count": 0,
        "annotated_attribute_count": 0,
        "function_count": 0,
        "partially_annotated_function_count": 0,
        "fully_annotated_function_count": 0,
        "line_count": 0,
    }

    for statistics_data in data.values():
        annotation_counts = coverage_data.AnnotationCountCollector.get_result_counts(
            statistics_data.annotations
        )
        for key in aggregate_annotations.keys():
            aggregate_annotations[key] += annotation_counts[key]

    return AggregatedStatisticsData(
        annotations=aggregate_annotations,
        fixmes=sum(
            len(fixmes.no_code) + len(fixmes.code)
            for fixmes in [statistics_data.fixmes for statistics_data in data.values()]
        ),
        ignores=sum(
            len(ignores.no_code) + len(ignores.code)
            for ignores in [
                statistics_data.ignores for statistics_data in data.values()
            ]
        ),
        strict=sum(
            1 if strictness.mode == coverage_data.ModuleMode.STRICT else 0
            for strictness in [
                statistics_data.strict for statistics_data in data.values()
            ]
        ),
        unsafe=sum(
            1 if strictness.mode == coverage_data.ModuleMode.UNSAFE else 0
            for strictness in [
                statistics_data.strict for statistics_data in data.values()
            ]
        ),
    )


def get_overall_annotation_percentage(data: AggregatedStatisticsData) -> float:
    total_annotation_slots = (
        data.annotations["return_count"]
        + data.annotations["parameter_count"]
        + data.annotations["attribute_count"]
        + data.annotations["globals_count"]
    )
    filled_annotation_slots = (
        data.annotations["annotated_return_count"]
        + data.annotations["annotated_parameter_count"]
        + data.annotations["annotated_attribute_count"]
        + data.annotations["annotated_globals_count"]
    )
    return filled_annotation_slots * 100.0 / total_annotation_slots


def get_summary(aggregated_data: AggregatedStatisticsData) -> str:
    annotation_rate = round(get_overall_annotation_percentage(aggregated_data), 2)
    total_suppressions = aggregated_data.fixmes + aggregated_data.ignores
    total_modules = aggregated_data.strict + aggregated_data.unsafe
    if total_modules > 0:
        strict_module_rate = round(aggregated_data.strict * 100.0 / total_modules, 2)
        unsafe_module_rate = round(aggregated_data.unsafe * 100.0 / total_modules, 2)
    else:
        strict_module_rate = 100.00
        unsafe_module_rate = 0.00
    total_function_count = aggregated_data.annotations["function_count"]
    if total_function_count > 0:
        partially_annotated_function_rate = round(
            aggregated_data.annotations["partially_annotated_function_count"]
            * 100.0
            / total_function_count,
            2,
        )
        fully_annotated_function_rate = round(
            aggregated_data.annotations["fully_annotated_function_count"]
            * 100.0
            / total_function_count,
            2,
        )
    else:
        partially_annotated_function_rate = 0.00
        fully_annotated_function_rate = 100.00
    return (
        f"Coverage summary:\nOverall annotation rate is {annotation_rate}%\n"
        + f"There are {total_suppressions} total error suppressions inline ({aggregated_data.fixmes} fixmes and {aggregated_data.ignores} ignores)\n"
        + f"Of {total_modules} modules, {strict_module_rate}% are strict and {unsafe_module_rate}% are unsafe\n"
        + f"Of {total_function_count} functions, {fully_annotated_function_rate}% are fully annotated and {partially_annotated_function_rate}% are partially annotated\n"
    )


def print_text_summary(data: Mapping[str, StatisticsData]) -> None:
    aggregated_data = aggregate_statistics(data)
    LOG.warning(get_summary(aggregated_data))


def print_json_summary(data: Mapping[str, StatisticsData]) -> None:
    aggregated_data = aggregate_statistics(data)
    log.stdout.write(json.dumps(dataclasses.asdict(aggregated_data), indent=4))


def process_json_statistics(
    data: Mapping[str, StatisticsData]
) -> Dict[str, Dict[str, Dict[str, int]]]:
    path_to_dictionary_statistics = {
        path: dataclasses.asdict(statistics_data)
        for (path, statistics_data) in data.items()
    }
    log.stdout.write(json.dumps(path_to_dictionary_statistics))
    return path_to_dictionary_statistics


def run_statistics(
    configuration: frontend_configuration.Base,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    data = collect_all_statistics(configuration, statistics_arguments)
    if statistics_arguments.print_summary:
        print_text_summary(data)
        return commands.ExitCode.SUCCESS

    if statistics_arguments.aggregate:
        print_json_summary(data)
        return commands.ExitCode.SUCCESS

    process_json_statistics(data)
    return commands.ExitCode.SUCCESS


def run(
    configuration: configuration_module.Configuration,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    LOG.info("Collecting statistics...")
    return run_statistics(
        frontend_configuration.OpenSource(configuration), statistics_arguments
    )
