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

We intend to replace this command with the newer `pyre report`, which uses
a more ganular and tabular data format that will work better for downstream
purproses.

The `pyre statistics` command is in maintenance mode while we build out `pyre
report` and migrate use cases.
"""


import dataclasses
import json
import logging
import re
from pathlib import Path
from typing import Dict, Iterable, List, Mapping, Optional

import libcst
from libcst.metadata import CodePosition, CodeRange

from .. import command_arguments, coverage_data, frontend_configuration, log
from . import commands


LOG: logging.Logger = logging.getLogger(__name__)


def location_to_code_range(location: coverage_data.Location) -> CodeRange:
    """
    Convert a location back to a libcst.CodeRange.

    This is only needed for backward-compatibility of statistics json ouptut.
    """
    return CodeRange(
        start=CodePosition(
            line=location.start_line,
            column=location.start_column,
        ),
        end=CodePosition(
            line=location.end_line,
            column=location.end_column,
        ),
    )


@dataclasses.dataclass(frozen=True)
class ModuleSuppressionData:
    code: Dict[coverage_data.ErrorCode, List[coverage_data.LineNumber]]
    no_code: List[coverage_data.LineNumber]


class SuppressionCountCollector(coverage_data.VisitorWithPositionData):
    def __init__(self, regex: str) -> None:
        self.no_code: List[int] = []
        self.codes: Dict[int, List[int]] = {}
        self.regex: re.Pattern[str] = re.compile(regex)

    def error_codes(self, line: str) -> Optional[List[coverage_data.ErrorCode]]:
        match = self.regex.match(line)
        if match is None:
            # No suppression on line
            return None
        code_group = match.group(1)
        if code_group is None:
            # Code-less error suppression
            return []
        code_strings = code_group.strip("[] ").split(",")
        try:
            codes = [int(code) for code in code_strings]
            return codes
        except ValueError:
            LOG.warning("Invalid error suppression code: %s", line)
            return []

    def visit_Comment(self, node: libcst.Comment) -> None:
        error_codes = self.error_codes(node.value)
        if error_codes is None:
            return
        suppression_line = self.location(node).start_line
        if len(error_codes) == 0:
            self.no_code.append(suppression_line)
            return
        for code in error_codes:
            if code in self.codes:
                self.codes[code].append(suppression_line)
            else:
                self.codes[code] = [suppression_line]

    def collect(
        self,
        module: libcst.MetadataWrapper,
    ) -> ModuleSuppressionData:
        module.visit(self)
        return ModuleSuppressionData(code=self.codes, no_code=self.no_code)


class FixmeCountCollector(SuppressionCountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *pyre-fixme(\[(\d* *,? *)*\])?")


class IgnoreCountCollector(SuppressionCountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *pyre-ignore(\[(\d* *,? *)*\])?")


class TypeIgnoreCountCollector(SuppressionCountCollector):
    def __init__(self) -> None:
        super().__init__(r".*# *type: ignore")


@dataclasses.dataclass(frozen=True)
class ModuleAnnotationData:
    line_count: int
    total_functions: List[CodeRange]
    partially_annotated_functions: List[CodeRange]
    fully_annotated_functions: List[CodeRange]
    total_parameters: List[CodeRange]
    annotated_parameters: List[CodeRange]
    total_returns: List[CodeRange]
    annotated_returns: List[CodeRange]
    total_globals: List[CodeRange]
    annotated_globals: List[CodeRange]
    total_attributes: List[CodeRange]
    annotated_attributes: List[CodeRange]

    def to_count_dict(self) -> Dict[str, int]:
        return {
            "return_count": len(self.total_returns),
            "annotated_return_count": len(self.annotated_returns),
            "globals_count": len(self.total_globals),
            "annotated_globals_count": len(self.annotated_globals),
            "parameter_count": len(self.total_parameters),
            "annotated_parameter_count": len(self.annotated_parameters),
            "attribute_count": len(self.total_attributes),
            "annotated_attribute_count": len(self.annotated_attributes),
            "function_count": len(self.total_functions),
            "partially_annotated_function_count": len(
                self.partially_annotated_functions
            ),
            "fully_annotated_function_count": len(self.fully_annotated_functions),
            "line_count": self.line_count,
        }


class AnnotationCountCollector(coverage_data.AnnotationCollector):
    def collect(
        self,
        module: libcst.MetadataWrapper,
    ) -> ModuleAnnotationData:
        module.visit(self)
        return ModuleAnnotationData(
            line_count=self.line_count,
            total_functions=[
                location_to_code_range(function.location) for function in self.functions
            ],
            partially_annotated_functions=[
                location_to_code_range(f.location)
                for f in self.functions
                if f.is_partially_annotated
            ],
            fully_annotated_functions=[
                location_to_code_range(f.location)
                for f in self.functions
                if f.is_fully_annotated
            ],
            total_parameters=[
                location_to_code_range(p.location) for p in list(self.parameters())
            ],
            annotated_parameters=[
                location_to_code_range(p.location)
                for p in self.parameters()
                if p.is_annotated
            ],
            total_returns=[location_to_code_range(r.location) for r in self.returns()],
            annotated_returns=[
                location_to_code_range(r.location)
                for r in self.returns()
                if r.is_annotated
            ],
            total_globals=[location_to_code_range(g.location) for g in self.globals],
            annotated_globals=[
                location_to_code_range(g.location)
                for g in self.globals
                if g.is_annotated
            ],
            total_attributes=[
                location_to_code_range(a.location) for a in self.attributes
            ],
            annotated_attributes=[
                location_to_code_range(a.location)
                for a in self.attributes
                if a.is_annotated
            ],
        )


@dataclasses.dataclass(frozen=True)
class StatisticsData:
    annotations: ModuleAnnotationData
    fixmes: ModuleSuppressionData
    ignores: ModuleSuppressionData
    strict: coverage_data.ModuleModeInfo


def collect_statistics(
    sources: Iterable[Path], strict_default: bool
) -> Dict[str, StatisticsData]:
    data: Dict[str, StatisticsData] = {}
    for path in sources:
        module = coverage_data.module_from_path(path)
        if module is None:
            continue
        try:
            annotations = AnnotationCountCollector().collect(module)
            fixmes = FixmeCountCollector().collect(module)
            ignores = IgnoreCountCollector().collect(module)
            modes = coverage_data.collect_mode(module, strict_default)
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
    if statistics_arguments.paths is None:
        paths = [configuration.get_local_root() or configuration.get_global_root()]
    else:
        paths = statistics_arguments.paths
    return collect_statistics(
        coverage_data.find_module_paths(
            paths=paths,
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
        annotation_counts = statistics_data.annotations.to_count_dict()
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
    configuration: frontend_configuration.Base,
    statistics_arguments: command_arguments.StatisticsArguments,
) -> commands.ExitCode:
    LOG.info("Collecting statistics...")
    return run_statistics(configuration, statistics_arguments)
