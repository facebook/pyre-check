# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Logic for the `pyre profile` command, which (depending on the
`--profile-output` flag) will summarize or display the raw logs
produced when running `pyre --enable-profiling` in various ways.

For example, `pyre profile --profile-output=trace_event` will
print the profiling data transformed into a format readable by
TraceEvent tools like Perfetto or the Chrome tracevent tool to
standard output.
"""


import dataclasses
import json
import logging
import subprocess
from collections import defaultdict
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from typing_extensions import Final

from .. import backend_arguments, command_arguments, frontend_configuration, identifiers
from . import commands


LOG: logging.Logger = logging.getLogger(__name__)

PHASE_NAME: str = "phase_name"
TRIGGERED_DEPENDENCIES: str = "number_of_triggered_dependencies"


@dataclasses.dataclass(frozen=True)
class EventMetadata:
    name: str
    worker_id: int
    pid: int
    timestamp: int
    tags: Dict[str, str]


@dataclasses.dataclass(frozen=True)
# pyre-fixme[13]: Attribute `metadata` is never initialized.
class Event:
    metadata: EventMetadata

    def __init__(self, metadata: EventMetadata) -> None:
        raise NotImplementedError()


@dataclasses.dataclass(frozen=True)
class DurationEvent(Event):
    duration: int

    def add_phase_duration_to_result(self, result: Dict[str, int]) -> None:
        tags = self.metadata.tags
        if PHASE_NAME in tags:
            phase_name = tags[PHASE_NAME]
            result[phase_name] = self.duration
            if TRIGGERED_DEPENDENCIES in tags:
                result[phase_name + ": triggered dependencies"] = int(
                    tags[TRIGGERED_DEPENDENCIES]
                )


@dataclasses.dataclass(frozen=True)
class CounterEvent(Event):
    description: Final[Optional[str]]


def _parse_tags(input: List[List[str]]) -> Dict[str, str]:
    return {key: value for [key, value] in input}


def _parse_metadata(input_json: Dict[str, Any]) -> EventMetadata:
    pid = input_json["pid"]
    return EventMetadata(
        name=input_json["name"],
        worker_id=input_json.get("worker_id", pid),
        pid=pid,
        timestamp=input_json["timestamp"],
        tags=_parse_tags(input_json.get("tags", [])),
    )


def parse_event(input_string: str) -> Event:
    input_json: Dict[str, Any] = json.loads(input_string)
    event_type = input_json["event_type"]
    metadata = _parse_metadata(input_json)
    if event_type[0] == "Duration":
        duration = event_type[1]
        return DurationEvent(duration=duration, metadata=metadata)
    elif event_type[0] == "Counter":
        description = None if len(event_type) <= 1 else event_type[1]
        return CounterEvent(description=description, metadata=metadata)
    else:
        raise ValueError(f"Unrecognized event type: {input}")


def parse_events(input_string: str) -> List[Event]:
    output: List[Event] = []
    for index, line in enumerate(input_string.splitlines()):
        try:
            line = line.strip()
            if len(line) == 0:
                continue
            output.append(parse_event(line))
        except Exception:
            raise RuntimeError(f"Malformed log entry detected on line {index + 1}")
    return output


class StatisticsOverTime:
    def __init__(self) -> None:
        self._data: List[Tuple[str, int]] = []

    def add(self, line: str) -> None:
        dividers = [
            " MEMORY Shared memory size (size: ",
            " MEMORY Shared memory size post-typecheck (size: ",
        ]
        for divider in dividers:
            if divider in line:
                time, size_component = line.split(divider)
                size_in_megabytes = int(size_component[:-2])
                size_in_bytes = size_in_megabytes * (10**6)
                self._data.append((time, size_in_bytes))

    def graph_total_shared_memory_size_over_time(self) -> None:
        try:
            gnuplot = subprocess.Popen(["gnuplot"], stdin=subprocess.PIPE)
            # pyre-fixme[16]: `Optional` has no attribute `write`.
            gnuplot.stdin.write(b"set term dumb 140 25\n")
            gnuplot.stdin.write(b"plot '-' using 1:2 title '' with linespoints \n")
            for i, (_time, size) in enumerate(self._data):
                # This is graphing size against # of updates, not time
                gnuplot.stdin.write(b"%f %f\n" % (i, size))
            gnuplot.stdin.write(b"e\n")
            # pyre-fixme[16]: `Optional` has no attribute `flush`.
            gnuplot.stdin.flush()
        except FileNotFoundError:
            LOG.error("gnuplot is not installed")

    def to_json(self) -> str:
        return json.dumps(self._data)


class TableStatistics:
    # category -> aggregation -> table name -> value
    # pyre-ignore: T62493941
    _data: Dict[str, Dict[str, Dict[str, str]]] = defaultdict(lambda: defaultdict(dict))
    _shared_heap_category: Final = "bytes serialized into shared heap"

    @staticmethod
    def sort_by_value(items: List[Tuple[str, str]]) -> None:
        def parse(number: str) -> float:
            if number[-1] == "G":
                return float(number[:-1]) * (10**9)
            if number[-1] == "M":
                return float(number[:-1]) * (10**6)
            if number[-1] == "K":
                return float(number[:-1]) * (10**3)
            return float(number)

        items.sort(key=lambda x: parse(x[1]), reverse=True)

    def add(self, line: str) -> None:
        divider = "stats -- "
        if divider in line:
            header, data = line.split(divider)
            cells = data[:-2].split(", ")
            collected = [cell.split(": ") for cell in cells]
            tag_and_category = header[:-2].split(" (")
            if len(tag_and_category) == 2:
                tag, category = tag_and_category
            elif header[:3] == "ALL":
                tag = "ALL"
                category = header[4:-1]
            elif header[:4] == "(ALL":
                tag = "ALL"
                category = header[5:-2]
            else:
                return
            if len(tag) > 0:
                for key, value in collected:
                    self._data[category][key][tag] = value

    def is_empty(self) -> bool:
        return len(self._data) == 0

    def get_totals(self) -> List[Tuple[str, str]]:
        totals = list(self._data[self._shared_heap_category]["total"].items())
        TableStatistics.sort_by_value(totals)
        return totals

    def get_counts(self) -> List[Tuple[str, str]]:
        counts = list(self._data[self._shared_heap_category]["samples"].items())
        TableStatistics.sort_by_value(counts)
        return counts


def _get_server_log(log_directory: Path) -> Path:
    server_stderr_path = (
        log_directory
        / identifiers.PyreFlavor.CLASSIC.server_log_subdirectory()
        / "server.stderr"
    )
    if not server_stderr_path.is_file():
        raise RuntimeError(f"Cannot find server output at `{server_stderr_path}`.")
    return server_stderr_path


def _collect_memory_statistics_over_time(log_directory: Path) -> StatisticsOverTime:
    server_log = _get_server_log(log_directory)
    extracted = StatisticsOverTime()
    # lint-ignore: NoUnsafeFilesystemRule
    with open(server_log) as server_log_file:
        for line in server_log_file.readlines():
            extracted.add(line)
    return extracted


def _read_profiling_events(log_directory: Path) -> List[Event]:
    profiling_output = backend_arguments.get_profiling_log_path(log_directory)
    if not profiling_output.is_file():
        raise RuntimeError(
            f"Cannot find profiling output at `{profiling_output}`. "
            + "Please run Pyre with `--enable-profiling` or "
            + "`--enable-memory-profiling` option first."
        )
    return parse_events(profiling_output.read_text())


def to_traceevents(events: Sequence[Event]) -> List[Dict[str, Any]]:
    def to_traceevent(event: Event) -> Optional[Dict[str, Any]]:
        if isinstance(event, DurationEvent):
            duration_us = event.duration
            start_time_us = event.metadata.timestamp - duration_us
            return {
                "pid": event.metadata.worker_id,
                "tid": event.metadata.pid,
                "ts": start_time_us,
                "ph": "X",
                "name": event.metadata.name,
                "dur": duration_us,
                "args": event.metadata.tags,
            }
        elif isinstance(event, CounterEvent):
            timestamp_us = event.metadata.timestamp
            arguments: Dict[str, Any] = {
                key: int(value) for key, value in event.metadata.tags.items()
            }
            return {
                "pid": event.metadata.worker_id,
                "tid": event.metadata.pid,
                "ts": timestamp_us,
                "ph": "C",
                "name": event.metadata.name,
                "args": arguments,
            }
        else:
            return None

    return [
        trace_event
        for trace_event in map(to_traceevent, events)
        if trace_event is not None
    ]


def split_pre_and_post_initialization(
    events: Sequence[Event],
) -> Tuple[Sequence[Event], Sequence[Event]]:
    initialization_point = next(
        (
            index + 1
            for index, event in enumerate(events)
            if event.metadata.name == "initialization"
        ),
        len(events),
    )
    return events[:initialization_point], events[initialization_point:]


def to_cold_start_phases(events: Sequence[Event]) -> Dict[str, int]:
    result: Dict[str, int] = {}
    pre_initialization_events, _ = split_pre_and_post_initialization(events)
    for event in pre_initialization_events:
        if not isinstance(event, DurationEvent):
            continue
        event.add_phase_duration_to_result(result)
        if event.metadata.name == "initialization":
            result["total"] = event.duration

    return result


def to_incremental_updates(events: Sequence[Event]) -> List[Dict[str, int]]:
    results: List[Dict[str, int]] = []
    current: Dict[str, int] = {}
    _, post_initialization_events = split_pre_and_post_initialization(events)
    for event in post_initialization_events:
        if not isinstance(event, DurationEvent):
            continue

        event.add_phase_duration_to_result(current)

        if event.metadata.name == "incremental check":
            current["total"] = event.duration
            results.append(current)
            current = {}
    return results


def to_taint(events: Sequence[Event]) -> Dict[str, int]:
    result: Dict[str, int] = {}
    for event in events:
        if not isinstance(event, DurationEvent):
            continue
        event.add_phase_duration_to_result(result)

    fixpoint_events = [
        event
        for event in events
        if isinstance(event, DurationEvent)
        and event.metadata.tags.get(PHASE_NAME) == "Static analysis fixpoint"
    ]
    if len(fixpoint_events) == 0:
        return result

    for name, value in fixpoint_events[-1].metadata.tags.items():
        if name == PHASE_NAME:
            continue
        result[name.capitalize()] = int(value)

    return result


def print_individual_table_sizes(log_directory: Path) -> None:
    server_log = _get_server_log(log_directory)
    extracted = TableStatistics()
    # lint-ignore: NoUnsafeFilesystemRule
    with open(str(server_log)) as server_log_file:
        for line in server_log_file.readlines():
            extracted.add(line)
    if extracted.is_empty():
        raise RuntimeError(
            "Cannot find table size data in "
            + f"`{server_log.as_posix()}`. "
            + "Please run Pyre with `--debug` option first."
        )
    sizes = json.dumps(extracted.get_totals())
    counts = json.dumps(extracted.get_counts())
    # I manually put together this json in order to be
    # simultaneously machine and human readable
    combined = (
        "{\n"
        + f'  "total_table_sizes": {sizes},\n'
        + f'  "table_key_counts": {counts}\n'
        + "}"
    )
    print(combined)


def print_total_shared_memory_size_over_time(log_directory: Path) -> None:
    memory_over_time = _collect_memory_statistics_over_time(log_directory).to_json()
    print(memory_over_time)


def print_total_shared_memory_size_over_time_graph(log_directory: Path) -> None:
    statistics = _collect_memory_statistics_over_time(log_directory)
    statistics.graph_total_shared_memory_size_over_time()


def print_trace_event(log_directory: Path) -> None:
    events = _read_profiling_events(log_directory)
    print(json.dumps({"traceEvents": to_traceevents(events)}))


def print_cold_start_phases(log_directory: Path) -> None:
    events = _read_profiling_events(log_directory)
    print(json.dumps(to_cold_start_phases(events), indent=2))


def print_incremental_updates(log_directory: Path) -> None:
    events = _read_profiling_events(log_directory)
    print(json.dumps(to_incremental_updates(events), indent=2))


def print_taint(log_directory: Path) -> None:
    events = _read_profiling_events(log_directory)
    print(json.dumps(to_taint(events), indent=2))


def run(
    configuration: frontend_configuration.Base,
    output: command_arguments.ProfileOutput,
) -> commands.ExitCode:
    log_directory = configuration.get_log_directory()
    if output == command_arguments.ProfileOutput.INDIVIDUAL_TABLE_SIZES:
        print_individual_table_sizes(log_directory)
    elif output == command_arguments.ProfileOutput.TOTAL_SHARED_MEMORY_SIZE_OVER_TIME:
        print_total_shared_memory_size_over_time(log_directory)
    elif (
        output
        == command_arguments.ProfileOutput.TOTAL_SHARED_MEMORY_SIZE_OVER_TIME_GRAPH
    ):
        print_total_shared_memory_size_over_time_graph(log_directory)
    elif output == command_arguments.ProfileOutput.TRACE_EVENT:
        print_trace_event(log_directory)
    elif output == command_arguments.ProfileOutput.COLD_START_PHASES:
        print_cold_start_phases(log_directory)
    elif output == command_arguments.ProfileOutput.INCREMENTAL_UPDATES:
        print_incremental_updates(log_directory)
    elif output == command_arguments.ProfileOutput.TAINT:
        print_taint(log_directory)
    else:
        raise RuntimeError(f"Unrecognized output format: {output}")
    return commands.ExitCode.SUCCESS
