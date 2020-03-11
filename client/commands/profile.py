# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
import os
import subprocess
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from typing_extensions import Final

from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import Command, ProfileOutput


LOG: logging.Logger = logging.getLogger(__name__)

PHASE_NAME: str = "phase_name"
TRIGGERED_DEPENDENCIES: str = "number_of_triggered_dependencies"


@dataclass(frozen=True)
class EventMetadata:
    name: str
    worker_id: int
    pid: int
    timestamp: int
    tags: Dict[str, str]


@dataclass(frozen=True)
# pyre-fixme[13]: Attribute `metadata` is never initialized.
class Event:
    metadata: EventMetadata

    def __init__(self, metadata: EventMetadata) -> None:
        raise NotImplementedError


@dataclass(frozen=True)
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


@dataclass(frozen=True)
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
        raise ValueError("Unrecognized event type: {}".format(input))


def parse_events(input_string: str) -> List[Event]:
    output: List[Event] = []
    for index, line in enumerate(input_string.splitlines()):
        try:
            line = line.strip()
            if len(line) == 0:
                continue
            output.append(parse_event(line))
        except Exception:
            raise RuntimeError(
                "Malformed log entry detected on line {}".format(index + 1)
            )
    return output


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
    events: Sequence[Event]
) -> Tuple[Sequence[Event], Sequence[Event]]:
    initialization_point = next(
        (
            index
            for index, event in enumerate(events)
            if event.metadata.name == "initialization"
        ),
        len(events) - 1,
    )
    return events[:initialization_point], events[initialization_point:]


def to_cold_start_phases(events: Sequence[Event]) -> Dict[str, int]:
    result: Dict[str, int] = {}
    pre_initialization_events, _ = split_pre_and_post_initialization(events)
    for event in pre_initialization_events:
        if not isinstance(event, DurationEvent):
            continue
        event.add_phase_duration_to_result(result)

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


class TableStatistics:
    # category -> aggregation -> table name -> value
    # pyre-ignore: T62493941
    _data: Dict[str, Dict[str, Dict[str, str]]] = defaultdict(lambda: defaultdict(dict))
    _shared_heap_category: Final = "bytes serialized into shared heap"

    @staticmethod
    def sort_by_value(items: List[Tuple[str, str]]) -> None:
        def parse(number: str) -> float:
            if number[-1] == "G":
                return float(number[:-1]) * (10 ** 9)
            if number[-1] == "M":
                return float(number[:-1]) * (10 ** 6)
            if number[-1] == "K":
                return float(number[:-1]) * (10 ** 3)
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


class StatisticsOverTime:
    _data: List[Tuple[str, int]] = []

    def add(self, line: str) -> None:
        divider = " MEMORY Shared memory size (size: "
        if divider in line:
            time, size = line.split(divider)
            self._data.append((time, int(size[:-2])))

    def graph_total_shared_memory_size_over_time(self) -> None:
        try:
            gnuplot = subprocess.Popen(["gnuplot"], stdin=subprocess.PIPE)
            gnuplot.stdin.write(b"set term dumb 140 25\n")
            gnuplot.stdin.write(b"plot '-' using 1:2 title '' with linespoints \n")
            for (i, (_time, size)) in enumerate(self._data):
                # This is graphing size against # of updates, not time
                gnuplot.stdin.write(b"%f %f\n" % (i, size))
            gnuplot.stdin.write(b"e\n")
            gnuplot.stdin.flush()
        except FileNotFoundError:
            LOG.error("gnuplot is not installed")


class Profile(Command):
    NAME = "profile"
    HIDDEN = True

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Profile, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._profile_output: ProfileOutput = arguments.profile_output

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        profile = parser.add_parser(cls.NAME)
        profile.set_defaults(command=cls)
        profile.add_argument(
            "--profile-output",
            type=ProfileOutput,
            choices=ProfileOutput,
            help="Specify what to output.",
            default=ProfileOutput.COLD_START_PHASES,
        )

    def get_stdout(self) -> Path:
        server_stdout_path = os.path.join(self._log_directory, "server/server.stdout")
        server_stdout = Path(server_stdout_path)
        if not server_stdout.is_file():
            raise RuntimeError(
                "Cannot find server output at `{}`.".format(server_stdout_path)
            )
        return server_stdout

    def _run(self) -> None:
        output = self._profile_output
        if output == ProfileOutput.INDIVIDUAL_TABLE_SIZES:
            server_stdout = self.get_stdout()
            extracted = TableStatistics()
            with open(server_stdout) as server_stdout_file:
                for line in server_stdout_file.readlines():
                    extracted.add(line)
            if extracted.is_empty():
                raise RuntimeError(
                    "Cannot find table size data in `{}`. "
                    "Please run Pyre with `--debug` option first.".format(
                        server_stdout.as_posix()
                    )
                )
            sizes = json.dumps(extracted.get_totals())
            counts = json.dumps(extracted.get_counts())
            # I manually put together this json in order to be
            # simultaneously machine and human readable
            combined = (
                "{\n"
                f'  "total_table_sizes": {sizes},\n'
                f'  "table_key_counts": {counts}\n'
                "}"
            )
            print(combined)
        elif output == ProfileOutput.TOTAL_SHARED_MEMORY_SIZE_OVER_TIME:
            server_stdout = self.get_stdout()
            extracted = StatisticsOverTime()
            with open(server_stdout) as server_stdout_file:
                for line in server_stdout_file.readlines():
                    extracted.add(line)
            extracted.graph_total_shared_memory_size_over_time()
        else:
            try:
                profiling_output = Path(self.profiling_log_path())
                if not profiling_output.is_file():
                    raise RuntimeError(
                        "Cannot find profiling output at `{}`. "
                        "Please run Pyre with `--enable-profiling` or "
                        "`--enable-memory-profiling` option first.".format(
                            profiling_output
                        )
                    )
                events = parse_events(profiling_output.read_text())
                if output == ProfileOutput.TRACE_EVENT:
                    print(json.dumps(to_traceevents(events)))
                elif output == ProfileOutput.COLD_START_PHASES:
                    print(json.dumps(to_cold_start_phases(events), indent=2))
                elif output == ProfileOutput.INCREMENTAL_UPDATES:
                    print(json.dumps(to_incremental_updates(events), indent=2))
                else:
                    raise RuntimeError("Unrecognized output format: {}".format(output))

            except Exception as e:
                LOG.error("Failed to inspect profiling log: {}".format(e))
                raise e
