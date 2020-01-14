# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import Command, ProfileOutput


LOG: logging.Logger = logging.getLogger(__name__)

PHASE_NAME: str = "phase_name"


@dataclass(frozen=True)
class EventMetadata:
    name: str
    worker_id: int
    pid: int
    timestamp: int
    tags: Dict[str, str]


@dataclass(frozen=True)
class Event:
    metadata: EventMetadata

    # pyre-fixme[13]: Attribute `metadata` is never initialized.
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


@dataclass(frozen=True)
class CounterEvent(Event):
    description: Optional[str]


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


class Profile(Command):
    NAME = "profile"

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

    def _run(self) -> None:
        try:
            profiling_output = Path(self.profiling_log_path())
            if not profiling_output.is_file():
                raise RuntimeError(
                    "Cannot find profiling output at `{}`. "
                    "Please run Pyre with `--enable-profiling` or "
                    "`--enable-memory-profiling` option first.".format(profiling_output)
                )
            events = parse_events(profiling_output.read_text())
            output = self._profile_output
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
