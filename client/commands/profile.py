# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

from .command import Command, ProfileOutput


LOG: logging.Logger = logging.getLogger(__name__)

PHASE_NAME: str = "phase_name"


@dataclass(frozen=True)
class EventMetadata:
    name: str
    pid: int
    timestamp: int
    tags: Dict[str, str]


@dataclass(frozen=True)
class Event:
    # pyre-ignore[13]: We shouldn't throw an uninitialized attribute error here
    metadata: EventMetadata

    def __init__(self, metadata: EventMetadata) -> None:
        raise NotImplementedError


@dataclass(frozen=True)
class DurationEvent(Event):
    duration: int


@dataclass(frozen=True)
class CounterEvent(Event):
    description: Optional[str]


def _parse_tags(input: List[List[str]]) -> Dict[str, str]:
    return {key: value for [key, value] in input}


def _parse_metadata(input_json: Dict[str, Any]) -> EventMetadata:
    return EventMetadata(
        name=input_json["name"],
        pid=input_json["pid"],
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
            duration_ms = event.duration
            start_time_ms = event.metadata.timestamp - duration_ms
            return {
                "pid": event.metadata.pid,
                "tid": 0,
                "ts": start_time_ms * 1000,
                "ph": "X",
                "name": event.metadata.name,
                "dur": duration_ms * 1000,
                "args": event.metadata.tags,
            }
        elif isinstance(event, CounterEvent):
            timestamp_ms = event.metadata.timestamp
            arguments: Dict[str, Any] = {
                key: int(value) for key, value in event.metadata.tags.items()
            }
            return {
                "pid": event.metadata.pid,
                "tid": 0,
                "ts": timestamp_ms * 1000,
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


def to_cold_start_phases(events: Sequence[Event]) -> Dict[str, Any]:
    result: Dict[str, Any] = {}
    for event in events:
        if event.metadata.name == "initialization":
            # Cold start ends with server initialization message
            break
        if not isinstance(event, DurationEvent):
            continue

        tags = event.metadata.tags
        if PHASE_NAME in tags:
            phase_name = tags[PHASE_NAME]
            result[phase_name] = event.duration
    return result


class Profile(Command):
    NAME = "profile"

    def _run(self) -> None:
        try:
            profiling_output = Path(self.profiling_log_path())
            if not profiling_output.is_file():
                raise RuntimeError(
                    "Cannot find profiling output at `{}`. "
                    "Please run Pyre with `--enable-profiling` option first.".format(
                        profiling_output
                    )
                )
            events = parse_events(profiling_output.read_text())
            output = self._arguments.output
            if output == ProfileOutput.TRACE_EVENT:
                print(json.dumps(to_traceevents(events)))
            elif output == ProfileOutput.COLD_START_PHASES:
                print(json.dumps(to_cold_start_phases(events), indent=2))
            else:
                raise RuntimeError("Unrecognized output format: {}".format(output))

        except Exception as e:
            LOG.error("Failed to inspect profiling log: {}".format(e))
