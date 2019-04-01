#!/usr/bin/env python3

# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import logging
import sys
from typing import Any, Dict


LOG: logging.Logger = logging.getLogger(__name__)


def _translate(event: Dict[str, Any]) -> Dict[str, Any]:
    name = event["name"]
    pid = event["pid"]
    event_type = event["event_type"]
    timestamp_ms = event["timestamp"]
    if event_type[0] == "Duration":
        duration_ms = event_type[1]
        start_time_ms = timestamp_ms - duration_ms
        trace_event_type = "X"
        arguments = {}
        if "tags" in event:
            for key, value in event["tags"]:
                arguments[key] = value
        return {
            "pid": pid,
            "ts": start_time_ms * 1000,
            "ph": trace_event_type,
            "name": name,
            "dur": duration_ms * 1000,
            "args": arguments,
        }
    else:
        raise ValueError(f"{event_type[0]}")


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.INFO,
    )

    try:
        events = [json.loads(line) for line in sys.stdin]
        translated_events = []
        for index, event in enumerate(events):
            try:
                translated_event = _translate(event)
                translated_events.append(translated_event)
            except (KeyError, IndexError) as e:
                LOG.error(f"Malformed log entry detected on line {index+1}")
                LOG.exception(f"Unexpected format: {e}")
            except ValueError as e:
                LOG.error(f"Malformed log entry on line {index+1}")
                LOG.exception(f"Unrecognized event type: {e}")
        final_output = {"traceEvents": translated_events}
        print(json.dumps(final_output))
    except json.decoder.JSONDecodeError as e:
        LOG.error(f"JSON parsing error: {e}")
        sys.exit(1)
