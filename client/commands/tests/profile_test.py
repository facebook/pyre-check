# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import unittest

from ..profile import (
    CounterEvent,
    DurationEvent,
    EventMetadata,
    parse_event,
    to_incremental_updates,
)


class ProfileTest(unittest.TestCase):
    def test_parse_event(self) -> None:
        self.assertEqual(
            parse_event(
                """
                {
                  "name": "Kara",
                  "pid": 400,
                  "event_type": [ "Duration", 11 ],
                  "timestamp": 42,
                  "tags": [["actor", "Valorie Curry"]]
                }
                """
            ),
            DurationEvent(
                duration=11,
                metadata=EventMetadata(
                    name="Kara", pid=400, timestamp=42, tags={"actor": "Valorie Curry"}
                ),
            ),
        )
        self.assertEqual(
            parse_event(
                """
                {
                  "name": "Conor",
                  "pid": 800,
                  "event_type": [ "Counter" ],
                  "timestamp": 43,
                  "tags": [["actor", "Bryan Dechart"]]
                }
                """
            ),
            CounterEvent(
                description=None,
                metadata=EventMetadata(
                    name="Conor", pid=800, timestamp=43, tags={"actor": "Bryan Dechart"}
                ),
            ),
        )
        self.assertEqual(
            parse_event(
                """
                {
                  "name": "Marcus",
                  "pid": 200,
                  "event_type": [ "Counter", "ra9" ],
                  "timestamp": 44
                }
                """
            ),
            CounterEvent(
                description="ra9",
                metadata=EventMetadata(name="Marcus", pid=200, timestamp=44, tags={}),
            ),
        )

        with self.assertRaises(Exception):
            parse_event("{}")
        with self.assertRaises(Exception):
            parse_event('{ name: "foo" }')
        with self.assertRaises(Exception):
            parse_event('{ "name": "foo", "pid": 42, "timestamp": 100}')
        with self.assertRaises(Exception):
            parse_event(
                '{ "name": "foo", "pid": 42, "event_type": "wat", "timestamp": 100}'
            )
        with self.assertRaises(Exception):
            parse_event(
                '{ "name": "foo", "pid": 42, "event_type": [ "Duration", "10" ]}'
            )

    def test_to_incremental_updates(self) -> None:
        self.assertEqual(
            to_incremental_updates(
                [
                    DurationEvent(
                        duration=11,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=11,
                        metadata=EventMetadata(
                            name="initialization", pid=400, timestamp=42, tags={}
                        ),
                    ),
                    DurationEvent(
                        duration=11,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=12,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase2"},
                        ),
                    ),
                    DurationEvent(
                        duration=13,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase3"},
                        ),
                    ),
                    DurationEvent(
                        duration=1,
                        metadata=EventMetadata(
                            name="incremental check", pid=400, timestamp=42, tags={}
                        ),
                    ),
                    DurationEvent(
                        duration=21,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=22,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase2"},
                        ),
                    ),
                    DurationEvent(
                        duration=2,
                        metadata=EventMetadata(
                            name="incremental check", pid=400, timestamp=42, tags={}
                        ),
                    ),
                    DurationEvent(
                        duration=31,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                ]
            ),
            [
                {"phase1": 11, "phase2": 12, "phase3": 13, "total": 1},
                {"phase1": 21, "phase2": 22, "total": 2},
            ],
        )
