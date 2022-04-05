# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from ..profile import (
    CounterEvent,
    DurationEvent,
    EventMetadata,
    parse_event,
    StatisticsOverTime,
    TableStatistics,
    to_cold_start_phases,
    to_incremental_updates,
)


class ProfileTest(testslide.TestCase):
    def test_parse_event(self) -> None:
        self.assertEqual(
            parse_event(
                """
                {
                  "name": "Kara",
                  "worker_id": 579102694,
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
                    name="Kara",
                    worker_id=579102694,
                    pid=400,
                    timestamp=42,
                    tags={"actor": "Valorie Curry"},
                ),
            ),
        )
        self.assertEqual(
            parse_event(
                """
                {
                  "name": "Conor",
                  "worker_id": 313248317,
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
                    name="Conor",
                    worker_id=313248317,
                    pid=800,
                    timestamp=43,
                    tags={"actor": "Bryan Dechart"},
                ),
            ),
        )
        self.assertEqual(
            parse_event(
                """
                {
                  "name": "Marcus",
                  "worker_id": 684842971,
                  "pid": 200,
                  "event_type": [ "Counter", "ra9" ],
                  "timestamp": 44
                }
                """
            ),
            CounterEvent(
                description="ra9",
                metadata=EventMetadata(
                    name="Marcus", worker_id=684842971, pid=200, timestamp=44, tags={}
                ),
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
                            worker_id=0,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=11,
                        metadata=EventMetadata(
                            name="initialization",
                            worker_id=1,
                            pid=400,
                            timestamp=42,
                            tags={},
                        ),
                    ),
                    DurationEvent(
                        duration=11,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=2,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=12,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=3,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase2"},
                        ),
                    ),
                    DurationEvent(
                        duration=13,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=0,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase3"},
                        ),
                    ),
                    DurationEvent(
                        duration=1,
                        metadata=EventMetadata(
                            name="incremental check",
                            worker_id=1,
                            pid=400,
                            timestamp=42,
                            tags={},
                        ),
                    ),
                    DurationEvent(
                        duration=21,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=2,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=22,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=3,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase2"},
                        ),
                    ),
                    DurationEvent(
                        duration=2,
                        metadata=EventMetadata(
                            name="incremental check",
                            worker_id=0,
                            pid=400,
                            timestamp=42,
                            tags={},
                        ),
                    ),
                    DurationEvent(
                        duration=31,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=1,
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

    def test_to_cold_start_phases(self) -> None:
        self.assertEqual(
            to_cold_start_phases(
                [
                    DurationEvent(
                        duration=11,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=0,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=14,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=0,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase2"},
                        ),
                    ),
                    DurationEvent(
                        duration=12,
                        metadata=EventMetadata(
                            name="initialization",
                            worker_id=1,
                            pid=400,
                            timestamp=42,
                            tags={},
                        ),
                    ),
                    DurationEvent(
                        duration=40,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=0,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase1"},
                        ),
                    ),
                    DurationEvent(
                        duration=50,
                        metadata=EventMetadata(
                            name="SomeUpdate",
                            worker_id=0,
                            pid=400,
                            timestamp=42,
                            tags={"phase_name": "phase2"},
                        ),
                    ),
                    DurationEvent(
                        duration=1,
                        metadata=EventMetadata(
                            name="incremental check",
                            worker_id=1,
                            pid=400,
                            timestamp=42,
                            tags={},
                        ),
                    ),
                ]
            ),
            {"phase1": 11, "phase2": 14, "total": 12},
        )

    def test_table_statistics(self) -> None:
        statistics = TableStatistics()
        lines = [
            "(ALL cache hit rate) stats -- samples: 183.378K, total: 143.256K, "
            "avg: 0.781206, stddev: 0.413429, max: 1, min: 0)",
            "ALL bytes deserialized from shared heap stats -- samples: 80.124K, "
            "total: 3.501M, avg: 43.692277, stddev: 338.921118, max: 22.434K, min: 0)",
            "ALL bytes saved in shared heap due to compression stats -- samples: "
            "51.672K, total: 6.504M, avg: 125.869813, stddev: 1.211K, "
            "max: 78.471K, min: 0)",
            "ALL bytes serialized into shared heap stats -- samples: 51.672K, "
            "total: 11.721M, avg: 226.833449, stddev: 1.331K, max: 89.739K, "
            "min: 3)",
            "ALL bytes shared heap compression ratio stats -- samples: 51.672K, "
            "total: 46.811K, avg: 0.905918, stddev: 0.132285, max: 1, min: 0.200000)",
            "AST (bytes deserialized from shared heap) stats -- samples: "
            "1.690K, total: 1.635M, avg: 967.172781, stddev: 1.721K, max: "
            "22.434K, min: 18)",
            "AST (bytes saved in shared heap due to compression) stats -- "
            "samples: 845, total: 2.571M, avg: 3.043K, stddev: 6.361K, max: "
            "78.471K, min: 0)",
            "AST (bytes serialized into shared heap) stats -- samples: 845, "
            "total: 3.270M, avg: 3.870K, stddev: 6.883K, max: 89.739K, "
            "min: 73)",
            "AST (shared heap compression ratio) stats -- samples: 845, "
            "total: 562.823112, avg: 0.666063, stddev: 0.124657, max: 1, "
            "min: 0.337358)",
            "Alias (bytes deserialized from shared heap) stats -- samples: "
            "46.897K, total: 255.028K, avg: 5.438045, stddev: 1.665136, "
            "max: 18, min: 5)",
            "Alias (bytes saved in shared heap due to compression) stats -- "
            "samples: 1.158K, total: 85.000000, avg: 0.073402, stddev: "
            "1.005495, max: 21, min: 0)",
            "Alias (bytes serialized into shared heap) stats -- samples: "
            "1.158K, total: 27.826K, avg: 24.029361, stddev: 8.903096, "
            "max: 73, min: 21)",
            "Alias (shared heap compression ratio) stats -- samples: 1.158K, "
            "total: 1.157K, avg: 0.999019, stddev: 0.012015, max: 1, min: "
            "0.769231)",
            "Class (bytes deserialized from shared heap) stats -- samples: "
            "3.430K, total: 677.011K, avg: 197.379300, stddev: 343.207031, "
            "max: 5.507K, min: 12)",
            "Class (bytes saved in shared heap due to compression) stats -- "
            "samples: 3.305K, total: 1.300M, avg: 393.340091, stddev: 1.101K, "
            "max: 19.893K, min: 0)",
            "Class (bytes serialized into shared heap) stats -- samples: "
            "3.305K, total: 2.556M, avg: 773.294100, stddev: 1.331K, max: "
            "22.030K, min: 51)",
            "Class (cache hit rate) stats -- samples: 19.701K, total: 16.100K, "
            "avg: 0.817217, stddev: 0.386488, max: 1, min: 0)",
            "Class (shared heap compression ratio) stats -- samples: 3.305K, "
            "total: 2.679K, avg: 0.810528, stddev: 0.129101, max: 1, min: "
            "0.437840)",
        ]
        for line in lines:
            statistics.add(line + "\n")
        self.assertEqual(
            statistics.get_totals(),
            [
                ("ALL", "11.721M"),
                ("AST", "3.270M"),
                ("Class", "2.556M"),
                ("Alias", "27.826K"),
            ],
        )
        self.assertEqual(
            statistics.get_counts(),
            [
                ("ALL", "51.672K"),
                ("Class", "3.305K"),
                ("Alias", "1.158K"),
                ("AST", "845"),
            ],
        )

    def test_statistics_over_time(self) -> None:
        statistics = StatisticsOverTime()
        lines = [
            "2020-04-27 20:08:35 MEMORY Shared memory size post-typecheck (size: 42)",
            "2020-02-19 10:35:57 PERFORMANCE Check_TypeCheck: 1.767435s",
            "2020-02-19 10:35:57 PROGRESS Postprocessing 51 sources...",
            "2020-02-19 10:35:57 PROGRESS Postprocessed 51 of 51 sources",
            "2020-02-19 10:35:57 MEMORY Shared memory size (size: 2105)",
            "2020-02-19 10:35:57 INFO Number of new errors = 0",
            "2020-02-19 10:35:57 PERFORMANCE Incremental check: 2.456214s",
            "2020-02-19 10:35:57 PERFORMANCE Server request: 2.456249s",
            "2020-02-19 10:35:57 PERFORMANCE Server request: 2.456372s",
            "2020-02-19 10:36:06 PERFORMANCE Module tracker updated: 0.000838s",
            "2020-02-19 10:36:06 INFO Parsing 9 updated modules...",
            "2020-02-19 10:36:07 INFO Repopulating the environment for 9 " "modules.",
            "2020-02-19 10:36:07 INFO Updating is from empty stub result "
            "Environment",
            "2020-02-19 10:36:07 INFO Updating Alias Environment",
            "2020-02-19 10:36:07 INFO Updating Edges Environment",
            "2020-02-19 10:36:07 INFO Updating Undecorated functions " "Environment",
            "2020-02-19 10:36:07 INFO Updating Class metadata Environment",
            "2020-02-19 10:36:07 INFO Updating parse annotation Environment",
            "2020-02-19 10:36:07 INFO Updating attributes Environment",
            "2020-02-19 10:36:07 INFO Updating Global Environment",
            "2020-02-19 10:36:07 INFO Updating Global Locations Environment",
            "2020-02-19 10:36:07 INFO Checking 295 functions...",
            "2020-02-19 10:36:09 PROGRESS Processed 295 of 295 functions",
            "2020-02-19 10:36:09 PERFORMANCE Check_TypeCheck: 2.156352s",
            "2020-02-19 10:36:09 PROGRESS Postprocessing 23 sources...",
            "2020-02-19 10:36:09 PROGRESS Postprocessed 23 of 23 sources",
            "2020-02-19 10:36:09 MEMORY Shared memory size (size: 2106)",
            "2020-02-19 10:36:09 INFO Number of new errors = 0",
        ]
        for line in lines:
            statistics.add(line + "\n")
        self.assertEqual(
            statistics._data,
            [
                ("2020-04-27 20:08:35", 42000000),
                ("2020-02-19 10:35:57", 2105000000),
                ("2020-02-19 10:36:09", 2106000000),
            ],
        )
