# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import testslide

from ..exceptions import InvalidConfiguration
from ..scheduler_policies import (
    FixedChunkCount,
    FixedChunkSize,
    SchedulerPolicies,
    SchedulerPolicy,
)


class SchedulerPoliciesTest(testslide.TestCase):
    def test_policy_from_and_to_json(self) -> None:
        def assert_parsed(input: object, expected: SchedulerPolicy) -> None:
            self.assertEqual(SchedulerPolicy.from_json(input, "<unknown>"), expected)
            self.assertEqual(input, expected.to_json())

        def assert_not_parsed(input: object) -> None:
            with self.assertRaises(InvalidConfiguration):
                SchedulerPolicy.from_json(input, "<unknown>")

        assert_not_parsed("")
        assert_not_parsed("derp")
        assert_not_parsed({})
        assert_not_parsed({"kind": 1})
        assert_not_parsed({"kind": "unknown"})
        assert_not_parsed({"kind": "fixed_chunk_size"})
        assert_not_parsed(
            {"kind": "fixed_chunk_size", "minimum_chunk_size": "not_integer"}
        )
        assert_not_parsed({"kind": "fixed_chunk_size", "minimum_chunk_size": 1})
        assert_not_parsed(
            {
                "kind": "fixed_chunk_size",
                "minimum_chunk_size": 1,
                "minimum_chunks_per_worker": 10,
                "preferred_chunk_size": -1,
            }
        )

        assert_parsed(
            {
                "kind": "fixed_chunk_size",
                "minimum_chunk_size": 1,
                "minimum_chunks_per_worker": 10,
                "preferred_chunk_size": 100,
            },
            SchedulerPolicy(
                value=FixedChunkSize(
                    minimum_chunk_size=1,
                    minimum_chunks_per_worker=10,
                    preferred_chunk_size=100,
                )
            ),
        )
        assert_parsed(
            {
                "kind": "fixed_chunk_size",
                "minimum_chunks_per_worker": 10,
                "preferred_chunk_size": 100,
            },
            SchedulerPolicy(
                value=FixedChunkSize(
                    minimum_chunk_size=None,
                    minimum_chunks_per_worker=10,
                    preferred_chunk_size=100,
                )
            ),
        )
        assert_parsed(
            {
                "kind": "fixed_chunk_count",
                "minimum_chunks_per_worker": 1,
                "minimum_chunk_size": 10,
                "preferred_chunks_per_worker": 100,
            },
            SchedulerPolicy(
                value=FixedChunkCount(
                    minimum_chunks_per_worker=1,
                    minimum_chunk_size=10,
                    preferred_chunks_per_worker=100,
                )
            ),
        )
        assert_parsed(
            {
                "kind": "fixed_chunk_count",
                "minimum_chunk_size": 10,
                "preferred_chunks_per_worker": 100,
            },
            SchedulerPolicy(
                value=FixedChunkCount(
                    minimum_chunks_per_worker=None,
                    minimum_chunk_size=10,
                    preferred_chunks_per_worker=100,
                )
            ),
        )

    def test_policies_from_and_to_json(self) -> None:
        def assert_parsed(input: object, expected: SchedulerPolicies) -> None:
            self.assertEqual(SchedulerPolicies.from_json(input), expected)
            self.assertEqual(input, expected.to_json())

        def assert_not_parsed(input: object) -> None:
            with self.assertRaises(InvalidConfiguration):
                SchedulerPolicies.from_json(input)

        assert_not_parsed("")
        assert_not_parsed({"taint_fixpoint": "foo"})
        assert_not_parsed({"taint_fixpoint": {}})

        assert_parsed(
            {
                "taint_fixpoint": {
                    "kind": "fixed_chunk_size",
                    "minimum_chunk_size": 1,
                    "minimum_chunks_per_worker": 10,
                    "preferred_chunk_size": 100,
                }
            },
            SchedulerPolicies(
                policies={
                    "taint_fixpoint": SchedulerPolicy(
                        value=FixedChunkSize(
                            minimum_chunk_size=1,
                            minimum_chunks_per_worker=10,
                            preferred_chunk_size=100,
                        )
                    )
                }
            ),
        )
