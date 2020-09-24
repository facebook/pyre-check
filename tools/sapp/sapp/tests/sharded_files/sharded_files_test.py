#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import unittest

from ...sharded_files import ShardedFile


test_path = "tools/sapp/sapp//tests/sharded_files"


class TestShardedFiles(unittest.TestCase):
    # pyre-fixme[3]: Return type must be annotated.
    def test_fails_for_no_sharding(self):
        pattern = os.path.join(test_path, "foo.bar")
        with self.assertRaisesRegex(ValueError, "Not a sharded file"):
            ShardedFile(pattern)

    # pyre-fixme[3]: Return type must be annotated.
    def test_returns_two_shards_for_star(self):
        pattern = os.path.join(test_path, "foo@*.bar")
        sf = ShardedFile(pattern)
        self.assertEqual(
            sf.get_filenames(),
            [
                os.path.join(test_path, "foo@00000-of-00002.bar"),
                os.path.join(test_path, "foo@00001-of-00002.bar"),
            ],
        )

    # pyre-fixme[3]: Return type must be annotated.
    def test_returns_two_shards_for_two(self):
        pattern = os.path.join(test_path, "foo@2.bar")
        sf = ShardedFile(pattern)
        self.assertEqual(
            sf.get_filenames(),
            [
                os.path.join(test_path, "foo@00000-of-00002.bar"),
                os.path.join(test_path, "foo@00001-of-00002.bar"),
            ],
        )

    # pyre-fixme[3]: Return type must be annotated.
    def test_returns_two_shards_for_two_ambiguous(self):
        pattern = os.path.join(test_path, "ambiguous@2.ext")
        sf = ShardedFile(pattern)
        self.assertEqual(
            sf.get_filenames(),
            [
                os.path.join(test_path, "ambiguous@00000-of-00002.ext"),
                os.path.join(test_path, "ambiguous@00001-of-00002.ext"),
            ],
        )

    # pyre-fixme[3]: Return type must be annotated.
    def test_returns_two_shards_for_one_ambiguous(self):
        pattern = os.path.join(test_path, "ambiguous@1.ext")
        sf = ShardedFile(pattern)
        self.assertEqual(
            sf.get_filenames(),
            [os.path.join(test_path, "ambiguous@00000-of-00001.ext")],
        )

    # pyre-fixme[3]: Return type must be annotated.
    def test_fails_for_bad_sharding_pattern(self):
        pattern = os.path.join(test_path, "foo@baz.bar")
        with self.assertRaisesRegex(ValueError, "Invalid shard specification: baz"):
            ShardedFile(pattern)

    # pyre-fixme[3]: Return type must be annotated.
    def test_fails_for_ambiguous_star_pattern(self):
        pattern = os.path.join(test_path, "ambiguous@*.ext")
        with self.assertRaisesRegex(
            ValueError, "@* matches ambiguous shard sets: @1 and @2"
        ):
            ShardedFile(pattern)

    # pyre-fixme[3]: Return type must be annotated.
    def test_fails_for_inconsistent_set(self):
        pattern = os.path.join(test_path, "inconsistent@2.baz")
        with self.assertRaisesRegex(
            ValueError,
            f"Shard {test_path}/inconsistent@00001-of-00002.baz does not exist.",
        ):
            ShardedFile(pattern)

    # pyre-fixme[3]: Return type must be annotated.
    def test_fails_for_inconsistent_set_star(self):
        pattern = os.path.join(test_path, "inconsistent@*.baz")
        with self.assertRaisesRegex(
            ValueError,
            f"Shard {test_path}/inconsistent@00001-of-00002.baz does not exist.",
        ):
            ShardedFile(pattern)
