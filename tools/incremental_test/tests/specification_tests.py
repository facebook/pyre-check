# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path

from ..specification import (
    BatchRepositoryUpdate,
    FileRepositoryState,
    FileRepositoryUpdate,
    HgRepositoryState,
    HgRepositoryUpdate,
    InvalidSpecificationException,
    PatchRepositoryUpdate,
    RepositoryState,
    RepositoryUpdate,
    Specification,
)


class SpecificationTest(unittest.TestCase):
    def test_create_repository_state(self) -> None:
        self.assertEqual(
            RepositoryState.from_json(
                {"kind": "hg", "repository": ".", "commit_hash": "facefacefaceb000"}
            ),
            HgRepositoryState(repository=Path("."), commit_hash="facefacefaceb000"),
        )
        files = {"a.py": "print('a')", "b.py": "print('b')"}
        self.assertEqual(
            RepositoryState.from_json({"kind": "file", "files": files}),
            FileRepositoryState(files),
        )

        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json({})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json({"kind": "hg", "commit_hash": "facefacefaceb000"})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json({"kind": "hg", "repository": "."})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json(
                {"repository": ".", "commit_hash": "facefacefaceb000"}
            )
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json(
                {"kind": "hg", "repository": 42, "commit_hash": "facefacefaceb000"}
            )
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json({"kind": "file", "no_files": ""})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json({"kind": "file", "files": "not_a_list"})

    def test_create_repository_update(self) -> None:
        self.assertEqual(
            RepositoryUpdate.from_json(
                {"kind": "hg", "commit_hash": "facefacefaceb000"}
            ),
            HgRepositoryUpdate("facefacefaceb000"),
        )
        self.assertEqual(
            RepositoryUpdate.from_json(
                {"kind": "patch", "patch": "my_patch", "patch_flags": "my_flags"}
            ),
            PatchRepositoryUpdate("my_patch", "my_flags"),
        )
        changes = {"a.py": "print('a')", "b.py": "print('b')"}
        removals = ["c.py", "d.py"]
        self.assertEqual(
            RepositoryUpdate.from_json(
                {"kind": "file", "changes": changes, "removals": removals}
            ),
            FileRepositoryUpdate(changes=changes, removals=removals),
        )
        self.assertEqual(
            RepositoryUpdate.from_json(
                {
                    "kind": "batch",
                    "updates": [
                        {"kind": "hg", "commit_hash": "my_hash"},
                        {"kind": "patch", "patch": "my_patch"},
                    ],
                }
            ),
            BatchRepositoryUpdate(
                [
                    HgRepositoryUpdate(commit_hash="my_hash"),
                    PatchRepositoryUpdate(patch="my_patch", patch_flags=""),
                ]
            ),
        )

        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "foo"})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "hg", "commit_hash_missing": ""})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "patch", "patch_missing": ""})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "file", "changes": "not_dict"})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "file", "removals": "not_list"})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "file", "no_file_change": ""})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "batch", "updates_missing": ""})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryUpdate.from_json({"kind": "batch", "updates": "not_list"})

    def test_create_specification(self) -> None:
        self.assertEqual(
            Specification.from_json(
                {
                    "old_state": {
                        "kind": "hg",
                        "repository": ".",
                        "commit_hash": "old_hash",
                    },
                    "new_state": {"kind": "hg", "commit_hash": "new_hash"},
                    "foo": ".",
                }
            ),
            Specification(
                old_state=HgRepositoryState(
                    repository=Path("."), commit_hash="old_hash"
                ),
                new_state=HgRepositoryUpdate(commit_hash="new_hash"),
                pyre_check_pyre_options="",
                pyre_check_options="",
                pyre_start_pyre_options="",
                pyre_start_options="",
                pyre_incremental_pyre_options="",
                pyre_incremental_options="",
            ),
        )
        self.assertEqual(
            Specification.from_json(
                {
                    "old_state": {
                        "kind": "hg",
                        "repository": ".",
                        "commit_hash": "old_hash",
                    },
                    "new_state": {"kind": "hg", "commit_hash": "new_hash"},
                    "pyre_check_pyre_options": "--option1",
                    "pyre_check_options": "--option2",
                    "pyre_start_pyre_options": "--option3",
                    "pyre_start_options": "--option4",
                    "pyre_incremental_pyre_options": "--option5",
                    "pyre_incremental_options": "--option6",
                }
            ),
            Specification(
                old_state=HgRepositoryState(
                    repository=Path("."), commit_hash="old_hash"
                ),
                new_state=HgRepositoryUpdate(commit_hash="new_hash"),
                pyre_check_pyre_options="--option1",
                pyre_check_options="--option2",
                pyre_start_pyre_options="--option3",
                pyre_start_options="--option4",
                pyre_incremental_pyre_options="--option5",
                pyre_incremental_options="--option6",
            ),
        )

        with self.assertRaises(InvalidSpecificationException):
            Specification.from_json({})
        with self.assertRaises(InvalidSpecificationException):
            Specification.from_json(
                {
                    "old_state": {
                        "kind": "hg",
                        "repository": 42,
                        "commit_hash": "old_hash",
                    }
                }
            )
        with self.assertRaises(InvalidSpecificationException):
            Specification.from_json({"old_state": {"kind": "hg", "repository": "foo"}})
        with self.assertRaises(InvalidSpecificationException):
            Specification.from_json(
                {
                    "old_state": {
                        "kind": "hg",
                        "repository": ".",
                        "commit_hash": "old_hash",
                    }
                }
            )
