# pyre-strict

import unittest
from pathlib import Path

from ..specification import (
    BaseRepositoryState,
    InvalidSpecificationException,
    RepositoryState,
    Specification,
)


class SpecificationTest(unittest.TestCase):
    def test_create_repository_state(self) -> None:
        self.assertEqual(
            RepositoryState.from_json(
                {"kind": "base", "repository": ".", "commit_hash": "facefacefaceb000"}
            ),
            BaseRepositoryState(repository=Path("."), commit_hash="facefacefaceb000"),
        )

        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json({})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json(
                {"kind": "base", "commit_hash": "facefacefaceb000"}
            )
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json({"kind": "base", "repository": "."})
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json(
                {"repository": ".", "commit_hash": "facefacefaceb000"}
            )
        with self.assertRaises(InvalidSpecificationException):
            RepositoryState.from_json(
                {"kind": "base", "repository": 42, "commit_hash": "facefacefaceb000"}
            )

    def test_create_specification(self) -> None:
        self.assertEqual(
            Specification.from_json(
                {
                    "old_state": {
                        "kind": "base",
                        "repository": ".",
                        "commit_hash": "old_hash",
                    },
                    "new_state": {
                        "kind": "base",
                        "repository": ".",
                        "commit_hash": "new_hash",
                    },
                    "foo": ".",
                }
            ),
            Specification(
                old_state=BaseRepositoryState(
                    repository=Path("."), commit_hash="old_hash"
                ),
                new_state=BaseRepositoryState(
                    repository=Path("."), commit_hash="new_hash"
                ),
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
                        "kind": "base",
                        "repository": ".",
                        "commit_hash": "old_hash",
                    },
                    "new_state": {
                        "kind": "base",
                        "repository": ".",
                        "commit_hash": "new_hash",
                    },
                    "pyre_check_pyre_options": "--option1",
                    "pyre_check_options": "--option2",
                    "pyre_start_pyre_options": "--option3",
                    "pyre_start_options": "--option4",
                    "pyre_incremental_pyre_options": "--option5",
                    "pyre_incremental_options": "--option6",
                }
            ),
            Specification(
                old_state=BaseRepositoryState(
                    repository=Path("."), commit_hash="old_hash"
                ),
                new_state=BaseRepositoryState(
                    repository=Path("."), commit_hash="new_hash"
                ),
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
                        "kind": "base",
                        "repository": 42,
                        "commit_hash": "old_hash",
                    }
                }
            )
        with self.assertRaises(InvalidSpecificationException):
            Specification.from_json(
                {"old_state": {"kind": "base", "repository": "foo"}}
            )
        with self.assertRaises(InvalidSpecificationException):
            Specification.from_json(
                {
                    "old_state": {
                        "kind": "base",
                        "repository": ".",
                        "commit_hash": "old_hash",
                    }
                }
            )
