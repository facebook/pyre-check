# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import glob
import unittest
from typing import List
from unittest.mock import MagicMock, patch

from .. import Builder, parser
from ..parser.build_target import BuildTarget, PythonBinary, PythonLibrary


class BuilderTest(unittest.TestCase):
    def assert_target_names_equal(self, list_a: List[str], list_b: List[str]) -> None:
        self.assertListEqual(sorted(list_a), sorted(list_b))

    def assert_targets_equal(
        self, targets: List[BuildTarget], target_names: List[str]
    ) -> None:
        self.assert_target_names_equal(
            [target.target for target in targets], target_names
        )

    def test_compute_targets_to_build_simple(self):
        # Dependency graph:
        #    a
        #  /  \
        # b <- c
        # |  /
        # d      e
        build_file = MagicMock()
        build_file.targets = {
            "a": PythonBinary("project", "a", ["//project:b", "//project:c"]),
            "b": PythonLibrary("project", "b", ["//project:d"], []),
            "c": PythonLibrary("project", "c", ["//project:b", "//project:d"], []),
            "d": PythonLibrary("project", "d", [], []),
            "e": PythonLibrary("project", "e", [], []),
        }

        with patch.object(parser.Parser, "parse_file", return_value=build_file):
            builder = Builder("/ROOT")

            targets = builder.compute_targets_to_build("//project:a")
            self.assert_targets_equal(
                targets, ["//project:a", "//project:b", "//project:c", "//project:d"]
            )

            targets = builder.compute_targets_to_build("//project:b")
            self.assert_targets_equal(targets, ["//project:b", "//project:d"])

            targets = builder.compute_targets_to_build("//project:c")
            self.assert_targets_equal(
                targets, ["//project:b", "//project:c", "//project:d"]
            )

            targets = builder.compute_targets_to_build("//project:d")
            self.assert_targets_equal(targets, ["//project:d"])

            targets = builder.compute_targets_to_build("//project:e")
            self.assert_targets_equal(targets, ["//project:e"])

            self.assertRaises(
                ValueError, builder.compute_targets_to_build, "//project:f"
            )

    def test_compute_targets_to_build_complex(self):
        # Dependency graph:
        #    a
        #  /  \
        # b    c
        # |    |
        # d <- e
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary("project1", "a", ["//project1:b", "//project2:c"]),
            "b": PythonLibrary("project1", "b", ["//project2:d"], []),
        }
        build_file_2 = MagicMock()
        build_file_2.targets = {
            "c": PythonLibrary("project2", "c", ["//project2:e"], []),
            "d": PythonLibrary("project2", "d", [], []),
            "e": PythonLibrary("project2", "e", ["//project2:d"], []),
        }
        build_file_mapping = {"project1": build_file_1, "project2": build_file_2}
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = Builder("/ROOT")

            targets = builder.compute_targets_to_build("//project1:a")
            self.assert_targets_equal(
                targets,
                [
                    "//project1:a",
                    "//project1:b",
                    "//project2:c",
                    "//project2:d",
                    "//project2:e",
                ],
            )

            targets = builder.compute_targets_to_build("//project1:b")
            self.assert_targets_equal(targets, ["//project1:b", "//project2:d"])

            targets = builder.compute_targets_to_build("//project2:c")
            self.assert_targets_equal(
                targets, ["//project2:c", "//project2:e", "//project2:d"]
            )

            targets = builder.compute_targets_to_build("//project2:d")
            self.assert_targets_equal(targets, ["//project2:d"])

            targets = builder.compute_targets_to_build("//project2:e")
            self.assert_targets_equal(targets, ["//project2:e", "//project2:d"])

            self.assertRaises(
                ValueError, builder.compute_targets_to_build, "//project1:f"
            )

    def test_targets_to_build_file_wildcard(self):
        # Dependency graph:
        #  a -> c -> d <- e   b
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary("project1", "a", ["//project2:c"]),
            "b": PythonLibrary("project1", "b", [], []),
        }
        build_file_2 = MagicMock()
        build_file_2.targets = {
            "c": PythonLibrary("project2", "c", ["//project2:d"], []),
            "d": PythonLibrary("project2", "d", [], []),
            "e": PythonLibrary("project2", "e", ["//project2:d"], []),
        }

        build_file_mapping = {"project1": build_file_1, "project2": build_file_2}
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = Builder("/ROOT")

            targets = builder.compute_targets_to_build("//project1:")
            self.assert_targets_equal(
                targets,
                ["//project1:a", "//project1:b", "//project2:c", "//project2:d"],
            )

            targets = builder.compute_targets_to_build("//project2:")
            self.assert_targets_equal(
                targets, ["//project2:c", "//project2:d", "//project2:e"]
            )

    def test_targets_to_build_directory_wildcard(self):
        # Dependency graph:
        # a -> c   d   b <- e
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary("project1", "a", ["//project1/subproject:c"]),
            "b": PythonLibrary("project1", "b", [], []),
        }

        build_file_2 = MagicMock()
        build_file_2.targets = {
            "c": PythonLibrary("project1/subproject", "c", [], []),
            "d": PythonLibrary("project1/subproject", "d", [], []),
        }

        build_file_3 = MagicMock()
        build_file_3.targets = {
            "e": PythonLibrary("project2", "e", ["//project1:b"], [])
        }

        build_file_mapping = {
            "project1": build_file_1,
            "project1/subproject": build_file_2,
            "project2": build_file_3,
        }
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = Builder("/ROOT")

            with patch.object(
                glob,
                "iglob",
                return_value=[
                    "/ROOT/project1/TARGETS",
                    "/ROOT/project1/subproject/TARGETS",
                    "/ROOT/project2/TARGETS",
                ],
            ):
                targets = builder.compute_targets_to_build("//...")
                self.assert_targets_equal(
                    targets,
                    [
                        "//project1:a",
                        "//project1:b",
                        "//project1/subproject:c",
                        "//project1/subproject:d",
                        "//project2:e",
                    ],
                )

            with patch.object(
                glob,
                "iglob",
                return_value=[
                    "/ROOT/project1/TARGETS",
                    "/ROOT/project1/subproject/TARGETS",
                ],
            ):
                targets = builder.compute_targets_to_build("//project1/...")
                self.assert_targets_equal(
                    targets,
                    [
                        "//project1:a",
                        "//project1:b",
                        "//project1/subproject:c",
                        "//project1/subproject:d",
                    ],
                )

            with patch.object(
                glob, "iglob", return_value=["/ROOT/project1/subproject/TARGETS"]
            ):
                targets = builder.compute_targets_to_build("//project1/subproject/...")
                self.assert_targets_equal(
                    targets, ["//project1/subproject:c", "//project1/subproject:d"]
                )

            with patch.object(glob, "iglob", return_value=["/ROOT/project2/TARGETS"]):
                targets = builder.compute_targets_to_build("//project2/...")
                self.assert_targets_equal(targets, ["//project2:e", "//project1:b"])

    def test_normalize_targets(self):
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonLibrary("project1", "a", [], []),
            "b": PythonLibrary("project1", "b", [], []),
            "c": PythonLibrary("project1", "c", [], []),
        }

        build_file_2 = MagicMock()
        build_file_2.targets = {
            "d": PythonLibrary("project1/subproject", "d", [], []),
            "e": PythonLibrary("project1/subproject", "e", [], []),
        }

        build_file_3 = MagicMock()
        build_file_3.targets = {"f": PythonLibrary("project2", "f", [], [])}

        build_file_mapping = {
            "project1": build_file_1,
            "project1/subproject": build_file_2,
            "project2": build_file_3,
        }
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = Builder("/ROOT")

            # Regular targets
            normalized_targets = builder._normalize_target("//project1:a")
            self.assert_target_names_equal(normalized_targets, ["//project1:a"])

            # File wildcard targets
            normalized_targets = builder._normalize_target("//project1:")
            self.assert_target_names_equal(
                normalized_targets, ["//project1:a", "//project1:b", "//project1:c"]
            )

            normalized_targets = builder._normalize_target("//project1/subproject:")
            self.assert_target_names_equal(
                normalized_targets,
                ["//project1/subproject:d", "//project1/subproject:e"],
            )

            normalized_targets = builder._normalize_target("//project2:")
            self.assert_target_names_equal(normalized_targets, ["//project2:f"])

            # Directory wildcard targets
            with patch.object(
                glob,
                "iglob",
                return_value=[
                    "/ROOT/project1/TARGETS",
                    "/ROOT/project1/subproject/TARGETS",
                    "/ROOT/project2/TARGETS",
                ],
            ) as fake_iglob:
                normalized_targets = builder._normalize_target("//...")
                self.assert_target_names_equal(
                    normalized_targets,
                    [
                        "//project1:a",
                        "//project1:b",
                        "//project1:c",
                        "//project1/subproject:d",
                        "//project1/subproject:e",
                        "//project2:f",
                    ],
                )
                fake_iglob.assert_called_once_with("/ROOT/**/TARGETS", recursive=True)

            with patch.object(
                glob,
                "iglob",
                return_value=[
                    "/ROOT/project1/TARGETS",
                    "/ROOT/project1/subproject/TARGETS",
                ],
            ) as fake_iglob:
                normalized_targets = builder._normalize_target("//project1/...")
                self.assert_target_names_equal(
                    normalized_targets,
                    [
                        "//project1:a",
                        "//project1:b",
                        "//project1:c",
                        "//project1/subproject:d",
                        "//project1/subproject:e",
                    ],
                )
                fake_iglob.assert_called_once_with(
                    "/ROOT/project1/**/TARGETS", recursive=True
                )

            with patch.object(
                glob, "iglob", return_value=["/ROOT/project1/subproject/TARGETS"]
            ) as fake_iglob:
                normalized_targets = builder._normalize_target(
                    "//project1/subproject/..."
                )
                self.assert_target_names_equal(
                    normalized_targets,
                    ["//project1/subproject:d", "//project1/subproject:e"],
                )
                fake_iglob.assert_called_once_with(
                    "/ROOT/project1/subproject/**/TARGETS", recursive=True
                )

            with patch.object(
                glob, "iglob", return_value=["/ROOT/project2/TARGETS"]
            ) as fake_iglob:
                normalized_targets = builder._normalize_target("//project2/...")
                self.assert_target_names_equal(normalized_targets, ["//project2:f"])
                fake_iglob.assert_called_once_with(
                    "/ROOT/project2/**/TARGETS", recursive=True
                )
