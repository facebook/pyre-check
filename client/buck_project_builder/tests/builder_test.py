# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import glob
import unittest
from typing import List, Optional
from unittest.mock import MagicMock, patch

from .. import BuilderException, FastBuckBuilder, Target, parser
from ..build_target import (
    BuildTarget,
    PythonBinary,
    PythonLibrary,
    PythonWheel,
    ThriftLibrary,
)
from ..filesystem import Sources
from .test_common import base


class BuilderTest(unittest.TestCase):
    def assert_target_names_equal(self, list_a: List[str], list_b: List[str]) -> None:
        self.assertListEqual(sorted(list_a), sorted(list_b))

    def assert_targets_equal(
        self, targets: List[BuildTarget], target_names: List[str]
    ) -> None:
        self.assert_target_names_equal(
            [target.target for target in targets], target_names
        )

    def assert_raises_builder_exception(self, function, *args, expected_targets=None):
        try:
            function(*args)
        except BuilderException as error:
            self.assert_target_names_equal(error.targets, expected_targets)
        else:
            self.fail("Expected BuilderException to be thrown.")

    def test_parse_target(self):
        builder = FastBuckBuilder("/ROOT")
        self.assertEqual(builder._parse_target("//a:b"), Target("a", "b"))

        self.assert_raises_builder_exception(
            builder._parse_target, "//a:", expected_targets=["//a:"]
        )

        self.assert_raises_builder_exception(
            builder._parse_target, "//a/...", expected_targets=["//a/..."]
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
            "a": PythonBinary(
                "/ROOT", "project", base("a", ["//project:b", "//project:c"])
            ),
            "b": PythonLibrary("/ROOT", "project", base("b", ["//project:d"])),
            "c": PythonLibrary(
                "/ROOT", "project", base("c", ["//project:b", "//project:d"])
            ),
            "d": PythonLibrary("/ROOT", "project", base("d")),
            "e": PythonLibrary("/ROOT", "project", base("e")),
        }

        with patch.object(parser.Parser, "parse_file", return_value=build_file):
            builder = FastBuckBuilder("/ROOT")

            targets = builder.compute_targets_to_build(["//project:a"])
            self.assert_targets_equal(
                targets, ["//project:a", "//project:b", "//project:c", "//project:d"]
            )

            targets = builder.compute_targets_to_build(["//project:b"])
            self.assert_targets_equal(targets, ["//project:b", "//project:d"])

            targets = builder.compute_targets_to_build(["//project:c"])
            self.assert_targets_equal(
                targets, ["//project:b", "//project:c", "//project:d"]
            )

            targets = builder.compute_targets_to_build(["//project:d"])
            self.assert_targets_equal(targets, ["//project:d"])

            targets = builder.compute_targets_to_build(["//project:e"])
            self.assert_targets_equal(targets, ["//project:e"])

            targets = builder.compute_targets_to_build(["//project:a", "//project:e"])
            self.assert_targets_equal(
                targets,
                [
                    "//project:a",
                    "//project:b",
                    "//project:c",
                    "//project:d",
                    "//project:e",
                ],
            )

            self.assert_raises_builder_exception(
                builder.compute_targets_to_build,
                ["//project:e", "//project:f", "//project:g"],
                expected_targets=["//project:f", "//project:g"],
            )

            builder = FastBuckBuilder("/ROOT", fail_on_unbuilt_target=False)
            targets = builder.compute_targets_to_build(
                ["//project:e", "//project:f", "//project:g"]
            )
            self.assert_targets_equal(targets, ["//project:e"])

    def test_compute_targets_to_build_complex(self):
        # Dependency graph:
        #    a
        #  /  \
        # b    c
        # |    |
        # d <- e
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary(
                "/ROOT", "project1", base("a", ["//project1:b", "//project2:c"])
            ),
            "b": PythonLibrary("/ROOT", "project1", base("b", ["//project2:d"])),
        }
        build_file_2 = MagicMock()
        build_file_2.targets = {
            "c": PythonLibrary("/ROOT", "project2", base("c", ["//project2:e"])),
            "d": PythonLibrary("/ROOT", "project2", base("d")),
            "e": PythonLibrary("/ROOT", "project2", base("e", ["//project2:d"])),
        }
        build_file_mapping = {"project1": build_file_1, "project2": build_file_2}
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = FastBuckBuilder("/ROOT")

            targets = builder.compute_targets_to_build(["//project1:a"])
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

            targets = builder.compute_targets_to_build(["//project1:b"])
            self.assert_targets_equal(targets, ["//project1:b", "//project2:d"])

            targets = builder.compute_targets_to_build(["//project2:c"])
            self.assert_targets_equal(
                targets, ["//project2:c", "//project2:e", "//project2:d"]
            )

            targets = builder.compute_targets_to_build(["//project2:d"])
            self.assert_targets_equal(targets, ["//project2:d"])

            targets = builder.compute_targets_to_build(["//project2:e"])
            self.assert_targets_equal(targets, ["//project2:e", "//project2:d"])

            self.assert_raises_builder_exception(
                builder.compute_targets_to_build,
                ["//project1:f"],
                expected_targets=["//project1:f"],
            )

    def test_targets_to_build_file_wildcard(self):
        # Dependency graph:
        #  a -> c -> d <- e   b
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary("/ROOT", "project1", base("a", ["//project2:c"])),
            "b": PythonLibrary("/ROOT", "project1", base("b")),
        }
        build_file_2 = MagicMock()
        build_file_2.targets = {
            "c": PythonLibrary("/ROOT", "project2", base("c", ["//project2:d"])),
            "d": PythonLibrary("/ROOT", "project2", base("d")),
            "e": PythonLibrary("/ROOT", "project2", base("e", ["//project2:d"])),
        }

        build_file_mapping = {"project1": build_file_1, "project2": build_file_2}
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = FastBuckBuilder("/ROOT")

            targets = builder.compute_targets_to_build(["//project1:"])
            self.assert_targets_equal(
                targets,
                ["//project1:a", "//project1:b", "//project2:c", "//project2:d"],
            )

            targets = builder.compute_targets_to_build(["//project2:"])
            self.assert_targets_equal(
                targets, ["//project2:c", "//project2:d", "//project2:e"]
            )

            targets = builder.compute_targets_to_build(["//project1:", "//project2:"])
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

    def test_targets_to_build_directory_wildcard(self):
        # Dependency graph:
        # a -> c   d   b <- e
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary(
                "/ROOT", "project1", base("a", ["//project1/subproject:c"])
            ),
            "b": PythonLibrary("/ROOT", "project1", base("b")),
        }

        build_file_2 = MagicMock()
        build_file_2.targets = {
            "c": PythonLibrary("/ROOT", "project1/subproject", base("c")),
            "d": PythonLibrary("/ROOT", "project1/subproject", base("d")),
        }

        build_file_3 = MagicMock()
        build_file_3.targets = {
            "e": PythonLibrary("/ROOT", "project2", base("e", ["//project1:b"]))
        }

        build_file_mapping = {
            "project1": build_file_1,
            "project1/subproject": build_file_2,
            "project2": build_file_3,
        }
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = FastBuckBuilder("/ROOT")

            with patch.object(
                glob,
                "iglob",
                return_value=[
                    "/ROOT/project1/TARGETS",
                    "/ROOT/project1/subproject/TARGETS",
                    "/ROOT/project2/TARGETS",
                ],
            ):
                targets = builder.compute_targets_to_build(["//..."])
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
                targets = builder.compute_targets_to_build(["//project1/..."])
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
                targets = builder.compute_targets_to_build(
                    ["//project1/subproject/..."]
                )
                self.assert_targets_equal(
                    targets, ["//project1/subproject:c", "//project1/subproject:d"]
                )

            with patch.object(glob, "iglob", return_value=["/ROOT/project2/TARGETS"]):
                targets = builder.compute_targets_to_build(["//project2/..."])
                self.assert_targets_equal(targets, ["//project2:e", "//project1:b"])

    def test_compute_targets_to_build_duplicates(self):
        # Dependency graph:
        #     a
        #   /   \
        # b-py  c-py
        #        |
        #        b
        build_file = MagicMock()
        thrift_target = ThriftLibrary(
            "/ROOT", "project", base("b"), ["b.thrift"], False
        )
        build_file.targets = {
            "a": PythonBinary(
                "/ROOT",
                "project",
                base("a", dependencies=["//project:b-py", "//project:c-py"]),
            ),
            "b": thrift_target,
            "b-py": thrift_target,
            "c-py": ThriftLibrary(
                "/ROOT",
                "project",
                base("c", dependencies=["//project:b"]),
                ["c.thrift"],
                False,
            ),
        }

        with patch.object(parser.Parser, "parse_file", return_value=build_file):
            builder = FastBuckBuilder("/ROOT")

            # b and b-py refer to the same build target; we should only build it once.
            targets = builder.compute_targets_to_build(["//project:a"])
            self.assert_targets_equal(
                targets, ["//project:a", "//project:b", "//project:c"]
            )

    def test_targets_to_build_wheels(self):
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonBinary(
                "/ROOT", "project1", base("a", ["//project2/wheel:wheel"])
            )
        }
        build_file_2 = MagicMock()
        build_file_2.targets = {
            "wheel": PythonWheel("/ROOT", "project2/wheel", base("wheel"), {}, {})
        }
        build_file_mapping = {"project1": build_file_1, "project2/wheel": build_file_2}
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = FastBuckBuilder("/ROOT")

            targets = builder.compute_targets_to_build(["//project1:a"])
            self.assert_targets_equal(
                targets, ["//project1:a", "//project2/wheel:wheel"]
            )

            targets = builder.compute_targets_to_build(["//project2/wheel:wheel"])
            self.assert_targets_equal(targets, ["//project2/wheel:wheel"])

    def test_compute_reverse_dependencies(self):
        # Dependency graph:
        #    a
        #  /  \
        # b <- c
        # |  /
        # d      e
        builder = FastBuckBuilder("/ROOT")

        a = PythonBinary("/ROOT", "project", base("a", ["//project:b", "//project:c"]))
        b = PythonLibrary("/ROOT", "project", base("b", ["//project:d"]))
        c = PythonLibrary("/ROOT", "project", base("c", ["//project:b", "//project:d"]))
        d = PythonLibrary("/ROOT", "project", base("d"))
        e = PythonLibrary("/ROOT", "project", base("e"))

        targets = [a, b, c, d, e]
        reverse_dependencies = builder.compute_reverse_dependencies(targets)
        self.assertDictEqual(
            dict(reverse_dependencies),
            {"//project:b": [a, c], "//project:c": [a], "//project:d": [b, c]},
        )
        self.assertEqual(reverse_dependencies["//project:a"], [])
        self.assertEqual(reverse_dependencies["//project:e"], [])

    def test_normalize_targets(self):
        build_file_1 = MagicMock()
        build_file_1.targets = {
            "a": PythonLibrary("/ROOT", "project1", base("a")),
            "b": PythonLibrary("/ROOT", "project1", base("b")),
            "c": PythonLibrary("/ROOT", "project1", base("c")),
        }

        build_file_2 = MagicMock()
        build_file_2.targets = {
            "d": PythonLibrary("/ROOT", "project1/subproject", base("d")),
            "e": PythonLibrary("/ROOT", "project1/subproject", base("e")),
        }

        build_file_3 = MagicMock()
        build_file_3.targets = {"f": PythonLibrary("/ROOT", "project2", base("f"))}

        build_file_mapping = {
            "project1": build_file_1,
            "project1/subproject": build_file_2,
            "project2": build_file_3,
        }
        with patch.object(
            parser.Parser, "parse_file", side_effect=build_file_mapping.get
        ):
            builder = FastBuckBuilder("/ROOT")

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

    def test_build(self):
        with patch.object(
            FastBuckBuilder, "compute_targets_to_build"
        ) as compute_targets_to_build:
            fake_targets = [MagicMock(), MagicMock(), MagicMock()]
            compute_targets_to_build.return_value = fake_targets

            builder = FastBuckBuilder("/ROOT", output_directory="/output")
            builder.build(["//target:"])

            for fake_target in fake_targets:
                fake_target.build.assert_called_once_with("/output")
