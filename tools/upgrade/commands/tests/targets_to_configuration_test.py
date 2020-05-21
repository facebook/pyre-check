# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import json
import libcst
import unittest
from pathlib import Path
from textwrap import dedent
from unittest.mock import MagicMock, call, mock_open, patch

from ... import errors, upgrade
from ...repository import Repository
from .. import targets_to_configuration
from ..targets_to_configuration import TargetPyreRemover, TargetsToConfiguration


repository = Repository()


class TargetRemoverTest(unittest.TestCase):
    def assert_targets_removed(self, source: str, expected_result: str) -> None:
        output = libcst.parse_module(dedent(source)).visit(TargetPyreRemover()).code
        self.assertEqual(dedent(expected_result), output)

    def test_remove_pyre_target_fields(self) -> None:
        source = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            check_types = True,
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = True,
            check_types_options = "mypy",
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        expected_result = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = True,
            check_types_options = "mypy",
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        self.assert_targets_removed(source, expected_result)

        source = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            check_types = True,
            check_types_options = "strict",
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = True,
            check_types_options = "strict, mypy",
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        expected_result = """
        load("@path:python_binary.bzl", "python_binary")

        python_binary(
            name = "target_name",
            main_module = "path.to.module",
            deps = [
                ":dependency_target_name",
            ],
        )

        python_unittest(
            name = "test_target_name",
            srcs = glob([
                "**/tests/*.py",
            ]),
            check_types = True,
            check_types_options = "strict, mypy",
            deps = [
                ":dependency_target_name",
            ],
        )
        """
        self.assert_targets_removed(source, expected_result)


class TargetsToConfigurationTest(unittest.TestCase):
    @patch("builtins.open")
    @patch(f"{targets_to_configuration.__name__}.Repository.revert_all")
    @patch(f"{targets_to_configuration.__name__}.Repository.add_paths")
    @patch(f"{targets_to_configuration.__name__}.find_targets")
    @patch(f"{targets_to_configuration.__name__}.get_filesystem")
    @patch.object(Path, "exists")
    @patch(f"{targets_to_configuration.__name__}.remove_non_pyre_ignores")
    @patch(f"{targets_to_configuration.__name__}.Configuration.get_errors")
    @patch(f"{targets_to_configuration.__name__}.add_local_mode")
    @patch.object(upgrade.ErrorSuppressingCommand, "_suppress_errors")
    @patch(f"{targets_to_configuration.__name__}.Repository.format")
    @patch(
        f"{targets_to_configuration.__name__}.TargetsToConfiguration.remove_target_typing_fields"
    )
    @patch(
        f"{targets_to_configuration.__name__}.TargetsToConfiguration.remove_pyre_typing_fields"
    )
    def test_convert_directory(
        self,
        remove_pyre_typing_fields,
        remove_target_typing_fields,
        repository_format,
        suppress_errors,
        add_local_mode,
        get_errors,
        remove_non_pyre_ignores,
        path_exists,
        get_filesystem,
        find_targets,
        add_paths,
        revert_all,
        open_mock,
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        arguments.lint = True
        arguments.glob = None
        arguments.fixme_threshold = None
        arguments.no_commit = False
        arguments.pyre_only = False
        find_targets.return_value = {
            "subdirectory/a/TARGETS": ["target_one"],
            "subdirectory/b/c/TARGETS": ["target_three", "target_two"],
        }
        filesystem_list = MagicMock()
        filesystem_list.return_value = []
        get_filesystem.list = filesystem_list
        path_exists.return_value = False
        pyre_errors = [
            {
                "line": 2,
                "column": 4,
                "path": "local.py",
                "code": 7,
                "name": "Kind",
                "concise_description": "Error",
                "inference": {},
                "ignore_error": False,
                "external_to_global_root": False,
            }
        ]

        # Create local project configuration
        get_errors.side_effect = [
            errors.Errors(pyre_errors),
            errors.Errors(pyre_errors),
        ]
        with patch("json.dump") as dump_mock:
            mocks = [mock_open(read_data="{}").return_value]
            open_mock.side_effect = mocks
            TargetsToConfiguration(arguments, repository).convert_directory(
                Path("subdirectory")
            )
            expected_configuration_contents = {
                "targets": [
                    "//subdirectory/a:target_one",
                    "//subdirectory/b/c:target_three",
                    "//subdirectory/b/c:target_two",
                ]
            }
            open_mock.assert_has_calls(
                [call(Path("subdirectory/.pyre_configuration.local"), "w")]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[0], indent=2, sort_keys=True
            )
            suppress_errors.assert_has_calls([call(errors.Errors(pyre_errors))])
            add_local_mode.assert_not_called()
            add_paths.assert_called_once_with(
                [Path("subdirectory/.pyre_configuration.local")]
            )
            remove_target_typing_fields.assert_called_once()

        # Add to existing local project configuration
        suppress_errors.reset_mock()
        open_mock.reset_mock()
        dump_mock.reset_mock()
        remove_target_typing_fields.reset_mock()
        path_exists.return_value = True
        get_errors.side_effect = [
            errors.Errors(pyre_errors),
            errors.Errors(pyre_errors),
        ]
        configuration_contents = json.dumps({"targets": ["//existing:target"]})
        with patch("json.dump") as dump_mock:
            mocks = [
                mock_open(read_data=configuration_contents).return_value,
                mock_open(read_data="{}").return_value,
            ]
            open_mock.side_effect = mocks
            TargetsToConfiguration(arguments, repository).convert_directory(
                Path("subdirectory")
            )
            expected_configuration_contents = {
                "targets": [
                    "//existing:target",
                    "//subdirectory/a:target_one",
                    "//subdirectory/b/c:target_three",
                    "//subdirectory/b/c:target_two",
                ]
            }
            open_mock.assert_has_calls(
                [
                    call(Path("subdirectory/.pyre_configuration.local")),
                    call(Path("subdirectory/.pyre_configuration.local"), "w"),
                ]
            )
            dump_mock.assert_called_once_with(
                expected_configuration_contents, mocks[1], indent=2, sort_keys=True
            )
        suppress_errors.assert_has_calls([call(errors.Errors(pyre_errors))])
        add_local_mode.assert_not_called()
        remove_target_typing_fields.assert_called_once()

    @patch(f"{targets_to_configuration.__name__}.find_files")
    @patch(f"{targets_to_configuration.__name__}.find_directories")
    def test_gather_directories(self, find_directories, find_files) -> None:
        arguments = MagicMock()
        find_files.return_value = ["subdirectory/.pyre_configuration.local"]
        expected_directories = [Path("subdirectory")]
        directories = TargetsToConfiguration(arguments, repository)._gather_directories(
            Path("subdirectory")
        )
        find_directories.assert_not_called()
        self.assertEqual(expected_directories, directories)

        find_files.return_value = ["subdirectory/a/.pyre_configuration.local"]
        find_directories.return_value = [
            "subdirectory/a",
            "subdirectory/b",
            "subdirectory/c",
        ]
        expected_directories = [
            Path("subdirectory/a"),
            Path("subdirectory/b"),
            Path("subdirectory/c"),
        ]
        directories = TargetsToConfiguration(arguments, repository)._gather_directories(
            Path("subdirectory")
        )
        find_directories.assert_called_once_with(Path("subdirectory"))
        self.assertEqual(expected_directories, directories)

        find_files.reset_mock()
        find_directories.reset_mock()
        find_files.return_value = [
            "subdirectory/a/.pyre_configuration.local",
            "subdirectory/b/.pyre_configuration.local",
            "subdirectory/c/x/.pyre_configuration.local",
        ]
        find_directories.side_effect = [
            ["subdirectory/a", "subdirectory/b", "subdirectory/c"],
            ["subdirectory/c/x", "subdirectory/c/y"],
        ]
        expected_directories = [
            Path("subdirectory/a"),
            Path("subdirectory/b"),
            Path("subdirectory/c/x"),
            Path("subdirectory/c/y"),
        ]
        directories = TargetsToConfiguration(arguments, repository)._gather_directories(
            Path("subdirectory")
        )
        find_directories.assert_has_calls(
            [call(Path("subdirectory")), call(Path("subdirectory/c"))]
        )
        self.assertEqual(expected_directories, directories)

        # Do not search for directories above `subdirectory/layer`
        find_files.reset_mock()
        find_directories.reset_mock()
        find_files.return_value = [
            "subdirectory/layer/a/.pyre_configuration.local",
            "subdirectory/layer/b/.pyre_configuration.local",
            "subdirectory/layer/c/x/.pyre_configuration.local",
        ]
        find_directories.side_effect = [
            ["subdirectory/layer/a", "subdirectory/layer/b", "subdirectory/layer/c"],
            ["subdirectory/layer/c/x", "subdirectory/layer/c/y"],
        ]
        expected_directories = [
            Path("subdirectory/layer/a"),
            Path("subdirectory/layer/b"),
            Path("subdirectory/layer/c/x"),
            Path("subdirectory/layer/c/y"),
        ]
        directories = TargetsToConfiguration(arguments, repository)._gather_directories(
            Path("subdirectory/layer/")
        )
        find_directories.assert_has_calls(
            [call(Path("subdirectory/layer")), call(Path("subdirectory/layer/c"))]
        )
        self.assertEqual(expected_directories, directories)

    @patch(f"{targets_to_configuration.__name__}.Repository.submit_changes")
    @patch(
        f"{targets_to_configuration.__name__}.TargetsToConfiguration._gather_directories"
    )
    @patch(
        f"{targets_to_configuration.__name__}.TargetsToConfiguration.convert_directory"
    )
    def test_run_targets_to_configuration(
        self, convert_directory, gather_directories, submit_changes
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = "subdirectory"
        arguments.lint = True
        arguments.glob = None
        arguments.fixme_threshold = None
        arguments.no_commit = False

        gather_directories.return_value = [Path("subdirectory")]
        TargetsToConfiguration(arguments, repository).run()
        convert_directory.assert_called_once_with(Path("subdirectory"))
        submit_changes.assert_called_once()

        convert_directory.reset_mock()
        gather_directories.return_value = [
            Path("subdirectory/a"),
            Path("subdirectory/b"),
        ]
        TargetsToConfiguration(arguments, repository).run()
        convert_directory.assert_has_calls(
            [call(Path("subdirectory/a")), call(Path("subdirectory/b"))]
        )

        convert_directory.reset_mock()
        gather_directories.return_value = [
            Path("subdirectory/a"),
            Path("subdirectory/a"),
        ]
        TargetsToConfiguration(arguments, repository).run()
        convert_directory.assert_called_once_with(Path("subdirectory/a"))

    @patch("subprocess.check_output")
    def test_deduplicate_targets(self, mock_check_output) -> None:
        configuration = upgrade.Configuration(Path("test"), {"targets": ["//a:a"]})
        configuration.deduplicate_targets()
        expected_targets = ["//a:a"]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"b"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/..."]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/...", "//b/..."]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"a"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/..."]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/..."]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"a\nb"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/..."]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/...", "//b/..."]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"a", b"//c:c"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/...", "//b/...", "//c:c"]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/...", "//b/..."]
        self.assertEqual(expected_targets, configuration.targets)

        mock_check_output.side_effect = [b"//a/b:x\n//a/b:y"]
        configuration = upgrade.Configuration(
            Path("test"), {"targets": ["//a/b:", "//a/b:x"]}
        )
        configuration.deduplicate_targets()
        expected_targets = ["//a/b:"]
        self.assertEqual(expected_targets, configuration.targets)
