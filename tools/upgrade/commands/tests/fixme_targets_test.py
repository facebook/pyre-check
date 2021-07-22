# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from ... import errors
from ...repository import Repository
from .. import fixme_targets
from ..fixme_targets import Configuration, ErrorSuppressingCommand, FixmeTargets, Target


repository = Repository()


class FixmeTargetsTest(unittest.TestCase):
    @patch("subprocess.run")
    @patch.object(
        Configuration,
        "find_project_configuration",
        return_value=Path(".pyre_configuration"),
    )
    @patch.object(FixmeTargets, "_run_fixme_targets_file")
    @patch(f"{fixme_targets.__name__}.find_targets")
    @patch(f"{fixme_targets.__name__}.Repository.commit_changes")
    def test_fixme_targets(
        self, commit_changes, find_targets, fix_file, find_configuration, subprocess
    ) -> None:
        arguments = MagicMock()
        arguments.subdirectory = None
        arguments.no_commit = False
        find_targets.return_value = {}
        FixmeTargets.from_arguments(arguments, repository).run()
        fix_file.assert_not_called()
        commit_changes.assert_not_called()

        find_targets.return_value = {"a/b/TARGETS": ["derp", "herp", "merp"]}
        FixmeTargets.from_arguments(arguments, repository).run()
        fix_file.assert_called_once_with(
            Path("."), "a/b/TARGETS", ["derp", "herp", "merp"]
        )
        commit_changes.assert_called_once_with(
            commit=True, title="Upgrade pyre version for . (TARGETS)"
        )

        # Test subdirectory
        fix_file.reset_mock()
        commit_changes.reset_mock()
        arguments.subdirectory = "derp"
        FixmeTargets.from_arguments(arguments, repository).run()
        fix_file.assert_called_once_with(
            Path("."), "a/b/TARGETS", ["derp", "herp", "merp"]
        )
        commit_changes.assert_called_once_with(
            commit=True, title="Upgrade pyre version for derp (TARGETS)"
        )

    @patch("subprocess.run")
    @patch.object(ErrorSuppressingCommand, "_apply_suppressions")
    def test_run_fixme_targets_file(self, apply_suppressions, subprocess) -> None:
        arguments = MagicMock()
        arguments.subdirectory = None
        arguments.no_commit = False
        arguments.lint = False
        buck_return = MagicMock()
        buck_return.returncode = 1
        buck_return.stderr = b"stderr"
        subprocess.return_value = buck_return
        FixmeTargets.from_arguments(arguments, repository)._run_fixme_targets_file(
            Path("."),
            "a/b",
            [
                Target("derp", strict=False, pyre=True, check_types=True),
                Target("herp", strict=False, pyre=True, check_types=True),
            ],
        )
        subprocess.assert_called_once_with(
            [
                "buck",
                "test",
                "--show-full-json-output",
                "a/b:derp-pyre-typecheck",
                "a/b:herp-pyre-typecheck",
                "--",
                "--run-disabled",
            ],
            stdout=-1,
            stderr=-1,
        )
        apply_suppressions.assert_not_called()

        buck_return.returncode = 0
        subprocess.return_value = buck_return
        FixmeTargets.from_arguments(arguments, repository)._run_fixme_targets_file(
            Path("."),
            "a/b",
            [
                Target("derp", strict=False, pyre=True, check_types=True),
                Target("herp", strict=False, pyre=True, check_types=True),
            ],
        )
        apply_suppressions.assert_not_called()

        buck_return.returncode = 32
        buck_return.stdout = b"""
        Discovering tests
        Running 1 tests
        Started new test run: https://url
              a/b:c-typecheck - a.b.c (c.TypeCheck) 57.547 1/1 (failed)
        Test output:
        > All OK.
        >         WARNING: Invoking pyre through buck TARGETS may...
        >         See `https://wiki/configuration/` to set up Pyre for your project.
        >
        > 	a/b/x.py:278:28 %s
        > 	a/b/x.py:325:41 %s
        > 	a/b/y.py:86:26 %s
               a/b:c-typecheck - main 0.000 (passed)
        Finished test run: https://url
        Summary (total time 58.75s):
          PASS: 1
          FAIL: 1
            a/b:c-typecheck - a.b.c (c.TypeCheck)
          SKIP: 0
          FATAL: 0
          TIMEOUT: 0
          OMIT: 0
        """ % (
            b"Undefined attribute [16]: `Optional` has no attribute `derp`.",
            b"Undefined attribute [16]: `Optional` has no attribute `herp`.",
            b"Incompatible parameter type [6]: Expected `str` for 1st positional only "
            + b"parameter to call `merp` but got `Optional[str]`.",
        )
        subprocess.return_value = buck_return
        expected_errors = errors.Errors(
            [
                {
                    "line": 278,
                    "column": 28,
                    "path": Path("a/b/x.py"),
                    "code": "16",
                    "description": "Undefined attribute [16]: `Optional` has no "
                    + "attribute `derp`.",
                    "concise_description": "Undefined attribute [16]: `Optional` has "
                    + "no attribute `derp`.",
                },
                {
                    "line": 325,
                    "column": 41,
                    "path": Path("a/b/x.py"),
                    "code": "16",
                    "description": "Undefined attribute [16]: `Optional` has no "
                    + "attribute `herp`.",
                    "concise_description": "Undefined attribute [16]: `Optional` has "
                    + "no attribute `herp`.",
                },
                {
                    "line": 86,
                    "column": 26,
                    "path": Path("a/b/y.py"),
                    "code": "6",
                    "description": "Incompatible parameter type [6]: Expected `str` "
                    + "for 1st positional only parameter to call `merp` but got "
                    + "`Optional[str]`.",
                    "concise_description": "Incompatible parameter type [6]: Expected "
                    + "`str` for 1st positional only parameter to call `merp` but got "
                    + "`Optional[str]`.",
                },
            ]
        )
        FixmeTargets.from_arguments(arguments, repository)._run_fixme_targets_file(
            Path("."),
            "a/b",
            [
                Target("derp", strict=False, pyre=True, check_types=True),
                Target("herp", strict=False, pyre=True, check_types=True),
            ],
        )
        apply_suppressions.assert_called_once_with(expected_errors)

        # Test fallback to type check targets with modified names
        subprocess.reset_mock()
        apply_suppressions.reset_mock()
        failed_buck_return = MagicMock()
        failed_buck_return.returncode = 5
        failed_buck_return.stdout = b""
        buck_query_return = MagicMock()
        buck_query_return.stdout = b"//target/to:retry-pyre-typecheck"
        subprocess.side_effect = [failed_buck_return, buck_query_return, buck_return]
        FixmeTargets.from_arguments(arguments, repository)._run_fixme_targets_file(
            Path("."),
            "a/b",
            [Target("derp", strict=False, pyre=True, check_types=True)],
        )
        subprocess.assert_has_calls(
            [
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "a/b:derp-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
                call(
                    ["buck", "query", "a/b:derp-pyre-typecheck"], stdout=-1, stderr=-1
                ),
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "//target/to:retry-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
            ]
        )
        apply_suppressions.assert_called_once_with(expected_errors)

        subprocess.reset_mock()
        apply_suppressions.reset_mock()
        failed_buck_return = MagicMock()
        failed_buck_return.returncode = 5
        failed_buck_return.stdout = b""
        buck_query_return = MagicMock()
        buck_query_return.stdout = b""
        buck_query_return.stderr = b"""
        Error in preloading targets. The rule //a/b:derp-pyre-typecheck could \
        not be found.
        Please check the spelling and whether it is one of the 10 targets in \
        /a/b/TARGETS. (1000 bytes)
        2 similar targets in \
        /data/users/szhu/fbsource/fbcode/tools/build/test/TARGETS are:
          //target/to:retry-pyre-typecheck
          //target/to:retry_non_typecheck
        """
        subprocess.side_effect = [failed_buck_return, buck_query_return, buck_return]
        FixmeTargets.from_arguments(arguments, repository)._run_fixme_targets_file(
            Path("."),
            "a/b",
            [Target("derp", strict=False, pyre=True, check_types=True)],
        )
        subprocess.assert_has_calls(
            [
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "a/b:derp-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
                call(
                    ["buck", "query", "a/b:derp-pyre-typecheck"], stdout=-1, stderr=-1
                ),
                call(
                    [
                        "buck",
                        "test",
                        "--show-full-json-output",
                        "//target/to:retry-pyre-typecheck",
                        "--",
                        "--run-disabled",
                    ],
                    stdout=-1,
                    stderr=-1,
                ),
            ]
        )
        apply_suppressions.assert_called_once_with(expected_errors)
