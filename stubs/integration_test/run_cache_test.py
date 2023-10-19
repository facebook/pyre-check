#!/usr/bin/env python3
# pyre-string
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
"""
Run an integration test for Pysa verifying that dumping the enviornment
to a cache and reloading produces expected results.

This test has no dependencies aside from stdlib, you should be able to run
it any environment simply by running `./run_cache_test.py` from the
containing directory (or any parent, it is not sensitive to your current
working directory.)
"""

import argparse
import copy
import json
import logging
import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Union, Optional, Callable, TypeVar


LOG: logging.Logger = logging.getLogger(__name__)


def _get_dot_pyre_directory_from_cache_path(cache_path: Path) -> Path:
    # Cache path is `.pyre/.pysa_cache`, so dot-pyre path should be its
    # parent
    return cache_path.parent


def _json_dump(obj: Union[List[Dict[str, Any]], Dict[str, Any]]) -> str:
    return json.dumps(obj, sort_keys=True, indent=2) + "\n"


def _normalized_json_dump(normalized: List[Dict[str, Any]]):
    normalized = sorted(
        normalized,
        key=lambda issue: (
            issue["path"],
            issue["line"],
            issue["column"],
            issue["name"],
        ),
    )
    return _json_dump(normalized)


def _compare(actual_str: str, expected_str: str) -> int:
    if actual_str != expected_str:
        with tempfile.NamedTemporaryFile(
            prefix="actual_"
        ) as actual_file, tempfile.NamedTemporaryFile(
            prefix="expected_"
        ) as expected_file:
            with open(actual_file.name, "w") as file:
                file.write(actual_str)
            with open(expected_file.name, "w") as file:
                file.write(expected_str)
            sys.stdout.write("Output differs from expected:\n")
            sys.stdout.flush()
            subprocess.run(["diff", "-u", expected_file.name, actual_file.name])
        return 30  # ExitCode.TEST_COMPARISON_DIFFERS
    else:
        return 0


def _run_and_check_output(
    command: List[str],
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    expected_cache_usage: Dict[str, Any],
) -> int:
    try:
        # Print log only if the command failed
        output = subprocess.check_output(command, stderr=subprocess.STDOUT, text=True)
    except subprocess.CalledProcessError as exception:
        LOG.error(f"`pyre analyze` failed with return code {exception.returncode}")
        sys.stdout.write(exception.output)
        return exception.returncode

    with open(save_results_to / "errors.json") as file:
        output_str = json.load(file)
    output_str = _normalized_json_dump(output_str)
    expected_str = _normalized_json_dump(expected)

    # TODO(T155501049): Delete the indirect test of cache, which compares the analysis results
    return_code = _compare(
        actual_str=output_str,
        expected_str=expected_str,
    )

    if return_code != 0:
        LOG.error(
            f"Result comparison failed (return code: {return_code}). Command output:\n{output}"
        )
        return return_code
    else:
        # Direct test of cache: Compare the usage of cache
        with open(save_results_to / "taint-metadata.json") as file:
            taint_metadata = json.load(file)
        actual_cache_usage = _json_dump(taint_metadata["cache"])
        expected_cache_usage = _json_dump(expected_cache_usage)
        return_code = _compare(
            actual_str=actual_cache_usage,
            expected_str=expected_cache_usage,
        )
        if return_code != 0:
            LOG.error(
                f"Result comparison failed (return code: {return_code}). Command output:\n{output}"
            )
        return return_code


def _pysa_command(
    typeshed_path: str,
    cache_path: Path,
    save_results_to: Path,
    use_cache: bool,
    maximum_overrides: Optional[int] = None,
) -> List[str]:
    command = [
        "pyre",
        "--dot-pyre-directory",
        str(_get_dot_pyre_directory_from_cache_path(cache_path)),
        "--typeshed",
        f"{typeshed_path}",
        "--noninteractive",
        "analyze",
        "--check-invariants",
        "--inline-decorators",
        "--save-results-to",
        save_results_to,
    ]
    if use_cache:
        command.append("--use-cache")
    if maximum_overrides is not None:
        command.append(f"--maximum-overrides-to-analyze={maximum_overrides}")
    return command


def _exit_or_continue(returncode: int, exit_on_error: bool) -> None:
    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        LOG.info(f"Test failed: {returncode}\n")
        if exit_on_error:
            sys.exit(returncode)


def _cache_file(cache_path: Path) -> Path:
    return cache_path / "sharedmem"


def _remove_cache_file(cache_path: Path) -> None:
    cache_file = _cache_file(cache_path)
    try:
        cache_file.unlink()
    except FileNotFoundError:
        pass


T = TypeVar("T")


def save_restore_cache(subtest: Callable[[T], None]) -> Callable[[T], None]:
    def inner(self: T) -> None:
        self.save_cache_file()
        subtest(self)
        self.restore_cache_file()

    return inner


@dataclass
class Test:
    """
    Invariant: Before and after each subtest, the cache file is the same as what is
    built from calling build_fresh_cache
    """

    typeshed_path: str
    cache_path: Path
    expected: List[Dict[str, Any]]
    save_results_to: Path
    exit_on_error: bool

    def temporary_cache_file(self) -> Path:
        return _cache_file(self.save_results_to)

    def save_cache_file(self) -> None:
        shutil.copyfile(
            src=_cache_file(self.cache_path), dst=self.temporary_cache_file()
        )

    def restore_cache_file(self) -> None:
        shutil.copyfile(
            src=self.temporary_cache_file(), dst=_cache_file(self.cache_path)
        )

    def build_fresh_cache_and_sanity_check(self) -> None:
        """
        Run Pysa with the cache argument for the first time. This should create
        the cache file and save state to it since the file doesn't exist already.
        Ensure the cache file doesn't already exist for a clean run.
        """
        try:
            shutil.rmtree(self.cache_path)
        except FileNotFoundError:
            pass

        LOG.info("Build cache with --use-cache flag on initial run:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": "NotFound",
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected,
            self.save_results_to,
            expected_cache_usage,
        )
        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_no_cache(self) -> None:
        """Run Pysa without the cache argument."""
        LOG.info("Testing with no --use-cache flag:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=False
        )
        expected_cache_usage = {
            "shared_memory_status": "Disabled",
            "save_cache": False,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected,
            self.save_results_to,
            expected_cache_usage,
        )
        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_cache_second_run(self) -> None:
        """
        Run Pysa with the cache argument for the second time. Since the file
        exists, Pysa should load the saved state from the file.
        """
        LOG.info("Testing behavior with --use-cache, using the built cache:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "Used",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "Used",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected,
            self.save_results_to,
            expected_cache_usage,
        )
        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_invalid_cache_file(self) -> None:
        """
        Run Pysa with an empty .pyre/.pysa_cache/sharedmem to simulate an invalid/corrupt
        cache file. Pysa should fall back to doing a clean run.
        """

        LOG.info("Testing fallback behavior with invalid cache file:")

        _remove_cache_file(cache_path=self.cache_path)
        (self.cache_path / "sharedmem").touch()

        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": "LoadError",
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected,
            self.save_results_to,
            expected_cache_usage,
        )
        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_pysa_file(self) -> None:
        """
        Run Pysa after adding a new Pysa model and ensure the cache is not invalidated.
        """

        LOG.info("Testing cache is not invalidated after .pysa file change:")

        test_model_path = Path("test_taint/PYSA_CACHE_TEST__tmp_model.pysa")
        try:
            test_model_path.unlink()
        except FileNotFoundError:
            pass

        test_model_path.touch()

        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "Used",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "Used",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected,
            self.save_results_to,
            expected_cache_usage,
        )

        # Clean up
        try:
            test_model_path.unlink()
        except FileNotFoundError:
            LOG.warning(
                f"Could not clean up {test_model_path.absolute()} after test run."
            )
            pass

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_taint_config_file(self) -> None:
        """
        Run Pysa after adding a new Pysa model and ensure the cache is not invalidated.
        """

        LOG.info("Testing cache is not invalidated after taint.config change:")

        test_taint_config = Path("test_taint/test_taint.config")
        try:
            test_taint_config.unlink()
        except FileNotFoundError:
            pass

        with open(test_taint_config, "w") as f:
            f.write(
                "{\n"
                '  "comment": "Test",\n'
                '  "sources": [],\n'
                '  "sinks": [],\n'
                '  "features": [],\n'
                '  "rules": []\n'
                "}"
            )

        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "Used",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "Used",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected,
            self.save_results_to,
            expected_cache_usage,
        )

        # Clean up
        try:
            test_taint_config.unlink()
        except FileNotFoundError:
            LOG.warning(
                f"Could not clean up {test_taint_config.absolute()} after test run."
            )
            pass

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_models(self) -> None:
        """
        Run Pysa after adding a new Pysa model and ensure the cache is not invalidated.
        """

        LOG.info("Testing results after models change:")

        # Remove a test taint file
        test_model_path = Path("test_taint/sanitize.pysa")
        # Save contents for cleanup phase
        original_content = open(test_model_path).read()
        try:
            test_model_path.unlink()
        except FileNotFoundError:
            LOG.warning(f"Could not remove up {test_model_path.absolute()}.")
            pass

        # Expected should have an additional issue from removing the sanitizer
        new_issue = {
            "code": 5001,
            "column": 9,
            "define": "integration_test.functools.test_cached_sanitizer",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 58,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/functools.py",
            "stop_column": 18,
            "stop_line": 58,
        }

        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "Used",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "Used",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected + [new_issue],
            self.save_results_to,
            expected_cache_usage,
        )

        # Restore the original model file
        open(test_model_path, "w").write(original_content)

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_source_files(self) -> None:
        """
        Run Pysa after adding a new file to test cache invalidation.
        Pysa should detect that the source has changed and fall back
        to doing a clean run.
        """

        LOG.info("Testing cache invalidation after source files change:")

        new_file_path = Path("fixture_source") / "PYSA_CACHE_TEST__tmp_file.py"
        try:
            new_file_path.unlink()
        except FileNotFoundError:
            pass

        new_file_path.touch()

        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": "InvalidByCodeChange",
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected,
            self.save_results_to,
            expected_cache_usage,
        )

        # Clean up
        try:
            new_file_path.unlink()
        except FileNotFoundError:
            LOG.warning(
                f"Could not clean up {new_file_path.absolute()} after test run."
            )
            pass

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_decorators(self) -> None:
        """
        Run Pysa after adding a new model with @IgnoreDecorator to test cache invalidation.
        Pysa should detect that the decorator modes have changed and fall back
        to doing a clean run.
        """

        LOG.info("Testing cache invalidation after decorator mode change:")

        new_model_path = Path("test_taint/test_decorator.pysa")
        try:
            new_model_path.unlink()
        except FileNotFoundError:
            pass

        with open(new_model_path, "w") as f:
            f.write(
                "@IgnoreDecorator\ndef integration_test.cache.ignore_decorator(): ...\n"
            )

        # Expected should have an additional issue from ignoring the decorator
        new_issue = {
            "code": 5001,
            "column": 19,
            "define": "integration_test.cache.test_ignore_decorator",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 23,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/cache.py",
            "stop_column": 27,
            "stop_line": 23,
        }

        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": "InvalidByDecoratorChange",
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected + [new_issue],
            self.save_results_to,
            expected_cache_usage,
        )

        # Clean up
        try:
            new_model_path.unlink()
        except FileNotFoundError:
            LOG.warning(
                f"Could not clean up {new_model_path.absolute()} after test run."
            )
            pass

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_analyze_all_overrides(self) -> None:
        """
        Run Pysa after removing a @AnalyzeAllOverrides model to test cache invalidation.
        Pysa should detect that the override graph has changed and fall back
        to doing a clean run.
        """
        # Remove a test taint file
        test_model_path = Path("test_taint/analyze_all_overrides.pysa")
        # Save contents for cleanup phase
        original_content = open(test_model_path).read()
        try:
            test_model_path.unlink()
        except FileNotFoundError:
            LOG.warning(f"Could not remove up {test_model_path.absolute()}.")
            pass

        LOG.info("Testing cache invalidation after changes in @AnalyzeAllOverrides:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "(Unused Stale)",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "(Unused Stale)",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }

        # Expected should have one less issue due to skipping overrides
        issue = {
            "code": 5001,
            "column": 13,
            "define": "integration_test.cache.test_analyze_all_overrides",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 87,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/cache.py",
            "stop_column": 21,
            "stop_line": 87
        }
        new_expected = copy.deepcopy(self.expected)
        new_expected.remove(issue)
        returncode = _run_and_check_output(
            pysa_command,
            new_expected,
            self.save_results_to,
            expected_cache_usage,
        )

        # Restore the original model file
        open(test_model_path, "w").write(original_content)

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_skip_overrides(self) -> None:
        """
        Run Pysa after removing a @SkipOverrides model to test cache invalidation.
        Pysa should detect that the override graph has changed and fall back
        to doing a clean run.
        """
        # Remove a test taint file
        test_model_path = Path("test_taint/skip_overrides.pysa")
        # Save contents for cleanup phase
        original_content = open(test_model_path).read()
        try:
            test_model_path.unlink()
        except FileNotFoundError:
            LOG.warning(f"Could not remove up {test_model_path.absolute()}.")
            pass

        # Expected should have an additional issue from not skipping overrides
        new_issue = {
            "code": 5001,
            "column": 20,
            "define": "integration_test.cache.test_skip_overrides",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 37,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/cache.py",
            "stop_column": 28,
            "stop_line": 37,
        }

        LOG.info("Testing cache invalidation after change in @SkipOverrides:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "(Unused Stale)",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "(Unused Stale)",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected + [new_issue],
            self.save_results_to,
            expected_cache_usage,
        )

        # Restore the original model file
        open(test_model_path, "w").write(original_content)

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_overrides_cap(self) -> None:
        """
        Run Pysa after limiting the max number of overrides to test cache invalidation.
        Pysa should detect that the override graph has changed and fall back to doing a clean run.
        """
        LOG.info(
            "Testing cache invalidation when changing --maximum-overrides-to-analyze:"
        )
        pysa_command = _pysa_command(
            self.typeshed_path,
            self.cache_path,
            self.save_results_to,
            use_cache=True,
            maximum_overrides=0,
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "(Unused Stale)",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "(Unused Stale)",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        # Expect an issue to disappear due to limiting overrides
        issue = {
            "code": 5001,
            "column": 20,
            "define": "integration_test.cache.test_overrides_cap",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 51,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/cache.py",
            "stop_column": 28,
            "stop_line": 51,
        }
        new_expected = copy.deepcopy(self.expected)
        new_expected.remove(issue)
        returncode = _run_and_check_output(
            pysa_command,
            new_expected,
            self.save_results_to,
            expected_cache_usage,
        )

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_skip_analysis(self) -> None:
        """
        Run Pysa after changing the skip analysis targets to test cache invalidation.
        Pysa should detect this and fall back to doing a clean run.
        """
        # Remove a test taint file
        test_model_path = Path("test_taint/skip_analysis.pysa")
        # Save contents for cleanup phase
        original_content = open(test_model_path).read()
        try:
            test_model_path.unlink()
        except FileNotFoundError:
            LOG.warning(f"Could not remove {test_model_path.absolute()}.")
            pass

        LOG.info("Testing cache invalidation when changing skip analysis targets:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "(Unused Stale)",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "Used",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        # Expect a new issue due to not skipping analyzing a callable
        new_issue = {
            "code": 5001,
            "column": 9,
            "define": "integration_test.cache.test_skip_analysis",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 55,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/cache.py",
            "stop_column": 17,
            "stop_line": 55,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected + [new_issue],
            self.save_results_to,
            expected_cache_usage,
        )

        # Restore the original model file
        open(test_model_path, "w").write(original_content)

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_definitions(self) -> None:
        """
        Run Pysa after adding a definition to test cache invalidation.
        Pysa should detect code changes and fall back to a clean run.
        """
        source_file_path = Path("fixture_source/integration_test/cache.py")
        # Save contents for cleanup phase
        original_content = open(source_file_path).read()
        try:
            new_definition = "def new_definition():\n    sink(source())"
            open(source_file_path, "w").write(f"{original_content}\n{new_definition}")
        except FileNotFoundError:
            LOG.warning(f"Could not update {source_file_path.absolute()}.")
            pass

        LOG.info("Testing cache invalidation when adding a new definition:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": "InvalidByCodeChange",
            "save_cache": True,
        }
        # Expect a new issue due to not skipping analyzing a callable
        new_issue = {
            "code": 5001,
            "column": 9,
            "define": "integration_test.cache.new_definition",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 90,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/cache.py",
            "stop_column": 17,
            "stop_line": 90,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected + [new_issue],
            self.save_results_to,
            expected_cache_usage,
        )

        # Restore the original source code file
        open(source_file_path, "w").write(original_content)

        _exit_or_continue(returncode, self.exit_on_error)

    @save_restore_cache
    def run_test_changed_attribute_targets(self) -> None:
        """
        Run Pysa after adding an attribute model to test cache invalidation.
        Pysa should detect code changes and fall back to a clean run.
        """
        test_model_path = Path("test_taint/attributes.pysa")
        # Save contents for cleanup phase
        original_content = open(test_model_path).read()
        try:
            new_model = "integration_test.cache.Token.token: TaintSource[UserControlled] = ..."
            open(test_model_path, "w").write(f"{original_content}\n{new_model}")
        except FileNotFoundError:
            LOG.warning(f"Could not update {test_model_path.absolute()}.")
            pass

        LOG.info("Testing cache invalidation when adding a new attribute model:")
        pysa_command = _pysa_command(
            self.typeshed_path, self.cache_path, self.save_results_to, use_cache=True
        )
        expected_cache_usage = {
            "shared_memory_status": {
                "Loaded": {
                    "CallGraph": "(Unused Stale)",
                    "ClassHierarchyGraph": "Used",
                    "ClassIntervalGraph": "Used",
                    "GlobalConstants": "Used",
                    "InitialCallables": "Used",
                    "PreviousAnalysisSetup": "Used",
                    "OverrideGraph": "Used",
                    "TypeEnvironment": "Used",
                }
            },
            "save_cache": True,
        }
        # Expect a new issue due to adding an attribute model
        new_issue = {
            "code": 5001,
            "column": 9,
            "define": "integration_test.cache.test_attribute",
            "description": "Possible shell injection [5001]: Data from [UserControlled] source(s) may reach [RemoteCodeExecution] sink(s)",
            "line": 63,
            "name": "Possible shell injection",
            "path": "fixture_source/integration_test/cache.py",
            "stop_column": 20,
            "stop_line": 63,
        }
        returncode = _run_and_check_output(
            pysa_command,
            self.expected + [new_issue],
            self.save_results_to,
            expected_cache_usage,
        )

        # Restore the original model file
        open(test_model_path, "w").write(original_content)

        _exit_or_continue(returncode, self.exit_on_error)


def run_tests(exit_on_error: bool) -> None:
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
    )

    # Switch to directory of this script.
    os.chdir(os.path.dirname(__file__))
    LOG.info("Running in `%s`", os.getcwd())

    cache_path = Path(".pyre/.pysa_cache")
    cache_path.mkdir(parents=True, exist_ok=True)
    LOG.info(f"Cache file path: {cache_path.resolve()}")

    typeshed_path = Path("../typeshed/typeshed").absolute().as_posix()
    with open("result.json") as file:
        expected = json.load(file)

    with tempfile.TemporaryDirectory() as save_results_to:
        save_results_to = Path(save_results_to)
        LOG.info(f"Saving results to directory: `{save_results_to}`")

        test_class = Test(
            typeshed_path=typeshed_path,
            cache_path=cache_path,
            expected=expected,
            save_results_to=save_results_to,
            exit_on_error=exit_on_error,
        )

        test_class.build_fresh_cache_and_sanity_check()

        test_class.run_test_no_cache()
        test_class.run_test_cache_second_run()
        test_class.run_test_invalid_cache_file()
        test_class.run_test_changed_pysa_file()
        test_class.run_test_changed_taint_config_file()
        test_class.run_test_changed_models()
        test_class.run_test_changed_source_files()
        test_class.run_test_changed_definitions()
        test_class.run_test_changed_decorators()
        test_class.run_test_changed_skip_overrides()
        test_class.run_test_changed_analyze_all_overrides()
        test_class.run_test_changed_overrides_cap()
        test_class.run_test_changed_skip_analysis()
        test_class.run_test_changed_attribute_targets()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run pysa cache test")
    parser.add_argument(
        "--no-exit-on-error",
        action="store_true",
        default=False,
        help=("Do not stop the test if any subtests fail."),
    )
    arguments = parser.parse_args()

    run_tests(exit_on_error=not arguments.no_exit_on_error)
