#!/usr/bin/env python3
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
import json
import logging
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any, Dict, List, Union


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
        sys.stdout.write(exception.output.decode())
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
    typeshed_path: str, cache_path: Path, save_results_to: Path, use_cache: bool
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
    return command


def _exit_or_continue(returncode: int, exit_on_error: bool) -> None:
    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        LOG.info(f"Test failed: {returncode}\n")
        if exit_on_error:
            sys.exit(returncode)


def run_test_no_cache(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
    """Run Pysa without the cache argument."""
    LOG.info("Testing with no --use-cache flag:")
    pysa_command = _pysa_command(
        typeshed_path, cache_path, save_results_to, use_cache=False
    )
    expected_cache_usage = {
        "shared_memory_status": "Disabled",
        "save_cache": False,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected,
        save_results_to,
        expected_cache_usage,
    )
    _exit_or_continue(returncode, exit_on_error)


def run_test_cache_first_and_second_runs(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
    """
    Run Pysa with the cache argument for the first time. This should create
    the cache file and save state to it since the file doesn't exist already.
    Ensure the cache file doesn't already exist for a clean run.

    Then, run Pysa with the cache argument for the second time. Since the file
    exists, Pysa should load the saved state from the file.
    """

    try:
        shutil.rmtree(cache_path)
    except FileNotFoundError:
        pass

    LOG.info("Testing behavior with --use-cache flag on initial run:")
    pysa_command = _pysa_command(
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": "NotFound",
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected,
        save_results_to,
        expected_cache_usage,
    )
    _exit_or_continue(returncode, exit_on_error)

    LOG.info("Testing behavior with --use-cache on subsequent runs:")
    pysa_command = _pysa_command(
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": {
            "Loaded": {
                "ClassHierarchyGraph": "Used",
                "ClassIntervalGraph": "Used",
                "InitialCallables": "Used",
                "TypeEnvironment": "Used",
            }
        },
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected,
        save_results_to,
        expected_cache_usage,
    )
    _exit_or_continue(returncode, exit_on_error)


def run_test_invalid_cache_file(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
    """
    Run Pysa with an empty .pyre/.pysa_cache/sharedmem to simulate an invalid/corrupt
    cache file. Pysa should fall back to doing a clean run.
    """

    LOG.info("Testing fallback behavior with invalid cache file:")

    cache_file = cache_path / "sharedmem"
    try:
        cache_file.unlink()
    except FileNotFoundError:
        pass
    (cache_path / "sharedmem").touch()

    pysa_command = _pysa_command(
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": "LoadError",
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected,
        save_results_to,
        expected_cache_usage,
    )
    _exit_or_continue(returncode, exit_on_error)


def run_test_changed_pysa_file(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
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
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": {
            "Loaded": {
                "ClassHierarchyGraph": "Used",
                "ClassIntervalGraph": "Used",
                "InitialCallables": "Used",
                "TypeEnvironment": "Used",
            }
        },
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected,
        save_results_to,
        expected_cache_usage,
    )

    # Clean up
    try:
        test_model_path.unlink()
    except FileNotFoundError:
        LOG.warning(f"Could not clean up {test_model_path.absolute()} after test run.")
        pass

    _exit_or_continue(returncode, exit_on_error)


def run_test_changed_taint_config_file(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
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
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": {
            "Loaded": {
                "ClassHierarchyGraph": "Used",
                "ClassIntervalGraph": "Used",
                "InitialCallables": "Used",
                "TypeEnvironment": "Used",
            }
        },
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected,
        save_results_to,
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

    _exit_or_continue(returncode, exit_on_error)


def run_test_changed_models(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
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
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": {
            "Loaded": {
                "ClassHierarchyGraph": "Used",
                "ClassIntervalGraph": "Used",
                "InitialCallables": "Used",
                "TypeEnvironment": "Used",
            }
        },
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected + [new_issue],
        save_results_to,
        expected_cache_usage,
    )

    # Restore the original model file
    open(test_model_path, "w").write(original_content)

    _exit_or_continue(returncode, exit_on_error)


def run_test_changed_source_files(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
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
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": "InvalidByCodeChange",
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected,
        save_results_to,
        expected_cache_usage,
    )

    # Clean up
    try:
        new_file_path.unlink()
    except FileNotFoundError:
        LOG.warning(f"Could not clean up {new_file_path.absolute()} after test run.")
        pass

    _exit_or_continue(returncode, exit_on_error)


def run_test_changed_decorators(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
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
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": "InvalidByDecoratorChange",
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected + [new_issue],
        save_results_to,
        expected_cache_usage,
    )

    # Clean up
    try:
        new_model_path.unlink()
    except FileNotFoundError:
        LOG.warning(f"Could not clean up {new_model_path.absolute()} after test run.")
        pass

    _exit_or_continue(returncode, exit_on_error)


def run_test_changed_overrides(
    typeshed_path: str,
    cache_path: Path,
    expected: List[Dict[str, Any]],
    save_results_to: Path,
    exit_on_error: bool,
) -> None:
    """
    Run Pysa after removing a @SkipOverrides model to test cache invalidation.
    Pysa should detect that the override graph has changed and fall back
    to doing a clean run.
    """

    LOG.info("Testing cache invalidation after skip override change:")

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

    pysa_command = _pysa_command(
        typeshed_path, cache_path, save_results_to, use_cache=True
    )
    expected_cache_usage = {
        "shared_memory_status": "InvalidByDecoratorChange",
        "save_cache": True,
    }
    returncode = _run_and_check_output(
        pysa_command,
        expected + [new_issue],
        save_results_to,
        expected_cache_usage,
    )

    # Restore the original model file
    open(test_model_path, "w").write(original_content)

    _exit_or_continue(returncode, exit_on_error)


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

        run_test_no_cache(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_cache_first_and_second_runs(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_invalid_cache_file(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_changed_pysa_file(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_changed_taint_config_file(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_changed_models(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_changed_source_files(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_changed_decorators(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )
        run_test_changed_overrides(
            typeshed_path,
            cache_path,
            expected,
            save_results_to,
            exit_on_error,
        )


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
