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

import json
import logging
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, Dict, List


LOG: logging.Logger = logging.getLogger(__name__)


def get_dot_pyre_directory_from_cache_path(cache_path: Path) -> Path:
    # Cache path is `.pyre/.pysa_cache`, so dot-pyre path should be its
    # parent
    return cache_path.parent


def normalized_json_dump(normalized: List[Dict[str, Any]]):
    normalized = sorted(
        normalized,
        key=lambda issue: (
            issue["path"],
            issue["line"],
            issue["column"],
            issue["name"],
        ),
    )
    return json.dumps(normalized, sort_keys=True, indent=2) + "\n"


def run_and_check_output(
    command: List[str],
    expected: List[Dict[str, Any]],
    output_file_name: str = "result.actual",
) -> int:
    try:
        output = subprocess.check_output(command).decode()
    except subprocess.CalledProcessError as exception:
        LOG.error(f"`pyre analyze` failed with return code {exception.returncode}")
        sys.stdout.write(exception.output.decode())
        return exception.returncode

    output_str = normalized_json_dump(json.loads(output))
    expected_str = normalized_json_dump(expected)

    if output_str != expected_str:
        with open(output_file_name, "w") as file:
            file.write(output_str)
        with open("result.expected", "w") as file:
            file.write(expected_str)
        sys.stdout.write("Output differs from expected:\n")
        sys.stdout.flush()
        subprocess.run(["diff", "-u", "result.expected", output_file_name])
        return 30  # ExitCode.TEST_COMPARISON_DIFFERS
    else:
        return 0


def run_test_no_cache(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    # Run Pysa without the cache argument.
    LOG.info("Testing with no --use-cache flag:")
    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected,
        output_file_name,
    )
    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_cache_first_and_second_runs(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_names: List[str]
) -> None:
    # Ensure the cache file doesn't already exist for a clean run.
    try:
        shutil.rmtree(cache_path)
    except FileNotFoundError:
        pass

    # Run Pysa with the cache argument for the first time. This should create
    # the cache file and save state to it since the file doesn't exist already.
    LOG.info("Testing behavior with --use-cache flag on initial run:")
    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected,
        output_file_names[0],
    )
    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)

    # Run Pysa with the cache argument for the second time. Since the file
    # exists, Pysa should load the saved state from the file.
    LOG.info("Testing behavior with --use-cache on subsequent runs:")
    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected,
        output_file_names[1],
    )
    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_invalid_cache_file(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    LOG.info("Testing fallback behavior with invalid cache file:")

    # Run Pysa with an empty .pyre/.pysa_cache/sharedmem to simulate an invalid/corrupt
    # cache file. Pysa should fall back to doing a clean run.
    cache_file = cache_path / "sharedmem"
    try:
        cache_file.unlink()
    except FileNotFoundError:
        pass
    (cache_path / "sharedmem").touch()

    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected,
        output_file_name,
    )

    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_changed_pysa_file(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    # Run Pysa after adding a new Pysa model and ensure the cache is not
    # invalidated.
    LOG.info("Testing cache is not invalidated after .pysa file change:")

    test_model_path = Path("test_taint/PYSA_CACHE_TEST__tmp_model.pysa")
    try:
        test_model_path.unlink()
    except FileNotFoundError:
        pass

    test_model_path.touch()

    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected,
        output_file_name,
    )

    # Clean up
    try:
        test_model_path.unlink()
    except FileNotFoundError:
        LOG.warning(f"Could not clean up {test_model_path.absolute()} after test run.")
        pass

    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_changed_taint_config_file(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    # Run Pysa after adding a new Pysa model and ensure the cache is not
    # invalidated.
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

    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected,
        output_file_name,
    )

    # Clean up
    try:
        test_taint_config.unlink()
    except FileNotFoundError:
        LOG.warning(
            f"Could not clean up {test_taint_config.absolute()} after test run."
        )
        pass

    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_changed_models(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    # Run Pysa after adding a new Pysa model and ensure the cache is not
    # invalidated.
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

    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected + [new_issue],
        output_file_name,
    )

    # Restore the original model file
    open(test_model_path, "w").write(original_content)

    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_changed_source_files(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    # Run Pysa after adding a new file to test cache invalidation.
    # Pysa should detect that the source has changed and fall back
    # to doing a clean run.
    LOG.info("Testing cache invalidation after source files change:")

    new_file_path = Path("PYSA_CACHE_TEST__tmp_file.py")
    try:
        new_file_path.unlink()
    except FileNotFoundError:
        pass

    new_file_path.touch()

    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected,
        output_file_name,
    )

    # Clean up
    try:
        new_file_path.unlink()
    except FileNotFoundError:
        LOG.warning(f"Could not clean up {new_file_path.absolute()} after test run.")
        pass

    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_changed_decorators(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    # Run Pysa after adding a new model with @IgnoreDecorator to test cache invalidation.
    # Pysa should detect that the decorator modes have changed and fall back
    # to doing a clean run.
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

    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected + [new_issue],
        output_file_name,
    )

    # Clean up
    try:
        new_model_path.unlink()
    except FileNotFoundError:
        LOG.warning(f"Could not clean up {new_model_path.absolute()} after test run.")
        pass

    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_test_changed_overrides(
    typeshed_path: str, cache_path: Path, expected: List[Dict[str, Any]], output_file_name: str
) -> None:
    # Run Pysa after removing a @SkipOverrides model to test cache invalidation.
    # Pysa should detect that the override graph has changed and fall back
    # to doing a clean run.
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

    returncode = run_and_check_output(
        [
            "pyre",
            "--dot-pyre-directory",
            str(get_dot_pyre_directory_from_cache_path(cache_path)),
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
            "--check-invariants",
            "--inline-decorators",
        ],
        expected + [new_issue],
        "result.cache9",
    )

    # Restore the original model file
    open(test_model_path, "w").write(original_content)

    if returncode == 0:
        LOG.info("Run produced expected results\n")
    else:
        sys.exit(returncode)


def run_tests() -> None:
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

    run_test_no_cache(typeshed_path, cache_path, expected, "result.no_cache")
    run_test_cache_first_and_second_runs(typeshed_path, cache_path, expected, ["result.cache1", "result.cache2"])
    run_test_invalid_cache_file(typeshed_path, cache_path, expected, "result.cache3")
    run_test_changed_pysa_file(typeshed_path, cache_path, expected, "result.cache4")
    run_test_changed_taint_config_file(typeshed_path, cache_path, expected, "result.cache5")
    run_test_changed_models(typeshed_path, cache_path, expected, "result.cache7")
    run_test_changed_source_files(typeshed_path, cache_path, expected, "result.cache6")
    run_test_changed_decorators(typeshed_path, cache_path, expected, "result.cache8")
    run_test_changed_overrides(typeshed_path, cache_path, expected, "result.cache9")

    LOG.info("All runs produced expected output.")


if __name__ == "__main__":
    run_tests()
