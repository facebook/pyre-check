#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any, Dict, List


def normalized_json_dump(input: Dict[str, Any]):
    normalized = json.loads(input)

    normalized = sorted(normalized, key=lambda issue: issue["path"])
    normalized = sorted(normalized, key=lambda issue: issue["line"])
    normalized = sorted(normalized, key=lambda issue: issue["column"])

    return json.dumps(normalized, sort_keys=True, indent=2) + "\n"


def run_and_check_output(
    command: List[str],
    expected: Dict[str, Any],
    output_file_name: str = "result.actual",
) -> bool:
    output = subprocess.check_output(command).decode()

    if normalized_json_dump(expected) != normalized_json_dump(output):
        with open(output_file_name, "w") as file:
            file.write(normalized_json_dump(output))
        logging.error("Output differs from expected:")
        subprocess.run(["diff", "result.json", output_file_name])
        return False
    else:
        return True


def run_test_clean_run(
    typeshed_path: str, cache_path: Path, expected: Dict[str, Any]
) -> None:
    # Ensure the cache file doesn't already exist for a clean run.
    try:
        cache_path.unlink()
    except FileNotFoundError:
        pass

    # Run Pysa without the cache argument.
    logging.info("Testing with no --use-cache flag:")
    result = run_and_check_output(
        [
            "pyre",
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
        ],
        expected,
        "result.no_cache",
    )
    if result:
        logging.info("Run produced expected results\n")
    else:
        sys.exit(1)


def run_test_cache_first_and_second_runs(
    typeshed_path: str, cache_path: Path, expected: Dict[str, Any]
) -> None:
    # Run Pysa with the cache argument for the first time. This should create
    # the cache file and save state to it since the file doesn't exist already.
    logging.info("Testing behavior with --use-cache flag on initial run:")
    result = run_and_check_output(
        [
            "pyre",
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
        ],
        expected,
        "result.cache1",
    )
    if result:
        logging.info("Run produced expected results\n")
    else:
        sys.exit(1)

    # Run Pysa with the cache argument for the second time. Since the file
    # exists, Pysa should load the saved state from the file.
    logging.info("Testing behavior with --use-cache on subsequent runs:")
    result = run_and_check_output(
        [
            "pyre",
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
        ],
        expected,
        "result.cache2",
    )
    if result:
        logging.info("Run produced expected results\n")
    else:
        sys.exit(1)


def run_test_invalid_cache_file(
    typeshed_path: str, cache_path: Path, expected: Dict[str, Any]
) -> None:
    # Run Pysa with an empty .pyre/pysa.cache to simulate an invalid/corrupt
    # cache file. Pysa should fall back to doing a clean run.
    logging.info("Testing fallback behavior with invalid cache file:")

    try:
        cache_path.unlink()
        cache_path.touch()
    except FileNotFoundError:
        sys.exit(1)

    result = run_and_check_output(
        [
            "pyre",
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
        ],
        expected,
        "result.cache3",
    )

    if result:
        logging.info("Run produced expected results\n")
    else:
        sys.exit(1)


def run_test_changed_pysa_models(
    typeshed_path: str, cache_path: Path, expected: Dict[str, Any]
) -> None:
    # Run Pysa after adding a new Pysa model and ensure the cache is not
    # invalidated.
    logging.info("Testing cache is not invalidated after model files change:")

    test_model_path = Path("test_taint/PYSA_CACHE_TEST__tmp_model.pysa")
    try:
        test_model_path.unlink()
    except FileNotFoundError:
        pass

    test_model_path.touch()

    result = run_and_check_output(
        [
            "pyre",
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
        ],
        expected,
        "result.cache4",
    )

    # Clean up
    try:
        test_model_path.unlink()
    except FileNotFoundError:
        logging.warning(
            f"Could not clean up {test_model_path.absolute()} after test run."
        )
        pass

    if result:
        logging.info("Run produced expected results\n")
    else:
        sys.exit(1)


def run_test_changed_taint_config(
    typeshed_path: str, cache_path: Path, expected: Dict[str, Any]
) -> None:
    # Run Pysa after adding a new Pysa model and ensure the cache is not
    # invalidated.
    logging.info("Testing cache is not invalidated after taint.config change:")

    test_taint_config = Path("test_taint/taint.config")
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

    result = run_and_check_output(
        [
            "pyre",
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
        ],
        expected,
        "result.cache5",
    )

    # Clean up
    try:
        test_taint_config.unlink()
    except FileNotFoundError:
        logging.warning(
            f"Could not clean up {test_taint_config.absolute()} after test run."
        )
        pass

    if result:
        logging.info("Run produced expected results\n")
    else:
        sys.exit(1)


def run_test_changed_source_files(
    typeshed_path: str, cache_path: Path, expected: Dict[str, Any]
) -> None:
    # Run Pysa after adding a new file to test cache invalidation.
    # Pysa should detect that the source has chagned and fall back
    # to doing a clean run.
    logging.info("Testing cache invalidation after source files change:")

    new_file_path = Path("PYSA_CACHE_TEST__tmp_file.py")
    try:
        new_file_path.unlink()
    except FileNotFoundError:
        pass

    new_file_path.touch()

    result = run_and_check_output(
        [
            "pyre",
            "--typeshed",
            f"{typeshed_path}",
            "--noninteractive",
            "analyze",
            "--use-cache",
        ],
        expected,
        "result.cache6",
    )

    # Clean up
    try:
        new_file_path.unlink()
    except FileNotFoundError:
        logging.warning(
            f"Could not clean up {new_file_path.absolute()} after test run."
        )
        pass

    if result:
        logging.info("Run produced expected results\n")
    else:
        sys.exit(1)


def run_tests() -> None:
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
    )

    # Switch to directory of this script.
    os.chdir(os.path.dirname(__file__))
    logging.info("Running in `%s`", os.getcwd())

    cache_path = Path(".pyre/pysa.cache")
    logging.info(f"Cache file path: {cache_path.resolve()}")

    # Extract typeshed.
    with tempfile.TemporaryDirectory() as directory:
        logging.info(f"Extracting typeshed into `{directory}`...")
        subprocess.check_call(["unzip", "../typeshed/typeshed.zip", "-d", directory])
        typeshed_path = f"{directory}/typeshed-master"

        expected = ""
        with open("result.json") as file:
            expected = file.read()

            run_test_clean_run(typeshed_path, cache_path, expected)
            run_test_cache_first_and_second_runs(typeshed_path, cache_path, expected)
            run_test_invalid_cache_file(typeshed_path, cache_path, expected)
            run_test_changed_pysa_models(typeshed_path, cache_path, expected)
            run_test_changed_taint_config(typeshed_path, cache_path, expected)
            run_test_changed_source_files(typeshed_path, cache_path, expected)

        logging.info("All runs produced expected output.")


if __name__ == "__main__":
    run_tests()
