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
) -> None:
    output = subprocess.check_output(command).decode()

    if normalized_json_dump(expected) != normalized_json_dump(output):
        with open(output_file_name, "w") as file:
            file.write(normalized_json_dump(output))
        logging.error("Output differs from expected:")
        subprocess.run(["diff", "result.json", output_file_name])
        sys.exit(1)
    else:
        logging.info("Run produced expected results")


def run_test() -> None:
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
    )

    # Ensure the cache file doesn't already exist for a clean run.
    cache_path = Path(".pyre/pysa.cache")
    try:
        cache_path.unlink()
    except FileNotFoundError:
        pass

    # Switch to directory of this script.
    os.chdir(os.path.dirname(__file__))
    logging.info("Running in `%s`", os.getcwd())

    # Extract typeshed.
    with tempfile.TemporaryDirectory() as directory:
        logging.info(f"Extracting typeshed into `{directory}`...")
        subprocess.check_call(["unzip", "../typeshed/typeshed.zip", "-d", directory])

        expected = ""
        with open("result.json") as file:
            expected = file.read()

        # Run Pysa without the cache argument.
        logging.info("Running `pyre analyze` without --use-cache:")
        run_and_check_output(
            [
                "pyre",
                "--typeshed",
                f"{directory}/typeshed-master",
                "--noninteractive",
                "analyze",
            ],
            expected,
            "result.no_cache",
        )

        # Run Pysa with the cache argument for the first time. This should create
        # the cache file and save state to it since the file doesn't exist already.
        logging.info("Running `pyre analyze` with --use-cache (first run):")
        run_and_check_output(
            [
                "pyre",
                "--typeshed",
                f"{directory}/typeshed-master",
                "--noninteractive",
                "analyze",
                "--use-cache",
            ],
            expected,
            "result.cache1",
        )

        # Run Pysa with the cache argument for the second time. Since the file
        # exists, Pysa should load the saved state from the file.
        logging.info("Running `pyre analyze` with --use-cache (second run):")
        run_and_check_output(
            [
                "pyre",
                "--typeshed",
                f"{directory}/typeshed-master",
                "--noninteractive",
                "analyze",
                "--use-cache",
            ],
            expected,
            "result.cache2",
        )

        # Run Pysa with an empty .pyre/pysa.cache to simulate an invalid/corrupt
        # cache file. Pysa should fall back to doing a clean run.
        try:
            cache_path.unlink()
            cache_path.touch()
        except FileNotFoundError:
            sys.exit(1)
        logging.info("Running `pyre analyze` with --use-cache (invalid cache file):")
        run_and_check_output(
            [
                "pyre",
                "--typeshed",
                f"{directory}/typeshed-master",
                "--noninteractive",
                "analyze",
                "--use-cache",
            ],
            expected,
            "result.cache3",
        )


if __name__ == "__main__":
    run_test()
