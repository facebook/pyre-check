#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
import os
import subprocess
import sys
from pathlib import Path

LOG: logging.Logger = logging.getLogger(__name__)


def normalized_json_dump(input: str) -> str:
    normalized = json.loads(input)

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


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run pysa stubs integration test")
    parser.add_argument(
        "-s",
        "--save-results-to",
        type=str,
        help=("Directory to write analysis results to. Default: output is not saved"),
    )
    arguments = parser.parse_args()

    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
    )

    # Verify `PYRE_BINARY` is set
    if "PYRE_BINARY" not in os.environ:
        LOG.error(
            "Required environment variable `PYRE_BINARY` is not set. "
            "`make all` from this script's directory will "
            "automatically set `PYRE_BINARY`"
        )
        sys.exit(1)

    # Switch to directory of this script.
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    LOG.info("Running in `%s`", os.getcwd())

    typeshed_directory = Path("../typeshed/typeshed").absolute().as_posix()

    LOG.info("Running `pyre analyze`")
    try:
        command = [
            "pyre",
            "--typeshed",
            typeshed_directory,
            "--noninteractive",
            "analyze",
            "--check-invariants",
            "--inline-decorators",
        ]
        if arguments.save_results_to is not None:
            command.extend(["--save-results-to", arguments.save_results_to])
        output = subprocess.check_output(command).decode()

        if arguments.save_results_to is not None:
            with open(f"{arguments.save_results_to}/errors.json") as file:
                output = file.read()

    except subprocess.CalledProcessError as exception:
        LOG.error(f"`pyre analyze` failed with return code {exception.returncode}")
        sys.stdout.write(exception.output.decode())
        sys.exit(exception.returncode)

    expected = ""
    with open("result.json") as file:
        expected = file.read()

    if normalized_json_dump(expected) != normalized_json_dump(output):
        with open("result.actual", "w") as file:
            file.write(normalized_json_dump(output))
        sys.stdout.write("Output differs from expected:\n")
        subprocess.run(["diff", "-u", "result.json", "result.actual"])
        sys.exit(30)  # ExitCode.TEST_COMPARISON_DIFFERS

    LOG.info("Run produced expected results")
