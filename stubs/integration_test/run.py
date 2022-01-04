#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import subprocess
import sys
import tempfile

LOG: logging.Logger = logging.getLogger(__name__)


def normalized_json_dump(input: str) -> str:
    normalized = json.loads(input)

    normalized = sorted(normalized, key=lambda issue: issue["path"])
    normalized = sorted(normalized, key=lambda issue: issue["line"])
    normalized = sorted(normalized, key=lambda issue: issue["column"])

    return json.dumps(normalized, sort_keys=True, indent=2) + "\n"


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
    )

    # Switch to directory of this script.
    os.chdir(os.path.dirname(__file__))
    LOG.info("Running in `%s`", os.getcwd())

    # Extract typeshed
    with tempfile.TemporaryDirectory() as directory:
        LOG.info(f"Extracting typeshed into `{directory}`...")
        subprocess.check_call(
            ["unzip", "../typeshed/typeshed.zip", "-d", directory],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

        LOG.info("Running `pyre analyze`")
        try:
            output = subprocess.check_output(
                [
                    "pyre",
                    "--typeshed",
                    f"{directory}/typeshed-master",
                    "--noninteractive",
                    "analyze",
                ]
            ).decode()
        except subprocess.CalledProcessError as exception:
            LOG.error(f"`pyre analyze` failed:\n{exception.output.decode()}")
            sys.exit(1)
        expected = ""
        with open("result.json") as file:
            expected = file.read()

        if normalized_json_dump(expected) != normalized_json_dump(output):
            with open("result.actual", "w") as file:
                file.write(normalized_json_dump(output))
            LOG.error("Output differs from expected:")
            subprocess.run(["diff", "result.json", "result.actual"])
            sys.exit(1)

        LOG.info("Run produced expected results")
