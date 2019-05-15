#!/usr/bin/env python3

import json
import logging
import os
import subprocess
import sys
import tempfile


def normalized_json_dump(input):
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
    logging.info("Running in `%s`", os.getcwd())

    # Extract typeshed
    with tempfile.TemporaryDirectory() as directory:
        logging.info(f"Extracting typeshed into `{directory}`...")
        subprocess.check_call(["unzip", "../typeshed/typeshed.zip", "-d", directory])

        logging.info("Running `pyre analyze`")
        output = subprocess.check_output(
            [
                "pyre",
                "--typeshed",
                f"{directory}/typeshed-master",
                "--noninteractive",
                "analyze",
            ]
        ).decode()

        expected = ""
        with open("result.json") as file:
            expected = file.read()

        if normalized_json_dump(expected) != normalized_json_dump(output):
            with open("result.actual", "w") as file:
                file.write(normalized_json_dump(output))
            logging.error("Output differs from expected:")
            subprocess.run(["diff", "result.json", "result.actual"])
            sys.exit(1)

        logging.info("Run produced expected results")
