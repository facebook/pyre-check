#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import sys
from pathlib import Path
from typing import Any, Dict, List

LOG: logging.Logger = logging.getLogger(__name__)


def to_sarif_result(error: Dict[str, Any]) -> Dict[str, Any]:
    """
    Takes in an error and returns it in SARIF result format
    """
    return {
        "ruleId": error["define"],
        "level": "error",
        "message": {"text": error["description"]},
        "locations": [
            {
                "physicalLocation": {
                    "artifactLocation": {
                        "uri": f"file://{Path.cwd() / error['path']}",
                    },
                    "region": {
                        "startLine": error["line"],
                        "startColumn": error["column"],
                    },
                }
            }
        ],
    }


def to_sarif(errors: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Takes in errors detected by Pysa and convert the results into a
    SARIF format
    """
    LOG.info(f"Transforming:\n{errors}")
    return {
        "version": "2.1.0",
        "$schema": "http://json.schemastore.org/sarif-2.1.0-rtm.4.json",
        "runs": [
            {
                "tool": {
                    "driver": {
                        "name": "Pysa",
                        "informationUri": "https://www.pyre-check.org/docs/pysa-basics",
                    }
                },
                "results": [to_sarif_result(error) for error in errors],
            }
        ],
    }


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s [%(levelname)s] %(message)s", level=logging.DEBUG
    )

    sarif = to_sarif(json.load(sys.stdin))
    json.dump(sarif, sys.stdout, indent=4)
