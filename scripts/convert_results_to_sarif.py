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


Error = Dict[str, Any]
Location = Dict[str, Any]


def _locations(errors: List[Error]) -> Dict[str, Location]:
    locations = {
        error["path"]: {
            "uri": f"file://{Path.cwd() / error['path']}",
            "index": 0,
        }
        for error in errors
    }
    for index, location in enumerate(locations.values()):
        location["index"] = index
    return locations


def _to_sarif_result(error: Error, locations: Dict[str, Location]) -> Dict[str, Any]:
    LOG.info(f"Transforming error:\n{error}")

    return {
        "ruleId": "type-error",
        "ruleIndex": 0,
        "level": "error",
        "message": {
            "text": error["description"],
        },
        "locations": [
            {
                "physicalLocation": {
                    "artifactLocation": locations[error["path"]],
                    "region": {
                        "startLine": error["line"],
                        "startColumn": error["column"],
                    },
                }
            }
        ],
    }


def _to_sarif(errors: List[Dict[str, Any]]) -> Dict[str, Any]:
    LOG.info(f"Transforming:\n{errors}")
    locations = _locations(errors)
    return {
        "version": "2.1.0",
        "$schema": "http://json.schemastore.org/sarif-2.1.0-rtm.4",
        "runs": [
            {
                "tool": {
                    "driver": {
                        "name": "Pyre",
                        "informationUri": "https://www.pyre-check.org",
                        "rules": [
                            {
                                "id": "type-error",
                                "shortDescription": {"text": "Type Error"},
                                "helpUri": "https://www.pyre-check.org",
                                "help": {"text": "Pyre is a type checker for Python"},
                            },
                        ],
                    }
                },
                "artifacts": [
                    {"location": location}
                    for location in sorted(
                        locations.values(), key=lambda location: location["index"]
                    )
                ],
                "results": [_to_sarif_result(error, locations) for error in errors],
            }
        ],
    }


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s [%(levelname)s] %(message)s", level=logging.DEBUG
    )

    sarif = _to_sarif(json.load(sys.stdin))
    json.dump(sarif, sys.stdout, indent=4)
