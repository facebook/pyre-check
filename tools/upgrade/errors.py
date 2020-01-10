# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import itertools
import json
import sys
from typing import Any, Dict, Iterator, List, Optional, Tuple

from .postprocess import LOG


def json_to_errors(json_string: Optional[str]) -> List[Dict[str, Any]]:
    if json_string:
        try:
            return json.loads(json_string)
        except json.decoder.JSONDecodeError:
            LOG.error(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
    else:
        LOG.error(
            "Recevied no input."
            "If piping from `pyre check` be sure to use `--output=json`."
        )
    return []


def sort_errors(
    errors: List[Dict[str, Any]]
) -> Iterator[Tuple[str, Iterator[Dict[str, Any]]]]:
    def error_path(error: Dict[str, Any]) -> str:
        return error["path"]

    return itertools.groupby(sorted(errors, key=error_path), error_path)


def filter_errors(
    arguments: argparse.Namespace, errors: List[Dict[str, Any]]
) -> List[Dict[str, Any]]:
    def matches_error_code(error: Dict[str, Any]) -> bool:
        return error["code"] == arguments.only_fix_error_code

    if arguments.only_fix_error_code:
        errors = list(filter(matches_error_code, errors))
    return errors


def errors_from_stdin(_arguments: argparse.Namespace) -> List[Dict[str, Any]]:
    input = sys.stdin.read()
    errors = json_to_errors(input)
    return filter_errors(_arguments, errors)
