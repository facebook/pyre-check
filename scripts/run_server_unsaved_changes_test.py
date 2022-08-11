# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import sys
from libfb.py.log import set_simple_logging
from logging import Logger
from argparse import Namespace

LOG: Logger = logging.getLogger(__name__)

def run_unsaved_changes_test(
    typeshed_zip_path: str, repository_path: str, debug: bool
) -> int:
    return 0

def run(repository_location: str, typeshed_zip_path: str, debug: bool) -> int:
    typeshed_zip_path = typeshed_zip_path
    return run_unsaved_changes_test(
        typeshed_zip_path, repository_location, debug
    )


if __name__ == "__main__":
    set_simple_logging(escape_newlines=False)
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repository_location", help="Path to directory with fake commit list"
    )
    parser.add_argument(
        "--typeshed-zip-path",
        help="Path to zip containing typeshed.",
    )
    parser.add_argument("--debug", action="store_true", default=False)
    arguments: Namespace = parser.parse_args()
    sys.exit(
        run(arguments.repository_location, arguments.typeshed_zip_path, arguments.debug)
    )
