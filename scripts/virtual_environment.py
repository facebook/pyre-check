#! /usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import argparse
import logging
import os
import subprocess
import sys
import venv
from pathlib import Path
from typing import NoReturn

LOG = logging.getLogger(__name__)


PATH = Path(os.path.expanduser("~/.venvs/pyre"))
ACTIVATE = PATH / "bin" / "activate"


def _setup(arguments: argparse._Namespace) -> NoReturn:
    LOG.info(f"Attempting to setup virtual environment in `{PATH}`...")
    venv.create(str(PATH), with_pip=True)

    LOG.info("Installing requirements...")
    requirements = Path(__file__).parent.parent / "requirements.txt"
    _run_in_environment(f"pip install -r {requirements}")


def _enter(arguments: argparse._Namespace) -> None:
    print(f"source {ACTIVATE}")


def _leave(arguments: argparse._Namespace) -> None:
    print("deactivate")


def _run_in_environment(command: str) -> None:
    subprocess.check_call(["sh", "-c", f"source {ACTIVATE}; {command}"])


def _run(arguments: argparse._Namespace) -> None:
    _run_in_environment(arguments.to_run)


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s [%(levelname)s] %(message)s", level=logging.DEBUG
    )

    parser = argparse.ArgumentParser(
        description="Run inside the Pyre virtual environment"
    )
    commands = parser.add_subparsers()

    setup = commands.add_parser("setup")
    setup.set_defaults(command=_setup)

    enter = commands.add_parser(
        "enter",
        help="Enter the virtual environment."
        "Run with `eval $(./scripts/virtual_environment.py enter)`",
    )
    enter.set_defaults(command=_enter)

    leave = commands.add_parser(
        "leave",
        help="Leave the virtual environment."
        "Run with `eval $(./scripts/virtual_environment.py leave)`",
    )
    leave.set_defaults(command=_leave)

    run = commands.add_parser("run", help="Run a command in the virtual environment")
    run.add_argument("to_run")
    run.set_defaults(command=_run)

    arguments = parser.parse_args()
    if not hasattr(arguments, "command"):
        LOG.warning("No command provided, see `--help` for more information.")
        sys.exit(1)

    arguments.command(arguments)
