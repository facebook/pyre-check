# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import subprocess
import sys
from pathlib import Path

from .. import log
from ..find_directories import find_taint_models_directory
from . import commands

LOG: logging.Logger = logging.getLogger(__name__)


class PysaInitializationError(Exception):
    pass


def run() -> int:
    try:
        working_directory = Path(os.getcwd()).resolve()
        # Check if venv is in working_directory
        venv_path = (
            # pyre-fixme[6]: Incompatible parameter type for `Path`, expected `str`
            Path(os.environ.get("VIRTUAL_ENV")).resolve()
            if os.environ.get("VIRTUAL_ENV", False)
            else None
        )
        if venv_path is not None and str(working_directory) in str(
            venv_path.parent.absolute()
        ):
            raise PysaInitializationError(
                "Can't use a virtual environment in the project directory."
                " Please use one outside the project directory."
            )

        # Install dependencies
        install_dependencies = log.get_yes_no_input(
            "Would you like to install the project dependencies now?"
        )
        if install_dependencies:
            requirements_file = "requirements.txt"
            if not os.path.isfile(working_directory / requirements_file):
                requirements_file = Path(
                    log.get_input(
                        f"requirements.txt file not found in `{working_directory}`"
                        " Please enter its location (eg: ./requirements-dev.txt): "
                    )
                )
                requirements_file = working_directory / requirements_file
            subprocess.run(
                [sys.executable, "-m", "pip", "install", "-r", requirements_file],
                check=True,
            )
        else:
            LOG.warning(
                "You have chosen not to install dependencies."
                " Please install dependencies before running Pysa to get best results."
            )
        run_infer = log.get_yes_no_input("Would you like to generate type annotations?")
        if run_infer:
            subprocess.run(["pyre", "infer", "-i"], check=True)
        pyre_check_path = find_taint_models_directory()
        if pyre_check_path and os.path.isdir(pyre_check_path / "pyre_check"):
            pyre_check_path = pyre_check_path / "pyre_check"
            LOG.info("Importing filters to sapp")
            subprocess.run(["sapp", "filter", "import", pyre_check_path], check=True)
        else:
            LOG.warning(
                "Couldn't infer filter directory, skipping filter import to sapp."
            )
        LOG.log(
            log.SUCCESS,
            "Successfully initialized an environemnt to run Pysa!\n"
            + "  You can now run Pysa with `pyre analyze`.",
        )
        return commands.ExitCode.SUCCESS
    except Exception as error:
        LOG.error(f"Couldn't complete Pysa initialization:\n{error}")
        return commands.ExitCode.FAILURE
