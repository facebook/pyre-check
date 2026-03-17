# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
import subprocess
import tempfile
from pathlib import Path
from typing import Optional

from .. import (
    backend_arguments,
    frontend_configuration,
    log,
)
from . import commands, start
from .analyze import (
    _download_pyrefly_binary,
    _flush_log_file,
    _get_server_start_command,
    _run_pyrefly,
    create_base_arguments_for_pyrefly,
)

LOG: logging.Logger = logging.getLogger(__name__)


def _run_pyrefly_query_command(
    pyre_binary: str,
    pyrefly_results: str,
    query: str,
    configuration: frontend_configuration.Base,
    output_file: Optional[str],
) -> commands.ExitCode:
    configuration_arguments = create_base_arguments_for_pyrefly(configuration)
    with (
        backend_arguments.temporary_argument_file(
            configuration_arguments
        ) as configuration_file_path,
        backend_arguments.backend_log_file(prefix="pyre_pyrefly_query") as log_file,
        start.background_logging(Path(log_file.name)),
    ):
        pyrefly_query_command = [
            pyre_binary,
            "pyrefly-query",
            "--pyrefly-results",
            pyrefly_results,
            "--query",
            query,
            "--configuration-file",
            str(configuration_file_path),
        ]
        if output_file is not None:
            pyrefly_query_command += ["--output-file", output_file]
        # lint-ignore: NoUnsafeExecRule
        result = subprocess.run(
            pyrefly_query_command,
            stdout=subprocess.PIPE,
            stderr=log_file.file,
            universal_newlines=True,
            errors="replace",
        )

        _flush_log_file()

        return_code = result.returncode
        if return_code == 0:
            log.stdout.write(result.stdout)
            return commands.ExitCode.SUCCESS
        else:
            LOG.error(
                f"Pyre pyrefly-query exited with non-zero return code: {return_code}."
            )
            return commands.ExitCode.FAILURE


def run(
    configuration: frontend_configuration.Base,
    query: str,
    pyrefly_results: Optional[str],
    output_file: Optional[str],
) -> commands.ExitCode:
    start_command = _get_server_start_command(configuration)

    if pyrefly_results is not None:
        return _run_pyrefly_query_command(
            start_command.get_pyre_binary_location(),
            pyrefly_results,
            query,
            configuration,
            output_file,
        )

    with (
        tempfile.TemporaryDirectory() as temporary_pyrefly_results,
        tempfile.NamedTemporaryFile(
            mode="w", delete=True, delete_on_close=False
        ) as temporary_file,
    ):
        pyrefly_binary_path = _download_pyrefly_binary(
            configuration,
            Path(temporary_file.name),
        )
        temporary_file.close()

        return_code = _run_pyrefly(
            pyrefly_binary_path,
            temporary_pyrefly_results,
            forward_stdout=False,
        )
        if return_code != commands.ExitCode.SUCCESS:
            return return_code

        return _run_pyrefly_query_command(
            start_command.get_pyre_binary_location(),
            temporary_pyrefly_results,
            query,
            configuration,
            output_file,
        )
