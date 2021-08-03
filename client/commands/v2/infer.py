# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path
from typing import List, Optional

from ... import commands, command_arguments, configuration as configuration_module
from . import remote_logging

LOG: logging.Logger = logging.getLogger(__name__)


def _check_arguments(infer_arguments: command_arguments.InferArguments) -> None:
    if (
        infer_arguments.annotate_from_existing_stubs
        and infer_arguments.paths_to_modify is None
    ):
        raise ValueError(
            "`--annotate-from-existing-stubs` cannot be used without the"
            " `--in-place` flag"
        )


def _check_working_directory(
    working_directory: Path, global_root: Path, relative_local_root: Optional[str]
) -> None:
    candidate_locations: List[str] = []
    if working_directory == global_root:
        return
    candidate_locations.append(f"`{global_root}` with `--local-configuration` set")

    if relative_local_root is not None:
        local_root = global_root / relative_local_root
        if working_directory == local_root:
            return
        candidate_locations.append(f"`{local_root}`")

    valid_locations = " or from ".join(candidate_locations)
    raise ValueError(
        f"Infer must run from {valid_locations}. "
        f"Cannot run from current working directory `{working_directory}`."
    )


def run_infer(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    working_directory = Path.cwd()
    _check_arguments(infer_arguments)
    _check_working_directory(
        working_directory=working_directory,
        global_root=Path(configuration.project_root),
        relative_local_root=configuration.relative_local_root,
    )
    LOG.warning("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="infer")
def run(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    try:
        return run_infer(configuration, infer_arguments)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during Pyre infer: {error}"
        ) from error
