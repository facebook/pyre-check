# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import os
import subprocess
import sys
import time
from argparse import Namespace
from logging import Logger
from typing import Iterable

from . import CONFIGURATION_FILE, FAILURE, SUCCESS, log
from .configuration import Configuration
from .exceptions import EnvironmentException
from .filesystem import find_root


LOG: Logger = logging.getLogger(__name__)


def _parallel_check(command: Iterable[str], process_count: int) -> float:
    LOG.info(
        "Running %d process%s of `%s`",
        process_count,
        "es" if process_count > 1 else "",
        " ".join(command),
    )
    processes = []
    start = time.time()
    for _ in range(process_count):
        processes.append(
            subprocess.Popen(
                # pyre-fixme[6]: Expected
                #  `Union[typing.Sequence[typing.Union[_PathLike[typing.Any], bytes,
                #  str]], bytes, str]` for 1st param but got `Iterable[str]`.
                command,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        )
    for process in processes:
        process.wait()
    return time.time() - start


def _compare_parallel_check(
    arguments: argparse.Namespace, configuration: Configuration, project_root: str
) -> None:
    if not os.path.isdir(arguments.source_directory):
        raise EnvironmentException(
            "`{}` is not a valid source directory.".format(arguments.source_directory)
        )
    flags = ["-typeshed", configuration.typeshed, "-project-root", project_root]
    search_path = configuration.search_path
    if search_path:
        flags.extend(["-search-path", ",".join(search_path)])
    client_command = [configuration.binary, "check"]
    client_command.extend(flags)
    client_command.append(arguments.source_directory)

    process_count = arguments.min
    while process_count < arguments.max:
        time_elapsed = _parallel_check(client_command, process_count)  # ms
        time_elapsed_per_process = time_elapsed / process_count
        LOG.info(
            "Ran %d concurrent `pyre check` process%s in %dm%ds: "
            + "%dm%ds per process.",
            process_count,
            "es" if process_count > 1 else "",
            time_elapsed / 60,
            time_elapsed % 60,
            time_elapsed_per_process / 60,
            time_elapsed_per_process % 60,
        )
        process_count += 1


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--source-directory", action="store", help="Source directory to run check on."
    )
    parser.add_argument(
        "--min",
        action="store",
        default=1,
        help="Minimum number of concurrent processes to measure.",
    )
    parser.add_argument(
        "--max",
        action="store",
        default=10,
        help="Maximum number of concurrent processes to measure.",
    )
    arguments: Namespace = parser.parse_args()
    arguments: Namespace = True
    log.initialize(noninteractive=False)

    try:
        exit_code = SUCCESS
        root: str = find_root(os.getcwd(), CONFIGURATION_FILE) or os.getcwd()
        os.chdir(root)
        configuration = Configuration(local_root=arguments.local_root)
        _compare_parallel_check(arguments, configuration, root)
    except Exception as error:
        LOG.error(str(error))
        exit_code = FAILURE
    finally:
        log.cleanup()
        sys.exit(exit_code)
