# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import datetime
import itertools
import json
import logging
import shutil
import subprocess
from pathlib import Path
from typing import Optional, TextIO, Sequence, List, Tuple

from .. import (
    command_arguments,
    configuration as configuration_module,
    log,
    version,
)
from . import commands, start, remote_logging

LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class Section:
    name: str
    content: str


def _print_section(section: Section, output: TextIO) -> None:
    print(f"{section.name}", file=output)
    separator = "=" * len(section.name)
    print(f"{separator}", file=output)
    print(section.content, file=output)
    print("", file=output, flush=True)


def _version_section(configuration: configuration_module.Configuration) -> Section:
    client_version_line = f"Client version: {version.__version__}"
    try:
        binary_version = configuration.get_binary_version()
        binary_version_line = f"Binary version: {binary_version}"
    except Exception as error:
        binary_version_line = f"Could not determine binary version: {error}"
    return Section(
        name="Versions", content="\n".join([client_version_line, binary_version_line])
    )


def _configuration_section(
    configuration: configuration_module.Configuration,
) -> Section:
    return Section(
        name="Configuration", content=json.dumps(configuration.to_json(), indent=2)
    )


def _get_subprocess_stdout(command: Sequence[str]) -> Optional[str]:
    result = subprocess.run(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True
    )
    if result.returncode != 0:
        return None
    return result.stdout


def _get_file_content(path: Path) -> Optional[str]:
    try:
        return path.read_text()
    except Exception:
        return None


def _mercurial_section(
    mercurial: str, name: str, additional_flags: Optional[Sequence[str]] = None
) -> Optional[Section]:
    output = _get_subprocess_stdout(
        [mercurial, name] + ([] if additional_flags is None else list(additional_flags))
    )
    return (
        None
        if output is None
        else Section(name=f"Mercurial {name.capitalize()}", content=output)
    )


def _watchman_section(watchman: str, name: str) -> Optional[Section]:
    output = _get_subprocess_stdout([watchman, name])
    return (
        None
        if output is None
        else Section(name=f"Watchman {name.capitalize()}", content=output)
    )


def _parse_log_file_name(name: str) -> Optional[datetime.datetime]:
    try:
        return datetime.datetime.strptime(name, start.SERVER_LOG_FILE_FORMAT)
    except ValueError:
        return None


def _get_server_log_timestamp_and_paths(
    log_directory: Path,
) -> List[Tuple[datetime.datetime, Path]]:
    try:
        return sorted(
            (
                (timestamp, path)
                for timestamp, path in (
                    (_parse_log_file_name(path.name), path)
                    for path in (log_directory / "new_server").iterdir()
                    if path.is_file()
                )
                if timestamp is not None
            ),
            key=lambda pair: pair[0],
            reverse=True,
        )
    except Exception:
        return []


def _server_log_sections(
    log_directory: Path, limit: Optional[int] = None
) -> List[Section]:
    # Log files are sorted according to server start time: recently started servers
    # will come first.
    timestamp_and_paths = _get_server_log_timestamp_and_paths(log_directory)

    sections: List[Section] = []
    for timestamp, path in timestamp_and_paths:
        if limit is not None and len(sections) >= limit:
            break
        content = _get_file_content(path)
        if content is None:
            continue
        sections.append(Section(name=f"Server Log ({timestamp})", content=content))
    return sections


def _client_log_section(log_directory: Path) -> Optional[Section]:
    content = _get_file_content(log_directory / "pyre.stderr")
    if content is None:
        return None
    return Section(name="Client Log", content=content)


def _print_configuration_sections(
    configuration: configuration_module.Configuration, output: TextIO
) -> None:
    LOG.info("Collecting information about Pyre configurations...")
    _print_section(_version_section(configuration), output)
    _print_section(_configuration_section(configuration), output)


def _print_mercurial_sections(output: TextIO) -> None:
    LOG.info("Collecting information about mercurial...")
    mercurial = shutil.which("hg")
    if mercurial is not None:
        for section in [
            _mercurial_section(mercurial, "id"),
            _mercurial_section(mercurial, "status"),
            _mercurial_section(mercurial, "diff"),
            _mercurial_section(
                mercurial, "reflog", additional_flags=["--limit", "100"]
            ),
        ]:
            if section is not None:
                _print_section(section, output)


def _print_watchman_sections(output: TextIO) -> None:
    LOG.info("Collecting information about watchman...")
    watchman = shutil.which("watchman")
    if watchman is not None:
        for section in [_watchman_section(watchman, "watch-list")]:
            if section is not None:
                _print_section(section, output)


def _print_log_file_sections(
    log_directory: Path, server_log_count: Optional[int], output: TextIO
) -> None:
    LOG.info("Collecting information from Pyre's log files...")
    for section in itertools.chain(
        _server_log_sections(log_directory, limit=server_log_count),
        [
            _client_log_section(log_directory),
        ],
    ):
        if section is not None:
            _print_section(section, output)


def run_rage(
    configuration: configuration_module.Configuration,
    arguments: command_arguments.RageArguments,
    output: TextIO,
) -> None:
    _print_configuration_sections(configuration, output)
    _print_mercurial_sections(output)
    _print_watchman_sections(output)
    _print_log_file_sections(
        Path(configuration.log_directory), arguments.server_log_count, output
    )
    LOG.info("Done\n")


@remote_logging.log_usage(command_name="rage")
def run(
    configuration: configuration_module.Configuration,
    arguments: command_arguments.RageArguments,
) -> commands.ExitCode:
    try:
        output_path = arguments.output
        if output_path is None:
            run_rage(configuration, arguments, log.stdout)
        else:
            with open(output_path) as output:
                run_rage(configuration, arguments, output)
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occurred during rage generation: {error}"
        ) from error
