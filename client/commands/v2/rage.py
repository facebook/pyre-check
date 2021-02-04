# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import logging
import shutil
import subprocess
from pathlib import Path
from typing import Optional, TextIO, Sequence

from ... import commands, configuration as configuration_module, log, version

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


def _server_log_section(log_directory: Path) -> Optional[Section]:
    content = _get_file_content(log_directory / "new_server" / "server.stderr")
    if content is None:
        return None
    return Section(name="Server Log", content=content)


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


def _print_log_file_sections(log_directory: Path, output: TextIO) -> None:
    LOG.info("Collecting information from Pyre's log files...")
    for section in [
        _server_log_section(log_directory),
        _client_log_section(log_directory),
    ]:
        if section is not None:
            _print_section(section, output)


def run_rage(configuration: configuration_module.Configuration, output: TextIO) -> None:
    _print_configuration_sections(configuration, output)
    _print_mercurial_sections(output)
    _print_watchman_sections(output)
    _print_log_file_sections(Path(configuration.log_directory), output)
    LOG.info("Done\n")


def run(
    configuration: configuration_module.Configuration, output_path: Optional[Path]
) -> commands.ExitCode:
    try:
        if output_path is None:
            run_rage(configuration, log.stdout)
        else:
            with open(output_path) as output:
                run_rage(configuration, output)
        return commands.ExitCode.SUCCESS
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during rage generation: {error}"
        ) from error
