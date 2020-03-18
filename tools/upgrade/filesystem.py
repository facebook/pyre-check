# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import re
import subprocess
from enum import Enum
from pathlib import Path
from typing import Dict, List

from ...client.filesystem import get_filesystem
from .ast import verify_stable_ast
from .postprocess import LOG


class LocalMode(Enum):
    IGNORE = "pyre-ignore-all-errors"
    UNSAFE = "pyre-unsafe"
    STRICT = "pyre-strict"

    def get_regex(self) -> str:
        return "^[ \t]*# *" + self.value + " *$"

    def get_comment(self) -> str:
        return "# " + self.value


def path_exists(filename: str) -> Path:
    path = Path(filename)
    if not path.exists():
        raise ValueError("No file at {}".format(filename))
    return path


def find_targets(search_root: Path) -> Dict[str, List[str]]:
    LOG.info("Finding typecheck targets in %s", search_root)
    # TODO(T56778370): Clean up code by parsing the TARGETS file rather than using grep.
    typing_field = "check_types ?="
    targets_regex = r"(?s)name = ((?!\n\s*name).)*{}((?!\n\s*name).)*".format(
        typing_field
    )
    find_targets_command = [
        "grep",
        "-RPzo",
        "--include=*TARGETS",
        targets_regex,
        search_root,
    ]
    find_targets = subprocess.run(
        find_targets_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    if find_targets.returncode == 1:
        LOG.info("Did not find any targets.")
        return {}
    if find_targets.returncode != 0:
        LOG.error("Failed to search for targets: %s", find_targets.stderr.decode())
        return {}
    output = find_targets.stdout.decode()
    targets = re.split(typing_field, output)
    target_pattern = re.compile(r".*?([^\s]*)\/TARGETS:.*name = \"([^\"]*)\".*")
    target_names = {}
    total_targets = 0
    for target in targets:
        matched = target_pattern.match(target.replace("\n", " ").strip())
        if matched:
            total_targets += 1
            path = matched.group(1).strip()
            target_name = matched.group(2)
            if path in target_names:
                target_names[path].append(target_name)
            else:
                target_names[path] = [target_name]
    LOG.info(
        "Found {} typecheck targets in {} TARGETS files to analyze".format(
            total_targets, len(target_names)
        )
    )
    return target_names


@verify_stable_ast
def add_local_mode(filename: str, mode: LocalMode) -> None:
    LOG.info("Processing `%s`", filename)
    path = Path(filename)
    text = path.read_text()
    if "@" "generated" in text:
        LOG.warning("Attempting to edit generated file %s, skipping.", filename)
        return

    lines = text.split("\n")  # type: List[str]

    # Check if a local mode is already set.
    for line in lines:
        for local_mode in LocalMode:
            if re.match(local_mode.get_regex(), line):
                return

    def is_header(line: str) -> bool:
        is_comment = line.lstrip().startswith("#")
        is_pyre_ignore = (
            re.match("^[ \t]*# *pyre-ignore *$", line)
            or re.match("^[ \t]*# *pyre-fixme *$", line)
            or re.match("^[ \t]*# *type: ignore *$", line)
        )
        return is_comment and not is_pyre_ignore

    # Add local mode.
    new_lines = []
    past_header = False
    for line in lines:
        if not past_header and not is_header(line):
            past_header = True
            if len(new_lines) != 0:
                new_lines.append("")
            new_lines.append(mode.get_comment())
        new_lines.append(line)
    new_text = "\n".join(new_lines)
    path.write_text(new_text)


def remove_non_pyre_ignores(subdirectory: Path) -> None:
    python_files = [
        str(subdirectory / path)
        for path in get_filesystem().list(str(subdirectory), patterns=[r"**/*.py"])
    ]
    if python_files:
        LOG.info("...cleaning %s python files", len(python_files))
        remove_type_ignore_command = [
            "sed",
            "-i",
            r"s/# \?type: \?ignore$//g",
        ] + python_files
        subprocess.check_output(remove_type_ignore_command)
