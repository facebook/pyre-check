# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import ast as builtin_ast
import logging
import re
import subprocess
from enum import Enum
from pathlib import Path
from typing import Dict, List, NamedTuple, Optional

from ...client.filesystem import get_filesystem
from . import ast


LOG: logging.Logger = logging.getLogger(__name__)


class LocalMode(Enum):
    IGNORE = "pyre-ignore-all-errors"
    UNSAFE = "pyre-unsafe"
    STRICT = "pyre-strict"

    def get_regex(self) -> str:
        return "^[ \t]*# *" + self.value + " *$"

    def get_comment(self) -> str:
        return "# " + self.value


class Target(NamedTuple):
    name: str
    strict: bool
    pyre: bool


class TargetCollector(builtin_ast.NodeVisitor):
    def __init__(self, pyre_only: bool) -> None:
        self._pyre_only: bool = pyre_only
        self._targets: List[Target] = []
        self._contains_strict: bool = False

    def visit_Call(self, node: builtin_ast.Call) -> None:
        target_fields = node.keywords
        check_types = False
        uses_pyre = True
        is_strict = False
        name = None
        for field in target_fields:
            value = field.value
            if field.arg == "name":
                if isinstance(value, builtin_ast.Str):
                    name = value.s
            elif field.arg == "check_types":
                if isinstance(value, builtin_ast.NameConstant):
                    check_types = check_types or value.value
            elif field.arg == "check_types_options":
                if isinstance(value, builtin_ast.Str):
                    uses_pyre = uses_pyre and "mypy" not in value.s.lower()
                    is_strict = is_strict or (uses_pyre and "strict" in value.s.lower())
            elif field.arg == "typing_options":
                if isinstance(value, builtin_ast.Str):
                    is_strict = is_strict or "strict" in value.s.lower()
        if name and check_types and (not self._pyre_only or uses_pyre):
            self._targets.append(Target(name, is_strict, uses_pyre))
        self._contains_strict = self._contains_strict or is_strict

    def result(self) -> List[Target]:
        return self._targets

    def contains_strict(self) -> bool:
        return self._contains_strict


def path_exists(filename: str) -> Path:
    path = Path(filename)
    if not path.exists():
        raise ValueError("No file at {}".format(filename))
    return path


def find_targets(search_root: Path, pyre_only: bool = False) -> Dict[str, List[Target]]:
    LOG.info("Finding typecheck targets in %s", search_root)
    target_files = find_files(search_root, "TARGETS")
    target_names = {}
    total_targets = 0
    for target_file in target_files:
        target_finder = TargetCollector(pyre_only)
        with open(target_file, "r") as source:
            tree = builtin_ast.parse(source.read())
            target_finder.visit(tree)
            targets = target_finder.result()
            if len(targets) > 0:
                target_names[target_file] = targets
                total_targets += len(targets)

    LOG.info(
        "Found {} typecheck targets in {} TARGETS files to analyze".format(
            total_targets, len(target_names)
        )
    )
    return target_names


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
    ast.check_stable(text, new_text)
    path.write_text(new_text)


def remove_non_pyre_ignores(subdirectory: Path) -> None:
    python_files = [
        subdirectory / path
        for path in get_filesystem().list(str(subdirectory), patterns=[r"**/*.py"])
    ]
    if python_files:
        LOG.info("...cleaning %s python files", len(python_files))
        remove_type_ignore_command = [
            "sed",
            "-i",
            r"s/\s*# \?type: \?ignore$//g",
        ] + [str(file) for file in python_files if file.exists()]
        subprocess.check_output(remove_type_ignore_command)


def find_files(
    directory: Path, name: str, grep_pattern: Optional[str] = None
) -> List[str]:
    grep_arguments = (
        ["-exec", "grep", "--files-with-matches", grep_pattern, "{}", "+"]
        if grep_pattern is not None
        else []
    )

    try:
        output = (
            subprocess.check_output(
                ["find", str(directory), "-name", name, *grep_arguments]
            )
            .decode("utf-8")
            .strip()
        )
    except subprocess.CalledProcessError as error:
        LOG.warning(
            "Failed to find files with name `%s` in directory `%s`:\n%s",
            name,
            directory,
            error.stderr,
        )
        return []

    if output == "":
        return []
    files = output.split("\n")
    return [file.strip() for file in files]


def find_directories(directory: Path) -> List[Path]:
    return [path for path in directory.iterdir() if path.is_dir()]
