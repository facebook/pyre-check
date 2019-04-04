# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import glob
import os
from collections import deque
from typing import List, NamedTuple, Optional

from . import parser
from .parser import BuildTarget


Target = NamedTuple("Target", [("build_file_directory", str), ("name", str)])


class BuilderException(Exception):
    pass


class Builder(object):
    def __init__(self, buck_root: str, build_file_name: str = "TARGETS") -> None:
        self.buck_root = buck_root
        self.build_file_name = build_file_name
        self.parser = parser.Parser(buck_root, build_file_name)

    def compute_targets_to_build(self, target: str) -> List[BuildTarget]:
        """
            Compute the set of targets to build for the given target using a
            breadth-first traversal.
        """
        normalized_targets = self._normalize_target(target)
        targets_to_parse = deque(normalized_targets)
        targets_seen = set(normalized_targets)
        targets_not_found = []
        build_targets = []

        while targets_to_parse:
            next_target_string = targets_to_parse.popleft()
            next_target = self._parse_target(next_target_string)
            build_file = self.parser.parse_file(next_target.build_file_directory)
            if next_target.name not in build_file.targets:
                targets_not_found.append(next_target_string)
                continue
            target_to_build = build_file.targets[next_target.name]
            build_targets.append(target_to_build)

            new_targets = [
                dependency
                for dependency in target_to_build.dependencies
                if dependency not in targets_seen
            ]
            targets_to_parse.extend(new_targets)
            targets_seen.update(new_targets)

        if targets_not_found:
            raise BuilderException(
                "Target(s) not found: {}".format(", ".join(targets_not_found))
            )
        return build_targets

    def _parse_target(self, target: str) -> Target:
        if target.endswith("...") or target.endswith(":"):
            raise BuilderException(
                "Target {} is not an absolute target.".format(target)
            )
        split = _strip(target, left="//").split(":")
        return Target(split[0], split[1])

    def _normalize_target(self, target: str) -> List[str]:
        if target.endswith("..."):
            # Find all build files and recursively add all rules from each file.
            targets = []
            directory = os.path.join(
                self.buck_root, _strip(target, left="//", right="...")
            )
            for build_file in glob.iglob(
                os.path.join(directory, "**", self.build_file_name), recursive=True
            ):
                relative_path = os.path.relpath(build_file, self.buck_root)
                build_file_directory = os.path.dirname(relative_path)
                wildcard_target = "//{}:".format(build_file_directory)
                targets.extend(self._normalize_target(wildcard_target))
            return targets
        elif target.endswith(":"):
            # Normalize to every target in the specified file.
            build_file = self.parser.parse_file(_strip(target, left="//", right=":"))
            return [build_target.target for build_target in build_file.targets.values()]
        else:
            return [target]


def _strip(string: str, left: Optional[str] = None, right: Optional[str] = None) -> str:
    if left and string.startswith(left):
        string = string[len(left) :]
    if right and string.endswith(right):
        string = string[: -len(right)]
    return string
