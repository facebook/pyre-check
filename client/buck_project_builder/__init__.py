# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import glob
import os
import tempfile
from collections import defaultdict, deque
from typing import Iterable, List, Mapping, NamedTuple, Optional

from . import parser
from ..filesystem import BuckBuilder
from .build_target import BuildTarget


Target = NamedTuple("Target", [("build_file_directory", str), ("name", str)])


class BuilderException(Exception):
    def __init__(self, message: str, targets: Optional[List[str]] = None) -> None:
        super(BuilderException, self).__init__(message)
        self.targets = targets or []  # type: List[str]


class FastBuckBuilder(BuckBuilder):
    def __init__(
        self,
        buck_root: str,
        build_file_name: str = "TARGETS",
        fail_on_unbuilt_target: bool = True,
        output_directory: Optional[str] = None,
    ) -> None:
        self.buck_root = buck_root
        self.build_file_name = build_file_name
        self.fail_on_unbuilt_target = fail_on_unbuilt_target
        self.output_directory = output_directory
        self.parser = parser.Parser(buck_root, build_file_name)

    def build(self, targets: Iterable[str]) -> Iterable[str]:
        output_directory = self.output_directory or tempfile.mkdtemp(prefix="pyre_tmp_")
        targets_to_build = self.compute_targets_to_build(targets)
        for target in targets_to_build:
            try:
                target.build(output_directory)
            except ValueError as error:
                raise BuilderException(str(error), targets=[target.target])
        return [output_directory]

    def compute_targets_to_build(self, targets: Iterable[str]) -> Iterable[BuildTarget]:
        """
            Compute the set of targets to build for the given targets using a
            breadth-first traversal.
        """
        normalized_targets = set()
        for target in targets:
            normalized_targets.update(self._normalize_target(target))

        targets_to_parse = deque(normalized_targets)
        targets_seen = set(normalized_targets)
        targets_not_found = []
        build_targets = set()

        while targets_to_parse:
            next_target_string = targets_to_parse.popleft()
            next_target = self._parse_target(next_target_string)
            build_file = self.parser.parse_file(next_target.build_file_directory)
            if next_target.name not in build_file.targets:
                targets_not_found.append(next_target_string)
                continue
            target_to_build = build_file.targets[next_target.name]
            build_targets.add(target_to_build)

            new_targets = [
                dependency
                for dependency in target_to_build.dependencies
                if dependency not in targets_seen
            ]
            targets_to_parse.extend(new_targets)
            targets_seen.update(new_targets)

        if targets_not_found and self.fail_on_unbuilt_target:
            raise BuilderException(
                "Target(s) could not be built: {}".format(", ".join(targets_not_found)),
                targets=targets_not_found,
            )
        return build_targets

    def compute_reverse_dependencies(
        self, targets: Iterable[BuildTarget]
    ) -> Mapping[str, Iterable[BuildTarget]]:
        """
            Compute the set of targets which depend on each target.
        """
        result = defaultdict(list)
        for target in targets:
            for dependency in target.dependencies:
                result[dependency].append(target)
        return result

    def _parse_target(self, target: str) -> Target:
        if target.endswith("...") or target.endswith(":"):
            raise BuilderException(
                "Target {} is not an absolute target.".format(target), targets=[target]
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
