# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from collections import deque
from typing import List, NamedTuple

from . import parser
from .parser import BuildTarget


Target = NamedTuple("Target", [("build_file_directory", str), ("name", str)])


def parse_target(target: str) -> Target:
    # TODO(T38892701): Handle 'a/b/c/...'- and 'a/b/c:'-style targets
    split = target.lstrip("//").split(":")
    return Target(split[0], split[1])


class Builder(object):
    def __init__(self, buck_root: str, build_file_name: str = "TARGETS") -> None:
        self.buck_root = buck_root
        self.parser = parser.Parser(buck_root, build_file_name)

    def compute_targets_to_build(self, target: str) -> List[BuildTarget]:
        """
            Compute the set of targets to build for the given target using a
            breadth-first traversal.
        """
        targets_to_parse = deque([target])
        targets_seen = {target}
        targets_not_found = []
        build_targets = []

        while targets_to_parse:
            next_target_string = targets_to_parse.popleft()
            next_target = parse_target(next_target_string)
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
            raise ValueError(
                "Target(s) not found: {}".format(", ".join(targets_not_found))
            )
        return build_targets
