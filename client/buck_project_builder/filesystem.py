# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import os
from typing import Iterable, List, NamedTuple, Optional

from ..filesystem import add_symbolic_link, get_filesystem


Glob = NamedTuple("Glob", [("patterns", List[str]), ("exclude", List[str])])


class Sources:
    def __init__(
        self, files: Optional[List[str]] = None, globs: Optional[List[Glob]] = None
    ) -> None:
        self.files = files or []  # type: List[str]
        self.globs = globs or []  # type: List[Glob]


def resolve_sources(directory: str, sources: Sources) -> Iterable[str]:
    """
        Returns an iterable of absolute paths to the files specified by the sources
        object. Files are not guaranteed to exist.
    """
    filesystem = get_filesystem()
    result = {os.path.join(directory, file) for file in sources.files}
    for glob in sources.globs:
        matches = filesystem.list(directory, glob.patterns, exclude=glob.exclude)
        result.update([os.path.join(directory, match) for match in matches])
    return result


def link_paths(
    paths: Iterable[str], source_directory: str, output_directory: str
) -> None:
    """
        For each path in the source directory, creates a symbolic link in the output
        directory to the original path at the appropriate location (based on its
        position relative to the source directory).
    """
    for path in paths:
        relative_path = os.path.relpath(path, source_directory)
        link_path = os.path.join(output_directory, relative_path)
        add_symbolic_link(link_path, path)
