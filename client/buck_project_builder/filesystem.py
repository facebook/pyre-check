# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import os
from typing import Iterable

from ..filesystem import get_filesystem
from .build_target import Sources


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
