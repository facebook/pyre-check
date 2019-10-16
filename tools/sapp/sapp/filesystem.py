# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
from pathlib import Path
from typing import List, Optional


def find_root(vcs_dirs: List[str]) -> Optional[str]:
    path = Path.cwd().resolve()
    while not path.samefile(path.root):
        if any((path / vcs_dir).exists() for vcs_dir in vcs_dirs):
            return os.fspath(path)
        path = path.parent
    return None
