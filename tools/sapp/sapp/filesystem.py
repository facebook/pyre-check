#!/usr/bin/env python

import os
from pathlib import Path
from typing import List, Optional


def find_root(vcs_dirs: List[str]) -> Optional[str]:
    path = Path.cwd().resolve()
    while not path.samefile(path.root):
        if any((path / vcs_dir).exists() for vcs_dir in vcs_dirs):
            # pyre-fixme[6]: Expected `str` for 1st param but got `Path`.
            return os.fspath(path)
        path = path.parent
    return None
