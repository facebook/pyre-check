# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import Optional


def get_project_identifier(
    global_root: Path,
    relative_local_root: Optional[str],
) -> str:
    project_identifier = str(global_root)
    if relative_local_root is not None:
        project_identifier = project_identifier + "//" + relative_local_root
    return project_identifier
