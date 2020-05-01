# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import List, Optional


class Repository:
    LINTERS_TO_SKIP: List[str] = []

    def get_changed_files(self) -> Optional[List[str]]:
        return None

    def commit_message(self, title: str, summary_override: Optional[str] = None) -> str:
        return ""

    def add_paths(self, paths: List[Path]) -> None:
        pass

    def submit_changes(
        self, submit: bool, message: str, ignore_failures: bool = False
    ) -> None:
        pass
