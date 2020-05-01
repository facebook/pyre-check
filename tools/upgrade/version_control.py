# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import List, Optional


class VersionControl:
    LINTERS_TO_SKIP: List[str] = []

    @staticmethod
    def get_changed_files() -> Optional[List[str]]:
        return None

    @staticmethod
    def commit_message(title: str, summary_override: Optional[str] = None) -> str:
        return ""

    @staticmethod
    def add_paths(paths: List[Path]) -> None:
        pass

    @staticmethod
    def submit_changes(
        submit: bool, message: str, ignore_failures: bool = False
    ) -> None:
        pass
