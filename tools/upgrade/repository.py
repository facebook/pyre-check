# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
from pathlib import Path
from typing import List, Optional


class Repository:
    MIGRATION_SUMMARY: str = "Migrating buck integration to use configurations"

    def commit_message(self, title: str, summary_override: Optional[str] = None) -> str:
        return ""

    def add_paths(self, paths: List[Path]) -> None:
        pass

    def remove_paths(self, paths: List[Path]) -> None:
        pass

    def submit_changes(
        self,
        commit: bool,
        submit: bool,
        title: Optional[str] = None,
        summary: Optional[str] = None,
        ignore_failures: bool = False,
        set_dependencies: bool = True,
    ) -> None:
        pass

    def revert_all(self, remove_untracked: bool) -> None:
        pass

    def format(self) -> bool:
        pass

    def force_format(self, paths: List[str]) -> None:
        subprocess.check_call(["pyfmt", *paths])
