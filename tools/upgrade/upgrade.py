# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
from typing import List, Optional

from typing_extensions import Final

from . import upgrade_core


class VersionControl:
    LINTERS_TO_SKIP: Final[List[str]] = []

    @staticmethod
    def commit_message(directory: str, summary_override: Optional[str] = None) -> str:
        return ""

    @staticmethod
    def submit_changes(submit: bool, message: str, ignore_failures: bool) -> None:
        pass


def main() -> None:
    version_control = VersionControl()
    upgrade_core.run(version_control)


if __name__ == "__main__":
    main()
