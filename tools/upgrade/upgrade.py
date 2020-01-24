# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
from typing import List, Optional

from . import upgrade_core


class ExternalVersionControl(upgrade_core.VersionControl):
    pass


def main() -> None:
    version_control = ExternalVersionControl()
    upgrade_core.run(version_control)


if __name__ == "__main__":
    main()
