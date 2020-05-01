# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
import argparse

from .version_control import VersionControl


class Command:
    def run(
        self, arguments: argparse.Namespace, version_control: VersionControl
    ) -> None:
        pass
