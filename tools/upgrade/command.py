# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
import argparse

from .repository import Repository


class Command:
    def __init__(self, repository: Repository) -> None:
        self._repository: Repository = repository

    def run(self, arguments: argparse.Namespace) -> None:
        pass
