# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import TextIO

class BaseCommand:
    stdout: TextIO = ...

class CommandError(Exception): ...
