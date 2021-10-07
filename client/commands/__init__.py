# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import List, Type

from .analyze import Analyze as Analyze
from .command import (  # noqa; noqa; noqa
    ClientException as ClientException,
    Command as Command,
    CommandParser as CommandParser,
    ExitCode as ExitCode,
)
from .reporting import Reporting as Reporting

COMMANDS: List[Type[CommandParser]] = [
    Analyze,
    Reporting,
]
