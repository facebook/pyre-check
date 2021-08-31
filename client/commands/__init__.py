# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import List, Type

from .analyze import Analyze as Analyze
from .check import Check as Check
from .command import (  # noqa; noqa; noqa
    ClientException as ClientException,
    Command as Command,
    CommandParser as CommandParser,
    ExitCode as ExitCode,
    ProfileOutput as ProfileOutput,
    IncrementalStyle as IncrementalStyle,
)
from .coverage import Coverage
from .deobfuscate import Deobfuscate as Deobfuscate
from .incremental import Incremental as Incremental
from .kill import Kill as Kill
from .persistent import Persistent as Persistent
from .profile import Profile as Profile
from .query import Query as Query
from .rage import Rage as Rage
from .reporting import Reporting as Reporting
from .restart import Restart as Restart
from .servers import Servers as Servers
from .start import Start
from .statistics import Statistics as Statistics
from .stop import Stop as Stop
from .validate_models import ValidateModels as ValidateModels

COMMANDS: List[Type[CommandParser]] = [
    Analyze,
    Check,
    Coverage,
    Deobfuscate,
    Incremental,
    Kill,
    Persistent,
    Profile,
    Query,
    Rage,
    Reporting,
    Restart,
    Servers,
    Start,
    Statistics,
    Stop,
    ValidateModels,
]
