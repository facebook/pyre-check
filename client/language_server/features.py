# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains all the different availability states that could apply to all of the language server features.

The actual availability of the each individual feature is handled by the vscode client
and is passed to the pyre client entry point (pyre.py) upon initialization of the persistent client.
"""

from __future__ import annotations

import dataclasses
import enum
from typing import Callable, Dict


class _Availability(enum.Enum):
    ENABLED = "enabled"
    DISABLED = "disabled"

    @staticmethod
    def from_enabled(enabled: bool) -> _Availability:
        return _Availability.ENABLED if enabled else _Availability.DISABLED

    def is_enabled(self) -> bool:
        return self == _Availability.ENABLED

    def is_disabled(self) -> bool:
        return self == _Availability.DISABLED


class TypeCoverageAvailability(enum.Enum):
    DISABLED = "disabled"
    FUNCTION_LEVEL = "function_level"
    EXPRESSION_LEVEL = "expression_level"


# User-facing features
StatusUpdatesAvailability = _Availability
TypeErrorsAvailability = _Availability
UnsavedChangesAvailability = _Availability

# Telemetry: is the editor able to forward events somewhere?
TelemetryAvailability = _Availability


@dataclasses.dataclass(frozen=True)
class LanguageServerFeatures:
    status_updates: StatusUpdatesAvailability = StatusUpdatesAvailability.ENABLED
    type_coverage: TypeCoverageAvailability = TypeCoverageAvailability.DISABLED
    type_errors: TypeErrorsAvailability = TypeErrorsAvailability.ENABLED
    unsaved_changes: UnsavedChangesAvailability = UnsavedChangesAvailability.DISABLED
    telemetry: TelemetryAvailability = TelemetryAvailability.DISABLED
