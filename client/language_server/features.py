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


class _AvailabilityWithShadow(enum.Enum):
    ENABLED = "enabled"
    DISABLED = "disabled"
    SHADOW = "shadow"

    @staticmethod
    def from_enabled(enabled: bool) -> _AvailabilityWithShadow:
        return (
            _AvailabilityWithShadow.ENABLED
            if enabled
            else _AvailabilityWithShadow.DISABLED
        )

    def is_enabled(self) -> bool:
        return self == _AvailabilityWithShadow.ENABLED

    def is_shadow(self) -> bool:
        return self == _AvailabilityWithShadow.SHADOW

    def is_disabled(self) -> bool:
        return self == _AvailabilityWithShadow.DISABLED


class TypeCoverageAvailability(enum.Enum):
    DISABLED = "disabled"
    FUNCTION_LEVEL = "function_level"
    EXPRESSION_LEVEL = "expression_level"


class CustomAvailability:
    _check: Callable[[], bool]

    def __init__(self, check: Callable[[], bool]) -> None:
        self._check = check

    @staticmethod
    def from_enabled(enabled: bool) -> CustomAvailability:
        return CustomAvailability(check=lambda: enabled)

    def get_availability(self) -> _Availability:
        return _Availability.from_enabled(self._check())

    def is_enabled(self) -> bool:
        return self._check()

    def is_disabled(self) -> bool:
        return not self._check()


# User-facing features
HoverAvailability = _Availability
DefinitionAvailability = _AvailabilityWithShadow
ReferencesAvailability = _Availability
DocumentSymbolsAvailability = _Availability
StatusUpdatesAvailability = _Availability
TypeErrorsAvailability = _Availability
UnsavedChangesAvailability = _Availability
CompletionAvailability = _Availability
CallHierarchyAvailability = _Availability
RenameAvailability = _Availability
SymbolSearchAvailability = _Availability
InlayHintAvailability = _Availability
FormattingAvailability = _Availability
GlobalLazyTypeErrorsAvailability = _Availability
PerTargetTypeErrorsAvailability = _Availability
PythonAutoTargetsAvailability = _Availability
SystemPyAutoTargetsAvailability = CustomAvailability
# TODO: T204372341 remove this once we have rolled out addition and removal features
PythonAutoTargetsRemovalAvailability = CustomAvailability

# Telemetry: is the editor able to forward events somewhere?
TelemetryAvailability = _Availability


@dataclasses.dataclass(frozen=True)
class LanguageServerFeatures:
    hover: HoverAvailability = HoverAvailability.DISABLED
    definition: DefinitionAvailability = DefinitionAvailability.DISABLED
    document_symbols: DocumentSymbolsAvailability = DocumentSymbolsAvailability.DISABLED
    references: ReferencesAvailability = ReferencesAvailability.DISABLED
    status_updates: StatusUpdatesAvailability = StatusUpdatesAvailability.ENABLED
    type_coverage: TypeCoverageAvailability = TypeCoverageAvailability.DISABLED
    type_errors: TypeErrorsAvailability = TypeErrorsAvailability.ENABLED
    global_lazy_type_errors: GlobalLazyTypeErrorsAvailability = (
        GlobalLazyTypeErrorsAvailability.ENABLED
    )
    per_target_type_errors: PerTargetTypeErrorsAvailability = (
        PerTargetTypeErrorsAvailability.ENABLED
    )
    unsaved_changes: UnsavedChangesAvailability = UnsavedChangesAvailability.DISABLED
    telemetry: TelemetryAvailability = TelemetryAvailability.DISABLED
    completion: CompletionAvailability = CompletionAvailability.DISABLED
    call_hierarchy: CallHierarchyAvailability = CallHierarchyAvailability.DISABLED
    rename: RenameAvailability = RenameAvailability.DISABLED
    symbol: SymbolSearchAvailability = SymbolSearchAvailability.DISABLED
    inlay_hint: InlayHintAvailability = InlayHintAvailability.DISABLED
    formatting: FormattingAvailability = FormattingAvailability.DISABLED
    python_auto_targets: PythonAutoTargetsAvailability = (
        PythonAutoTargetsAvailability.ENABLED
    )
    use_system_pyautotargets: SystemPyAutoTargetsAvailability = (
        SystemPyAutoTargetsAvailability.from_enabled(False)
    )
    # TODO: T204372341 remove this once we have rolled out addition and removal features
    python_auto_targets_removal: PythonAutoTargetsRemovalAvailability = (
        PythonAutoTargetsRemovalAvailability.from_enabled(False)
    )

    def capabilities(self) -> Dict[str, bool]:
        return {
            "hover_provider": not self.hover.is_disabled(),
            "definition_provider": not self.definition.is_disabled(),
            "document_symbol_provider": not self.document_symbols.is_disabled(),
            "references_provider": not self.references.is_disabled(),
            "completion_provider": not self.completion.is_disabled(),
            "call_hierarchy_provider": not self.call_hierarchy.is_disabled(),
            "rename_provider": not self.rename.is_disabled(),
            "workspace_symbol_provider": not self.symbol.is_disabled(),
            "inlay_hint_provider": not self.inlay_hint.is_disabled(),
            "document_formatting_provider": not self.formatting.is_disabled(),
        }
