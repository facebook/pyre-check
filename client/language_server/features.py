# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains all the different availability states that could apply to all of the language server features.

The actual availability of the each individual feature is handled by the vscode client
and is passed to the pyre client entry point (pyre.py) upon initialization of the persistent client.
"""


from __future__ import annotations

import dataclasses
import enum

from typing import Dict


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
    unsaved_changes: UnsavedChangesAvailability = UnsavedChangesAvailability.DISABLED
    telemetry: TelemetryAvailability = TelemetryAvailability.DISABLED
    completion: CompletionAvailability = CompletionAvailability.DISABLED
    call_hierarchy: CallHierarchyAvailability = CallHierarchyAvailability.DISABLED
    rename: RenameAvailability = RenameAvailability.DISABLED

    def capabilities(self) -> Dict[str, bool]:
        return {
            "hover_provider": not self.hover.is_disabled(),
            "definition_provider": not self.definition.is_disabled(),
            "document_symbol_provider": not self.document_symbols.is_disabled(),
            "references_provider": not self.references.is_disabled(),
            "completion_provider": not self.completion.is_disabled(),
            "call_hierarchy_provider": not self.call_hierarchy.is_disabled(),
            "rename_provider": not self.rename.is_disabled(),
        }
