# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
from typing import ClassVar, Dict, Optional


@dataclasses.dataclass(frozen=True)
class IdeFeatures:
    hover_enabled: Optional[bool] = None
    DEFAULT_HOVER_ENABLED: ClassVar[bool] = False
    go_to_definition_enabled: Optional[bool] = None
    DEFAULT_GO_TO_DEFINITION_ENABLED: ClassVar[bool] = False

    @staticmethod
    def merge(base: "IdeFeatures", override: "IdeFeatures") -> "IdeFeatures":
        override_hover_enabled = override.hover_enabled
        override_go_to_definition_enabled = override.go_to_definition_enabled
        return IdeFeatures(
            hover_enabled=override_hover_enabled
            if override_hover_enabled is not None
            else base.hover_enabled,
            go_to_definition_enabled=override_go_to_definition_enabled
            if override_go_to_definition_enabled is not None
            else base.go_to_definition_enabled,
        )

    @staticmethod
    def merge_optional(
        base: "Optional[IdeFeatures]", override: "Optional[IdeFeatures]"
    ) -> "Optional[IdeFeatures]":
        if override is None:
            return base
        if base is None:
            return override
        return IdeFeatures.merge(base, override)

    def to_json(self) -> Dict[str, int]:
        return {
            **(
                {"hover_enabled": self.hover_enabled}
                if self.hover_enabled is not None
                else {}
            ),
            **(
                {"go_to_definition_enabled": self.go_to_definition_enabled}
                if self.go_to_definition_enabled is not None
                else {}
            ),
        }

    def is_hover_enabled(self) -> bool:
        return (
            self.hover_enabled
            if self.hover_enabled is not None
            else self.DEFAULT_HOVER_ENABLED
        )

    def is_go_to_definition_enabled(self) -> bool:
        return (
            self.go_to_definition_enabled
            if self.go_to_definition_enabled is not None
            else self.DEFAULT_GO_TO_DEFINITION_ENABLED
        )
