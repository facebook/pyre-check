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
