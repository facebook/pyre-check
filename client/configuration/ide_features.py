# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
from typing import ClassVar, Dict, Optional

import dataclasses_json

from .. import dataclasses_merge
from . import exceptions


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.SNAKE,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses_merge.dataclass_merge
@dataclasses.dataclass(frozen=True)
class IdeFeatures:
    hover_enabled: Optional[bool] = None
    DEFAULT_HOVER_ENABLED: ClassVar[bool] = False
    go_to_definition_enabled: Optional[bool] = None
    DEFAULT_GO_TO_DEFINITION_ENABLED: ClassVar[bool] = False
    find_symbols_enabled: Optional[bool] = None
    DEFAULT_FIND_SYMBOLS_ENABLED: ClassVar[bool] = False

    @staticmethod
    def merge_optional(
        base: "Optional[IdeFeatures]", override: "Optional[IdeFeatures]"
    ) -> "Optional[IdeFeatures]":
        if override is None:
            return base
        if base is None:
            return override
        # pyre-ignore[16]: Pyre does not understand `dataclass_merge`
        return IdeFeatures.merge(base, override)

    @staticmethod
    def create_from_json(input: object) -> "IdeFeatures":
        try:
            # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
            return IdeFeatures.schema().load(input)
        except (
            TypeError,
            KeyError,
            ValueError,
            dataclasses_json.mm.ValidationError,
        ) as error:
            raise exceptions.InvalidConfiguration(
                f"Invalid JSON for `IdeFeatures`: {str(error)}"
            )

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
            **(
                {"find_symbols_enabled": self.find_symbols_enabled}
                if self.find_symbols_enabled is not None
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

    def is_find_symbols_enabled(self) -> bool:
        return (
            self.find_symbols_enabled
            if self.find_symbols_enabled is not None
            else self.DEFAULT_FIND_SYMBOLS_ENABLED
        )
