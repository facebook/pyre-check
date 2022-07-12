# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
from typing import ClassVar, Optional

import dataclasses_json

from .. import dataclasses_json_extensions as json_mixins, dataclasses_merge
from . import exceptions


@dataclasses_merge.dataclass_merge
@dataclasses.dataclass(frozen=True)
class IdeFeatures(json_mixins.SnakeCaseAndExcludeJsonMixin):
    hover_enabled: Optional[bool] = None
    DEFAULT_HOVER_ENABLED: ClassVar[bool] = False
    go_to_definition_enabled: Optional[bool] = None
    DEFAULT_GO_TO_DEFINITION_ENABLED: ClassVar[bool] = False
    find_symbols_enabled: Optional[bool] = None
    DEFAULT_FIND_SYMBOLS_ENABLED: ClassVar[bool] = False
    find_all_references_enabled: Optional[bool] = None
    DEFAULT_FIND_ALL_REFERENCES_ENABLED: ClassVar[bool] = False
    expression_level_coverage_enabled: Optional[bool] = None
    DEFAULT_EXPRESSION_LEVEL_COVERAGE_ENABLED: ClassVar[bool] = False
    consume_unsaved_changes_enabled: Optional[bool] = None
    DEFAULT_CONSUME_UNSAVED_CHANGES_ENABLED: ClassVar[bool] = False

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
            # pyre-ignore[6, 7]: Imprecise typing of `load()`
            return IdeFeatures.cached_schema().load(input)
        except (
            TypeError,
            KeyError,
            ValueError,
            dataclasses_json.mm.ValidationError,
        ) as error:
            raise exceptions.InvalidConfiguration(
                f"Invalid JSON for `IdeFeatures`: {str(error)}"
            )

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

    def is_find_all_references_enabled(self) -> bool:
        return (
            self.find_all_references_enabled
            if self.find_all_references_enabled is not None
            else self.DEFAULT_FIND_ALL_REFERENCES_ENABLED
        )

    def is_expression_level_coverage_enabled(self) -> bool:
        return (
            self.expression_level_coverage_enabled
            if self.expression_level_coverage_enabled is not None
            else self.DEFAULT_EXPRESSION_LEVEL_COVERAGE_ENABLED
        )

    def is_consume_unsaved_changes_enabled(self) -> bool:
        return (
            self.consume_unsaved_changes_enabled
            if self.consume_unsaved_changes_enabled is not None
            else self.DEFAULT_CONSUME_UNSAVED_CHANGES_ENABLED
        )
