# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses

from . import exceptions


@dataclasses.dataclass(frozen=True)
class Element:
    suffix: str
    include_suffix_in_module_qualifier: bool = False

    def command_line_argument(self) -> str:
        options = ""
        if self.include_suffix_in_module_qualifier:
            options = "$" + "include_suffix_in_module_qualifier"
        return self.suffix + options

    @staticmethod
    def from_json(json: object) -> "Element":
        if isinstance(json, str):
            return Element(suffix=json, include_suffix_in_module_qualifier=False)
        elif isinstance(json, dict):
            suffix = json.get("suffix", None)
            include_suffix_in_module_qualifier = json.get(
                "include_suffix_in_module_qualifier", None
            )

            if isinstance(suffix, str) and isinstance(
                include_suffix_in_module_qualifier, bool
            ):
                return Element(
                    suffix=suffix,
                    include_suffix_in_module_qualifier=include_suffix_in_module_qualifier,
                )
        raise exceptions.InvalidConfiguration(f"Invalid extension element: {json}")
