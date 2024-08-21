# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides logic for specifying platform-dependent
configuration logic, which allows Pyre to handle a project differently
depending on the operating system.
"""

import dataclasses
import platform
from typing import Dict, Generic, Optional, TypeVar

from .. import dataclasses_merge
from .exceptions import InvalidConfiguration

T = TypeVar("T")
U = TypeVar("U")

PLATFORM_MAPPING = {
    "Darwin": "macos",
    "Windows": "windows",
    "Linux": "linux",
    "default": "default",
}


@dataclasses_merge.dataclass_merge
@dataclasses.dataclass(frozen=True)
class PlatformAware(Generic[T]):
    default: Optional[T] = None
    windows: Optional[T] = None
    macos: Optional[T] = None
    linux: Optional[T] = None

    @staticmethod
    def from_json(value: object, field_name: str) -> "Optional[PlatformAware[T]]":
        if value is None:
            return None
        elif isinstance(value, dict):
            if len(value) == 0:
                return None

            invalid_keys = value.keys() - PLATFORM_MAPPING.values()
            if not len(invalid_keys) == 0:
                raise InvalidConfiguration(
                    f"Configuration `{field_name}` only supports platforms: "
                    f"{PLATFORM_MAPPING.values()} but got: `{invalid_keys}`."
                )

            default = value.get("default", None)
            return PlatformAware(
                default=default,
                windows=value["windows"] if "windows" in value else None,
                macos=value["macos"] if "macos" in value else None,
                linux=value["linux"] if "linux" in value else None,
            )
        else:
            # pyre-ignore[7]: we can't verify the type of value
            return PlatformAware(default=value)

    @staticmethod
    def merge_optional(
        base: "Optional[PlatformAware[U]]", override: "Optional[PlatformAware[U]]"
    ) -> "Optional[PlatformAware[U]]":
        if base is None:
            return override
        elif override is None:
            return base
        else:
            # pyre-ignore[16]: Pyre does not understand `dataclass_merge`
            return PlatformAware.merge(base, override)

    def get(self, key: Optional[str] = None) -> Optional[T]:
        if key is None:
            key = PLATFORM_MAPPING[platform.system()]
        value: T = self.__getattribute__(key)
        return value if value is not None else self.default

    def to_json(self) -> Dict[str, T]:
        result: Dict[str, T] = {}
        if self.default is not None:
            result["default"] = self.default
        if self.windows is not None:
            result["windows"] = self.windows
        if self.linux is not None:
            result["linux"] = self.linux
        if self.macos is not None:
            result["macos"] = self.macos
        return result
