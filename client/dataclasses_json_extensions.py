# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides several mixin classes that extend
`dataclasses_json.DataClassJsonMixin` with schema caching and specific
configurations for translating between json field names and class field
names.
"""


import functools
from typing import Mapping

import dataclasses_json


class DataclassJsonMixinWithCachedSchema(dataclasses_json.DataClassJsonMixin):
    @classmethod
    @functools.lru_cache(maxsize=64)
    def cached_schema(cls) -> dataclasses_json.api.SchemaType:
        return cls.schema()


class CamlCaseAndExcludeJsonMixin(DataclassJsonMixinWithCachedSchema):
    dataclass_json_config: Mapping[str, object] = dataclasses_json.config(
        letter_case=dataclasses_json.LetterCase.CAMEL,
        undefined=dataclasses_json.Undefined.EXCLUDE,
    )["dataclasses_json"]


class SnakeCaseAndExcludeJsonMixin(DataclassJsonMixinWithCachedSchema):
    dataclass_json_config: Mapping[str, object] = dataclasses_json.config(
        letter_case=dataclasses_json.LetterCase.SNAKE,
        undefined=dataclasses_json.Undefined.EXCLUDE,
    )["dataclasses_json"]
