# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Mapping

import dataclasses_json


class CamlCaseAndExcludeJsonMixin(dataclasses_json.DataClassJsonMixin):
    dataclass_json_config: Mapping[str, object] = dataclasses_json.config(
        # pyre-ignore[6]: Incorrect typing in upstream `dataclasses_json`
        letter_case=dataclasses_json.LetterCase.CAMEL,
        undefined=dataclasses_json.Undefined.EXCLUDE,
    )["dataclasses_json"]


class SnakeCaseAndExcludeJsonMixin(dataclasses_json.DataClassJsonMixin):
    dataclass_json_config: Mapping[str, object] = dataclasses_json.config(
        # pyre-ignore[6]: Incorrect typing in upstream `dataclasses_json`
        letter_case=dataclasses_json.LetterCase.SNAKE,
        undefined=dataclasses_json.Undefined.EXCLUDE,
    )["dataclasses_json"]
