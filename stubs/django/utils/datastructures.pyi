# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import typing

from collections.abc import Mapping

class MultiValueDict(typing.Dict[typing.Any, typing.Any]):
    def copy(self) -> MultiValueDict: ...

class CaseInsensitiveMapping(Mapping):
    def copy(self) -> CaseInsensitiveMapping: ...
