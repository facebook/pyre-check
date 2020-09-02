# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Sequence

from django.http.response import HttpResponseBase

def patch_vary_headers(
    response: HttpResponseBase, newheaders: Sequence[str]
) -> None: ...
