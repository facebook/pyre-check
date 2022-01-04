# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Dict

from django.http import HttpRequest

def csrf(request: HttpRequest) -> Dict[str, str]: ...
