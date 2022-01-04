# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass

from views import operate_on_twos


@dataclass
class UrlPattern:
    path: str
    callback: str


urlpatterns = [UrlPattern(r"^operate_on_twos/(.*)", operate_on_twos)]
