# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

import pyre
from django.http import HttpRequest


# Integration test illustrating that we check top-level functions.

request: HttpRequest = ...
eval(request.GET["bad"])
