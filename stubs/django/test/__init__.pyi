# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import contextlib
from unittest import TestCase as TestCase

from django.test.client import Client as Client
from django.test.utils import override_settings as override_settings

class SimpleTestCase(TestCase):
    client: Client
    @contextlib.contextmanager
    def settings(self, **kwargs) -> None: ...
