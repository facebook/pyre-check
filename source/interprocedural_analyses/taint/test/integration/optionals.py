# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import Optional


class Client:
    def offer(self, message):
        _test_sink(message)


class ClientSingleton:
    def get_instance(self) -> Optional[Client]:
        return Client()


client: ClientSingleton = ClientSingleton()


def test():
    client.get_instance().offer(_test_source())
