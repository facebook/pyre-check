# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path

import testslide

from ..server_event import (
    ServerException,
    ServerInitialized,
    SocketCreated,
    create_from_string,
)


class ServerEventTest(testslide.TestCase):
    def test_create(self) -> None:
        self.assertIsNone(create_from_string("derp"))
        self.assertIsNone(create_from_string("[]"))
        self.assertEqual(
            create_from_string('["SocketCreated", "/foo/bar"]'),
            SocketCreated(Path("/foo/bar")),
        )
        self.assertIsNone(create_from_string('["SocketCreated"]'))
        self.assertEqual(
            create_from_string('["ServerInitialized"]'), ServerInitialized()
        )
        self.assertEqual(
            create_from_string('["Exception", "Burn baby burn!"]'),
            ServerException("Burn baby burn!"),
        )
        self.assertIsNone(create_from_string('["Exception"]'))
        self.assertIsNone(create_from_string('["UNRECOGNIZABLE", "message"]'))
