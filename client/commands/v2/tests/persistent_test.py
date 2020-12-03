# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import testslide

from .... import json_rpc
from ....tests import setup
from .. import language_server_protocol as lsp
from ..async_server_connection import (
    TextReader,
    TextWriter,
    MemoryBytesReader,
    MemoryBytesWriter,
    create_memory_text_writer,
)
from ..persistent import (
    try_initialize,
    InitializationSuccess,
    InitializationFailure,
    InitializationExit,
)


async def _create_input_channel_with_request(request: json_rpc.Request) -> TextReader:
    bytes_writer = MemoryBytesWriter()
    await lsp.write_json_rpc(TextWriter(bytes_writer), request)
    return TextReader(MemoryBytesReader(bytes_writer.items()[0]))


class PersistentTest(testslide.TestCase):
    @setup.async_test
    async def test_try_initialize_success(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(
                id=0,
                method="initialize",
                parameters=json_rpc.ByNameParameters(
                    {
                        "processId": 42,
                        "rootUri": None,
                        "capabilities": {
                            "textDocument": {
                                "publishDiagnostics": {},
                                "synchronization": {
                                    "didSave": True,
                                },
                            },
                        },
                    }
                ),
            )
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationSuccess)

    @setup.async_test
    async def test_try_initialize_failure__not_a_request(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(method="derp", parameters=None)
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_failure__invalid_parameters(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(id=0, method="initialize", parameters=None)
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationFailure)

    @setup.async_test
    async def test_try_initialize_exit(self) -> None:
        input_channel = await _create_input_channel_with_request(
            json_rpc.Request(method="exit", parameters=None)
        )
        output_channel = create_memory_text_writer()
        result = await try_initialize(input_channel, output_channel)
        self.assertIsInstance(result, InitializationExit)
