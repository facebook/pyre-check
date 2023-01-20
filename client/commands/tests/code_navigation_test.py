# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import testslide

from ...language_server import connections, protocol as lsp
from ...tests import setup
from .. import persistent, pyre_server_options, server_state as state, subscription

from ..code_navigation import PyreCodeNavigationDaemonLaunchAndSubscribeHandler
from ..tests import server_setup


class PyreCodeNavigationDaemonLaunchAndSubscribeHandlerTest(testslide.TestCase):
    @setup.async_test
    async def test_subscription_protocol(self) -> None:
        server_state = state.ServerState(
            client_capabilities=lsp.ClientCapabilities(
                window=lsp.WindowClientCapabilities(
                    status=lsp.ShowStatusRequestClientCapabilities(),
                ),
            ),
            server_options=server_setup.mock_initial_server_options,
        )
        bytes_writer = connections.MemoryBytesWriter()

        def fake_server_options_reader() -> pyre_server_options.PyreServerOptions:
            # Server start option is not relevant to this test
            raise NotImplementedError()

        client_output_channel = connections.AsyncTextWriter(bytes_writer)

        server_handler = PyreCodeNavigationDaemonLaunchAndSubscribeHandler(
            server_options_reader=fake_server_options_reader,
            server_state=server_state,
            client_status_message_handler=persistent.ClientStatusMessageHandler(
                client_output_channel, server_state
            ),
            client_type_error_handler=persistent.ClientTypeErrorHandler(
                client_output_channel, server_state
            ),
            querier=server_setup.MockDaemonQuerier(),
        )
        await server_handler.handle_status_update_subscription(
            subscription.StatusUpdate(kind="BusyChecking")
        )
        await server_handler.handle_status_update_subscription(
            subscription.StatusUpdate(kind="Idle")
        )

        client_messages = [x.decode("utf-8") for x in bytes_writer.items()]
        # We don't forward code-nav updates to the window.
        self.assertTrue(len(client_messages) == 0)
