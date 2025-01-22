# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from pathlib import Path

import testslide

from ... import (
    backend_arguments,
    configuration as configuration_module,
    frontend_configuration,
    identifiers,
)
from ...language_server import features
from .. import pyre_server_options, start


class FakeFrontendConfiguration(frontend_configuration.OpenSource):
    def __init__(self) -> None:
        self.configuration = configuration_module.Configuration(
            global_root=Path("test"),
            targets=[],
            relative_local_root="local",
        )

    def get_server_start_command(
        self, download_if_needed: bool
    ) -> frontend_configuration.ServerStartCommand:
        return frontend_configuration.DefaultServerStartCommand("/fake/binary")


class ServerOptionsTest(testslide.TestCase):
    def test_create(self) -> None:
        language_server_features = features.LanguageServerFeatures()
        start_arguments = start.Arguments(
            backend_arguments.BaseArguments(
                source_paths=backend_arguments.SimpleSourcePath(),
                log_path="/log/path",
                global_root="/global/root",
            ),
            socket_path=Path("irrelevant_socket_path.sock"),
        )
        configuration = FakeFrontendConfiguration()
        result = pyre_server_options.PyreServerOptions.create_from_start_arguments(
            start_arguments,
            configuration,
            language_server_features,
            identifiers.PyreFlavor.CLASSIC,
            True,
        )

        expected = pyre_server_options.PyreServerOptions(
            server_start_command=frontend_configuration.DefaultServerStartCommand(
                "/fake/binary"
            ),
            project_identifier="test//local",
            start_arguments=start_arguments,
            language_server_features=language_server_features,
            strict_default=False,
            excludes=[],
            flavor=identifiers.PyreFlavor.CLASSIC,
        )

        self.assertEqual(result, expected)
