# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module describes all of the flags that control behavior of a Pyre
language server - for example, the location of the backend binary and
which features to enable.
"""


from __future__ import annotations

import dataclasses
import logging
import traceback
from pathlib import Path
from typing import Callable, Optional, Sequence

from .. import (
    command_arguments,
    configuration as configuration_module,
    daemon_socket,
    identifiers,
)
from ..language_server import features

from . import backend_arguments, commands, frontend_configuration, log_lsp_event, start

PyreServerOptionsReader = Callable[[], "PyreServerOptions"]
FrontendConfigurationReader = Callable[[], frontend_configuration.Base]
LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class PyreServerOptions:
    binary: str
    project_identifier: str
    start_arguments: start.Arguments
    language_server_features: features.LanguageServerFeatures
    strict_default: bool
    excludes: Sequence[str]
    flavor: identifiers.PyreFlavor

    def get_socket_path(self) -> Path:
        return daemon_socket.get_socket_path(
            self.project_identifier,
            flavor=self.flavor,
        )

    @staticmethod
    def create(
        start_command_argument: command_arguments.StartArguments,
        configuration: frontend_configuration.Base,
        language_server_features: features.LanguageServerFeatures,
    ) -> PyreServerOptions:
        binary_location = configuration.get_binary_location(download_if_needed=True)
        if binary_location is None:
            raise configuration_module.InvalidConfiguration(
                "Cannot locate a Pyre binary to run."
            )

        start_arguments = start.create_server_arguments(
            configuration,
            start_command_argument,
        )
        if start_arguments.watchman_root is None:
            raise commands.ClientException(
                "Cannot locate a `watchman` root. Pyre's server will not function "
                "properly."
            )

        return PyreServerOptions(
            binary=str(binary_location),
            project_identifier=configuration.get_project_identifier(),
            start_arguments=start_arguments,
            language_server_features=language_server_features,
            strict_default=configuration.is_strict(),
            excludes=configuration.get_excludes(),
            flavor=start_command_argument.flavor,
        )

    @staticmethod
    def create_reader(
        start_command_argument: command_arguments.StartArguments,
        read_frontend_configuration: FrontendConfigurationReader,
        language_server_features: features.LanguageServerFeatures,
    ) -> PyreServerOptionsReader:
        def read() -> PyreServerOptions:
            return PyreServerOptions.create(
                start_command_argument=start_command_argument,
                configuration=read_frontend_configuration(),
                language_server_features=language_server_features,
            )

        return read


def read_server_options(
    server_options_reader: PyreServerOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> "PyreServerOptions":
    try:
        LOG.info("Reading Pyre server configurations...")
        return server_options_reader()
    except Exception:
        log_lsp_event._log_lsp_event(
            remote_logging=remote_logging,
            event=log_lsp_event.LSPEvent.NOT_CONFIGURED,
            normals={
                "exception": traceback.format_exc(),
            },
        )
        raise
