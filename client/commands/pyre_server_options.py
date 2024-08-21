# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

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
    backend_arguments,
    command_arguments,
    configuration as configuration_module,
    daemon_socket,
    frontend_configuration,
    identifiers,
    log_lsp_event,
)
from ..language_server import features
from . import commands, start

PyreServerOptionsReader = Callable[[], "PyreServerOptions"]
FrontendConfigurationReader = Callable[[], frontend_configuration.Base]
LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class PyreServerOptions:
    server_start_command: frontend_configuration.ServerStartCommand
    project_identifier: str
    start_arguments: start.Arguments
    language_server_features: features.LanguageServerFeatures
    strict_default: bool
    excludes: Sequence[str]
    flavor: identifiers.PyreFlavor
    using_errpy_parser: bool

    def get_socket_path(self) -> Path:
        return daemon_socket.get_socket_path(
            self.project_identifier,
            flavor=self.flavor,
        )

    @staticmethod
    def create_from_start_arguments(
        start_arguments: start.Arguments,
        configuration: frontend_configuration.Base,
        language_server_features: features.LanguageServerFeatures,
        flavor: identifiers.PyreFlavor,
        unsaved_changes_only: bool = False,
    ) -> PyreServerOptions:
        server_start_command = configuration.get_server_start_command(
            download_if_needed=True
        )
        if server_start_command is None:
            raise configuration_module.InvalidConfiguration(
                "Cannot locate a Pyre binary to run."
            )
        if start_arguments.watchman_root is None and not unsaved_changes_only:
            raise commands.ClientException(
                "Cannot locate a `watchman` root. Pyre's server will not function "
                "properly."
            )

        return PyreServerOptions(
            server_start_command=server_start_command,
            project_identifier=configuration.get_project_identifier(),
            start_arguments=start_arguments,
            language_server_features=language_server_features,
            strict_default=configuration.is_strict(),
            excludes=configuration.get_excludes(),
            flavor=flavor,
            using_errpy_parser=configuration.get_use_errpy_parser(),
        )

    @staticmethod
    def create(
        start_command_argument: command_arguments.StartArguments,
        configuration: frontend_configuration.Base,
        language_server_features: features.LanguageServerFeatures,
        unsaved_changes_only: bool = False,
    ) -> PyreServerOptions:
        start_arguments = start.create_server_arguments(
            configuration,
            start_command_argument,
        )

        return PyreServerOptions.create_from_start_arguments(
            start_arguments,
            configuration,
            language_server_features,
            start_command_argument.flavor,
            unsaved_changes_only,
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
        log_lsp_event.log(
            remote_logging=remote_logging,
            event=log_lsp_event.LSPEvent.NOT_CONFIGURED,
            normals={
                "exception": traceback.format_exc(),
            },
        )
        raise
