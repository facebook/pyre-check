# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import Union

from pyre_extensions import override
from tools.pyre.client import daemon_socket

from ...client import configuration as configuration_module, identifiers

from ...client.commands import frontend_configuration, initialization

from ...client.tests import setup

from ..daemon_launcher import (
    _start_server,
    PyreServerStarterBase,
    StartedServerInfo,
    StartFailure,
)


class FakeFrontendConfiguration(frontend_configuration.OpenSource):
    def __init__(self) -> None:
        self.configuration = configuration_module.Configuration(
            "test", Path("test"), "test", targets=[]
        )

    def get_project_identifier(self) -> str:
        return "test"


class AlreadyStartedServerStarter(PyreServerStarterBase):
    @override
    async def server_already_exists(
        self,
        socket_path: Path,
    ) -> bool:
        return True

    @override
    async def run(
        self,
        binary_location: str,
        configuration: frontend_configuration.Base,
        flavor: identifiers.PyreFlavor,
    ) -> Union[
        initialization.StartSuccess,
        initialization.BuckStartFailure,
        initialization.OtherStartFailure,
    ]:
        raise NotImplementedError()


class NotStartedServerStarter(PyreServerStarterBase):
    @override
    async def server_already_exists(
        self,
        socket_path: Path,
    ) -> bool:
        return False


class SuccessfulPyreServerStarter(NotStartedServerStarter):
    @override
    async def run(
        self,
        binary_location: str,
        configuration: frontend_configuration.Base,
        flavor: identifiers.PyreFlavor,
    ) -> Union[
        initialization.StartSuccess,
        initialization.BuckStartFailure,
        initialization.OtherStartFailure,
    ]:
        return initialization.StartSuccess()


class FailedPyreServerStarter(NotStartedServerStarter):
    @override
    async def run(
        self,
        binary_location: str,
        configuration: frontend_configuration.Base,
        flavor: identifiers.PyreFlavor,
    ) -> Union[
        initialization.StartSuccess,
        initialization.BuckStartFailure,
        initialization.OtherStartFailure,
    ]:
        return initialization.BuckStartFailure("message")


class DaemonLauncherTest(unittest.TestCase):
    @setup.async_test
    async def test_successful_start(self) -> None:
        configuration = FakeFrontendConfiguration()
        result = await _start_server(configuration, SuccessfulPyreServerStarter())
        assert isinstance(result, StartedServerInfo)
        self.assertEqual(
            result,
            StartedServerInfo(
                daemon_socket.get_socket_path(
                    configuration.get_project_identifier(),
                    flavor=identifiers.PyreFlavor.CODE_NAVIGATION,
                ),
            ),
        )

    @setup.async_test
    async def test_failed_start(self) -> None:
        result = await _start_server(
            FakeFrontendConfiguration(), FailedPyreServerStarter()
        )
        self.assertEqual(result, StartFailure("message"))

    @setup.async_test
    async def test_already_started(self) -> None:
        configuration = FakeFrontendConfiguration()
        result = await _start_server(configuration, AlreadyStartedServerStarter())
        self.assertEqual(
            result,
            StartedServerInfo(
                daemon_socket.get_socket_path(
                    configuration.get_project_identifier(),
                    flavor=identifiers.PyreFlavor.CODE_NAVIGATION,
                ),
            ),
        )
