# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import Union

from pyre_extensions import override

from ...client import (
    configuration as configuration_module,
    daemon_socket,
    frontend_configuration,
    identifiers,
)
from ...client.commands import initialization

from ...client.tests import setup

from ..daemon_launcher import (
    _start_server,
    PyreServerStarterBase,
    RegistrationFailure,
    RegitrationSuccess,
    StartedServerInfo,
    StartFailure,
)


class FakeFrontendConfiguration(frontend_configuration.OpenSource):
    def __init__(self) -> None:
        self.configuration = configuration_module.Configuration(
            global_root=Path("test"), targets=[]
        )

    def get_project_identifier(self) -> str:
        return "test"


class TestServerStarterBase(PyreServerStarterBase):
    def __init__(self, client_id: str, is_started_initially: bool) -> None:
        self.client_id: str = client_id
        self.is_started: bool = is_started_initially

    @override
    async def register_client(
        self,
        socket_path: Path,
    ) -> Union[RegitrationSuccess, RegistrationFailure]:
        if self.is_started:
            return RegitrationSuccess(client_id=self.client_id)
        else:
            return RegistrationFailure("Stopped server cannot perform registration")

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


class AlreadyStartedServerStarter(TestServerStarterBase):
    def __init__(self, client_id: str) -> None:
        super().__init__(client_id=client_id, is_started_initially=True)


class SuccessfulPyreServerStarter(TestServerStarterBase):
    def __init__(self, client_id: str) -> None:
        super().__init__(client_id=client_id, is_started_initially=False)

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
        self.is_started = True
        return initialization.StartSuccess()


class FailedPyreServerStarter(TestServerStarterBase):
    def __init__(self) -> None:
        super().__init__(client_id="irrelevant", is_started_initially=False)

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


class AlwaysFailsRegistrationPyreServerStarter(PyreServerStarterBase):
    def __init__(self, error_message: str) -> None:
        self.error_message: str = error_message

    @override
    async def register_client(
        self,
        socket_path: Path,
    ) -> Union[RegitrationSuccess, RegistrationFailure]:
        return RegistrationFailure(self.error_message)

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


class DaemonLauncherTest(unittest.TestCase):
    @setup.async_test
    async def test_successful_start(self) -> None:
        configuration = FakeFrontendConfiguration()
        client_id = "test"
        result = await _start_server(
            configuration, SuccessfulPyreServerStarter(client_id)
        )
        assert isinstance(result, StartedServerInfo)
        self.assertEqual(
            result,
            StartedServerInfo(
                daemon_socket.get_socket_path(
                    configuration.get_project_identifier(),
                    flavor=identifiers.PyreFlavor.CODE_NAVIGATION,
                ),
                client_id=client_id,
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
        client_id = "test"
        result = await _start_server(
            configuration, AlreadyStartedServerStarter(client_id)
        )
        self.assertEqual(
            result,
            StartedServerInfo(
                daemon_socket.get_socket_path(
                    configuration.get_project_identifier(),
                    flavor=identifiers.PyreFlavor.CODE_NAVIGATION,
                ),
                client_id=client_id,
            ),
        )

    @setup.async_test
    async def test_failed_registration(self) -> None:
        error_message = "test message"
        result = await _start_server(
            FakeFrontendConfiguration(),
            AlwaysFailsRegistrationPyreServerStarter(error_message),
        )
        self.assertEqual(result, StartFailure(error_message))
