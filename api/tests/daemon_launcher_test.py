# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from pathlib import Path
from typing import Union

from pyre_extensions import override

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


class SuccessfulPyreServerStarter(PyreServerStarterBase):
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


class FailedPyreServerStarter(PyreServerStarterBase):
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
        result = await _start_server(
            FakeFrontendConfiguration(), SuccessfulPyreServerStarter()
        )
        self.assertEqual(result, StartedServerInfo())

    @setup.async_test
    async def test_failed_start(self) -> None:
        result = await _start_server(
            FakeFrontendConfiguration(), FailedPyreServerStarter()
        )
        self.assertEqual(result, StartFailure("message"))
