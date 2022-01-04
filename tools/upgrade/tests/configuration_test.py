# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ..configuration import Configuration


class ConfigurationTest(unittest.TestCase):

    mock_completed_process = MagicMock(stdout="[]")

    @patch.object(subprocess, "call")
    # pyre-fixme[56]: Pyre was not able to infer the type of argument `subprocess`
    #  to decorator factory `unittest.mock.patch.object`.
    @patch.object(subprocess, "run", return_value=mock_completed_process)
    def test_get_errors__no_targets(
        self, run: MagicMock, buck_clean: MagicMock
    ) -> None:
        configuration = Configuration(Path("path"), {})
        configuration.get_errors()
        buck_clean.assert_not_called()
        run.assert_called_once()

    @patch.object(subprocess, "call")
    # pyre-fixme[56]: Pyre was not able to infer the type of argument `subprocess`
    #  to decorator factory `unittest.mock.patch.object`.
    @patch.object(subprocess, "run", return_value=mock_completed_process)
    def test_get_errors__targets(self, run: MagicMock, buck_clean: MagicMock) -> None:
        configuration = Configuration(Path("path"), {})
        configuration.targets = ["//target/..."]
        configuration.get_errors()
        buck_clean.assert_called_once()
        run.assert_called_once()

    def test_get_contents__preserve_explicit_false_options(self) -> None:
        configuration = Configuration(
            Path("path"), json_contents={"strict": False, "use_buck_builder": False}
        )
        self.assertEqual(
            configuration.get_contents(), {"strict": False, "use_buck_builder": False}
        )

    def test_get_contents__preserve_untracked_option(self) -> None:
        configuration = Configuration(Path("path"), json_contents={"foo": True})
        self.assertEqual(configuration.get_contents(), {"foo": True})

    def test_get_contents__include_new_attribute(self) -> None:
        configuration = Configuration(Path("path"), json_contents={"strict": False})
        configuration.version = "1234"
        self.assertEqual(
            configuration.get_contents(), {"strict": False, "version": "1234"}
        )

    def test_get_contents__update_existing_attribute(self) -> None:
        configuration = Configuration(Path("path"), json_contents={"strict": False})
        configuration.strict = True
        self.assertEqual(configuration.get_contents(), {"strict": True})
