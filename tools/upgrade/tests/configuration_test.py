# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import subprocess  # noqa
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from ..configuration import Configuration


class ConfigurationTest(unittest.TestCase):

    mock_completed_process = MagicMock()
    mock_completed_process.stdout.decode = MagicMock(return_value="[]")

    @patch("subprocess.call")
    @patch("subprocess.run", return_value=mock_completed_process)
    def test_get_errors(self, run, call) -> None:
        configuration = Configuration(Path("path"), {})
        configuration.get_errors()
        call.assert_not_called()
        assert run.call_count == 1

        call.reset_mock()
        run.reset_mock()

        configuration.targets = ["//target/..."]
        configuration.get_errors()
        assert call.call_count == 1
        assert run.call_count == 1

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
