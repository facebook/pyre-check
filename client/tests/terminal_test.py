# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import MagicMock, patch

from .. import terminal


class PyreTest(unittest.TestCase):
    def test_is_capable_terminal(self) -> None:
        with patch("os.isatty", side_effect=lambda x: x), patch(
            "os.getenv", return_value="vim"
        ):
            file = MagicMock()
            file.fileno = lambda: True
            self.assertEqual(terminal.is_capable(file), True)
            file.fileno = lambda: False
            self.assertEqual(terminal.is_capable(file), False)
