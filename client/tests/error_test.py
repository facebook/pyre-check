# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import patch

from .. import __name__ as client
from ..error import Error


class ErrorTest(unittest.TestCase):
    fake_error = {
        "line": 4,
        "column": 11,
        "path": "c.py",
        "code": -1,
        "name": "Revealed type",
        "description": "Fake error",
        "inference": {},
        "define": "c.$toplevel",
    }

    def test_repr(self) -> None:
        error = Error(**self.fake_error)

        with patch("{}.error.is_capable_terminal".format(client), return_value=True):
            self.assertEqual(
                repr(error),
                "\x1b[31mc.py\x1b[0m:\x1b[33m4\x1b[0m:\x1b[33m11\x1b[0m Fake error",
            )

        with patch("{}.error.is_capable_terminal".format(client), return_value=False):
            self.assertEqual(repr(error), "c.py:4:11 Fake error")

    def test_key_with_color(self) -> None:
        error = Error(**self.fake_error)

        self.assertEqual(
            error._key_with_color(),
            "\x1b[31mc.py\x1b[0m:\x1b[33m4\x1b[0m:\x1b[33m11\x1b[0m",
        )

    def test_long_description(self) -> None:
        error = Error(**self.fake_error)
        self.assertEqual(error.long_description, "")

        error_with_long_description = self.fake_error
        error_with_long_description[
            "long_description"
        ] = "Fake error.\nAnd this is why this is an error."
        error = Error(**error_with_long_description)
        self.assertEqual(
            error.long_description, "Fake error.\nAnd this is why this is an error."
        )
