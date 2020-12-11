# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import textwrap
import unittest
from typing import Set
from unittest.mock import mock_open, patch

from ..get_flask_models import FlaskModelGenerator


class AnnotatedFreeFunctionWithDecoratorGeneratorTest(unittest.TestCase):
    def assert_expected_annotations(
        self,
        source: str,
        expected: Set[str],
    ) -> None:
        cleaned_source = textwrap.dedent(source)
        with patch("builtins.open", mock_open(read_data=cleaned_source)):
            generator = FlaskModelGenerator(root="/root")
            self.assertSetEqual(
                {
                    str(model)
                    for model in generator.generator._annotate_functions(
                        "/root/module.py"
                    )
                },
                set(expected),
            )

    def test_model_generation(self) -> None:
        # test app route
        self.assert_expected_annotations(
            """
            @app.route("/", methods=["GET", "PUT"])
            def decorated(arg1: str, arg2: int):
                pass
            """,
            {
                "def module.decorated("
                + "arg1: TaintSource[UserControlled, UserControlled_Parameter], "
                + "arg2: TaintSource[UserControlled, UserControlled_Parameter]"
                + ") -> TaintSink[ReturnedToUser]: ..."
            },
        )

        # test app error handler
        self.assert_expected_annotations(
            """
            @app.errorhandler(404)
            def decorated(arg1: str, arg2: int):
                pass
            """,
            {"def module.decorated(arg1, arg2) -> TaintSink[ReturnedToUser]: ..."},
        )

        # don't match
        self.assert_expected_annotations(
            """
            @app("/")
            @route("/")
            def decorated(arg1: str, arg2: int):
                pass
            """,
            set(),
        )
