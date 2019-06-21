# pyre-strict

import textwrap
import unittest
from typing import IO, Any, Callable, Dict
from unittest.mock import MagicMock, call, mock_open, patch

from .. import taint_annotator


class TaintAnnotatorTest(unittest.TestCase):
    def test_annotate_function(self) -> None:
        function_dfn = MagicMock()
        function_dfn.args.args = [MagicMock(arg="a1"), MagicMock(arg="a2")]
        function_dfn.args.vararg.arg = "v1"
        function_dfn.args.kwarg.arg = "k1"
        function_name = "function"

        # Test just arg
        model = taint_annotator.Model(arg=": Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1: Annotated, a2: Annotated, *v1, **k1): ...",
        )

        # Test just vararg
        model = taint_annotator.Model(vararg=": Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1, a2, *v1: Annotated, **k1): ...",
        )

        # Test just kwarg
        model = taint_annotator.Model(kwarg=": Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1, a2, *v1, **k1: Annotated): ...",
        )

        # Test just returns
        model = taint_annotator.Model(returns=" -> Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1, a2, *v1, **k1) -> Annotated: ...",
        )

        # Test whitelist
        with patch("ast.dump") as ast_dump:
            ast_dump.side_effect = ["SomeType", "Whitelisted"]
            model = taint_annotator.Model(arg=": Annotated")
            self.assertEqual(
                taint_annotator.annotate_function(
                    function_name, function_dfn, model, ["Whitelisted"]
                ),
                "def function(a1: Annotated, a2, *v1, **k1): ...",
            )
