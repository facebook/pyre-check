# pyre-strict

import textwrap
import unittest
from typing import IO, Any, Callable, Dict
from unittest.mock import MagicMock, call, mock_open, patch

from .. import taint_annotator


def test_function(argument: str, *variable: str, **keyword: str) -> None:
    pass


class TaintAnnotatorTest(unittest.TestCase):
    def test_annotate_function(self) -> None:
        function_dfn = MagicMock()
        function_dfn.args.args = [MagicMock(arg="a1"), MagicMock(arg="a2")]
        function_dfn.args.vararg.arg = "v1"
        function_dfn.args.kwarg.arg = "k1"
        function_name = "function"

        # Test just arg
        model = taint_annotator.Model(arg="Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1: Annotated, a2: Annotated, *v1, **k1): ...",
        )

        # Test just vararg
        model = taint_annotator.Model(vararg="Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1, a2, *v1: Annotated, **k1): ...",
        )

        # Test just kwarg
        model = taint_annotator.Model(kwarg="Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1, a2, *v1, **k1: Annotated): ...",
        )

        # Test just returns
        model = taint_annotator.Model(returns="Annotated")
        self.assertEqual(
            taint_annotator.annotate_function(function_name, function_dfn, model),
            "def function(a1, a2, *v1, **k1) -> Annotated: ...",
        )

        # Test whitelist
        with patch("ast.dump") as ast_dump:
            ast_dump.side_effect = ["SomeType", "Whitelisted"]
            model = taint_annotator.Model(arg="Annotated")
            self.assertEqual(
                taint_annotator.annotate_function(
                    function_name, function_dfn, model, ["Whitelisted"]
                ),
                "def function(a1: Annotated, a2, *v1, **k1): ...",
            )

    def test_generate_model(self) -> None:

        name = f"{__name__}.test_function"
        self.assertEqual(
            taint_annotator.Model(arg="TaintSource[tainted]").generate(test_function),
            f"def {name}(argument: TaintSource[tainted], *variable, **keyword): ...",
        )
        self.assertEqual(
            taint_annotator.Model(arg="TaintSource[tainted]").generate(
                test_function, ["str"]
            ),
            f"def {name}(argument, *variable, **keyword): ...",
        )
        self.assertEqual(
            taint_annotator.Model(vararg="TaintSource[tainted]").generate(
                test_function
            ),
            f"def {name}(argument, *variable: TaintSource[tainted], **keyword): ...",
        )
        self.assertEqual(
            taint_annotator.Model(kwarg="TaintSource[tainted]").generate(test_function),
            f"def {name}(argument, *variable, **keyword: TaintSource[tainted]): ...",
        )
        self.assertEqual(
            taint_annotator.Model(returns="TaintSink[returned]").generate(
                test_function
            ),
            f"def {name}(argument, *variable, **keyword) -> TaintSink[returned]: ...",
        )

        # We don't generate models for local functions.
        def local_function(x: int, *args: str) -> None:
            ...

        self.assertEqual(
            taint_annotator.Model(returns="TaintSink[returned]").generate(
                local_function
            ),
            None,
        )

        # Ensure that we don't choke on malformed types of functions.
        class CallMe:
            def __call__(self) -> None:
                pass

        self.assertEqual(taint_annotator.Model().generate(CallMe), None)
