# pyre-strict

import textwrap
import unittest
from typing import IO, Any, Callable, Dict
from unittest.mock import MagicMock, call, mock_open, patch

from .. import taint_annotator


def test_function(argument: str, *variable: str, **keyword: str) -> None:
    pass


class TaintAnnotatorTest(unittest.TestCase):
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

    def test_assignment_model(self) -> None:
        self.assertEqual(
            taint_annotator.AssignmentModel(annotation="TaintSink[Test]").generate(
                "name"
            ),
            "name: TaintSink[Test] = ...",
        )
