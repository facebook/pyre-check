# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import textwrap
import unittest

from .. import model


def test_function(argument: str, *variable: str, **keyword: str) -> None:
    pass


class ModelTest(unittest.TestCase):
    def test_callable_model(self) -> None:
        name = f"{__name__}.test_function"
        self.assertEqual(
            model.CallableModel(
                callable=test_function, arg="TaintSource[tainted]"
            ).generate(),
            f"def {name}(argument: TaintSource[tainted], *variable, **keyword): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function,
                arg="TaintSource[tainted]",
                whitelisted_parameters=["str"],
            ).generate(),
            f"def {name}(argument, *variable, **keyword): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function, vararg="TaintSource[tainted]"
            ).generate(),
            f"def {name}(argument, *variable: TaintSource[tainted], **keyword): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function, kwarg="TaintSource[tainted]"
            ).generate(),
            f"def {name}(argument, *variable, **keyword: TaintSource[tainted]): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function, returns="TaintSink[returned]"
            ).generate(),
            f"def {name}(argument, *variable, **keyword) -> TaintSink[returned]: ...",
        )

        # We don't generate models for local functions.
        def local_function(x: int, *args: str) -> None:
            ...

        self.assertEqual(
            model.CallableModel(
                callable=local_function, returns="TaintSink[returned]"
            ).generate(),
            None,
        )

        # Ensure that we don't choke on malformed types of functions.
        class CallMe:
            def __call__(self) -> None:
                pass

        self.assertEqual(model.CallableModel(callable=CallMe).generate(), None)

    def assert_modeled(self, source: str, expected: str, **kwargs: str) -> None:
        parsed_function = ast.parse(textwrap.dedent(source)).body[0]

        parsed_function: model.FunctionDefinition

        self.assertEqual(
            model.FunctionDefinitionModel(
                definition=parsed_function, **kwargs
            ).generate(),
            expected,
        )

    def test_function_definition_model(self) -> None:
        all_args_source = """
            def test_fn(arg1, arg2, *v, **kw):
                pass
        """

        # Check that annotations work one at a time
        self.assert_modeled(
            all_args_source,
            "def test_fn(arg1: Arg, arg2: Arg, *v, **kw): ...",
            arg="Arg",
        )

        self.assert_modeled(
            all_args_source,
            "def test_fn(arg1, arg2, *v: Vararg, **kw): ...",
            vararg="Vararg",
        )

        self.assert_modeled(
            all_args_source,
            "def test_fn(arg1, arg2, *v, **kw: Kwarg): ...",
            kwarg="Kwarg",
        )

        self.assert_modeled(
            all_args_source,
            "def test_fn(arg1, arg2, *v, **kw) -> Return: ...",
            returns="Return",
        )

        self.assert_modeled(
            all_args_source,
            "def qualifier.test_fn(arg1, arg2, *v, **kw): ...",
            qualifier="qualifier",
        )

        # Check that all the bells and whistles work
        self.assert_modeled(
            all_args_source,
            "def qualifier.test_fn(arg1: Arg, arg2: Arg, *v: Vararg, "
            "**kw: Kwarg) -> Return: ...",
            arg="Arg",
            vararg="Vararg",
            kwarg="Kwarg",
            returns="Return",
            qualifier="qualifier",
        )

        # Check that we handle functions without all arg types
        self.assert_modeled(
            """
            def test_fn(arg1: str, arg2):
                pass
            """,
            "def test_fn(arg1: Arg, arg2: Arg): ...",
            arg="Arg",
        )

        self.assert_modeled(
            """
            def test_fn(*v):
                pass
            """,
            "def test_fn(*v: Vararg): ...",
            vararg="Vararg",
        )

        self.assert_modeled(
            """
            def test_fn(**kw):
                pass
            """,
            "def test_fn(**kw: Kwarg): ...",
            kwarg="Kwarg",
        )

        # Check that we handle async functions
        self.assert_modeled(
            """
            async def test_fn(arg1, arg2, *v, **kw):
                pass
            """,
            "def qualifier.test_fn(arg1: Arg, arg2: Arg, *v: Vararg, "
            "**kw: Kwarg) -> Return: ...",
            arg="Arg",
            vararg="Vararg",
            kwarg="Kwarg",
            returns="Return",
            qualifier="qualifier",
        )

        # Check that we gracefully handle unused annotation parameters
        self.assert_modeled(
            """
            def test_fn():
                pass
            """,
            "def qualifier.test_fn() -> Return: ...",
            arg="Arg",
            vararg="Vararg",
            kwarg="Kwarg",
            returns="Return",
            qualifier="qualifier",
        )

    def test_assignment_model(self) -> None:
        self.assertEqual(
            model.AssignmentModel(
                annotation="TaintSink[Test]", target="name"
            ).generate(),
            "name: TaintSink[Test] = ...",
        )
