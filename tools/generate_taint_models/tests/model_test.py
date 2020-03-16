# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import textwrap
import unittest
from unittest.mock import patch

from .. import model


def test_function(argument: str, *variable: str, **keyword: str) -> None:
    pass


class ModelTest(unittest.TestCase):
    def test_callable_model(self) -> None:
        name = f"{__name__}.test_function"
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function, arg="TaintSource[tainted]"
                )
            ),
            f"def {name}(argument: TaintSource[tainted], *variable, **keyword): ...",
        )
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function, vararg="TaintSource[tainted]"
                )
            ),
            f"def {name}(argument, *variable: TaintSource[tainted], **keyword): ...",
        )
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function, kwarg="TaintSource[tainted]"
                )
            ),
            f"def {name}(argument, *variable, **keyword: TaintSource[tainted]): ...",
        )
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function, returns="TaintSink[returned]"
                )
            ),
            f"def {name}(argument, *variable, **keyword) -> TaintSink[returned]: ...",
        )

        # We don't generate models for local functions.
        def local_function(x: int, *args: str) -> None:
            ...

        with self.assertRaises(ValueError):
            model.CallableModel(
                callable_object=local_function, returns="TaintSink[returned]"
            )

        # Ensure that we don't choke on malformed types of functions.
        class CallMe:
            def __call__(self) -> None:
                pass

        with self.assertRaises(ValueError):
            model.CallableModel(callable_object=CallMe)

    def assert_modeled(self, source: str, expected: str, **kwargs: str) -> None:
        parsed_function = ast.parse(textwrap.dedent(source)).body[0]

        parsed_function: model.FunctionDefinition

        self.assertEqual(
            # pyre-ignore[6]: Expected `typing.Optional[typing.Set[str]]` for
            # 2nd positional only parameter to call
            # `model.FunctionDefinitionModel.__init__` but got `str`.
            str(model.FunctionDefinitionModel(definition=parsed_function, **kwargs)),
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
        model_1 = model.AssignmentModel(
            annotation="TaintSink[A]", target="fully.qualified.name"
        )
        model_2 = model.AssignmentModel(
            annotation="TaintSink[B]", target="fully.qualified.name"
        )

        self.assertEqual(str(model_1), "fully.qualified.name: TaintSink[A] = ...")

        self.assertEqual(model_1, model_2)

        test_set = set()
        test_set.add(model_1)
        # Checking for 'model_2' despite putting in 'model_1' is deliberate; we
        # are testing the effectiveness of the hash equivalence
        self.assertIn(model_2, test_set)

        with self.assertRaises(ValueError):
            model.AssignmentModel(
                annotation="TaintSink[Test]", target="do-not-generate"
            )

    @patch.object(model.RawCallableModel, "__abstractmethods__", set())
    def test_raw_callable_model(self) -> None:

        with patch.object(
            model.RawCallableModel,
            "_get_fully_qualified_callable_name",
            return_value="qualified.C.name",
        ):
            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("self", None, model.ArgumentKind.ARG),
                    model.Parameter("a", None, model.ArgumentKind.ARG),
                ],
            ):
                self.assertEqual(
                    # pyre-ignore[45]: Cannot instantiate abstract class
                    str(model.RawCallableModel(arg="TaintSource[UserControlled]")),
                    "def qualified.C.name(self: TaintSource[UserControlled], "
                    "a: TaintSource[UserControlled]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("self", None, model.ArgumentKind.ARG),
                    model.Parameter("*args", None, model.ArgumentKind.VARARG),
                ],
            ):
                self.assertEqual(
                    # pyre-ignore[45]: Cannot instantiate abstract class
                    str(model.RawCallableModel(vararg="TaintSource[Var]")),
                    "def qualified.C.name(self, *args: TaintSource[Var]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("self", None, model.ArgumentKind.ARG),
                    model.Parameter("**kwargs", None, model.ArgumentKind.KWARG),
                ],
            ):
                self.assertEqual(
                    # pyre-ignore[45]: Cannot instantiate abstract class
                    str(model.RawCallableModel(kwarg="TaintSource[UC]")),
                    "def qualified.C.name(self, **kwargs: TaintSource[UC]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", "int", model.ArgumentKind.ARG),
                    model.Parameter("b", None, model.ArgumentKind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            arg="TaintSource[UC]", parameter_type_whitelist=["int"]
                        )
                    ),
                    "def qualified.C.name(a, b: TaintSource[UC]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", None, model.ArgumentKind.ARG),
                    model.Parameter("b", None, model.ArgumentKind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            arg="TaintSource[UC]", parameter_name_whitelist={"b"}
                        )
                    ),
                    "def qualified.C.name(a: TaintSource[UC], b): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", None, model.ArgumentKind.ARG),
                    model.Parameter("b", None, model.ArgumentKind.ARG),
                ],
            ):
                # pyre-ignore[45]: Cannot instantiate abstract class
                model_1 = model.RawCallableModel(
                    arg="TaintSource[A]",
                    vararg="TaintSource[A]",
                    kwarg="TaintSource[A]",
                    returns="TaintSource[A]",
                )
                # pyre-ignore[45]: Cannot instantiate abstract class
                model_2 = model.RawCallableModel(
                    arg="TaintSource[B]",
                    vararg="TaintSource[B]",
                    kwarg="TaintSource[B]",
                    returns="TaintSource[B]",
                )
                self.assertEqual(model_1, model_2)

                test_set = set()
                test_set.add(model_1)
                # Checking for 'model_2' despite putting in 'model_1' is deliberate; we
                # are testing the effectiveness of the hash equivalence
                self.assertIn(model_2, test_set)

        with self.assertRaises(ValueError), patch.object(
            model.RawCallableModel,
            "_get_fully_qualified_callable_name",
            return_value="my-qualifier.C.name",
        ):
            # pyre-ignore[45]: Cannot instantiate abstract class
            model.RawCallableModel()

    def test_class_model(self) -> None:
        model_1 = model.ClassModel(
            class_name="qualified.C.name", annotation="TaintSource[A]"
        )
        model_2 = model.ClassModel(
            class_name="qualified.C.name", annotation="TaintSource[B]"
        )
        self.assertEqual(str(model_1), "class qualified.C.name(TaintSource[A]): ...")

        self.assertEqual(model_1, model_2)

        test_set = set()
        test_set.add(model_1)
        # Checking for 'model_2' despite putting in 'model_1' is deliberate; we
        # are testing the effectiveness of the hash equivalence
        self.assertIn(model_2, test_set)
