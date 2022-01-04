# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import ast
import textwrap
import unittest
from typing import Union
from unittest.mock import patch

from .. import model
from ..generator_specifications import (
    AllParametersAnnotation,
    AnnotationSpecification,
    ParameterAnnotation,
    WhitelistSpecification,
)


def test_function(argument: str, *variable: str, **keyword: str) -> None:
    pass


class ModelTest(unittest.TestCase):
    def test_callable_model(self) -> None:
        name = f"{__name__}.test_function"
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function,
                    parameter_annotation=AllParametersAnnotation(
                        arg="TaintSource[tainted]"
                    ),
                )
            ),
            f"def {name}(argument: TaintSource[tainted], *variable, **keyword): ...",
        )
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function,
                    parameter_annotation=AllParametersAnnotation(
                        vararg="TaintSource[tainted]"
                    ),
                )
            ),
            f"def {name}(argument, *variable: TaintSource[tainted], **keyword): ...",
        )
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function,
                    parameter_annotation=AllParametersAnnotation(
                        kwarg="TaintSource[tainted]"
                    ),
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

        # We handle the combined AnnotationSpecification
        annotations = AnnotationSpecification(
            parameter_annotation=AllParametersAnnotation(
                arg="TaintSource[tainted]",
                vararg="TaintSource[tainted]",
                kwarg="TaintSource[tainted]",
            ),
            returns="TaintSink[returned]",
        )
        self.assertEqual(
            str(
                model.CallableModel(
                    callable_object=test_function, annotations=annotations
                )
            ),
            f"def {name}(argument: TaintSource[tainted],"
            + " *variable: TaintSource[tainted],"
            + " **keyword: TaintSource[tainted]) -> TaintSink[returned]: ...",
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

    def assert_modeled(
        self, source: str, expected: str, **kwargs: Union[str, ParameterAnnotation]
    ) -> None:
        parsed_function = ast.parse(textwrap.dedent(source)).body[0]

        # pyre-fixme[35]: Target cannot be annotated.
        parsed_function: model.FunctionDefinition

        self.assertEqual(
            # pyre-ignore[6]: Expected `typing.Optional[typing.Set[str]]` for
            # 2nd positional only parameter to call
            # `model.FunctionDefinitionModel.__init__` but got `str`.
            str(model.FunctionDefinitionModel(definition=parsed_function, **kwargs)),
            expected,
        )

        # We handle the combined AnnotationSpecification
        annotations = AnnotationSpecification(
            # pyre-ignore[6]: Too dynamic.
            parameter_annotation=kwargs.get("parameter_annotation"),
            # pyre-fixme[6]: Expected `Optional[str]` for 2nd param but got
            #  `Union[None, ParameterAnnotation, str]`.
            returns=kwargs.get("returns"),
        )
        self.assertEqual(
            str(
                model.FunctionDefinitionModel(
                    definition=parsed_function,
                    annotations=annotations,
                    # pyre-ignore[6]: Too dynamic.
                    qualifier=kwargs.get("qualifier"),
                )
            ),
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
            parameter_annotation=AllParametersAnnotation(arg="Arg"),
        )

        self.assert_modeled(
            all_args_source,
            "def test_fn(arg1, arg2, *v: Vararg, **kw): ...",
            parameter_annotation=AllParametersAnnotation(vararg="Vararg"),
        )

        self.assert_modeled(
            all_args_source,
            "def test_fn(arg1, arg2, *v, **kw: Kwarg): ...",
            parameter_annotation=AllParametersAnnotation(kwarg="Kwarg"),
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
            parameter_annotation=AllParametersAnnotation(
                arg="Arg", vararg="Vararg", kwarg="Kwarg"
            ),
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
            parameter_annotation=AllParametersAnnotation(arg="Arg"),
        )

        self.assert_modeled(
            """
            def test_fn(*v):
                pass
            """,
            "def test_fn(*v: Vararg): ...",
            parameter_annotation=AllParametersAnnotation(vararg="Vararg"),
        )

        self.assert_modeled(
            """
            def test_fn(**kw):
                pass
            """,
            "def test_fn(**kw: Kwarg): ...",
            parameter_annotation=AllParametersAnnotation(kwarg="Kwarg"),
        )

        # Check that we handle async functions
        self.assert_modeled(
            """
            async def test_fn(arg1, arg2, *v, **kw):
                pass
            """,
            "def qualifier.test_fn(arg1: Arg, arg2: Arg, *v: Vararg, "
            "**kw: Kwarg) -> Return: ...",
            parameter_annotation=AllParametersAnnotation(
                arg="Arg", vararg="Vararg", kwarg="Kwarg"
            ),
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
            parameter_annotation=AllParametersAnnotation(
                arg="Arg", vararg="Vararg", kwarg="Kwarg"
            ),
            returns="Return",
            qualifier="qualifier",
        )
        self.assert_modeled(
            """
            def test_fn(x, *, keyword_only):
                pass
            """,
            "def qualifier.test_fn(x: Arg, *, keyword_only: Arg) -> Return: ...",
            parameter_annotation=AllParametersAnnotation(
                arg="Arg", vararg="Vararg", kwarg="Kwarg"
            ),
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

    # pyre-fixme[56]: Argument `set()` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
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
                    model.Parameter("self", None, model.Parameter.Kind.ARG),
                    model.Parameter("a", None, model.Parameter.Kind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                arg="TaintSource[UserControlled]"
                            )
                        )
                    ),
                    "def qualified.C.name(self: TaintSource[UserControlled], "
                    "a: TaintSource[UserControlled]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("self", None, model.Parameter.Kind.ARG),
                    model.Parameter("*args", None, model.Parameter.Kind.VARARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                vararg="TaintSource[Var]"
                            )
                        )
                    ),
                    "def qualified.C.name(self, *args: TaintSource[Var]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("self", None, model.Parameter.Kind.ARG),
                    model.Parameter("**kwargs", None, model.Parameter.Kind.KWARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                kwarg="TaintSource[UC]"
                            )
                        )
                    ),
                    "def qualified.C.name(self, **kwargs: TaintSource[UC]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", "int", model.Parameter.Kind.ARG),
                    model.Parameter("b", None, model.Parameter.Kind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                arg="TaintSource[UC]"
                            ),
                            parameter_type_whitelist=["int"],
                        )
                    ),
                    "def qualified.C.name(a, b: TaintSource[UC]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", None, model.Parameter.Kind.ARG),
                    model.Parameter("b", None, model.Parameter.Kind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                arg="TaintSource[UC]"
                            ),
                            parameter_name_whitelist={"b"},
                        )
                    ),
                    "def qualified.C.name(a: TaintSource[UC], b): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", "int", model.Parameter.Kind.ARG),
                    model.Parameter("b", None, model.Parameter.Kind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                arg="TaintSource[UC]"
                            ),
                            whitelist=WhitelistSpecification(parameter_type={"int"}),
                        )
                    ),
                    "def qualified.C.name(a, b: TaintSource[UC]): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", None, model.Parameter.Kind.ARG),
                    model.Parameter("b", None, model.Parameter.Kind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                arg="TaintSource[UC]"
                            ),
                            whitelist=WhitelistSpecification(parameter_name={"b"}),
                        )
                    ),
                    "def qualified.C.name(a: TaintSource[UC], b): ...",
                )

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", None, model.Parameter.Kind.ARG),
                    model.Parameter("b", None, model.Parameter.Kind.ARG),
                ],
            ):
                # pyre-ignore[45]: Cannot instantiate abstract class
                model_1 = model.RawCallableModel(
                    parameter_annotation=AllParametersAnnotation(
                        arg="TaintSource[A]",
                        vararg="TaintSource[A]",
                        kwarg="TaintSource[A]",
                    ),
                    returns="TaintSource[A]",
                )
                # pyre-ignore[45]: Cannot instantiate abstract class
                model_2 = model.RawCallableModel(
                    parameter_annotation=AllParametersAnnotation(
                        arg="TaintSource[B]",
                        vararg="TaintSource[B]",
                        kwarg="TaintSource[B]",
                    ),
                    returns="TaintSource[B]",
                )
                self.assertEqual(model_1, model_2)

                test_set = set()
                test_set.add(model_1)
                # Checking for 'model_2' despite putting in 'model_1' is deliberate; we
                # are testing the effectiveness of the hash equivalence
                self.assertIn(model_2, test_set)

            with patch.object(
                model.RawCallableModel,
                "_generate_parameters",
                return_value=[
                    model.Parameter("a", None, model.Parameter.Kind.ARG),
                    model.Parameter("*", None, model.Parameter.Kind.ARG),
                    model.Parameter("keyword_only", None, model.Parameter.Kind.ARG),
                ],
            ):
                self.assertEqual(
                    str(
                        # pyre-ignore[45]: Cannot instantiate abstract class
                        model.RawCallableModel(
                            parameter_annotation=AllParametersAnnotation(
                                arg="TaintSource[A]"
                            ),
                            returns="TaintSource[A]",
                        )
                    ),
                    "def qualified.C.name(a: TaintSource[A], *, keyword_only: TaintSource[A]) -> TaintSource[A]: ...",
                )

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

    def test_property_model(self) -> None:
        self.assertEqual(
            str(
                model.PropertyModel(
                    class_name="a.C", attribute_name="attr", annotation="TaintSource[A]"
                )
            ),
            "@property\ndef a.C.attr(self) -> TaintSource[A]: ...",
        )

        self.assertEqual(
            model.PropertyModel(
                class_name="a.C", attribute_name="attr", annotation="TaintSource[A]"
            ),
            model.PropertyModel(
                class_name="a.C", attribute_name="attr", annotation="TaintSource[B]"
            ),
        )
        self.assertNotEqual(
            model.PropertyModel(
                class_name="a.C", attribute_name="attr", annotation="TaintSource[A]"
            ),
            model.PropertyModel(
                class_name="a.D", attribute_name="attr", annotation="TaintSource[A]"
            ),
        )
        self.assertNotEqual(
            model.PropertyModel(
                class_name="a.C", attribute_name="attr1", annotation="TaintSource[A]"
            ),
            model.PropertyModel(
                class_name="a.C", attribute_name="attr2", annotation="TaintSource[A]"
            ),
        )
