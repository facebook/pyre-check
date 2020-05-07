# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import textwrap
import unittest
from typing import List, Set
from unittest.mock import mock_open, patch

from ..generator_specifications import DecoratorAnnotationSpecification
from ..get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)


class AnnotatedFreeFunctionWithDecoratorGeneratorTest(unittest.TestCase):
    def assert_expected_annotations(
        self,
        source: str,
        annotation_specifications: List[DecoratorAnnotationSpecification],
        expected: Set[str],
    ) -> None:
        cleaned_source = textwrap.dedent(source)
        with patch("builtins.open", mock_open(read_data=cleaned_source)):
            generator = AnnotatedFreeFunctionWithDecoratorGenerator(
                root="/root", annotation_specifications=annotation_specifications
            )
            self.assertSetEqual(
                {
                    str(model)
                    for model in generator._annotate_functions("/root/module.py")
                },
                set(expected),
            )

    def test_model_generation(self) -> None:

        # Test argument annotations only.
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator", arg_annotation="Arg"
                )
            ],
            {"def module.decorated(arg1: Arg, arg2: Arg, *v, **kw): ..."},
        )

        # Test argument annotations with unnamed attributed_decorator.
        self.assert_expected_annotations(
            """
            @target_decorator("some_attribute")
            def decorated_unnamed_attributes(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator='@target_decorator("some_attribute")',
                    arg_annotation="Arg",
                )
            ],
            {
                "def module.decorated_unnamed_attributes(arg1: Arg, arg2: Arg, "
                "*v, **kw): ..."
            },
        )

        # Test argument annotations with named attributed_decorator.
        self.assert_expected_annotations(
            """
            @target_decorator(key="value")
            def decorated_named_attributes(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator='@target_decorator(key="value")', arg_annotation="Arg"
                )
            ],
            {
                "def module.decorated_named_attributes(arg1: Arg, arg2: Arg, *v, "
                "**kw): ..."
            },
        )

        # Test argument annotations with multiple filter criteria.
        self.assert_expected_annotations(
            """
            @target_decorator("some_attribute", "another_attribute", key="value", \
            key2="another_value")
            def decorated_multiple_filter_attributes(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator=(
                        '@target_decorator("some_attribute", "another_attribute", '
                        'key2="another_value")'
                    ),
                    arg_annotation="Arg",
                )
            ],
            {
                "def module.decorated_multiple_filter_attributes(arg1: Arg, "
                "arg2: Arg, *v, **kw): ..."
            },
        )

        # Test argument annotations with attributes not found.
        self.assert_expected_annotations(
            """
            @target_decorator("some_attribute", key="value")
            def decorated_attributes_not_found(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator('some_attribute_not_found')",
                    arg_annotation="Arg",
                )
            ],
            set(),
        )

        # Test vararg annotations only.
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator", vararg_annotation="Vararg"
                )
            ],
            {"def module.decorated(arg1, arg2, *v: Vararg, **kw): ..."},
        )

        # Test kwarg annotations only.
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator", kwarg_annotation="Kwarg"
                )
            ],
            {"def module.decorated(arg1, arg2, *v, **kw: Kwarg): ..."},
        )

        # Test return annotations only.
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator", return_annotation="Return"
                )
            ],
            {"def module.decorated(arg1, arg2, *v, **kw) -> Return: ..."},
        )

        # Test async functions.
        self.assert_expected_annotations(
            """
            @target_decorator
            async def decorated_async(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    arg_annotation="Arg",
                    vararg_annotation="Vararg",
                    kwarg_annotation="Kwarg",
                    return_annotation="Return",
                )
            ],
            {
                "def module.decorated_async(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ..."
            },
        )

        # Test with other decorators present.
        self.assert_expected_annotations(
            """
            @random_decorator1
            @target_decorator
            @random_decorator2
            def decorated_multi(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    arg_annotation="Arg",
                    vararg_annotation="Vararg",
                    kwarg_annotation="Kwarg",
                    return_annotation="Return",
                )
            ],
            {
                "def module.decorated_multi(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ..."
            },
        )

        # Test functions that shouldn't trigger.
        self.assert_expected_annotations(
            """
            def undecorated():
                pass
            @random_decorator
            @namespace.target_decorator
            @namespace.target_decorator()
            def decorated_with_random():
                pass
            class C:
                @target_decorator
                def my_fn():
                    pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    arg_annotation="Arg",
                    vararg_annotation="Vararg",
                    kwarg_annotation="Kwarg",
                    return_annotation="Return",
                )
            ],
            set(),
        )

        # Test argument with target decorator attributes.
        self.assert_expected_annotations(
            """
            @target_decorator
            def target_decorator_attributes(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator('some_attribute')",
                    arg_annotation="Arg",
                )
            ],
            set(),
        )

        # Test callable decorators.
        self.assert_expected_annotations(
            """
            @target_decorator()
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    arg_annotation="Arg",
                    vararg_annotation="Vararg",
                    kwarg_annotation="Kwarg",
                )
            ],
            {
                "def module.decorated(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg): ..."
            },
        )

        # Test everything at once.
        self.assert_expected_annotations(
            """
            def undecorated():
                pass
            @random_decorator
            def decorated_with_random():
                pass
            class C:
                @target_decorator
                def my_fn():
                    pass
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            @target_decorator
            async def decorated_async(arg1: str, arg2, *v, **kw):
                pass
            @random_decorator1
            @target_decorator
            @random_decorator2
            def decorated_multi(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    arg_annotation="Arg",
                    vararg_annotation="Vararg",
                    kwarg_annotation="Kwarg",
                    return_annotation="Return",
                )
            ],
            {
                "def module.decorated(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ...",
                "def module.decorated_async(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ...",
                "def module.decorated_multi(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ...",
            },
        )

        # Test more than one specification.
        self.assert_expected_annotations(
            """
            def undecorated():
                pass
            @target_decorator1
            def decorated1(arg: str, *v, **kw):
                pass
            @target_decorator2
            def decorated2(arg: str, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator1",
                    arg_annotation="Arg1",
                    vararg_annotation="Vararg1",
                    kwarg_annotation="Kwarg1",
                    return_annotation="Return1",
                ),
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator2",
                    arg_annotation="Arg2",
                    vararg_annotation="Vararg2",
                    kwarg_annotation="Kwarg2",
                    return_annotation="Return2",
                ),
            ],
            {
                "def module.decorated1(arg: Arg1, *v: Vararg1, "
                "**kw: Kwarg1) -> Return1: ...",
                "def module.decorated2(arg: Arg2, *v: Vararg2, "
                "**kw: Kwarg2) -> Return2: ...",
            },
        )
