# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import textwrap
import unittest
from typing import IO, Set
from unittest.mock import mock_open, patch

from ..generator_specs import DecoratorAnnotationSpec
from ..get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)


class AnnotatedFreeFunctionWithDecoratorGeneratorTest(unittest.TestCase):
    def assert_expected_annotations(
        self, source: str, spec: DecoratorAnnotationSpec, expected: Set[str]
    ) -> None:
        cleaned_source = textwrap.dedent(source)
        with patch("builtins.open", mock_open(read_data=cleaned_source)):
            generator = AnnotatedFreeFunctionWithDecoratorGenerator()
            self.assertSetEqual(
                set(generator._annotate_fns(spec, "/root", "/root/module.py")),
                set(expected),
            )

    def test_globals(self) -> None:

        # Test arg annotations only
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            DecoratorAnnotationSpec(decorator="target_decorator", arg_annotation="Arg"),
            {"def module.decorated(arg1: Arg, arg2: Arg, *v, **kw): ..."},
        )

        # Test vararg annotations only
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            DecoratorAnnotationSpec(
                decorator="target_decorator", vararg_annotation="Vararg"
            ),
            {"def module.decorated(arg1, arg2, *v: Vararg, **kw): ..."},
        )

        # Test kwarg annotations only
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            DecoratorAnnotationSpec(
                decorator="target_decorator", kwarg_annotation="Kwarg"
            ),
            {"def module.decorated(arg1, arg2, *v, **kw: Kwarg): ..."},
        )

        # Test return annotations only
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            DecoratorAnnotationSpec(
                decorator="target_decorator", return_annotation="Return"
            ),
            {"def module.decorated(arg1, arg2, *v, **kw) -> Return: ..."},
        )

        # Test async functions
        self.assert_expected_annotations(
            """
            @target_decorator
            async def decorated_async(arg1: str, arg2, *v, **kw):
                pass
            """,
            DecoratorAnnotationSpec(
                decorator="target_decorator",
                arg_annotation="Arg",
                vararg_annotation="Vararg",
                kwarg_annotation="Kwarg",
                return_annotation="Return",
            ),
            {
                "def module.decorated_async(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ..."
            },
        )

        # Test other decorators present
        self.assert_expected_annotations(
            """
            @random_decorator1
            @target_decorator
            @random_decorator2
            def decorated_multi(arg1: str, arg2, *v, **kw):
                pass
            """,
            DecoratorAnnotationSpec(
                decorator="target_decorator",
                arg_annotation="Arg",
                vararg_annotation="Vararg",
                kwarg_annotation="Kwarg",
                return_annotation="Return",
            ),
            {
                "def module.decorated_multi(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ..."
            },
        )

        # Test functions that shouldn't trigger
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
            DecoratorAnnotationSpec(
                decorator="target_decorator",
                arg_annotation="Arg",
                vararg_annotation="Vararg",
                kwarg_annotation="Kwarg",
                return_annotation="Return",
            ),
            set(),
        )

        # Test callable decorators
        self.assert_expected_annotations(
            """
            @target_decorator()
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            DecoratorAnnotationSpec(
                decorator="target_decorator",
                arg_annotation="Arg",
                vararg_annotation="Vararg",
                kwarg_annotation="Kwarg",
            ),
            {
                "def module.decorated(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg): ..."
            },
        )

        # Test everything at once
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
            DecoratorAnnotationSpec(
                decorator="target_decorator",
                arg_annotation="Arg",
                vararg_annotation="Vararg",
                kwarg_annotation="Kwarg",
                return_annotation="Return",
            ),
            {
                "def module.decorated(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ...",
                "def module.decorated_async(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ...",
                "def module.decorated_multi(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ...",
            },
        )
