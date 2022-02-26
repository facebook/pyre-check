# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os  # noqa
import re
import textwrap
import unittest
from typing import List, Set
from unittest.mock import MagicMock, mock_open, patch

from .. import annotated_function_generator
from ..generator_specifications import (
    AllParametersAnnotation,
    AnnotationSpecification,
    DecoratorAnnotationSpecification,
    WhitelistSpecification,
)
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
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg")
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg")
                    ),
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
                    decorator='@target_decorator(key="value")',
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg")
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg")
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg")
                    ),
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
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(vararg="Vararg")
                    ),
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
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(kwarg="Kwarg")
                    ),
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
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(returns="Return"),
                )
            ],
            {"def module.decorated(arg1, arg2, *v, **kw) -> Return: ..."},
        )

        # Test parameter type whitelist
        self.assert_expected_annotations(
            """
            @target_decorator
            async def decorated_async(arg1: str, arg2: int, arg3: bool, arg4):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg"),
                        returns="Return",
                    ),
                    whitelist=WhitelistSpecification(parameter_type={"str", "int"}),
                )
            ],
            {
                "def module.decorated_async(arg1, arg2, arg3: Arg, arg4: Arg"
                ") -> Return: ..."
            },
        )

        # Test parameter name whitelist
        self.assert_expected_annotations(
            """
            @target_decorator
            async def decorated_async(arg1: str, arg2: int, arg3: bool, arg4):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg"),
                        returns="Return",
                    ),
                    whitelist=WhitelistSpecification(parameter_name={"arg1", "arg4"}),
                )
            ],
            {
                "def module.decorated_async(arg1, arg2: Arg, arg3: Arg, arg4"
                ") -> Return: ..."
            },
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg", vararg="Vararg", kwarg="Kwarg"
                        ),
                        returns="Return",
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg", vararg="Vararg", kwarg="Kwarg"
                        ),
                        returns="Return",
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg", vararg="Vararg", kwarg="Kwarg"
                        ),
                        returns="Return",
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg")
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg", vararg="Vararg", kwarg="Kwarg"
                        )
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg", vararg="Vararg", kwarg="Kwarg"
                        ),
                        returns="Return",
                    ),
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
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg1", vararg="Vararg1", kwarg="Kwarg1"
                        ),
                        returns="Return1",
                    ),
                ),
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator2",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg2", vararg="Vararg2", kwarg="Kwarg2"
                        ),
                        returns="Return2",
                    ),
                ),
            ],
            {
                "def module.decorated1(arg: Arg1, *v: Vararg1, "
                "**kw: Kwarg1) -> Return1: ...",
                "def module.decorated2(arg: Arg2, *v: Vararg2, "
                "**kw: Kwarg2) -> Return2: ...",
            },
        )

        # Test unified annotations.
        self.assert_expected_annotations(
            """
            @target_decorator
            def decorated(arg1: str, arg2, *v, **kw):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="Arg", vararg="Vararg", kwarg="Kwarg"
                        ),
                        returns="Return",
                    ),
                )
            ],
            {
                "def module.decorated(arg1: Arg, arg2: Arg, *v: Vararg, "
                "**kw: Kwarg) -> Return: ..."
            },
        )

        # Test unified whitelist
        self.assert_expected_annotations(
            """
            @target_decorator
            async def decorated_async(arg1: str, arg2: int, arg3: bool, arg4):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target_decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg"),
                        returns="Return",
                    ),
                    whitelist=WhitelistSpecification(
                        parameter_type={"str", "int"}, parameter_name={"arg1", "arg4"}
                    ),
                )
            ],
            {
                "def module.decorated_async(arg1, arg2, arg3: Arg, arg4"
                ") -> Return: ..."
            },
        )

        # Test function attribute decorator
        self.assert_expected_annotations(
            """
            @target.decorator
            def decorated(arg1: str, arg2: int):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target.decorator",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg"),
                        returns="Return",
                    ),
                )
            ],
            {"def module.decorated(arg1: Arg, arg2: Arg) -> Return: ..."},
        )

        # Test function attribute decorator with args
        self.assert_expected_annotations(
            """
            @target.decorator(darg, foo, kwdarg=val, blarg=val)
            def decorated(arg1: str, arg2: int):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target.decorator(darg, kwdarg=val)",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg"),
                        returns="Return",
                    ),
                )
            ],
            {"def module.decorated(arg1: Arg, arg2: Arg) -> Return: ..."},
        )

        # negative match for attribute decorator
        self.assert_expected_annotations(
            """
            @target.shmecorator(darg, foo, kwdarg=val, blarg=val)
            def decorated(arg1: str, arg2: int):
                pass

            @shmarget.decorator(darg, foo, kwdarg=val, blarg=val)
            def decorated(arg1: str, arg2: int):
                pass
            """,
            [
                DecoratorAnnotationSpecification(
                    decorator="@target.decorator(darg, kwdarg=val)",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(arg="Arg"),
                        returns="Return",
                    ),
                )
            ],
            set(),
        )

    # pyre-ignore[56]: Pyre was not able to infer the type of argument
    # `get_annotated_free_functions_with_decorator`
    @patch.object(
        annotated_function_generator,
        "find_all_paths",
        return_value=["/root/one.py", "/root/two.py", "/root/three.py"],
    )
    def test_path_selection(self, find_paths_mock: MagicMock) -> None:
        unused_specification = [DecoratorAnnotationSpecification(decorator="unused")]

        # Using user-provided paths
        self.assertListEqual(
            AnnotatedFreeFunctionWithDecoratorGenerator(
                root="/root",
                annotation_specifications=unused_specification,
                paths=["/custom/one.py", "/custom/two.py"],
            ).paths,
            ["/custom/one.py", "/custom/two.py"],
        )
        find_paths_mock.assert_not_called()

        # Extracting paths from root
        self.assertListEqual(
            AnnotatedFreeFunctionWithDecoratorGenerator(
                root="/root", annotation_specifications=unused_specification
            ).paths,
            ["/root/one.py", "/root/two.py", "/root/three.py"],
        )
        find_paths_mock.assert_called_with("/root")

        # Filtering paths with excluded
        self.assertListEqual(
            AnnotatedFreeFunctionWithDecoratorGenerator(
                root="/root",
                annotation_specifications=unused_specification,
                exclude_paths=[re.compile(r"one\.py"), re.compile(r"two\.py")],
            ).paths,
            ["/root/three.py"],
        )
