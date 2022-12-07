# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import textwrap
import unittest
from typing import Dict, List, Optional

import libcst as cst
from libcst.metadata import CodePosition, CodeRange, MetadataWrapper

from ..libcst_collectors import (
    AnnotationCollector,
    AnnotationCountCollector,
    collect_coverage_for_module,
    FixmeCountCollector,
    FunctionAnnotationKind,
    IgnoreCountCollector,
    ModuleMode,
    StrictCountCollector,
)


def parse_source(source: str) -> cst.Module:
    return cst.parse_module(textwrap.dedent(source.rstrip()))


class AnnotationCollectorTest(unittest.TestCase):
    def _build_and_visit_annotation_collector(self, source: str) -> AnnotationCollector:
        source_module = MetadataWrapper(parse_source(source))
        collector = AnnotationCollector()
        source_module.visit(collector)
        return collector

    def test_return_code_range(self) -> None:
        collector = self._build_and_visit_annotation_collector(
            """
            def foobar():
                pass
            """
        )
        returns = list(collector.returns())
        self.assertEqual(len(returns), 1)
        self.assertEqual(
            returns[0].code_range,
            CodeRange(CodePosition(2, 4), CodePosition(2, 10)),
        )

    def test_line_count(self) -> None:
        source_module = MetadataWrapper(cst.parse_module("# No trailing newline"))
        collector = AnnotationCollector()
        source_module.visit(collector)
        self.assertEqual(collector.line_count, 1)
        source_module = MetadataWrapper(cst.parse_module("# With trailing newline\n"))
        collector = AnnotationCollector()
        source_module.visit(collector)
        self.assertEqual(collector.line_count, 2)


class AnnotationCountCollectorTest(unittest.TestCase):
    def assert_counts(self, source: str, expected: Dict[str, int]) -> None:
        collector = AnnotationCountCollector()
        source_module = MetadataWrapper(parse_source(source))
        source_module.visit(collector)
        result = collector.build_result()
        self.assertDictEqual(
            expected, AnnotationCountCollector.get_result_counts(result)
        )

    def test_count_annotations(self) -> None:
        self.assert_counts(
            """
            def foo(x) -> int:
                pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 0,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )

        self.assert_counts(
            """
            def bar(x: int, y):
                pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )

        self.assert_counts(
            """
            a = foo()
            b: int = bar()
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 2,
                "annotated_parameter_count": 0,
                "return_count": 0,
                "globals_count": 2,
                "parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )

        self.assert_counts(
            """
            class A:
                a: int = 100
                b = ""
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 0,
                "return_count": 0,
                "globals_count": 0,
                "parameter_count": 0,
                "attribute_count": 2,
                "annotated_attribute_count": 2,
                "function_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
                "line_count": 4,
            },
        )

        # For now, don't count annotations inside of functions
        self.assert_counts(
            """
            def foo():
                a: int = 100
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 0,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )

        self.assert_counts(
            """
            def foo():
                def bar(x: int) -> int:
                    pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 2,
                "globals_count": 0,
                "parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 2,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 1,
                "line_count": 4,
            },
        )

        self.assert_counts(
            """
            class A:
                def bar(self, x: int):
                    pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 4,
            },
        )

        self.assert_counts(
            """
            class A:
                def bar(this, x: int) -> None:
                    pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 1,
                "line_count": 4,
            },
        )

        self.assert_counts(
            """
            class A:
                @classmethod
                def bar(cls, x: int):
                    pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 5,
            },
        )

        self.assert_counts(
            """
            def bar(self, x: int):
                pass
            """,
            {
                "annotated_return_count": 0,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )

        self.assert_counts(
            """
            class A:
                @staticmethod
                def bar(self, x: int) -> None:
                    pass
            """,
            {
                "annotated_return_count": 1,
                "annotated_globals_count": 0,
                "annotated_parameter_count": 1,
                "return_count": 1,
                "globals_count": 0,
                "parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 5,
            },
        )

        self.assert_counts(
            """
            def foo(x: str) -> str:
                return x
            """,
            {
                "return_count": 1,
                "annotated_return_count": 1,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 1,
                "annotated_parameter_count": 1,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 1,
                "line_count": 3,
            },
        )
        self.assert_counts(
            """
            class Test:
                def foo(self, input: str) -> None:
                    class Foo:
                        pass

                    pass

                def bar(self, input: str) -> None:
                    pass
            """,
            {
                "return_count": 2,
                "annotated_return_count": 2,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 2,
                "annotated_parameter_count": 2,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 2,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 2,
                "line_count": 10,
            },
        )
        # Ensure globals and attributes with literal values are considered annotated.
        self.assert_counts(
            """
            x: int = 1
            y = 2
            z = foo

            class Foo:
                x = 1
                y = foo
            """,
            {
                "return_count": 0,
                "annotated_return_count": 0,
                "globals_count": 3,
                "annotated_globals_count": 3,
                "parameter_count": 0,
                "annotated_parameter_count": 0,
                "attribute_count": 2,
                "annotated_attribute_count": 2,
                "function_count": 0,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
                "line_count": 8,
            },
        )

    def test_count_annotations__partially_annotated_methods(self) -> None:
        self.assert_counts(
            """
            class A:
                def bar(self): ...
            """,
            {
                "return_count": 1,
                "annotated_return_count": 0,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 0,
                "annotated_parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )
        self.assert_counts(
            """
            class A:
                def bar(self) -> None: ...
            """,
            {
                "return_count": 1,
                "annotated_return_count": 1,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 0,
                "annotated_parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 1,
                "line_count": 3,
            },
        )
        self.assert_counts(
            """
            class A:
                def baz(self, x): ...
            """,
            {
                "return_count": 1,
                "annotated_return_count": 0,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 1,
                "annotated_parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 0,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )
        self.assert_counts(
            """
            class A:
                def baz(self, x) -> None: ...
            """,
            {
                "return_count": 1,
                "annotated_return_count": 1,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 1,
                "annotated_parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )
        self.assert_counts(
            """
            class A:
                def baz(self: Foo): ...
            """,
            {
                "return_count": 1,
                "annotated_return_count": 0,
                "globals_count": 0,
                "annotated_globals_count": 0,
                "parameter_count": 0,
                "annotated_parameter_count": 0,
                "attribute_count": 0,
                "annotated_attribute_count": 0,
                "function_count": 1,
                "partially_annotated_function_count": 1,
                "fully_annotated_function_count": 0,
                "line_count": 3,
            },
        )


class FunctionAnnotationKindTest(unittest.TestCase):
    def test_from_function_data(self) -> None:
        three_parameters = [
            cst.Param(name=cst.Name("x1"), annotation=None),
            cst.Param(name=cst.Name("x2"), annotation=None),
            cst.Param(name=cst.Name("x3"), annotation=None),
        ]
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=True,
                annotated_parameter_count=3,
                is_method_or_classmethod=False,
                parameters=three_parameters,
            ),
            FunctionAnnotationKind.FULLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=True,
                annotated_parameter_count=0,
                is_method_or_classmethod=False,
                parameters=three_parameters,
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                annotated_parameter_count=0,
                is_method_or_classmethod=False,
                parameters=three_parameters,
            ),
            FunctionAnnotationKind.NOT_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                annotated_parameter_count=1,
                is_method_or_classmethod=False,
                parameters=three_parameters,
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        # An untyped `self` parameter of a method does not count for partial
        # annotation. As per PEP 484, we need an explicitly annotated parameter.
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                annotated_parameter_count=1,
                is_method_or_classmethod=True,
                parameters=three_parameters,
            ),
            FunctionAnnotationKind.NOT_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                annotated_parameter_count=2,
                is_method_or_classmethod=True,
                parameters=three_parameters,
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        # An annotated `self` suffices to make Pyre typecheck the method.
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                annotated_parameter_count=1,
                is_method_or_classmethod=True,
                parameters=[
                    cst.Param(
                        name=cst.Name("self"),
                        annotation=cst.Annotation(cst.Name("Foo")),
                    )
                ],
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                annotated_parameter_count=0,
                is_method_or_classmethod=True,
                parameters=[],
            ),
            FunctionAnnotationKind.NOT_ANNOTATED,
        )


class FixmeCountCollectorTest(unittest.TestCase):
    def assert_counts(
        self,
        source: str,
        expected_codes: Dict[int, List[int]],
        expected_no_codes: List[int],
    ) -> None:
        source_module = MetadataWrapper(
            parse_source(source.replace("FIXME", "pyre-fixme"))
        )
        collector = FixmeCountCollector()
        source_module.visit(collector)
        result = collector.build_result()
        self.assertEqual(expected_codes, result.code)
        self.assertEqual(expected_no_codes, result.no_code)

    def test_count_fixmes(self) -> None:
        self.assert_counts("# FIXME[2]: Example Error Message", {2: [1]}, [])
        self.assert_counts(
            "# FIXME[3]: Example Error Message \n\n\n # FIXME[34]: Example",
            {3: [1], 34: [4]},
            [],
        )
        self.assert_counts(
            "# FIXME[2]: Example Error Message\n\n\n# FIXME[2]: message",
            {2: [1, 4]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x  # FIXME[7]
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                # FIXME[7]: comments
                return x
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x # unrelated # FIXME[7]
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x # unrelated   #  FIXME[7] comments
            """,
            {7: [3]},
            [],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x  # FIXME
            """,
            {},
            [3],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x  # FIXME: comments
            """,
            {},
            [3],
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x # FIXME[7, 8]
            """,
            {7: [3], 8: [3]},
            [],
        )
        self.assert_counts(
            """
            # FIXME[8]
            def foo(x: str) -> int:
                return x # FIXME[7, 8]
            """,
            {7: [4], 8: [2, 4]},
            [],
        )
        # Invalid suppression
        self.assert_counts(
            """
            # FIXME[8,]
            def foo(x: str) -> int:
                return x
            """,
            {},
            [2],
        )


class IgnoreCountCollectorTest(unittest.TestCase):
    def assert_counts(
        self,
        source: str,
        expected_codes: Dict[int, List[int]],
        expected_no_codes: List[int],
    ) -> None:
        source_module = MetadataWrapper(
            parse_source(source.replace("IGNORE", "pyre-ignore"))
        )
        collector = IgnoreCountCollector()
        source_module.visit(collector)
        result = collector.build_result()
        self.assertEqual(expected_codes, result.code)
        self.assertEqual(expected_no_codes, result.no_code)

    def test_count_ignores(self) -> None:
        self.assert_counts("# IGNORE[2]: Example Error Message", {2: [1]}, [])
        self.assert_counts(
            "# IGNORE[3]: Example Error Message \n\n\n # pyre-ignore[34]: Example",
            {3: [1], 34: [4]},
            [],
        )
        self.assert_counts(
            "# IGNORE[2]: Example Error Message\n\n\n# pyre-ignore[2]: message",
            {2: [1, 4]},
            [],
        )


class StrictCountCollectorTest(unittest.TestCase):
    def assert_counts(
        self,
        source: str,
        default_strict: bool,
        mode: ModuleMode,
        explicit_comment_line: Optional[int],
    ) -> None:
        source_module = MetadataWrapper(parse_source(source))
        collector = StrictCountCollector(default_strict)
        source_module.visit(collector)
        result = collector.build_result()
        self.assertEqual(mode, result.mode)
        self.assertEqual(explicit_comment_line, result.explicit_comment_line)

    def test_strict_files(self) -> None:
        self.assert_counts(
            """
            # pyre-unsafe

            def foo():
                return 1
            """,
            default_strict=True,
            mode=ModuleMode.UNSAFE,
            explicit_comment_line=2,
        )
        self.assert_counts(
            """
            # pyre-strict
            def foo():
                return 1
            """,
            default_strict=False,
            mode=ModuleMode.STRICT,
            explicit_comment_line=2,
        )
        self.assert_counts(
            """
            def foo():
                return 1
            """,
            default_strict=False,
            mode=ModuleMode.UNSAFE,
            explicit_comment_line=None,
        )
        self.assert_counts(
            """
            def foo():
                return 1
            """,
            default_strict=True,
            mode=ModuleMode.STRICT,
            explicit_comment_line=None,
        )
        self.assert_counts(
            """
            # pyre-ignore-all-errors
            def foo():
                return 1
            """,
            default_strict=True,
            mode=ModuleMode.UNSAFE,
            explicit_comment_line=2,
        )
        self.assert_counts(
            """
            def foo(x: str) -> int:
                return x
            """,
            default_strict=False,
            mode=ModuleMode.UNSAFE,
            explicit_comment_line=None,
        )
        self.assert_counts(
            """
            #  pyre-strict
            def foo(x: str) -> int:
                return x
            """,
            default_strict=False,
            mode=ModuleMode.STRICT,
            explicit_comment_line=2,
        )
        self.assert_counts(
            """
            #  pyre-ignore-all-errors[56]
            def foo(x: str) -> int:
                return x
            """,
            default_strict=True,
            mode=ModuleMode.STRICT,
            explicit_comment_line=None,
        )


class CoverageTest(unittest.TestCase):
    def assert_coverage_equal(
        self,
        file_content: str,
        expected_covered: List[int],
        expected_uncovered: List[int],
    ) -> None:
        module = cst.parse_module(textwrap.dedent(file_content).strip())
        actual_coverage = collect_coverage_for_module(
            "test.py", module, strict_default=False
        )
        self.assertEqual(
            expected_covered, actual_coverage.covered_lines, "Covered mismatch"
        )
        self.assertEqual(
            expected_uncovered, actual_coverage.uncovered_lines, "Not covered mismatch"
        )

    def test_coverage_covered(self) -> None:
        self.assert_coverage_equal(
            """
            def foo() -> int:
                return 5
            """,
            expected_covered=[0, 1],
            expected_uncovered=[],
        )

    def test_coverage_uncovered(self) -> None:
        self.assert_coverage_equal(
            """
            def foo():
                return 5
            """,
            expected_covered=[],
            expected_uncovered=[0, 1],
        )

    def test_coverage_mixed(self) -> None:
        self.assert_coverage_equal(
            """
            import os

            X = 5

            def foo():
                return 5

            class Bar():

                def baz(self, y) -> int:
                    return y + 5
            """,
            expected_covered=[0, 1, 2, 3, 6, 7, 8, 9, 10],
            expected_uncovered=[4, 5],
        )

    def test_coverage_nested(self) -> None:
        self.assert_coverage_equal(
            """
            def f():

                def bar(x: int) -> None:
                    return x

                return 5
            """,
            expected_covered=[2, 3],
            expected_uncovered=[0, 1, 4, 5],
        )
        self.assert_coverage_equal(
            """
            level0: None = None
            def level1():
                def level2() -> None:
                    def level3():
                        def level4() -> None:
                            def level5(): ...
            """,
            expected_covered=[0, 2, 4],
            expected_uncovered=[1, 3, 5],
        )

    def contains_uncovered_lines(self, file_content: str, strict_default: bool) -> bool:
        module = cst.parse_module(textwrap.dedent(file_content).strip())
        actual_coverage = collect_coverage_for_module("test.py", module, strict_default)
        return len(actual_coverage.uncovered_lines) > 0

    def test_coverage_strict(self) -> None:
        self.assertTrue(
            self.contains_uncovered_lines(
                """
                # No file specific comment
                def foo(): ...
                """,
                strict_default=False,
            )
        )
        self.assertFalse(
            self.contains_uncovered_lines(
                """
                # pyre-strict
                def foo(): ...
                """,
                strict_default=False,
            )
        )
        self.assertFalse(
            self.contains_uncovered_lines(
                """
                # No file specific comment
                def foo(): ...
                """,
                strict_default=True,
            )
        )
        self.assertTrue(
            self.contains_uncovered_lines(
                """
                # pyre-unsafe
                def foo(): ...
                """,
                strict_default=True,
            )
        )
