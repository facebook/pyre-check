# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
import textwrap
from pathlib import Path
from typing import Optional, Sequence

import libcst as cst

import testslide
from libcst.metadata import MetadataWrapper

from .. import coverage_data
from ..coverage_data import (
    AnnotationCollector,
    find_module_paths,
    FunctionAnnotationInfo,
    FunctionAnnotationStatus,
    FunctionIdentifier,
    Location,
    module_from_code,
    module_from_path,
    ModuleMode,
    ParameterAnnotationInfo,
    ReturnAnnotationInfo,
    SuppressionKind,
    TypeErrorSuppression,
)

from ..tests import setup


def parse_code(code: str) -> MetadataWrapper:
    module = module_from_code(textwrap.dedent(code.rstrip()))
    if module is None:
        raise RuntimeError(f"Failed to parse code {code}")
    return module


class ParsingHelpersTest(testslide.TestCase):
    def test_module_from_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            source_path = root_path / "source.py"
            source_path.write_text("reveal_type(42)")

            self.assertIsNotNone(module_from_path(source_path))
            self.assertIsNone(module_from_path(root_path / "nonexistent.py"))

    def test_module_from_code(self) -> None:
        self.assertIsNotNone(
            module_from_code(
                textwrap.dedent(
                    """
                    def foo() -> int:
                        pass
                    """
                )
            )
        )
        self.assertIsNone(
            module_from_code(
                textwrap.dedent(
                    """
                    def foo() ->
                    """
                )
            )
        )


class AnnotationCollectorTest(testslide.TestCase):
    maxDiff = 2000

    def _build_and_visit_annotation_collector(self, source: str) -> AnnotationCollector:
        source_module = parse_code(source)
        collector = AnnotationCollector()
        source_module.visit(collector)
        return collector

    def test_return_location(self) -> None:
        collector = self._build_and_visit_annotation_collector(
            """
            def foobar():
                pass
            """
        )
        returns = list(collector.returns())
        self.assertEqual(len(returns), 1)
        self.assertEqual(
            returns[0].location,
            Location(
                start_line=2,
                start_column=4,
                end_line=2,
                end_column=10,
            ),
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

    def _assert_function_annotations(
        self,
        code: str,
        expected: Sequence[FunctionAnnotationInfo],
    ) -> None:
        module = parse_code(code)
        actual = coverage_data.collect_functions(module)
        self.assertEqual(
            actual,
            expected,
        )

    def test_function_annotations__standalone_no_annotations(self) -> None:
        self._assert_function_annotations(
            """
            def f(x):
                pass
            """,
            [
                FunctionAnnotationInfo(
                    identifier=FunctionIdentifier(
                        parent=None,
                        name="f",
                    ),
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=3,
                        end_column=8,
                    ),
                    annotation_status=FunctionAnnotationStatus.NOT_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        is_annotated=False,
                        location=Location(
                            start_line=2,
                            start_column=4,
                            end_line=2,
                            end_column=5,
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            name="x",
                            is_annotated=False,
                            location=Location(
                                start_line=2,
                                start_column=6,
                                end_line=2,
                                end_column=7,
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                )
            ],
        )

    def test_function_annotations__standalone_partially_annotated(self) -> None:
        self._assert_function_annotations(
            """
            def f(x) -> None:
                pass

            def g(x: int):
                pass
            """,
            [
                FunctionAnnotationInfo(
                    identifier=FunctionIdentifier(
                        parent=None,
                        name="f",
                    ),
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=3,
                        end_column=8,
                    ),
                    annotation_status=FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        is_annotated=True,
                        location=Location(
                            start_line=2,
                            start_column=4,
                            end_line=2,
                            end_column=5,
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            name="x",
                            is_annotated=False,
                            location=Location(
                                start_line=2,
                                start_column=6,
                                end_line=2,
                                end_column=7,
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                ),
                FunctionAnnotationInfo(
                    identifier=FunctionIdentifier(
                        parent=None,
                        name="g",
                    ),
                    location=Location(
                        start_line=5,
                        start_column=0,
                        end_line=6,
                        end_column=8,
                    ),
                    annotation_status=FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        is_annotated=False,
                        location=Location(
                            start_line=5,
                            start_column=4,
                            end_line=5,
                            end_column=5,
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            name="x",
                            is_annotated=True,
                            location=Location(
                                start_line=5,
                                start_column=6,
                                end_line=5,
                                end_column=7,
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                ),
            ],
        )

    def test_function_annotations__standalone_fully_annotated(self) -> None:
        self._assert_function_annotations(
            """
            def f(x: int) -> None:
                pass
            """,
            [
                FunctionAnnotationInfo(
                    identifier=FunctionIdentifier(
                        parent=None,
                        name="f",
                    ),
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=3,
                        end_column=8,
                    ),
                    annotation_status=FunctionAnnotationStatus.FULLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        is_annotated=True,
                        location=Location(
                            start_line=2,
                            start_column=4,
                            end_line=2,
                            end_column=5,
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            name="x",
                            is_annotated=True,
                            location=Location(
                                start_line=2,
                                start_column=6,
                                end_line=2,
                                end_column=7,
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                )
            ],
        )

    def test_function_annotations__annotated_method(self) -> None:
        self._assert_function_annotations(
            """
            class A:
                def f(self, x: int) -> None:
                    pass
            """,
            [
                FunctionAnnotationInfo(
                    identifier=FunctionIdentifier(
                        parent="A",
                        name="f",
                    ),
                    location=Location(
                        start_line=3,
                        start_column=4,
                        end_line=4,
                        end_column=12,
                    ),
                    annotation_status=FunctionAnnotationStatus.FULLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        is_annotated=True,
                        location=Location(
                            start_line=3,
                            start_column=8,
                            end_line=3,
                            end_column=9,
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            name="self",
                            is_annotated=False,
                            location=Location(
                                start_line=3,
                                start_column=10,
                                end_line=3,
                                end_column=14,
                            ),
                        ),
                        ParameterAnnotationInfo(
                            name="x",
                            is_annotated=True,
                            location=Location(
                                start_line=3,
                                start_column=16,
                                end_line=3,
                                end_column=17,
                            ),
                        ),
                    ],
                    is_method_or_classmethod=True,
                )
            ],
        )

    def test_function_annotations__partially_annotated_static_method(self) -> None:
        self._assert_function_annotations(
            """
            class A:
                class Inner:
                    @staticmethod
                    def f(self, x: int) -> None:
                        pass
            """,
            [
                FunctionAnnotationInfo(
                    identifier=FunctionIdentifier(
                        parent="A.Inner",
                        name="f",
                    ),
                    location=Location(
                        start_line=5,
                        start_column=8,
                        end_line=6,
                        end_column=16,
                    ),
                    annotation_status=FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        is_annotated=True,
                        location=Location(
                            start_line=5,
                            start_column=12,
                            end_line=5,
                            end_column=13,
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            name="self",
                            is_annotated=False,
                            location=Location(
                                start_line=5,
                                start_column=14,
                                end_line=5,
                                end_column=18,
                            ),
                        ),
                        ParameterAnnotationInfo(
                            name="x",
                            is_annotated=True,
                            location=Location(
                                start_line=5,
                                start_column=20,
                                end_line=5,
                                end_column=21,
                            ),
                        ),
                    ],
                    is_method_or_classmethod=False,
                )
            ],
        )


class FunctionAnnotationStatusTest(testslide.TestCase):

    ANNOTATION = cst.Annotation(cst.Name("Foo"))

    def _parameter(self, name: str, annotated: bool) -> cst.Param:
        return cst.Param(
            name=cst.Name(name),
            annotation=self.ANNOTATION if annotated else None,
        )

    def test_from_function_data(self) -> None:
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=True,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=True),
                    self._parameter("x1", annotated=True),
                    self._parameter("x2", annotated=True),
                ],
            ),
            FunctionAnnotationStatus.FULLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=True,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=False),
                    self._parameter("x1", annotated=False),
                    self._parameter("x2", annotated=False),
                ],
            ),
            FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=False,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=False),
                    self._parameter("x1", annotated=False),
                    self._parameter("x2", annotated=False),
                ],
            ),
            FunctionAnnotationStatus.NOT_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=False,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=True),
                    self._parameter("x1", annotated=False),
                    self._parameter("x2", annotated=False),
                ],
            ),
            FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
        )
        # An untyped `self` parameter of a method is not required, but it also
        # does not count for partial annotation. As per PEP 484, we need an
        # explicitly annotated parameter or return before we'll typecheck a method.
        #
        # Check several edge cases related to this.
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=True,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=False),
                    self._parameter("x1", annotated=False),
                ],
            ),
            FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=True,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=True),
                    self._parameter("x1", annotated=False),
                ],
            ),
            FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=True,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=False),
                ],
            ),
            FunctionAnnotationStatus.FULLY_ANNOTATED,
        )
        # An explicitly annotated `self` suffices to make Pyre typecheck the method.
        self.assertEqual(
            FunctionAnnotationStatus.from_function_data(
                is_return_annotated=False,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=True),
                ],
            ),
            FunctionAnnotationStatus.PARTIALLY_ANNOTATED,
        )


class SuppressionCollectorTest(testslide.TestCase):
    maxDiff = 2000

    def _assert_suppressions(
        self, source: str, expected: Sequence[TypeErrorSuppression]
    ) -> None:
        source_module = parse_code(
            source.replace("PYRE_FIXME", "pyre-fixme")
            .replace("PYRE_IGNORE", "pyre-ignore")
            .replace("TYPE_IGNORE", "type: ignore")
        )
        actual = coverage_data.collect_suppressions(source_module)
        self.assertEqual(actual, expected)

    def test_find_fixmes__simple(self) -> None:
        self._assert_suppressions(
            """
            # PYRE_FIXME
            # PYRE_FIXME with message
            # PYRE_FIXME[1]
            # PYRE_FIXME[10, 11] with message
            # PYRE_FIXME[10,]  (trailing comma is illegal, codes are ignored)
            """,
            [
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=2,
                        end_column=12,
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    location=Location(
                        start_line=3,
                        start_column=0,
                        end_line=3,
                        end_column=25,
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    location=Location(
                        start_line=4,
                        start_column=0,
                        end_line=4,
                        end_column=15,
                    ),
                    error_codes=[1],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    location=Location(
                        start_line=5,
                        start_column=0,
                        end_line=5,
                        end_column=33,
                    ),
                    error_codes=[10, 11],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    location=Location(
                        start_line=6,
                        start_column=0,
                        end_line=6,
                        end_column=65,
                    ),
                    error_codes=[],
                ),
            ],
        )

    def test_find_ignores__simple(self) -> None:
        self._assert_suppressions(
            """
            # PYRE_IGNORE
            # PYRE_IGNORE with message
            # PYRE_IGNORE[1]
            # PYRE_IGNORE[10, 11]
            # PYRE_IGNORE[10, 11] with message
            # PYRE_IGNORE[10,]  (trailing comma is illegal, codes are ignored)
            """,
            [
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=2,
                        end_column=13,
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=3,
                        start_column=0,
                        end_line=3,
                        end_column=26,
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=4,
                        start_column=0,
                        end_line=4,
                        end_column=16,
                    ),
                    error_codes=[1],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=5,
                        start_column=0,
                        end_line=5,
                        end_column=21,
                    ),
                    error_codes=[10, 11],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=6,
                        start_column=0,
                        end_line=6,
                        end_column=34,
                    ),
                    error_codes=[10, 11],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=7,
                        start_column=0,
                        end_line=7,
                        end_column=66,
                    ),
                    error_codes=[],
                ),
            ],
        )

    def test_find_type_ignores(self) -> None:
        self._assert_suppressions(
            """
            # TYPE_IGNORE
            # TYPE_IGNORE[1]  (codes won't be parsed)
            """,
            [
                TypeErrorSuppression(
                    kind=SuppressionKind.TYPE_IGNORE,
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=2,
                        end_column=14,
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.TYPE_IGNORE,
                    location=Location(
                        start_line=3,
                        start_column=0,
                        end_line=3,
                        end_column=42,
                    ),
                    error_codes=None,
                ),
            ],
        )

    def test_find_suppressions__trailing_comments(self) -> None:
        self._assert_suppressions(
            """
            a: int = 42.0 # PYRE_FIXME
            b: int = 42.0 # leading comment # PYRE_FIXME[3, 4]
            c: int = 42.0 # leading comment # PYRE_IGNORE[5]
            f: int = 42.0 # leading comment # TYPE_IGNORE
            """,
            [
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    location=Location(
                        start_line=2,
                        start_column=14,
                        end_line=2,
                        end_column=26,
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    location=Location(
                        start_line=3,
                        start_column=14,
                        end_line=3,
                        end_column=50,
                    ),
                    error_codes=[3, 4],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=4,
                        start_column=14,
                        end_line=4,
                        end_column=48,
                    ),
                    error_codes=[5],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.TYPE_IGNORE,
                    location=Location(
                        start_line=5,
                        start_column=14,
                        end_line=5,
                        end_column=46,
                    ),
                    error_codes=None,
                ),
            ],
        )

    def test_find_suppressions__multiline_string(self) -> None:
        self._assert_suppressions(
            """
            '''
            # PYRE_IGNORE
            '''
            """,
            [],
        )

    def test_find_suppressions__nested_suppressions(self) -> None:
        # If there are multiple suppressions, we count all of them. This is unlikely
        # to arise in practice but needs to have well-defined behavior.
        self._assert_suppressions(
            """
            # # PYRE_IGNORE # TYPE_IGNORE
            """,
            [
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=2,
                        end_column=30,
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.TYPE_IGNORE,
                    location=Location(
                        start_line=2,
                        start_column=0,
                        end_line=2,
                        end_column=30,
                    ),
                    error_codes=None,
                ),
            ],
        )


class ModuleModecollectorTest(testslide.TestCase):
    def assert_counts(
        self,
        source: str,
        default_strict: bool,
        mode: ModuleMode,
        explicit_comment_line: Optional[int],
    ) -> None:
        source_module = parse_code(source)
        result = coverage_data.collect_mode(source_module, default_strict)
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
            mode=ModuleMode.IGNORE_ALL,
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


class ModuleFindingHelpersTest(testslide.TestCase):
    def test_find_module_paths__basic(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(
                root_path,
                ["s0.py", "a/s1.py", "b/s2.py", "b/c/s3.py", "b/s4.txt", "b/__s5.py"],
            )
            setup.ensure_directories_exists(root_path, ["b/d"])
            self.assertCountEqual(
                find_module_paths(
                    [
                        root_path / "a/s1.py",
                        root_path / "b/s2.py",
                        root_path / "b/s4.txt",
                    ],
                    excludes=[],
                ),
                [
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
                ],
            )
            self.assertCountEqual(
                find_module_paths([root_path], excludes=[]),
                [
                    root_path / "s0.py",
                    root_path / "a/s1.py",
                    root_path / "b/s2.py",
                    root_path / "b/c/s3.py",
                ],
            )

    def test_find_module_paths__with_exclude(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(
                root_path,
                ["s0.py", "a/s1.py", "b/s2.py", "b/c/s3.py", "b/s4.txt", "b/__s5.py"],
            )
            setup.ensure_directories_exists(root_path, ["b/d"])
            self.assertCountEqual(
                find_module_paths(
                    [
                        root_path / "a/s1.py",
                        root_path / "b/s2.py",
                        root_path / "b/s4.txt",
                    ],
                    excludes=[r".*2\.py"],
                ),
                [
                    root_path / "a/s1.py",
                ],
            )
            self.assertCountEqual(
                find_module_paths(
                    [root_path],
                    excludes=[r".*2\.py"],
                ),
                [
                    root_path / "s0.py",
                    root_path / "a/s1.py",
                    root_path / "b/c/s3.py",
                ],
            )

    def test_find_module_paths__with_duplicates(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            setup.ensure_files_exist(
                root_path,
                ["a/s1.py", "a/s2.py"],
            )
            self.assertCountEqual(
                find_module_paths(
                    [
                        root_path / "a/s1.py",
                        root_path / "a",
                    ],
                    excludes=[],
                ),
                [
                    root_path / "a/s1.py",
                    root_path / "a/s2.py",
                ],
            )
