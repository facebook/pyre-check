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
from libcst.metadata import CodePosition, CodeRange, MetadataWrapper

from ..coverage_data import (
    AnnotationCollector,
    find_module_paths,
    FunctionAnnotationInfo,
    FunctionAnnotationKind,
    FunctionIdentifier,
    get_paths_to_collect,
    module_from_code,
    module_from_path,
    ModuleMode,
    ModuleModeCollector,
    ParameterAnnotationInfo,
    ReturnAnnotationInfo,
    SuppressionCollector,
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

    def _assert_function_annotations(
        self,
        code: str,
        expected: Sequence[FunctionAnnotationInfo],
    ) -> None:
        module = parse_code(code)
        collector = AnnotationCollector()
        module.visit(collector)
        actual = collector.functions
        self.assertEqual(
            actual,
            expected,
        )

    def test_function_annotations__standalone_no_annotations(self) -> None:
        f = FunctionIdentifier(
            parent=None,
            name="f",
        )
        self._assert_function_annotations(
            """
            def f(x):
                pass
            """,
            [
                FunctionAnnotationInfo(
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=3, column=8),
                    ),
                    annotation_kind=FunctionAnnotationKind.NOT_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        function_identifier=f,
                        is_annotated=False,
                        code_range=CodeRange(
                            start=CodePosition(line=2, column=4),
                            end=CodePosition(line=2, column=5),
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            function_identifier=f,
                            name="x",
                            is_annotated=False,
                            code_range=CodeRange(
                                start=CodePosition(line=2, column=6),
                                end=CodePosition(line=2, column=7),
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                )
            ],
        )

    def test_function_annotations__standalone_partially_annotated(self) -> None:
        f = FunctionIdentifier(
            parent=None,
            name="f",
        )
        g = FunctionIdentifier(
            parent=None,
            name="g",
        )
        self._assert_function_annotations(
            """
            def f(x) -> None:
                pass

            def g(x: int):
                pass
            """,
            [
                FunctionAnnotationInfo(
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=3, column=8),
                    ),
                    annotation_kind=FunctionAnnotationKind.PARTIALLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        function_identifier=f,
                        is_annotated=True,
                        code_range=CodeRange(
                            start=CodePosition(line=2, column=4),
                            end=CodePosition(line=2, column=5),
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            function_identifier=f,
                            name="x",
                            is_annotated=False,
                            code_range=CodeRange(
                                start=CodePosition(line=2, column=6),
                                end=CodePosition(line=2, column=7),
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                ),
                FunctionAnnotationInfo(
                    code_range=CodeRange(
                        start=CodePosition(line=5, column=0),
                        end=CodePosition(line=6, column=8),
                    ),
                    annotation_kind=FunctionAnnotationKind.PARTIALLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        function_identifier=g,
                        is_annotated=False,
                        code_range=CodeRange(
                            start=CodePosition(line=5, column=4),
                            end=CodePosition(line=5, column=5),
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            function_identifier=g,
                            name="x",
                            is_annotated=True,
                            code_range=CodeRange(
                                start=CodePosition(line=5, column=6),
                                end=CodePosition(line=5, column=7),
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                ),
            ],
        )

    def test_function_annotations__standalone_fully_annotated(self) -> None:
        f = FunctionIdentifier(
            parent=None,
            name="f",
        )
        self._assert_function_annotations(
            """
            def f(x: int) -> None:
                pass
            """,
            [
                FunctionAnnotationInfo(
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=3, column=8),
                    ),
                    annotation_kind=FunctionAnnotationKind.FULLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        function_identifier=f,
                        is_annotated=True,
                        code_range=CodeRange(
                            start=CodePosition(line=2, column=4),
                            end=CodePosition(line=2, column=5),
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            function_identifier=f,
                            name="x",
                            is_annotated=True,
                            code_range=CodeRange(
                                start=CodePosition(line=2, column=6),
                                end=CodePosition(line=2, column=7),
                            ),
                        )
                    ],
                    is_method_or_classmethod=False,
                )
            ],
        )

    def test_function_annotations__annotated_method(self) -> None:
        a_dot_f = FunctionIdentifier(
            parent="A",
            name="f",
        )
        self._assert_function_annotations(
            """
            class A:
                def f(self, x: int) -> None:
                    pass
            """,
            [
                FunctionAnnotationInfo(
                    code_range=CodeRange(
                        start=CodePosition(line=3, column=4),
                        end=CodePosition(line=4, column=12),
                    ),
                    annotation_kind=FunctionAnnotationKind.FULLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        function_identifier=a_dot_f,
                        is_annotated=True,
                        code_range=CodeRange(
                            start=CodePosition(line=3, column=8),
                            end=CodePosition(line=3, column=9),
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            function_identifier=a_dot_f,
                            name="self",
                            is_annotated=False,
                            code_range=CodeRange(
                                start=CodePosition(line=3, column=10),
                                end=CodePosition(line=3, column=14),
                            ),
                        ),
                        ParameterAnnotationInfo(
                            function_identifier=a_dot_f,
                            name="x",
                            is_annotated=True,
                            code_range=CodeRange(
                                start=CodePosition(line=3, column=16),
                                end=CodePosition(line=3, column=17),
                            ),
                        ),
                    ],
                    is_method_or_classmethod=True,
                )
            ],
        )

    def test_function_annotations__partially_annotated_static_method(self) -> None:
        a_dot_f = FunctionIdentifier(
            parent="A",
            name="f",
        )
        self._assert_function_annotations(
            """
            class A:

                @staticmethod
                def f(self, x: int) -> None:
                    pass
            """,
            [
                FunctionAnnotationInfo(
                    code_range=CodeRange(
                        start=CodePosition(line=5, column=4),
                        end=CodePosition(line=6, column=12),
                    ),
                    annotation_kind=FunctionAnnotationKind.PARTIALLY_ANNOTATED,
                    returns=ReturnAnnotationInfo(
                        function_identifier=a_dot_f,
                        is_annotated=True,
                        code_range=CodeRange(
                            start=CodePosition(line=5, column=8),
                            end=CodePosition(line=5, column=9),
                        ),
                    ),
                    parameters=[
                        ParameterAnnotationInfo(
                            function_identifier=a_dot_f,
                            name="self",
                            is_annotated=False,
                            code_range=CodeRange(
                                start=CodePosition(line=5, column=10),
                                end=CodePosition(line=5, column=14),
                            ),
                        ),
                        ParameterAnnotationInfo(
                            function_identifier=a_dot_f,
                            name="x",
                            is_annotated=True,
                            code_range=CodeRange(
                                start=CodePosition(line=5, column=16),
                                end=CodePosition(line=5, column=17),
                            ),
                        ),
                    ],
                    is_method_or_classmethod=False,
                )
            ],
        )


class FunctionAnnotationKindTest(testslide.TestCase):

    ANNOTATION = cst.Annotation(cst.Name("Foo"))

    def _parameter(self, name: str, annotated: bool) -> cst.Param:
        return cst.Param(
            name=cst.Name(name),
            annotation=self.ANNOTATION if annotated else None,
        )

    def test_from_function_data(self) -> None:
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=True,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=True),
                    self._parameter("x1", annotated=True),
                    self._parameter("x2", annotated=True),
                ],
            ),
            FunctionAnnotationKind.FULLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=True,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=False),
                    self._parameter("x1", annotated=False),
                    self._parameter("x2", annotated=False),
                ],
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=False),
                    self._parameter("x1", annotated=False),
                    self._parameter("x2", annotated=False),
                ],
            ),
            FunctionAnnotationKind.NOT_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                is_non_static_method=False,
                parameters=[
                    self._parameter("x0", annotated=True),
                    self._parameter("x1", annotated=False),
                    self._parameter("x2", annotated=False),
                ],
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        # An untyped `self` parameter of a method is not required, but it also
        # does not count for partial annotation. As per PEP 484, we need an
        # explicitly annotated parameter or return before we'll typecheck a method.
        #
        # Check several edge cases related to this.
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=True,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=False),
                    self._parameter("x1", annotated=False),
                ],
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=True,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=True),
                    self._parameter("x1", annotated=False),
                ],
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
        )
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=True,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=False),
                ],
            ),
            FunctionAnnotationKind.FULLY_ANNOTATED,
        )
        # An explicitly annotated `self` suffices to make Pyre typecheck the method.
        self.assertEqual(
            FunctionAnnotationKind.from_function_data(
                is_return_annotated=False,
                is_non_static_method=True,
                parameters=[
                    self._parameter("self", annotated=True),
                ],
            ),
            FunctionAnnotationKind.PARTIALLY_ANNOTATED,
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
        actual = SuppressionCollector().collect(source_module)
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
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=2, column=12),
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    code_range=CodeRange(
                        start=CodePosition(line=3, column=0),
                        end=CodePosition(line=3, column=25),
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    code_range=CodeRange(
                        start=CodePosition(line=4, column=0),
                        end=CodePosition(line=4, column=15),
                    ),
                    error_codes=[1],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    code_range=CodeRange(
                        start=CodePosition(line=5, column=0),
                        end=CodePosition(line=5, column=33),
                    ),
                    error_codes=[10, 11],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    code_range=CodeRange(
                        start=CodePosition(line=6, column=0),
                        end=CodePosition(line=6, column=65),
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
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=2, column=13),
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=3, column=0),
                        end=CodePosition(line=3, column=26),
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=4, column=0),
                        end=CodePosition(line=4, column=16),
                    ),
                    error_codes=[1],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=5, column=0),
                        end=CodePosition(line=5, column=21),
                    ),
                    error_codes=[10, 11],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=6, column=0),
                        end=CodePosition(line=6, column=34),
                    ),
                    error_codes=[10, 11],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=7, column=0),
                        end=CodePosition(line=7, column=66),
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
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=2, column=14),
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.TYPE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=3, column=0),
                        end=CodePosition(line=3, column=42),
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
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=14),
                        end=CodePosition(line=2, column=26),
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_FIXME,
                    code_range=CodeRange(
                        start=CodePosition(line=3, column=14),
                        end=CodePosition(line=3, column=50),
                    ),
                    error_codes=[3, 4],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.PYRE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=4, column=14),
                        end=CodePosition(line=4, column=48),
                    ),
                    error_codes=[5],
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.TYPE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=5, column=14),
                        end=CodePosition(line=5, column=46),
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
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=2, column=30),
                    ),
                    error_codes=None,
                ),
                TypeErrorSuppression(
                    kind=SuppressionKind.TYPE_IGNORE,
                    code_range=CodeRange(
                        start=CodePosition(line=2, column=0),
                        end=CodePosition(line=2, column=30),
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
        result = ModuleModeCollector(default_strict).collect(source_module)
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
    def test_get_paths_to_collect__duplicate_directories(self) -> None:
        self.assertCountEqual(
            get_paths_to_collect(
                [Path("/root/foo.py"), Path("/root/bar.py"), Path("/root/foo.py")],
                root=Path("/root"),
            ),
            [Path("/root/foo.py"), Path("/root/bar.py")],
        )

        self.assertCountEqual(
            get_paths_to_collect(
                [Path("/root/foo"), Path("/root/bar"), Path("/root/foo")],
                root=Path("/root"),
            ),
            [Path("/root/foo"), Path("/root/bar")],
        )

    def test_get_paths_to_collect__expand_directories(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                self.assertCountEqual(
                    get_paths_to_collect(
                        [Path("foo.py"), Path("bar.py")],
                        root=root_path,
                    ),
                    [root_path / "foo.py", root_path / "bar.py"],
                )

    def test_get_paths_to_collect__invalid_given_subdirectory(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()  # resolve is necessary on OSX 11.6
            with setup.switch_working_directory(root_path):
                # this is how a valid call behaves: subdirectory lives under project_root
                self.assertCountEqual(
                    get_paths_to_collect(
                        [Path("project_root/subdirectory")],
                        root=root_path / "project_root",
                    ),
                    [root_path / "project_root/subdirectory"],
                )
                # ./subdirectory isn't part of ./project_root
                self.assertRaisesRegex(
                    ValueError,
                    ".* is not nested under the project .*",
                    get_paths_to_collect,
                    [Path("subdirectory")],
                    root=root_path / "project_root",
                )
                # ./subdirectory isn't part of ./local_root
                self.assertRaisesRegex(
                    ValueError,
                    ".* is not nested under the project .*",
                    get_paths_to_collect,
                    [Path("subdirectory")],
                    root=root_path / "local_root",
                )

    def test_get_paths_to_collect__no_explicit_paths(self) -> None:
        self.assertCountEqual(
            get_paths_to_collect(
                None,
                root=Path("/root/local"),
            ),
            [Path("/root/local")],
        )

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
