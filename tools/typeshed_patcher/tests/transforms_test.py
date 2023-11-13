# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import textwrap

import testslide

from .. import patch_specs, transforms


class PatchTransformsTest(testslide.TestCase):
    def assert_transform(
        self,
        original_code: str,
        patch: patch_specs.Patch,
        expected_code: str,
    ) -> None:
        actual_output = transforms.apply_patches_in_sequence(
            code=textwrap.dedent(original_code),
            patches=[patch],
        )
        try:
            self.assertEqual(
                actual_output.strip(),
                textwrap.dedent(expected_code).strip(),
            )
        except AssertionError as err:
            print("--- Expected ---")
            print(textwrap.dedent(expected_code))
            print("--- Actual ---")
            print(actual_output)
            raise err

    def test_add_to_module__top(self) -> None:
        self.assert_transform(
            original_code=(
                """
                b: str
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.AddAction(
                    content=textwrap.dedent(
                        """
                        from foo import Bar
                        a: Bar
                        """
                    ),
                    position=patch_specs.AddPosition.TOP_OF_SCOPE,
                ),
            ),
            expected_code=(
                """
                from foo import Bar
                a: Bar
                b: str
                """
            ),
        )

    def test_add_to_module__bottom(self) -> None:
        self.assert_transform(
            original_code=(
                """
                b: str
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.AddAction(
                    content=textwrap.dedent(
                        """
                        def f(x: int) -> int: ...
                        y: float
                        """
                    ),
                    position=patch_specs.AddPosition.BOTTOM_OF_SCOPE,
                ),
            ),
            expected_code=(
                """
                b: str
                def f(x: int) -> int: ...
                y: float
                """
            ),
        )

    def test_add_to_class__top(self) -> None:
        self.assert_transform(
            original_code=(
                """
                class MyClass:
                    b: int
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string("MyClass"),
                action=patch_specs.AddAction(
                    content=textwrap.dedent(
                        """
                        a: float
                        def f(self, x: int) -> int: ...
                        """
                    ),
                    position=patch_specs.AddPosition.TOP_OF_SCOPE,
                ),
            ),
            expected_code=(
                """
                class MyClass:
                    a: float
                    def f(self, x: int) -> int: ...
                    b: int
                """
            ),
        )

    def test_add_to_class__bottom(self) -> None:
        self.assert_transform(
            original_code=(
                """
                class MyClass:
                    b: int
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string("MyClass"),
                action=patch_specs.AddAction(
                    content=textwrap.dedent(
                        """
                        a: float
                        def f(self, x: int) -> int: ...
                        """
                    ),
                    position=patch_specs.AddPosition.BOTTOM_OF_SCOPE,
                ),
            ),
            expected_code=(
                """
                class MyClass:
                    b: int
                    a: float
                    def f(self, x: int) -> int: ...
                """
            ),
        )

    def test_add_to_class__force_indent(self) -> None:
        # This test is needed to exercise the forced conversion of
        # class bodies that aren't indented (which is a type cast in libcst)
        self.assert_transform(
            original_code=(
                """
                class MyClass: b: int
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string("MyClass"),
                action=patch_specs.AddAction(
                    content=textwrap.dedent(
                        """
                        a: float
                        def f(self, x: int) -> int: ...
                        """
                    ),
                    position=patch_specs.AddPosition.BOTTOM_OF_SCOPE,
                ),
            ),
            expected_code=(
                """
                class MyClass:
                    b: int
                    a: float
                    def f(self, x: int) -> int: ...
                """
            ),
        )

    def test_add_to_class_nested_classes(self) -> None:
        # This test is needed to exercise the forced conversion of
        # class bodies that aren't indented (which is a type cast in libcst).
        # We also add extra classes to make sure the name tracking works.
        self.assert_transform(
            original_code=(
                """
                class OuterClass0:
                    pass
                class OuterClass1:
                    class InnerClass0:
                        b: int
                    class InnerClass1:
                        b: int
                    class InnerClass2:
                        b: int
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string("OuterClass1.InnerClass1"),
                action=patch_specs.AddAction(
                    content=textwrap.dedent(
                        """
                        def f(self, x: int) -> int: ...
                        """
                    ),
                    position=patch_specs.AddPosition.BOTTOM_OF_SCOPE,
                ),
            ),
            expected_code=(
                """
                class OuterClass0:
                    pass
                class OuterClass1:
                    class InnerClass0:
                        b: int
                    class InnerClass1:
                        b: int
                        def f(self, x: int) -> int: ...
                    class InnerClass2:
                        b: int
                """
            ),
        )

    def test_add_under_import_header(self) -> None:
        self.assert_transform(
            original_code=(
                """
                import foo, bar
                if condition:
                    from baz import Baz
                if condition1:
                    import qux1 as quux
                elif condition2:
                    import qux2 as quux
                else:
                    import qux3 as quux
                x: int
                import this_is_out_of_order
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.AddAction(
                    content=textwrap.dedent(
                        """
                        from typing import TypeVar
                        T = TypeVar('T')
                        """
                    ),
                    position=patch_specs.AddPosition.TOP_OF_SCOPE,
                ),
            ),
            expected_code=(
                """
                import foo, bar
                if condition:
                    from baz import Baz
                if condition1:
                    import qux1 as quux
                elif condition2:
                    import qux2 as quux
                else:
                    import qux3 as quux
                from typing import TypeVar
                T = TypeVar('T')
                x: int
                import this_is_out_of_order
                """
            ),
        )

    def test_delete__ann_assign(self) -> None:
        self.assert_transform(
            original_code=(
                """
                x: int
                y: str
                z: float
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="y",
                ),
            ),
            expected_code=(
                """
                x: int
                z: float
                """
            ),
        )

    def test_delete__if_block(self) -> None:
        self.assert_transform(
            original_code=(
                """
                y: str
                if condition0:
                    def f(x: int) -> int: ...
                elif condition1:
                    def f(x: str) -> str: ...
                else:
                    def f(x: float) -> float: ...
                z: float
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="f",
                ),
            ),
            expected_code=(
                """
                y: str
                z: float
                """
            ),
        )

    def test_delete__typealias(self) -> None:
        self.assert_transform(
            original_code=(
                """
                from foo import Foo
                Baz = Foo
                x: int
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="Baz",
                ),
            ),
            expected_code=(
                """
                from foo import Foo
                x: int
                """
            ),
        )

    def test_delete__class(self) -> None:
        self.assert_transform(
            original_code=(
                """
                class A: pass
                @classdecorator
                class B: pass
                class C: pass
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="B",
                ),
            ),
            expected_code=(
                """
                class A: pass
                class C: pass
                """
            ),
        )

    def test_delete__function(self) -> None:
        self.assert_transform(
            original_code=(
                """
                def f(x: int) -> int: ...
                def g(x: int) -> int: ...
                def h(x: int) -> int: ...
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="g",
                ),
            ),
            expected_code=(
                """
                def f(x: int) -> int: ...
                def h(x: int) -> int: ...
                """
            ),
        )

    def test_delete__overloads(self) -> None:
        self.assert_transform(
            original_code=(
                """
                def f(x: int) -> int: ...
                @overload
                def g(x: int) -> int: ...
                @overload
                def g(x: int) -> int: ...
                def g(object) -> object: ...
                def h(x: int) -> int: ...
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="g",
                ),
            ),
            expected_code=(
                """
                def f(x: int) -> int: ...
                def h(x: int) -> int: ...
                """
            ),
        )

    def test_delete__in_nested_class(self) -> None:
        self.assert_transform(
            original_code=(
                """
                class OuterClass0:
                    class InnerClass1:
                        x: int
                class OuterClass1:
                    class InnerClass0:
                        x: int
                    class InnerClass1:
                        x: int
                    class InnerClass2:
                        x: int
                class OuterClass2:
                    class InnerClass1:
                        x: int
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string("OuterClass1.InnerClass1"),
                action=patch_specs.DeleteAction(
                    name="x",
                ),
            ),
            expected_code=(
                """
                class OuterClass0:
                    class InnerClass1:
                        x: int
                class OuterClass1:
                    class InnerClass0:
                        x: int
                    class InnerClass1:
                        pass
                    class InnerClass2:
                        x: int
                class OuterClass2:
                    class InnerClass1:
                        x: int
                """
            ),
        )

    def test_delete__import(self) -> None:
        self.assert_transform(
            original_code=(
                """
                import foo
                if condition0:
                    import bar
                else:
                    from baz import bar
                from bar import qux
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="bar",
                ),
            ),
            expected_code=(
                """
                import foo
                from bar import qux
                """
            ),
        )

    def test_delete__import_as(self) -> None:
        self.assert_transform(
            original_code=(
                """
                import foo
                import bar_alt as bar
                from baz import bar_alt as bar
                from bar import qux
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.DeleteAction(
                    name="bar",
                ),
            ),
            expected_code=(
                """
                import foo
                from bar import qux
                """
            ),
        )

    def test_replace__ann_assign(self) -> None:
        self.assert_transform(
            original_code=(
                """
                x: int
                y: str
                z: float
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.ReplaceAction(
                    name="y",
                    content=textwrap.dedent(
                        """
                        w: str
                        """
                    ),
                ),
            ),
            expected_code=(
                """
                x: int
                w: str
                z: float
                """
            ),
        )

    def test_replace__function_with_overloads(self) -> None:
        self.assert_transform(
            original_code=(
                """
                def f(x: int) -> int: ...
                @overload
                def g(x: int) -> int: ...
                @overload
                def g(x: float) -> float: ...
                def h(x: int) -> int: ...
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.ReplaceAction(
                    name="g",
                    content=textwrap.dedent(
                        """
                        T = TypeVar('T')
                        def g(x: T) -> T: ...
                        """
                    ),
                ),
            ),
            expected_code=(
                """
                def f(x: int) -> int: ...
                T = TypeVar('T')
                def g(x: T) -> T: ...
                def h(x: int) -> int: ...
                """
            ),
        )

    def test_replace__nested_class(self) -> None:
        self.assert_transform(
            original_code=(
                """
                class OuterClass0:
                    class InnerClass1:
                        x: int
                class OuterClass1:
                    class InnerClass0:
                        x: int
                    class InnerClass1:
                        x: int
                    class InnerClass2:
                        x: int
                class OuterClass2:
                    class InnerClass1:
                        x: int
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string("OuterClass1.InnerClass1"),
                action=patch_specs.ReplaceAction(
                    name="x",
                    content="y: float",
                ),
            ),
            expected_code=(
                """
                class OuterClass0:
                    class InnerClass1:
                        x: int
                class OuterClass1:
                    class InnerClass0:
                        x: int
                    class InnerClass1:
                        y: float
                    class InnerClass2:
                        x: int
                class OuterClass2:
                    class InnerClass1:
                        x: int
                """
            ),
        )

    def test_replace__import(self) -> None:
        self.assert_transform(
            original_code=(
                """
                import baz
                from foo import Bar
                x: Bar
                """
            ),
            patch=patch_specs.Patch(
                parent=patch_specs.QualifiedName.from_string(""),
                action=patch_specs.ReplaceAction(
                    name="Bar",
                    content="Bar = baz.Bar",
                ),
            ),
            expected_code=(
                """
                import baz
                Bar = baz.Bar
                x: Bar
                """
            ),
        )
