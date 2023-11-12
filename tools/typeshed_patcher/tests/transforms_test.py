# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import textwrap

import testslide

from .. import patch, transforms


class PatchTransformsTest(testslide.TestCase):
    def assert_transform(
        self,
        original_code: str,
        transform: transforms.PatchTransform,
        expected_code: str,
    ) -> None:
        actual_output = transforms.run_transform(
            code=textwrap.dedent(original_code),
            transform=transform,
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
            transform=transforms.AddTransform(
                parent=patch.QualifiedName.from_string(""),
                content=textwrap.dedent(
                    """
                    from foo import Bar
                    a: Bar
                    """
                ),
                add_position=patch.AddPosition.TOP_OF_SCOPE,
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
            transform=transforms.AddTransform(
                parent=patch.QualifiedName.from_string(""),
                content=textwrap.dedent(
                    """
                    def f(x: int) -> int: ...
                    y: float
                    """
                ),
                add_position=patch.AddPosition.BOTTOM_OF_SCOPE,
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
            transform=transforms.AddTransform(
                parent=patch.QualifiedName.from_string("MyClass"),
                content=textwrap.dedent(
                    """
                    a: float
                    def f(self, x: int) -> int: ...
                    """
                ),
                add_position=patch.AddPosition.TOP_OF_SCOPE,
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
            transform=transforms.AddTransform(
                parent=patch.QualifiedName.from_string("MyClass"),
                content=textwrap.dedent(
                    """
                    a: float
                    def f(self, x: int) -> int: ...
                    """
                ),
                add_position=patch.AddPosition.BOTTOM_OF_SCOPE,
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
            transform=transforms.AddTransform(
                parent=patch.QualifiedName.from_string("MyClass"),
                content=textwrap.dedent(
                    """
                    a: float
                    def f(self, x: int) -> int: ...
                    """
                ),
                add_position=patch.AddPosition.BOTTOM_OF_SCOPE,
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
            transform=transforms.AddTransform(
                parent=patch.QualifiedName.from_string("OuterClass1.InnerClass1"),
                content=textwrap.dedent(
                    """
                    def f(self, x: int) -> int: ...
                    """
                ),
                add_position=patch.AddPosition.BOTTOM_OF_SCOPE,
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

    def test_delete__ann_assign(self) -> None:
        self.assert_transform(
            original_code=(
                """
                x: int
                y: str
                z: float
                """
            ),
            transform=transforms.DeleteTransform(
                parent=patch.QualifiedName.from_string(""),
                name="y",
            ),
            expected_code=(
                """
                x: int
                z: float
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
            transform=transforms.DeleteTransform(
                parent=patch.QualifiedName.from_string(""),
                name="B",
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
            transform=transforms.DeleteTransform(
                parent=patch.QualifiedName.from_string(""),
                name="g",
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
            transform=transforms.DeleteTransform(
                parent=patch.QualifiedName.from_string(""),
                name="g",
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
            transform=transforms.DeleteTransform(
                parent=patch.QualifiedName.from_string("OuterClass1.InnerClass1"),
                name="x",
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

    def test_replace__ann_assign(self) -> None:
        self.assert_transform(
            original_code=(
                """
                x: int
                y: str
                z: float
                """
            ),
            transform=transforms.ReplaceTransform(
                parent=patch.QualifiedName.from_string(""),
                name="y",
                content=textwrap.dedent(
                    """
                    w: str
                    """
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
            transform=transforms.ReplaceTransform(
                parent=patch.QualifiedName.from_string(""),
                name="g",
                content=textwrap.dedent(
                    """
                    T = TypeVar('T')
                    def g(x: T) -> T: ...
                    """
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
            transform=transforms.ReplaceTransform(
                parent=patch.QualifiedName.from_string("OuterClass1.InnerClass1"),
                name="x",
                content="y: float",
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
