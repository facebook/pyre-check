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
