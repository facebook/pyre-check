(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_assert_is_none context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          self.assertIsNotNone(2)
    |}
    [];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertIsNotNone(x)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self, iter: typing.List[A]) -> None:
          a = None
          for i in iter:
            a = i
          self.assertIsNotNone(a)
          attribute = a.x
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.x = 1

      def foo(f: Foo) -> None:
        assert f.x is None
        reveal_type(f.x)
    |}
    ["Revealed type [-1]: Revealed type for `f.x` is `int`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertTrue(x is not None)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertFalse(x is None)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      from dataclasses import dataclass
      from typing import Optional, Final
      class NormalClass():
        x: Optional[int] = None
      class ClassWithFinalAttribute():
        def __init__(self, x: Optional[int]) -> None:
          self.x: Final[Optional[int]] = x
      @dataclass
      class UnfrozenDataClass():
        x: Optional[int]
      @dataclass(frozen=True)
      class FrozenDataClass():
        x: Optional[int]
      class ReadOnlyPropertyClass():
        state: bool = True
        @property
        def x(self) -> Optional[int]:
          self.state = not self.state
          if self.state:
            return None
          else:
            return 8
      def foo() -> None:
        normal_class: Final[NormalClass]
        class_with_final_attribute: Final[ClassWithFinalAttribute]
        unfrozen_dataclass: Final[UnfrozenDataClass]
        frozen_dataclass: Final[FrozenDataClass]
        read_only_property_class: Final[ReadOnlyPropertyClass]
        if normal_class.x is not None:
          reveal_type(normal_class.x)
        if class_with_final_attribute.x is not None:
          reveal_type(class_with_final_attribute.x)
        if unfrozen_dataclass.x is not None:
          reveal_type(unfrozen_dataclass.x)
        if frozen_dataclass.x is not None:
          reveal_type(frozen_dataclass.x)
        if read_only_property_class.x is not None:
          reveal_type(read_only_property_class.x)
    |}
    [
      "Revealed type [-1]: Revealed type for `normal_class.x` is `Optional[int]`.";
      "Revealed type [-1]: Revealed type for `class_with_final_attribute.x` is `Optional[int]` \
       (inferred: `int`).";
      "Revealed type [-1]: Revealed type for `unfrozen_dataclass.x` is `Optional[int]`.";
      "Revealed type [-1]: Revealed type for `frozen_dataclass.x` is `Optional[int]` (inferred: \
       `int`).";
      "Revealed type [-1]: Revealed type for `read_only_property_class.x` is `Optional[int]`.";
    ];
  assert_type_errors
    {|
      from dataclasses import dataclass
      from typing import Optional, Final
      class NormalClass():
        x: Optional[int] = None
      class ClassWithFinalAttribute():
        def __init__(self, x: Optional[int]) -> None:
          self.x: Final[Optional[int]] = x
      @dataclass
      class UnfrozenDataClass():
        x: Optional[int]
      @dataclass(frozen=True)
      class FrozenDataClass():
        x: Optional[int]
      class ReadOnlyPropertyClass():
        state: bool = True
        @property
        def x(self) -> Optional[int]:
          self.state = not self.state
          if self.state:
            return None
          else:
            return 8
      def foo() -> None:
        normal_class: Final[NormalClass] = ...
        class_with_final_attribute: Final[ClassWithFinalAttribute] = ...
        unfrozen_dataclass: Final[UnfrozenDataClass] = ...
        frozen_dataclass: Final[FrozenDataClass] = ...
        read_only_property_class: Final[ReadOnlyPropertyClass] = ...
        if normal_class.x is None:
          reveal_type(normal_class.x)
        if class_with_final_attribute.x is None:
          reveal_type(class_with_final_attribute.x)
        if unfrozen_dataclass.x is None:
          reveal_type(unfrozen_dataclass.x)
        if frozen_dataclass.x is None:
          reveal_type(frozen_dataclass.x)
        if read_only_property_class.x is None:
          reveal_type(read_only_property_class.x)
    |}
    [
      "Revealed type [-1]: Revealed type for `normal_class.x` is `Optional[int]`.";
      "Revealed type [-1]: Revealed type for `class_with_final_attribute.x` is `None`.";
      "Revealed type [-1]: Revealed type for `unfrozen_dataclass.x` is `Optional[int]`.";
      "Revealed type [-1]: Revealed type for `frozen_dataclass.x` is `None`.";
      "Revealed type [-1]: Revealed type for `read_only_property_class.x` is `Optional[int]`.";
    ];
  assert_type_errors
    {|
      from dataclasses import dataclass
      from typing import Optional, Final
      class NormalClass():
        x: float = 3.14
      class ClassWithFinalAttribute():
        def __init__(self, x: float) -> None:
          self.x: Final[float] = x
      @dataclass
      class UnfrozenDataClass():
        x: float
      @dataclass(frozen=True)
      class FrozenDataClass():
        x: float
      class ReadOnlyPropertyClass():
        state: bool = True
        @property
        def x(self) -> float:
          self.state = not self.state
          if self.state:
            return 8.2
          else:
            return 8
      def foo() -> None:
        normal_class: Final[NormalClass] = ...
        class_with_final_attribute: Final[ClassWithFinalAttribute] = ...
        unfrozen_dataclass: Final[UnfrozenDataClass] = ...
        frozen_dataclass: Final[FrozenDataClass] = ...
        read_only_property_class: Final[ReadOnlyPropertyClass] = ...
        if isinstance(normal_class.x, int):
          reveal_type(normal_class.x)
        if isinstance(class_with_final_attribute.x, int):
          reveal_type(class_with_final_attribute.x)
        if isinstance(unfrozen_dataclass.x, int):
          reveal_type(unfrozen_dataclass.x)
        if isinstance(frozen_dataclass.x, int):
          reveal_type(frozen_dataclass.x)
        if isinstance(read_only_property_class.x, int):
          reveal_type(read_only_property_class.x)
    |}
    [
      "Revealed type [-1]: Revealed type for `normal_class.x` is `float`.";
      "Revealed type [-1]: Revealed type for `class_with_final_attribute.x` is `int`.";
      "Revealed type [-1]: Revealed type for `unfrozen_dataclass.x` is `float`.";
      "Revealed type [-1]: Revealed type for `frozen_dataclass.x` is `int`.";
      "Revealed type [-1]: Revealed type for `read_only_property_class.x` is `float`.";
    ];
  assert_type_errors
    {|
      from dataclasses import dataclass
      from typing import Optional, Final
      @dataclass(frozen=True)
      class InnerFrozenDataClass():
        x: Optional[int]
      @dataclass(frozen=True)
      class FrozenDataClass():
        inner: InnerFrozenDataClass
      @dataclass
      class UnfrozenDataClass():
        inner: InnerFrozenDataClass
      def foo() -> None:
        unfrozen_dataclass: Final[UnfrozenDataClass] = ...
        frozen_dataclass: Final[FrozenDataClass] = ...
        if unfrozen_dataclass.inner.x is not None:
          reveal_type(unfrozen_dataclass.inner.x)
        if frozen_dataclass.inner.x is not None:
          reveal_type(frozen_dataclass.inner.x)
    |}
    [
      "Revealed type [-1]: Revealed type for `unfrozen_dataclass.inner.x` is `Optional[int]`.";
      "Revealed type [-1]: Revealed type for `frozen_dataclass.inner.x` is `Optional[int]` \
       (inferred: `int`).";
    ];
  ()


let test_check_global_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      def foo() -> None:
        a = A(3)
        if a.x:
          reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `typing.Optional[int]`."];
  assert_type_errors
    {|
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          self.assertIsNotNone(a.x)
          reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `typing.Optional[int]`."];
  assert_type_errors
    {|
      MY_GLOBAL: typing.Optional[int] = 1

      def foo() -> None:
        if MY_GLOBAL:
          reveal_type(MY_GLOBAL)
    |}
    ["Revealed type [-1]: Revealed type for `MY_GLOBAL` is `typing.Optional[int]`."];

  assert_type_errors
    {|
      x: typing.Optional[int] = 1

      def foo() -> None:
        global x
        x = 1
        if x is not None:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]`."]


let test_check_local_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
        if x:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];

  assert_type_errors
    {|
      class FakeTest(unittest.TestCase):
        def foo(self, x: typing.Optional[int]) -> None:
          self.assertIsNotNone(x)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];

  assert_type_errors
    {|
      x: typing.Optional[int] = 1

      def foo(test: bool) -> None:
        if test:
          x = 1
        else:
          x = None
        if x:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing_extensions.Literal[1]`."];

  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Optional[int]
        if x is not None:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];

  assert_type_errors
    {|
      def foo(x: typing.Optional[str]) -> typing.Optional[str]:
        d = {"a": "a"}
        if x in d:
          reveal_type(x)
          return d[x]
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[str]` (inferred: `str`)."]


let test_check_isinstance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
        if isinstance(x, int):
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];

  assert_type_errors
    {|
      MY_GLOBAL: typing.Union[int, str] = 1

      def foo() -> None:
        if isinstance(MY_GLOBAL, str):
          reveal_type(MY_GLOBAL)
    |}
    ["Revealed type [-1]: Revealed type for `MY_GLOBAL` is `typing.Union[int, str]`."];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.x: typing.Union[int, str] = 1

      def foo(f: Foo) -> None:
        if isinstance(f.x, str):
          reveal_type(f.x)
    |}
    ["Revealed type [-1]: Revealed type for `f.x` is `typing.Union[int, str]`."]


let test_assert_contains_none context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(x: typing.List[typing.Optional[int]]) -> None:
        assert None not in x
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.List[typing.Optional[int]]` (inferred: \
       `typing.List[int]`).";
    ];

  assert_type_errors
    {|
      def bar(i: typing.Optional[int]) -> bool:
        return i is not None

      def foo(x: typing.List[typing.Optional[int]]) -> None:
        x = [1, 2, 3, 4, None, 5]
        y = [i for i in x if bar(i)]
        assert None not in y
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.List[int]`."]


let () =
  "assert_is_not_none"
  >::: [
         "check_assert_is_none" >:: test_assert_is_none;
         "check_global_refinement" >:: test_check_global_refinement;
         "check_local_refinement" >:: test_check_local_refinement;
         "check_isinstance" >:: test_check_isinstance;
         "check_assert_contains_none" >:: test_assert_contains_none;
       ]
  |> Test.run
