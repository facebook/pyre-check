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
      import unittest
      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          self.assertIsNotNone(2)
    |}
    [];
  assert_type_errors
    {|
      import typing
      import unittest
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
      import typing
      import unittest
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertIsNotNone(x, 'x should not be None')
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      import typing
      import unittest
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
      import typing
      import unittest
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
      import typing
      import unittest
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertTrue(x is not None, "x should not be None")
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      import typing
      import unittest
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
      import typing
      import unittest
      class A:
        def __init__(self, x: typing.Optional[int]) -> None:
          self.x = x

      class FakeTest(unittest.TestCase):
        def foo(self) -> None:
          a = A(3)
          x = a.x
          self.assertFalse(x is None, "x should not be None")
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


let test_assert_is context =
  assert_type_errors
    ~context
    {|
      from typing import Type
      class Foo:
        x: int = 1
      def foo(o: Type[object]) -> None:
        if (o is Foo):
          o.x
    |}
    [];
  ()


let test_check_global_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
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
      import typing
      import unittest
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
      import typing
      MY_GLOBAL: typing.Optional[int] = 1

      def foo() -> None:
        if MY_GLOBAL:
          reveal_type(MY_GLOBAL)
    |}
    ["Revealed type [-1]: Revealed type for `MY_GLOBAL` is `typing.Optional[int]`."];
  assert_type_errors
    {|
      import typing
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
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if x:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Union[int, str, None]) -> None:
        if x:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[None, int, str]` (inferred: \
       `typing.Union[int, str]`).";
    ];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Union[int, str, None]) -> None:
        if x is None:
          x = 42
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[None, int, str]` (inferred: \
       `typing.Union[int, str]`).";
    ];
  assert_type_errors
    {|
      import typing
      import unittest
      class FakeTest(unittest.TestCase):
        def foo(self, x: typing.Optional[int]) -> None:
          self.assertIsNotNone(x)
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];
  assert_type_errors
    {|
      import typing
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
      import typing
      def foo() -> None:
        x: typing.Optional[int]
        if x is not None:
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[int]` (inferred: `int`)."];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[str]) -> typing.Optional[str]:
        d = {"a": "a"}
        if x in d:
          reveal_type(x)
          return d[x]
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Optional[str]` (inferred: `str`)."];
  (* We don't actually care about the errors here, just that this terminates *)
  assert_type_errors
    {|
    def f(y):
      while True:
          if y in (None, []):
             pass
          if True:
             pass
    |}
    [
      "Missing return annotation [3]: Return type is not specified.";
      "Missing parameter annotation [2]: Parameter `y` has no type specified.";
    ];
  ()


let test_check_isinstance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if isinstance(x, int):
          reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `int`."];
  assert_type_errors
    {|
      import typing
      MY_GLOBAL: typing.Union[int, str] = 1

      def foo() -> None:
        if isinstance(MY_GLOBAL, str):
          reveal_type(MY_GLOBAL)
    |}
    ["Revealed type [-1]: Revealed type for `MY_GLOBAL` is `typing.Union[int, str]`."];
  assert_type_errors
    {|
      import typing
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
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      import typing
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
      import typing
      def bar(i: typing.Optional[int]) -> bool:
        return i is not None

      def foo(x: typing.List[typing.Optional[int]]) -> None:
        x = [1, 2, 3, 4, None, 5]
        y = [i for i in x if bar(i)]
        assert None not in y
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.List[int]`."];

  (* Invalid assertions *)
  assert_type_errors
    {|
      def foo(x: None) -> None:
        assert None not in x
    |}
    ["Undefined attribute [16]: `None` has no attribute `__getitem__`."];
  assert_type_errors
    {|
      def foo(x: Derp) -> None:
        assert None not in x
    |}
    ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      import typing
      def foo(x: typing.Any) -> None:
        assert None not in x
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.Any`."];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.List[Derp]) -> None:
        assert None not in x
        reveal_type(x)
    |}
    [
      "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
      "Revealed type [-1]: Revealed type for `x` is `unknown`.";
    ];
  assert_default_type_errors
    {|
      import typing
      def foo(x: typing.List[typing.Any]) -> None:
        assert None not in x
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `typing.List[typing.Any]`."];
  ()


let test_check_callable context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Dict, Optional
      class CallableClass:
        def __call__(self, x:int) -> str:
          return "A"
      def foo(x: Dict[int, Optional[CallableClass]]) -> None:
        ret = x[0]
        if callable(ret):
          reveal_type(ret)
    |}
    ["Revealed type [-1]: Revealed type for `ret` is `CallableClass`."];
  assert_type_errors
    {|
      from typing import Dict, Callable, Optional
      def foo(x: Dict[int, Optional[Callable[[], int]]]) -> None:
        ret = x[0]
        if callable(ret):
          reveal_type(ret)
        reveal_type(ret)
    |}
    [
      "Revealed type [-1]: Revealed type for `ret` is `typing.Callable[[], int]`.";
      "Revealed type [-1]: Revealed type for `ret` is `Optional[typing.Callable[[], int]]`.";
    ];
  assert_type_errors
    {|
      from typing import Union, Callable
      def foo(x: Union[Callable[[], int], int]) -> None:
        if callable(x):
          reveal_type(x)
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable[[], int]`.";
      "Revealed type [-1]: Revealed type for `x` is `Union[typing.Callable[[], int], int]`.";
    ];
  assert_type_errors
    {|
      from typing import Union, Type
      class Constructable:
        def __init__(self, x:int) -> None:
          return
      def foo(x: Union[int, Type[Constructable]]) -> None:
        if callable(x):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Type[Constructable]`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ];
  assert_type_errors
    {|
      from typing import Union, Callable
      def foo(x: Union[Callable[[int], str], int]) -> None:
        if not callable(x):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable[[int], str]`.";
    ];
  (* Look into bottoming callable expressions *)
  assert_type_errors
    {|
      from typing import Callable
      def foo(x: Callable[[], int]) -> None:
        if callable(x):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable[[], int]`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable[[], int]`.";
    ];
  assert_type_errors
    {|
      def foo(x:int) -> None:
        if callable(x):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Callable[..., object]`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ];
  ()


let test_check_final_attribute_refinement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Final, Optional

      class Boo: ...

      class Baz:
        z: Final[Optional[Boo]] = None

      class Bar:
        y: Final[Optional[Baz]] = None

      class Foo:
        x: Final[Optional[Bar]] = None

      def bar(foo: Foo) -> None:
        assert (foo.x and foo.x.y and foo.x.y.z)
        reveal_type(foo.x)
        reveal_type(foo.x.y)
        reveal_type(foo.x.y.z)

      def bar2(foo: Foo) -> None:
        # This produces the same underlying Assert as above after normalization.
        if not foo.x or not foo.x.y:
          pass
        else:
          reveal_type(foo.x)
          reveal_type(foo.x.y)
    |}
    [
      "Revealed type [-1]: Revealed type for `foo.x` is `Optional[Bar]` (inferred: `Bar`).";
      "Revealed type [-1]: Revealed type for `foo.x.y` is `Optional[Baz]` (inferred: `Baz`).";
      "Revealed type [-1]: Revealed type for `foo.x.y.z` is `Optional[Boo]` (inferred: `Boo`).";
      "Revealed type [-1]: Revealed type for `foo.x` is `Optional[Bar]` (inferred: `Bar`).";
      "Revealed type [-1]: Revealed type for `foo.x.y` is `Optional[Baz]` (inferred: `Baz`).";
    ];
  assert_type_errors
    {|
      from typing import Optional
      from dataclasses import dataclass
      def expects_str(x: str) -> None:
        pass

      @dataclass(frozen=True)
      class Actor:
        name: Optional[str]
        def __init__(self, name: Optional[str]) -> None:
          self.name = name

      def foo(a: Actor) -> None:
        if a.name is not None:
          expects_str(a.name)
    |}
    [];
  assert_type_errors
    {|
      from typing import Optional, Final
      def expects_str(x: str) -> None:
        pass

      class Actor:
        name: Final[Optional[str]]
        def __init__(self, name: Optional[str]) -> None:
          self.name = name

      def foo(a: Actor) -> None:
        if a.name is not None:
          expects_str(a.name)
    |}
    [];
  assert_type_errors
    {|
      from typing import Optional
      def expects_str(x: str) -> None:
        pass

      class Actor:
        name: Optional[str]
        def __init__(self, name: Optional[str]) -> None:
          self.name = name

      def foo(a: Actor) -> None:
        if a.name is not None:
          expects_str(a.name)
    |}
    [
      "Incompatible parameter type [6]: Expected `str` for 1st positional only parameter to call \
       `expects_str` but got `Optional[str]`.";
    ];
  assert_type_errors
    {|
      from typing import Optional
      from dataclasses import dataclass
      def expects_str(x: str) -> None:
        pass

      @dataclass(frozen=True)
      class Actor:
        name: Optional[str]
        def __init__(self, name: Optional[str]) -> None:
          self.name = name

      def foo(a: Actor) -> None:
        if a.name is not None:
          a = Actor(None)
          expects_str(a.name)
    |}
    [
      "Incompatible parameter type [6]: Expected `str` for 1st positional only parameter to call \
       `expects_str` but got `Optional[str]`.";
    ];
  assert_type_errors
    {|
      from typing import Optional
      from dataclasses import dataclass
      def expects_str(x: str) -> None:
        pass

      @dataclass(frozen=True)
      class Actor:
        name: Optional[str]
        def __init__(self, name: Optional[str]) -> None:
          self.name = name

      def foo(a: Actor) -> None:
        if a.name is not None:
          a = Actor(None)
          if a.name:
            reveal_type(a.name)
          reveal_type(a.name)
    |}
    [
      "Revealed type [-1]: Revealed type for `a.name` is `Optional[str]` (inferred: `str`).";
      "Revealed type [-1]: Revealed type for `a.name` is `Optional[str]`.";
    ];
  assert_type_errors
    {|
      from typing import Optional
      from dataclasses import dataclass

      @dataclass(frozen=True)
      class Base:
        name: Optional[str]
        def __init__(self, name: Optional[str]) -> None:
          self.name = name

      @dataclass(frozen=True)
      class ChildA(Base):
        name: Optional[str]
        age: int
        def __init__(self, name: Optional[str]) -> None:
          self.name = name
          self.age = 0

      @dataclass(frozen=True)
      class ChildB(Base):
        name: Optional[str]
        year: int
        def __init__(self, name: Optional[str]) -> None:
          self.name = name
          self.year = 2020

      def expects_non_optional_and_a(x: str, y: ChildA) -> None:
        pass
      def expects_non_optional_and_b(x: str, y: ChildB) -> None:
        pass

      def foo(o: Base) -> None:
        if o.name:
          if isinstance(o, ChildA):
            expects_non_optional_and_a(o.name, o)
          if isinstance(o, ChildB):
            expects_non_optional_and_b(o.name, o)
    |}
    [];
  assert_type_errors
    {|
      from typing import Union
      from dataclasses import dataclass
      def expects_int(x: int) -> None:
        pass

      @dataclass(frozen=True)
      class Foo:
        x: Union[int, str]
        def __init__(self, x: Union[int, str]) -> None:
          self.x = x

      def f(a: Foo) -> None:
        if isinstance(a.x, int):
          expects_int(a.x)
    |}
    [];
  assert_type_errors
    {|
      from typing import Union
      from dataclasses import dataclass
      def expects_int(x: int) -> None:
        pass

      @dataclass(frozen=True)
      class Foo:
        x: Union[int, str]
        def __init__(self, x: Union[int, str]) -> None:
          self.x = x

      def f(a: Foo) -> None:
        if isinstance(a.x, int):
          a = Foo("bar")
          expects_int(a.x)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
       `expects_int` but got `Union[int, str]`.";
    ];
  assert_type_errors
    {|
      from typing import Union
      from dataclasses import dataclass
      def expects_int(x: int) -> None:
        pass

      @dataclass(frozen=True)
      class Foo:
        x: Union[int, str]
        def __init__(self, x: Union[int, str]) -> None:
          self.x = x

      def f(a: Foo) -> None:
        if type(a.x) is int:
          expects_int(a.x)
        reveal_type(a.x)
    |}
    ["Revealed type [-1]: Revealed type for `a.x` is `Union[int, str]`."];
  assert_type_errors
    {|
      from typing import Union, Callable
      from dataclasses import dataclass
      def expects_int(x: int) -> None:
        pass

      @dataclass(frozen=True)
      class Foo:
        x: Union[int, Callable[[], int]]
        def __init__(self, x: Union[int, Callable[[], int]]) -> None:
          self.x = x

      def f(a: Foo) -> None:
        if callable(a.x):
          reveal_type(a.x)
        reveal_type(a.x)
    |}
    [
      "Revealed type [-1]: Revealed type for `a.x` is `typing.Callable[[], int]`.";
      "Revealed type [-1]: Revealed type for `a.x` is `Union[typing.Callable[[], int], int]`.";
    ];
  assert_type_errors
    {|
      from typing import Union, Callable
      from dataclasses import dataclass
      def expects_int(x: int) -> None:
        pass

      @dataclass(frozen=True)
      class Foo:
        x: Union[int, Callable[[], int]]
        def __init__(self, x: Union[int, Callable[[], int]]) -> None:
          self.x = x

      def f(a: Foo) -> None:
        if callable(a.x):
          a = Foo(42)
          reveal_type(a.x)
        reveal_type(a.x)
    |}
    [
      "Revealed type [-1]: Revealed type for `a.x` is `Union[typing.Callable[[], int], int]`.";
      "Revealed type [-1]: Revealed type for `a.x` is `Union[typing.Callable[[], int], int]`.";
    ];
  assert_type_errors
    {|
    from typing import NamedTuple, Optional
    from dataclasses import dataclass

    class Foo(NamedTuple):
        value: Optional[int]
        other: int = 1

    def f() -> None:
        foo = Foo(value=1)
        reveal_type(foo.value)
        if foo.value:
            reveal_type(foo.value)
    |}
    [
      "Revealed type [-1]: Revealed type for `foo.value` is `Optional[int]`.";
      "Revealed type [-1]: Revealed type for `foo.value` is `Optional[int]` (inferred: `int`).";
    ];
  ()


let () =
  "refinement"
  >::: [
         "check_assert_is_none" >:: test_assert_is_none;
         "check_assert_is" >:: test_assert_is;
         "check_global_refinement" >:: test_check_global_refinement;
         "check_local_refinement" >:: test_check_local_refinement;
         "check_isinstance" >:: test_check_isinstance;
         "check_assert_contains_none" >:: test_assert_contains_none;
         "check_callable" >:: test_check_callable;
         "check_final_attribute_refinement" >:: test_check_final_attribute_refinement;
       ]
  |> Test.run
