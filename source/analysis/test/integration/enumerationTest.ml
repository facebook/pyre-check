(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_enumeration_inheritance =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from enum import Enum

              class NoDefinedMembers(Enum):
                pass

              class HasDefinedMembers(NoDefinedMembers):
                RED = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from enum import Enum

              class HasDefinedMembers(Enum):
                RED = 1

              class InvalidChildEnum(HasDefinedMembers):
                pass
            |}
           [
             "Invalid inheritance [39]: Cannot inherit from final enum `HasDefinedMembers`. Enums \
              with defined members cannot be extended.";
           ];
      (* StringEnum should be extensible
         https://www.internalfb.com/code/fbsource/[18e22aca0c7dcf600fa15510a72af801118ff07b]/fbcode/instagram-server/distillery/util/enum.pyi?lines=13 *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from enum import Enum

              class StringEnum(Enum):
                value: str = ...

              class OtherStringEnum(StringEnum):
                pass
            |}
           [];
    ]


let test_enumeration_methods =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import reveal_type
              from enum import Enum, member

              class MyEnum(Enum):
                @member
                def foo(self) -> None:
                  pass

              reveal_type(MyEnum.foo)
            |}
           [
             "Revealed type [-1]: Revealed type for `test.MyEnum.foo` is \
              `typing_extensions.Literal[MyEnum.foo]` (final).";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import reveal_type
              from enum import Enum

              class Color(Enum):
                RED = 1
                GREEN = 2
                BLUE = 3

              reveal_type(Color.RED)
            |}
           [
             "Revealed type [-1]: Revealed type for `test.Color.RED` is \
              `typing_extensions.Literal[Color.RED]` (final).";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import reveal_type
              from enum import IntEnum

              class Color(IntEnum):
                RED = 1
                GREEN = 2
                BLUE = 3

              reveal_type(Color.RED._value_)
            |}
           ["Revealed type [-1]: Revealed type for `test.Color.RED._value_` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import reveal_type
              from enum import IntEnum

              class Color(IntEnum):
                RED = 1
                GREEN = 2
                BLUE = 3

              reveal_type(Color.RED.value)
            |}
           ["Revealed type [-1]: Revealed type for `test.Color.RED.value` is `int` (final)."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
            from enum import Enum

            class UnloadModelEvent(Enum):
                UNLOAD_MODEL_SCHEDULED = 0
                UNLOADING = 1

            class LoadModelEvent(Enum):
                LOAD_SCHEDULED = 0
                LOADING = 1


            def fn(e: Enum) -> str:
                return e.name

            fn(UnloadModelEvent.UNLOADING)
            fn(LoadModelEvent.LOADING.name)
            |}
           [
             "Incompatible parameter type [6]: In call `fn`, for 1st positional argument, expected \
              `Enum` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import reveal_type
              from enum import EnumMeta

              class CustomEnumType(EnumMeta):
                pass

              class CustomEnum(metaclass=CustomEnumType):
                pass

              class Color(CustomEnum):
                RED = 1
                GREEN = 2
                BLUE = 3

              reveal_type(Color.RED)
            |}
           [
             "Revealed type [-1]: Revealed type for `test.Color.RED` is \
              `typing_extensions.Literal[Color.RED]` (final).";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class C(enum.Enum):
                A = 1
              reveal_type(C.A)
              reveal_type(C.__members__)
            |}
           [
             "Revealed type [-1]: Revealed type for `test.C.A` is `typing_extensions.Literal[C.A]` \
              (final).";
             "Revealed type [-1]: Revealed type for `test.C.__members__` is \
              `BoundMethod[typing.Callable(enum.EnumMeta.__members__)[[Named(self, \
              typing.Type[Variable[enum._T]])], typing.Mapping[str, Variable[enum._T]]], \
              typing.Type[C]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class C(enum.IntEnum):
                A = 1
              reveal_type(C.A)
              reveal_type(C.__members__)
            |}
           [
             "Revealed type [-1]: Revealed type for `test.C.A` is `typing_extensions.Literal[C.A]` \
              (final).";
             "Revealed type [-1]: Revealed type for `test.C.__members__` is \
              `BoundMethod[typing.Callable(enum.EnumMeta.__members__)[[Named(self, \
              typing.Type[Variable[enum._T]])], typing.Mapping[str, Variable[enum._T]]], \
              typing.Type[C]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class Foo(enum.IntEnum):
                A: int = 1
              class Bar:
                A = Foo.A.value
            |}
           [
             "Missing attribute annotation [4]: Attribute `A` of class `Bar` has type `int` but no \
              type is specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class C(enum.Enum):
                ...
              def f() -> None:
                all_cases = [kind for kind in C]
                reveal_type(all_cases)
            |}
           ["Revealed type [-1]: Revealed type for `all_cases` is `typing.List[C]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class Foo(enum.IntEnum):
                A: int = 1
              def f() -> None:
                b = 2 in Foo
                reveal_type(b)
            |}
           ["Revealed type [-1]: Revealed type for `b` is `bool`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class StringEnum(enum.Enum, str):
                pass
              class Foo(StringEnum):
                A = "A"
              def f() -> None:
                b = "other" in Foo
                reveal_type(b)
            |}
           ["Revealed type [-1]: Revealed type for `b` is `bool`."];
    ]


let test_check_enumeration_attributes =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from enum import Enum

              class C(Enum):
                _value_: str
                X = "X"
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from enum import Enum

              class C(Enum):
                _value_: str
                X = 1
            |}
           [
             "Incompatible attribute type [8]: Attribute `X` declared in class `C` has type `str` \
              but is used as type `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from enum import Enum

              class C(Enum):
                _value_: str
                def __init__(self) -> None:
                  self._value_ = 1
            |}
           [
             "Incompatible attribute type [8]: Attribute `_value_` declared in class `C` has type \
              `str` but is used as type `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum

              class C(enum.IntEnum):
                a: int = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum

              class C(enum.IntEnum):
                a = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum

              class C(enum.IntEnum):
                a: int
            |}
           [
             "Uninitialized attribute [13]: Attribute `a` is declared in class `C` to have type \
              `int` but is never initialized.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class C(enum.IntEnum):
                a: str = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class C(enum.Enum):
                a = enum.auto()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class Color(enum.Enum):
                RED = "red"
                BLUE = "blue"

              def foo() -> Color:
                return Color.RED

              def bar() -> str:
                return Color.RED
            |}
           ["Incompatible return type [7]: Expected `str` but got `Color`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class Color(enum.Enum):
                RED = "red"
                BLUE = "blue"

              def foo() -> None:
                x = Color.PURPLE
            |}
           ["Undefined attribute [16]: `Color` has no attribute `PURPLE`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              from typing import List
              class A(enum.Enum):
                ONE = ("un", ["ein"])
                TWO = ("deux", ["zwei", ""])

                def __init__(self, name: str, example: List[str]) -> None:
                  self.example: List[str] = example

                reveal_type(A.ONE.example)
              |}
           ["Revealed type [-1]: Revealed type for `test.A.ONE.example` is `List[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import enum
              class A(enum.Enum):
                  x = ""
                  def __init__(self, _) -> None:
                      self.x: str = "another string"
              reveal_type(A.x)
            |}
           ["Revealed type [-1]: Revealed type for `test.A.x` is `str`."];
    ]


let test_functional_syntax =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from enum import Enum

              Color = Enum("Color", ("RED", "GREEN", "BLUE"))

              def main(x: Color) -> None:
                y: Color = Color.RED
                reveal_type(x)
                reveal_type(y)
                reveal_type(x.value)
            |}
           [
             "Revealed type [-1]: Revealed type for `x` is `Color`.";
             "Revealed type [-1]: Revealed type for `y` is `Color` (inferred: \
              `typing_extensions.Literal[Color.RED]`).";
             "Revealed type [-1]: Revealed type for `x.value` is `unknown` (final).";
           ];
    ]


let () =
  "enumeration"
  >::: [
         test_check_enumeration_attributes;
         test_enumeration_methods;
         test_functional_syntax;
         test_enumeration_inheritance;
       ]
  |> Test.run
