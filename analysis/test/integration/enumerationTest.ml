(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_enumeration_methods _ =
  assert_type_errors
    {|
      class C(enum.Enum):
        A = 1
      reveal_type(C.A)
      reveal_type(C.__members__)
    |}
    [ "Revealed type [-1]: Revealed type for `C.A` is `C`.";
      "Revealed type [-1]: Revealed type for `C.__members__` is \
       `typing.Callable(enum.EnumMeta.__members__)[[], unknown]`." ];
  assert_type_errors
    {|
      class C(enum.IntEnum):
        A = 1
      reveal_type(C.A)
      reveal_type(C.__members__)
    |}
    [ "Revealed type [-1]: Revealed type for `C.A` is `C`.";
      "Revealed type [-1]: Revealed type for `C.__members__` is \
       `typing.Callable(enum.EnumMeta.__members__)[[], unknown]`." ];
  assert_type_errors
    {|
      class Foo(enum.IntEnum):
        A: int = 1
      class Bar:
        A = Foo.A.value
    |}
    [ "Missing attribute annotation [4]: Attribute `A` of class `Bar` has type `int` but no type \
       is specified." ];
  assert_type_errors
    {|
      class C(enum.Enum):
        ...
      def f() -> None:
        all_cases = [kind for kind in C]
        reveal_type(all_cases)
    |}
    ["Revealed type [-1]: Revealed type for `all_cases` is `typing.List[C]`."]


let test_check_enumeration_attributes _ =
  assert_type_errors {|
      class C(enum.IntEnum):
        a: int = 1
    |} [];
  assert_type_errors {|
      class C(enum.IntEnum):
        a = 1
    |} [];
  assert_type_errors
    {|
      class C(enum.IntEnum):
        a: int
    |}
    [ "Uninitialized attribute [13]: Attribute `a` is declared in class `C` to have type `C` but \
       is never initialized." ];
  assert_type_errors
    {|
      class C(enum.IntEnum):
        a: str = 1
    |}
    [ "Incompatible attribute type [8]: Attribute `a` declared in class `C` has type `str` but is \
       used as type `int`." ];
  assert_type_errors {|
      class C(enum.Enum):
        a = enum.auto()
    |} [];
  assert_type_errors
    {|
      class Color(enum.Enum):
        RED = "red"
        BLUE = "blue"

      def foo() -> Color:
        return Color.RED

      def bar() -> str:
        return Color.RED
    |}
    ["Incompatible return type [7]: Expected `str` but got `Color`."];
  assert_type_errors
    {|
      class Color(enum.Enum):
        RED = "red"
        BLUE = "blue"

      def foo() -> None:
        x = Color.PURPLE
    |}
    ["Undefined attribute [16]: `Color` has no attribute `PURPLE`."];
  ()


let () =
  "enumeration"
  >::: [ "enumeration_attributes" >:: test_check_enumeration_attributes;
         "enumeration_methods" >:: test_enumeration_methods ]
  |> Test.run
