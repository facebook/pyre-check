(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_data_class context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo('a')
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`." ];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo(4,5)
    |}
    [ "Too many arguments [19]: Call `Foo.__init__` expects 1 positional argument, "
      ^ "2 were provided." ];
  assert_type_errors
    {|
      @dataclasses.dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo(4,5)
    |}
    [ "Too many arguments [19]: Call `Foo.__init__` expects 1 positional argument, "
      ^ "2 were provided." ];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x = 1
      def boo() -> None:
          b = Foo(2)
    |}
    [ "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` "
      ^ "but no type is specified.";
      "Too many arguments [19]: Call `Foo.__init__` expects 0 positional arguments, 1 was"
      ^ " provided." ];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo()
    |}
    []


let test_check_attr context =
  assert_type_errors
    ~context
    ~update_environment_with:
      [ {
          handle = "attr/__init__.pyi";
          source =
            {|
        _T = typing.TypeVar("T")
        class Attribute(typing.Generic[_T]):
          name: str
          default: Optional[_T]
          validator: Optional[_ValidatorType[_T]]
        def s( *args, **kwargs) -> typing.Any: ...
        def ib(default: _T) -> _T: ...
      |};
        } ]
    {|
      import attr
      @attr.s
      class C:
        x: typing.Optional[int] = attr.ib(default=None)
        @x.validator
        def check(self, attribute: attr.Attribute[int], value: typing.Optional[int]) -> None:
          pass
    |}
    []


let () =
  "dataclass"
  >::: ["check_dataclass" >:: test_check_data_class; "check_attr" >:: test_check_attr]
  |> Test.run
