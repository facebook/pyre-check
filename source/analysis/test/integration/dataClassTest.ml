(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_dataclasses =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo('a')
         |}
           [
             "Incompatible parameter type [6]: In call `Foo.__init__`, for 1st positional \
              argument, expected `int` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo(4,5)
         |}
           [
             "Too many arguments [19]: Call `Foo.__init__` expects 1 positional argument, "
             ^ "2 were provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           import dataclasses
           @dataclasses.dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo(4,5)
         |}
           [
             "Too many arguments [19]: Call `Foo.__init__` expects 1 positional argument, "
             ^ "2 were provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x = 1
           def boo() -> None:
               b = Foo(2)
         |}
           [
             "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` "
             ^ "but no type is specified.";
             "Too many arguments [19]: Call `Foo.__init__` expects 0 positional arguments, 1 was"
             ^ " provided.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             x: int = 1
           def boo() -> None:
               b = Foo()
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Foo():
             dangan: int
           def boo() -> None:
               b = Foo(1)
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class Base():
             x: int
           class Child(Base):
             pass
           def boo() -> None:
               b = Child(1)
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           from typing import Dict, Any
           @dataclass(frozen=False)
           class Base:
             x: str

             def __init__(
                 self,
                 *,
                 x: str,
             ) -> None:
                 self.x = x

             def as_dict(self) -> Dict[str, Any]:
                 x = self.x
                 return {
                     "x": x,
                 }
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=True)
           class Base:
             x: float
           @dataclass(frozen=True)
           class Child(Base):
             x: int = 1
             y: str
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           from placeholder_stub import X
           @dataclass
           class Foo(X):
             x: int = 1
           def boo() -> None:
               b = Foo(1)
         |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=True)
           class F:
             x = 1
         |}
           [
             "Missing attribute annotation [4]: Attribute `x` of class `F` has type `int` but no \
              type is specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from typing import ClassVar
           from dataclasses import dataclass
           @dataclass
           class A:
             x: ClassVar[int] = 42
             y: str = "a"
           A("a")
         |}
           [];
      (* Actually a test of descriptors to make sure it doesn't infinitely loop *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass
           class D:
             x: C = C()
           @dataclass
           class C:
             x: D = D()
           def foo() -> None:
             reveal_type(D().x)
             reveal_type(D().x.x.x)
         |}
           [
             "Revealed type [-1]: Revealed type for `test.D().x` is `C`.";
             "Revealed type [-1]: Revealed type for `test.D().x.x.x` is `C`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(kw_only=True)
           class A:
             x: int
           reveal_type(A.__init__)
         |}
           [
             "Revealed type [-1]: Revealed type for `test.A.__init__` is \
              `typing.Callable(A.__init__)[[Named(self, A), KeywordOnly(x, int)], None]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass, KW_ONLY
           @dataclass
           class A:
             x: int
             _: KW_ONLY
             y: int
           reveal_type(A.__init__)
         |}
           [
             "Revealed type [-1]: Revealed type for `test.A.__init__` is \
              `typing.Callable(A.__init__)[[Named(self, A), Named(x, int), KeywordOnly(y, int, \
              default)], None]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=True)
           class A:
             x: int
           a = A(x=42)
           a.x = 43
         |}
           ["Invalid assignment [41]: Cannot reassign final attribute `a.x`."];
      (* TODO(T178998636) Do not allow InitVar attributes to be accessed *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass, InitVar
           @dataclass
           class A:
             x: int
             y: InitVar[int]

           a = A(x=42, y=42)
           reveal_type(a.x)
           reveal_type(a.y)
         |}
           [
             "Uninitialized attribute [13]: Attribute `y` is declared in class `A` to have type \
              `InitVar[int]` but is never initialized.";
             "Revealed type [-1]: Revealed type for `a.x` is `int`.";
             "Revealed type [-1]: Revealed type for `a.y` is `InitVar[int]`.";
           ];
      (* TODO(T178998636) Do not allow `frozen` dataclasses to inherit from non-frozen. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from dataclasses import dataclass
           @dataclass(frozen=False)
           class A:
             x: int
           @dataclass(frozen=True)
           class B:
             y: str
         |}
           [];
    ]


let test_check_attrs =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~other_sources:
             [
               {
                 handle = "attr/__init__.pyi";
                 source =
                   {|
                    import typing
                    _T = typing.TypeVar("T")
                    class Attribute(typing.Generic[_T]):
                      name: str
                      default: Optional[_T]
                      validator: Optional[_ValidatorType[_T]]
                    def s( *args, **kwargs) -> typing.Any: ...
                    def ib(default: _T) -> _T: ...
                  |};
               };
             ]
           {|
           import typing
           import attr
           @attr.s
           class C:
             x: typing.Optional[int] = attr.ib(default=None)
             @x.validator
             def check(self, attribute: attr.Attribute[int], value: typing.Optional[int]) -> None:
               pass
         |}
           [];
    ]


let () = "dataclassTest.ml" >::: [test_check_dataclasses; test_check_attrs] |> Test.run
