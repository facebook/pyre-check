(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_undefined_type =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Derp) -> Herp:
                pass
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
           ];
      (* Don't crash when returning a bad type. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(a: gurbage) -> None:
                return a
            |}
           ["Unbound name [10]: Name `gurbage` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(a: gurbage) -> int:
                a = 1
                return a
            |}
           ["Unbound name [10]: Name `gurbage` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Derp, y: Herp) -> None:
                pass
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: int) -> Herp:
                return x
            |}
           ["Unbound name [10]: Name `Herp` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              import typing
              def foo(x: typing.Union[Derp, Herp]) -> typing.List[Herp]:
                pass
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Derp[int]) -> None:
                pass
            |}
           ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Derp[int, str]) -> None:
                pass
            |}
           ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              import typing
              def foo(x: typing.Optional[Derp[int]]) -> typing.List[Herp]:
                pass
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Optional) -> None:
                pass
            |}
           ["Unbound name [10]: Name `Optional` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Optional[Any]) -> None:
                pass
            |}
           [
             "Unbound name [10]: Name `Optional` is used but not defined in the current scope.";
             "Unbound name [10]: Name `Any` is used but not defined in the current scope.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Dict) -> None:
                pass
            |}
           ["Unbound name [10]: Name `Dict` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo() -> None:
                x: undefined = 1
                return
            |}
           ["Unbound name [10]: Name `undefined` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              def foo(x: Derp) -> None:
                y: undefined = 1
                return
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Unbound name [10]: Name `undefined` is used but not defined in the current scope.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar('T')
              def foo(x: T) -> typing.Union[str, T]:
                return x
            |}
           [];
      (* Ensure other errors are not missed when undefined type is thrown. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import override
              class Bar:
                  async def undefined(self, x: Derp) -> Derp:
                      return x
              class Foo(Bar):
                  def error(self) -> int:
                      return None
                  @override
                  async def undefined(self, x: Herp) -> Herp:
                      return x
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Incompatible return type [7]: Expected `int` but got `None`.";
             "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              import typing
              def foo() -> typing.Optional["Herp"]:
                return None
            |}
           ["Undefined or invalid type [11]: Annotation `Herp` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           ~enable_strict_any_check:true
           {|
              import typing
              class Foo:
                def __getitem__(self, other) -> typing.Any: ...

              def foo() -> Foo["Herp"]:
                return 1
            |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
             "Missing parameter annotation [2]: Parameter `other` has no type specified.";
             "Undefined or invalid type [11]: Annotation `Herp` is not defined as a type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              import typing as t

              def foo() -> t.Set:
                return set()
            |}
           ["Invalid type parameters [24]: Generic type `set` expects 1 type parameter."];
      (* Attributes *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Foo:
                x: int = 1
                y: Derp = 1

                def __init__(self) -> None:
                  self.z: Herp = 1
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Unbound name [10]: Name `Herp` is used but not defined in the current scope.";
           ];
      (* Class bases *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Foo(Bar): ...
            |}
           ["Unbound name [10]: Name `Bar` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              _T = typing.TypeVar('_T')
              class Foo(Generic[_T]): ...
            |}
           [
             "Unbound name [10]: Name `Generic` is used but not defined in the current scope.";
             "Invalid type variable [34]: The current class isn't generic with respect to the type \
              variable `Variable[_T]`. To reference the type variable, you can modify the class to \
              inherit from `typing.Generic[_T]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class AA: ...
              class CC: ...
              class Foo(AA, BB, CC, DD): ...
            |}
           [
             "Unbound name [10]: Name `BB` is used but not defined in the current scope.";
             "Unbound name [10]: Name `DD` is used but not defined in the current scope.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class AA: ...
              class CC(BB): ...
              class Foo(AA, BB, CC, DD): ...
            |}
           [
             "Unbound name [10]: Name `BB` is used but not defined in the current scope.";
             "Unbound name [10]: Name `DD` is used but not defined in the current scope.";
           ];
      (* Globals *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              x: Derp = 1
              y: typing.List[Derp] = 1
              z: Derp
            |}
           ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
      (* Literals *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing_extensions import Literal
              x = 1
              test: Literal[x] = 1
            |}
           ["Invalid type [31]: Expression `x` is not a literal value."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing_extensions import Literal

              def foo() -> None:
                x = 1
                test: Literal[x] = 1
                valid: Literal[1] = 1
            |}
           ["Invalid type [31]: Expression `foo.x` is not a literal value."];
      (* Assigns *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: Derp = 1
                y: typing.List[Derp] = 1
                z: Derp
            |}
           ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
      (* cast, isinstance *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: int = 1
                typing.cast(Derp, x)
            |}
           ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              Derp = typing.Any
              Herp = typing.List[typing.Any]
              def foo() -> None:
                x: int = 1
                typing.cast(Derp, x)
                typing.cast(Herp, x)
            |}
           [
             "Prohibited any [33]: `Derp` cannot alias to `Any`.";
             "Prohibited any [33]: `Herp` cannot alias to a type containing `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              def foo() -> None:
                x: int = 1
                if isinstance(x, Derp):
                  return x
                return

            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Incompatible return type [7]: Expected `None` but got `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~other_sources:
             [
               { Test.handle = "a.py"; source = "class A: pass" };
               { Test.handle = "b.py"; source = "from a import A as B" };
             ]
           {|
             import b
             def test() -> b.B: ...
           |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~other_sources:
             [
               {
                 Test.handle = "a.pyi";
                 source = "from typing import Any\ndef __getattr__(name: str) -> Any: ...";
               };
             ]
           {|
             import a
             def test() -> a.A: ...
           |}
           [];
    ]


let test_check_invalid_type =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              MyType = int
              x: MyType = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing_extensions
              MyType: typing_extensions.TypeAlias = int
              x: MyType = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              MyType: typing.TypeAlias = int
              x: MyType = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              # Type aliases cannot be annotated
              MyType: typing.Type[int] = int
              x: MyType = 1
            |}
           ["Undefined or invalid type [11]: Annotation `MyType` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              x: MyType = 1
            |}
           ["Unbound name [10]: Name `MyType` is used but not defined in the current scope."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              MyType: int
              x: MyType = 1
            |}
           ["Undefined or invalid type [11]: Annotation `MyType` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              MyType = 1
              x: MyType = 1
            |}
           ["Undefined or invalid type [11]: Annotation `MyType` is not defined as a type."];
      (* Type aliases cannot be nested *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                def foo() -> None:
                  MyType = int
                  x: MyType = 1
              |}
           ["Undefined or invalid type [11]: Annotation `foo.MyType` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Foo:
                X = int
              x: Foo.X = ...
            |}
           ["Undefined or invalid type [11]: Annotation `Foo.X` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                import typing_extensions
                def foo() -> None:
                  MyType: typing_extensions.TypeAlias = int
                  x: MyType = 1
              |}
           [
             "Invalid type [31]: Expression `MyType` is not a valid type. All type alias \
              declarations must be made in the module definition.";
             "Undefined or invalid type [11]: Annotation `foo.MyType` is not defined as a type.";
           ];
      (* Type aliases to Any *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              MyType: typing.Any
              x: MyType = 1
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `MyType` "
             ^ "must be specified as type other than `Any`.";
             "Undefined or invalid type [11]: Annotation `MyType` is not defined as a type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              MyType = typing.Any
              x: MyType = 1
            |}
           ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              MyType = typing.Any
              x: typing.List[MyType] = [1]
            |}
           ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
      (* Un-parseable expressions *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> (int, str):
                return 1
            |}
           ["Invalid type [31]: Expression `(int, str)` is not a valid type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int + str) -> None:
                return
            |}
           ["Invalid type [31]: Expression `int + str` is not a valid type."];
      (* Using expressions of type meta-type: only OK in isinstance *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def f(my_type: typing.Type[int]) -> None:
               x: my_type = ...
            |}
           ["Undefined or invalid type [11]: Annotation `my_type` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def f(my_type: typing.Type[int]) -> None:
               y = typing.cast(my_type, "string")
            |}
           ["Undefined or invalid type [11]: Annotation `my_type` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def takes_exception(x: Exception) -> None: ...
              def f(exception_type: typing.Type[Exception]) -> None:
               try:
                 pass
               except exception_type as myexception:
                 takes_exception(myexception)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def takes_exception(x: Exception) -> None: ...
              def f(int_type: typing.Type[int]) -> None:
               try:
                 pass
               except int_type as myexception:
                 takes_exception(myexception)
            |}
           [
             "Invalid except clause [66]: Exception handler type annotation `int` must extend \
              BaseException.";
             "Incompatible parameter type [6]: In call `takes_exception`, for 1st positional \
              argument, expected `Exception` but got `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def takes_exception(x: BaseException) -> None: ...
              def f(exception_group_type: typing.Type[ExceptionGroup[Exception]]) -> None:
               try:
                 pass
               except exception_group_type as myexception:
                 takes_exception(myexception)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def takes_exception(x: BaseException) -> None: ...
              def f(exception_group_type: typing.Type[ExceptionGroup[Exception]]) -> None:
               try:
                 pass
               except* exception_group_type as myexception:
                 takes_exception(myexception)
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `ExceptionGroup[Exception]` may not extend BaseExceptionGroup.";
             "Undefined attribute [16]: `ExceptionGroup` has no attribute `__getitem__`.";
             "Incompatible parameter type [6]: In call `takes_exception`, for 1st positional \
              argument, expected `BaseException` but got `unknown`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def takes_exception(x: BaseException) -> None: ...
              def f(exception_types: typing.Tuple[typing.Type[Exception]]) -> None:
               try:
                 pass
               except exception_types as myexceptions:
                 takes_exception(myexceptions)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import ClassVar, Tuple, Type
              class Foo:
                EXCEPTIONS: ClassVar[Tuple[Type[Exception]]] = (KeyError,)
                VARIADIC_EXCEPTIONS: ClassVar[Tuple[Type[Exception], ...]] = ...
                VALUE_ERROR: ClassVar[Type[Exception]] = ValueError

              def takes_exception(x: BaseException) -> None: ...
              def f() -> None:
               try:
                 pass
               except Foo.EXCEPTIONS as myexception:
                 takes_exception(myexception)
               except Foo.VARIADIC_EXCEPTIONS as myexception:
                 takes_exception(myexception)
               except Foo.VALUE_ERROR as myexception:
                 takes_exception(myexception)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              x: typing.Dict[int, [str]]
            |}
           [
             "Invalid type parameters [24]: Single type parameter `Variable[_VT]` expected, but a \
              callable parameters `[str]` was given for generic type dict.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeVar, Generic

              TValue = TypeVar("TValue", bound=int)

              class Foo(Generic[TValue]): pass

              def foo() -> Foo[garbage]: ...
            |}
           ["Unbound name [10]: Name `garbage` is used but not defined in the current scope."];
      (* Malformed alias assignment *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              X, Y = int
              x: X = ...
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `X` has no type specified.";
             "Unable to unpack [23]: Unable to unpack `typing.Type[int]` into 2 values.";
             "Missing global annotation [5]: Globally accessible variable `Y` has no type \
              specified.";
             "Undefined or invalid type [11]: Annotation `X` is not defined as a type.";
           ];
    ]


let test_check_illegal_annotation_target =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Bar:
                a: str = "string"
              class Foo:
                def foo(self) -> None:
                  x = Bar()
                  x.a: int = 1
                  reveal_type(x.a)
            |}
           [
             "Illegal annotation target [35]: Target `x.a` cannot be annotated.";
             "Revealed type [-1]: Revealed type for `x.a` is `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Bar: ...
              class Foo:
                def foo(self, x: Bar) -> None:
                  self.a: int = 1
                  x.a: int = 1
            |}
           [
             "Undefined attribute [16]: `Foo` has no attribute `a`.";
             "Illegal annotation target [35]: Target `x.a` cannot be annotated.";
             "Undefined attribute [16]: `Bar` has no attribute `a`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Foo:
                a: int = 1

              Foo.a: str = "string"
              reveal_type(Foo.a)
            |}
           [
             "Illegal annotation target [35]: Target `test.Foo.a` cannot be annotated.";
             "Revealed type [-1]: Revealed type for `test.Foo.a` is `int`.";
           ];
    ]


let test_check_missing_type_parameters =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("_T")
              class C(typing.Generic[T]): ...
              def f(c: C) -> None:
                return None
            |}
           ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("_T")
              class C(typing.Generic[T]): ...
              def f(c: typing.List[C]) -> None:
                return None
            |}
           ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("_T")
              class C(typing.Generic[T]): ...
              def f() -> typing.List[C]:
                return []
            |}
           ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("_T")
              S = typing.TypeVar("_S")
              class C(typing.Generic[T, S]): ...
              def f() -> typing.List[C]:
                return []
            |}
           ["Invalid type parameters [24]: Generic type `C` expects 2 type parameters."];
    ]


let test_check_analysis_failure =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> Derp:
                pass

              def bar(x: int = foo()) -> int:
                return x
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Incompatible variable type [9]: x is declared to have type `int` "
             ^ "but is used as type `unknown`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int) -> None:
                pass

              def bar(x: Derp) -> None:
                test = foo( **x )
            |}
           [
             "Unbound name [10]: Name `Derp` is used but not defined in the current scope.";
             "Invalid argument [32]: Keyword argument `x` has type `unknown` "
             ^ "but must be a mapping with string keys.";
           ];
    ]


let test_check_immutable_annotations =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              a: int = None
              def foobar() -> None:
                  b: int = None
            |}
           [
             "Incompatible variable type [9]: a is declared to have type `int` "
             ^ "but is used as type `None`.";
             "Incompatible variable type [9]: b is declared to have type `int` "
             ^ "but is used as type `None`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                x: int = 1
                x = 'string'
            |}
           [
             "Incompatible variable type [9]: x is declared to have type `int` but is used as type \
              `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from builtins import int_to_str
              def f(x: int) -> None:
                x: str = int_to_str(x)
            |}
           [
             "Illegal annotation target [35]: Target `x` cannot be annotated after it is first \
              declared.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
            constant: int
            def foo() -> None:
              global constant
              constant = "hi"
            |}
           [
             "Incompatible variable type [9]: constant is declared to have type `int` but is used \
              as "
             ^ "type `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              import typing
              def expects_str(x: str) -> None:
                pass

              def foo(x: int, y: typing.Any) -> None:
                x = y
                expects_str(x)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: str = 1) -> str:
                return x
            |}
           [
             "Incompatible variable type [9]: x is declared to have type `str` but is used as "
             ^ "type `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar('T')
              def foo(x: T = 1) -> T:
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar('T', int, float)
              def foo(x: T = 1) -> T:
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar('T', int, float)
              def foo(x: T = "str") -> T:
                return x
            |}
           [
             "Incompatible variable type [9]: "
             ^ "x is declared to have type `Variable[T <: [int, float]]` but is used as type `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              class B: pass
              class C(B): pass
              T = typing.TypeVar('T', bound=B)
              def foo(x: T = C()) -> T:
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              class O: pass
              class B: pass
              class C(B): pass
              T = typing.TypeVar('T', bound=B)
              def foo(x: T = O()) -> T:
                return x
            |}
           [
             "Incompatible variable type [9]: "
             ^ "x is declared to have type `Variable[T (bound to B)]` but is used as type `O`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def bar() -> typing.Any:
                ...
              def foo(x: str = bar()) -> str:
                return x
            |}
           [
             "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              constant: int
              def foo() -> None:
                constant = "hi"
            |}
           [];
      (* TODO (T56371223): Emit an invalid assignment error when trying to re-annotated a global
         like this *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              constant: int
              def foo() -> None:
                global constant
                constant: str
                constant = "hi"
            |}
           [
             "Illegal annotation target [35]: Target `constant` cannot be annotated after it is \
              first declared.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              constant: typing.Union[int, str]
              def foo() -> None:
                global constant
                constant = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              constant: typing.Optional[int]
              def foo() -> int:
                if constant is not None:
                  return constant
                return 0
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              constant: typing.Optional[int]
              def call() -> None: pass
              def foo() -> int:
                if constant is not None:
                  call()
                  return constant
                return 0
            |}
           ["Incompatible return type [7]: Expected `int` but got `Optional[int]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> int:
                constant: typing.Optional[int]
                if constant is not None:
                  return constant
                return 0
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> int:
                constant: typing.Optional[str]
                if constant is not None:
                  return constant
                return 0
            |}
           ["Incompatible return type [7]: Expected `int` but got `str`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> int:
                constant: typing.Optional[int]
                if constant is not None:
                  return 0
                return constant
            |}
           ["Incompatible return type [7]: Expected `int` but got `None`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              constant: typing.Any
              def foo() -> None:
                global constant
                constant = 1
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `constant` must be \
              specified as type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              constant: int
              def foo(x: int) -> str:
                if x > 10:
                  global constant
                  constant: str
                return constant
            |}
           [
             "Illegal annotation target [35]: Target `constant` cannot be annotated after it is \
              first declared.";
             "Incompatible return type [7]: Expected `str` but got `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int) -> None:
                x = "hi"
            |}
           [
             "Incompatible variable type [9]: x is declared to have type `int` but is used as "
             ^ "type `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(x: typing.Optional[int]) -> None:
                x = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int) -> None:
                x: str
                x = "hi"
            |}
           [
             "Illegal annotation target [35]: Target `x` cannot be annotated after it is first \
              declared.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                x = 1
                y: str
                y = x
                x = y
            |}
           [
             "Incompatible variable type [9]: y is declared to have type `str` but is used as "
             ^ "type `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(any: typing.Any) -> None:
                x: int = any
            |}
           ["Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           ~enable_strict_any_check:true
           {|
              import typing
              def foo(any: typing.Any) -> None:
                x: int = any
            |}
           ["Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int) -> None:
                if x > 10:
                  y: int
                else:
                  y: str

                y = "hi"
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(x: int) -> None:
                if x > 10:
                  y: int
                else:
                  y: str
                y = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Foo():
                attribute = ...
              def bar() -> int:
                foo = Foo()
                foo.attribute = 1
                return foo.attribute
            |}
           [
             "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has no type \
              specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: typing.Dict[str, typing.Any] = {}
                x = { 'a': 'b' }
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: typing.Dict[str, typing.List[typing.Any]] = {}
            |}
           ["Prohibited any [33]: Explicit annotation for `x` cannot contain `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              constant = 1
              def foo() -> None:
                global constant
                constant = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Foo():
                constant = ...
              def foo() -> None:
                foo = Foo()
                foo.constant = 1
            |}
           [
             "Missing attribute annotation [4]: Attribute `constant` of class `Foo` has no type \
              specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              x = 1
              y: typing.Any = 2
              z: typing.List[typing.Any] = [3]
              a: typing.Any

              def foo() -> int:
                global a
                a = 1
                return a
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `y` has type `int` "
             ^ "but type `Any` is specified.";
             "Missing global annotation [5]: Globally accessible variable `z` must be specified "
             ^ "as type that does not contain `Any`.";
             "Missing global annotation [5]: Globally accessible variable `a` must be specified as \
              type other than `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              class Foo():
                __slots__: typing.List[str] = ['name']
                def foo(self) -> str:
                  return self.name
            |}
           ["Incompatible return type [7]: Expected `str` but got `unknown`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              class Foo():
                __slots__: typing.List[str] = ['name', 'attribute']
                def foo(self) -> str:
                  return self.name + self.attribute + self.constant
            |}
           ["Undefined attribute [16]: `Foo` has no attribute `constant`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              class Foo():
                __slots__: typing.List[str] = ['name']
                def foo(self) -> str:
                  return self.name
                def __init__(self) -> None:
                  self.name: int = 1
            |}
           ["Incompatible return type [7]: Expected `str` but got `int`."];
    ]


let test_check_incomplete_annotations =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: typing.Any = 1
            |}
           [
             "Prohibited any [33]: Expression `x` has type `int`; "
             ^ "given explicit type cannot be `Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: typing.List[typing.Any] = []
            |}
           ["Prohibited any [33]: Explicit annotation for `x` cannot contain `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x = 1
                typing.cast(typing.Any, x)
            |}
           ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot be `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x = 1
                typing.cast(typing.List[typing.Any], x)
            |}
           ["Prohibited any [33]: Explicit annotation for `typing.cast` cannot contain `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              import typing
              def foo() -> None:
                x: typing.Any = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: typing.Dict[str, typing.Any] = {}
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                x: typing.List[typing.Dict[str, typing.Any]] = []
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              import typing
              def foo() -> None:
                x = 1
                typing.cast(typing.Any, x)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              MyDict = typing.Dict[str, typing.Any]
            |}
           [];
    ]


let test_check_incomplete_callable =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(x: int) -> str:
                return "foo"
              bar: typing.Callable[[int], bool] = foo
            |}
           [
             "Incompatible variable type [9]: bar is declared to have type `typing.Callable[[int], \
              bool]` but is used as type `typing.Callable(foo)[[Named(x, int)], str]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(x: int) -> str:
                return "foo"
              bar: typing.Callable[[int]] = foo

              def baz(x: typing.Callable[[int]]) -> typing.Callable[[int]]: ...
            |}
           [
             "Invalid type [31]: Expression `typing.Callable[[int]]` is not a valid type.";
             "Invalid type [31]: Expression `typing.Callable[[int]]` is not a valid type.";
             "Invalid type [31]: Expression `typing.Callable[[int]]` is not a valid type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~show_error_traces:true
           {|
              from typing import Callable
              x: Callable[int]
              y: Callable[int, str]
            |}
           [
             "Invalid type [31]: Expression `typing.Callable[int]` is not a valid type. Expected \
              `Callable[[<parameters>], <return type>]`.";
             "Invalid type [31]: Expression `typing.Callable[(int, str)]` is not a valid type. \
              Expected `Callable[[<parameters>], <return type>]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~show_error_traces:true
           {|
              from typing import Callable
              x: Callable[[int, str], ...]
            |}
           [
             "Invalid type [31]: Expression `typing.Callable[([int, str], ...)]` is not a valid \
              type. Expected annotation other than ... for return type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~show_error_traces:true
           {|
              from typing import Callable
              x: Callable[..., ...]
            |}
           [
             "Invalid type [31]: Expression `typing.Callable[(..., ...)]` is not a valid type. \
              Expected annotation other than ... for return type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~show_error_traces:true
           {|
              from typing import Callable
              x: Callable[..., str]
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~show_error_traces:true
           {|
              from typing import Callable
              x: Callable[[...], str]
            |}
           [
             "Invalid type [31]: Expression `typing.Callable[([...], str)]` is not a valid type. \
              Expected `Callable[[<parameters>], <return type>]` or `Callable[..., <return \
              type>]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~concise:true
           ~show_error_traces:true
           {|
              from typing import Callable
              x: Callable[[...], str]
            |}
           [
             "Invalid type [31]: Expression `typing.Callable[([...], str)]` is not a valid type.\n\
              Expected `Callable[[<parameters>], <return type>]` or `Callable[..., <return type>]`.";
           ];
    ]


let test_check_tuple_typeform =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              x: tuple[int, int, ...]
            |}
           ["Invalid type [31]: Expression `tuple[(int, int, ...)]` is not a valid type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              x: tuple[...]
            |}
           ["Invalid type [31]: Expression `tuple[...]` is not a valid type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              x: tuple[..., int]
            |}
           ["Invalid type [31]: Expression `tuple[(..., int)]` is not a valid type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              x: tuple[int, ..., int]
            |}
           ["Invalid type [31]: Expression `tuple[(int, ..., int)]` is not a valid type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              x: tuple[*tuple[str], ...]
              y: tuple[str, ...]
              z: tuple[*tuple[str, ...], ...]
            |}
           [
             "Invalid type [31]: Expression `tuple[(*tuple[str], ...)]` is not a valid type.";
             "Invalid type [31]: Expression `tuple[(*tuple[(str, ...)], ...)]` is not a valid type.";
           ];
    ]


let test_check_refinement =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def takes_int(a: int) -> None: pass
              def foo() -> None:
                x: float
                x = 1
                takes_int(x)
                x = 1.0
            |}
           [];
      (* List[Any] correctly can refine to List[int] *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                l: typing.List[typing.Any] = []
                l = [1]
                l.append('asdf')
            |}
           ["Prohibited any [33]: Explicit annotation for `l` cannot contain `Any`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                l: typing.List[int] = []
                l.append('a')
            |}
           [
             "Incompatible parameter type [6]: In call `list.append`, for 1st positional argument, \
              expected `int` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo() -> None:
                l: typing.List[int] = None
                l.append('a')
            |}
           [
             "Incompatible variable type [9]: l is declared to have type `List[int]` "
             ^ "but is used as type `None`.";
             "Incompatible parameter type [6]: In call `list.append`, for 1st positional argument, \
              expected `int` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(x: typing.Optional[int]) -> int:
                if not x:
                  return 1
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def foo(x: typing.Optional[int]) -> int:
                if not x:
                  y = x
                return x
            |}
           ["Incompatible return type [7]: Expected `int` but got `Optional[int]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              class A:
                  a: typing.Optional[int] = None
                  def foo(self) -> None:
                      if self.a is None:
                          self.a = 5
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              class A:
                  a: typing.Optional[int] = None
                  def bar(self) -> int:
                      if self.a is not None:
                          return self.a
                      else:
                          return 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def call() -> None: pass
              class A:
                  a: typing.Optional[int] = None
                  def bar(self) -> int:
                      if self.a is not None:
                          call()
                          return self.a
                      else:
                          return 1
            |}
           ["Incompatible return type [7]: Expected `int` but got `Optional[int]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from builtins import int_to_int
              import typing
              def bar(x: typing.Optional[int]) -> None:
                  if x and int_to_int(x) < 0:
                      y = 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              def bar(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
                  if not input:
                    input = set()
                  return input
            |}
           [];
    ]


let test_check_invalid_type_variables =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              def f(x: T) -> T:
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              def f() -> T:
                return T
            |}
           [
             "Invalid type variable [34]: The type variable `Variable[T]` isn't present in the \
              function's parameters.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              class C:
                x: T = 1
            |}
           [
             "Invalid type variable [34]: The current class isn't generic with respect to the type \
              variable `Variable[T]`. To reference the type variable, you can modify the class to \
              inherit from `typing.Generic[T]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              x: T = ...
            |}
           [
             "Invalid type variable [34]: The type variable `Variable[T]` can only be used to \
              annotate generic classes or functions.";
           ];
      (* We don't error for inferred generics. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              class C(typing.Generic[T]):
                pass
              class D(C[T]):
                pass
            |}
           [];
      (* The inline Callable type does not actually make a new type variable scope *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T")
              def f() -> typing.Callable[[T], T]:
                def g(x: T) -> T:
                  return x
                return g
            |}
           [
             "Invalid type variable [34]: The type variable `Variable[T]` isn't present in the \
              function's parameters.";
           ];
      (* Check invalid type variables in parameters and returns. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T", covariant=True)
              class Foo(typing.Generic[T]):
                def foo(self, x: T) -> T:
                  return x
            |}
           [
             "Invalid type variance [46]: The type variable `Variable[T](covariant)` is covariant "
             ^ "and cannot be a parameter type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T", covariant=True)
              class Foo(typing.Generic[T]):
                def __init__(self, x: T) -> None:
                  return
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T", covariant=True)
              class Foo(typing.Generic[T]):
                def foo(self, x: typing.List[T]) -> T:
                  return x[0]
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T", contravariant=True)
              class Foo(typing.Generic[T]):
                def foo(self, x: T) -> T:
                  return x
            |}
           [
             "Invalid type variance [46]: The type variable `Variable[T](contravariant)` is "
             ^ "contravariant and cannot be a return type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T", contravariant=True)
              class Foo(typing.Generic[T]):
                def foo(self, x: T) -> typing.List[T]:
                  return [x]
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing

              T = typing.TypeVar("T", contravariant=True)

              def foo(x: T) -> None:
                def inner(y: T) -> T:
                    return y
                print(inner(x))
            |}
           [];
      (* Pyre currently does not model functions as type constructors, and as a result does not care
         about variance checks for generic functions or methods. For motivation see summary of
         D62068860 *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T", covariant=True)
              def foo(x: T) -> T:
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              T = typing.TypeVar("T", covariant=True)
              S = typing.TypeVar("S")

              class Foo(typing.Generic[S]):
                def foo(self, x: T) -> T:
                  return x
            |}
           [];
    ]


let test_check_aliases =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing_extensions
              class C(typing_extensions.Protocol):
                ...
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing_extensions
              class C(typing_extensions.Protocol[int]):
                ...
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class FOO:
                x: int = 0
              class BAR:
                pass
              FOO = BAR
              def foo(a: FOO) -> int:
                return a.x
              foo(FOO())
            |}
           [
             "Incompatible return type [7]: Expected `int` but got `unknown`.";
             "Undefined attribute [16]: `BAR` has no attribute `x`.";
             "Incompatible parameter type [6]: In call `foo`, for 1st positional argument, \
              expected `BAR` but got `FOO`.";
           ];
      (* Locals are not aliases *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo() -> None:
                x = int
                y: x = 1
            |}
           ["Undefined or invalid type [11]: Annotation `foo.x` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              def foo(type: int) -> None:
                x = type
            |}
           [];
      (* Aliases to undefined types *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              MyAlias = typing.Union[int, UndefinedName]
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `MyAlias` has no type \
              specified.";
             "Unbound name [10]: Name `UndefinedName` is used but not defined in the current scope.";
           ];
      (* TODO (T61917464): Surface explicit type aliases registeration failures as type errors *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              import typing_extensions
              MyAlias: typing_extensions.TypeAlias = typing.Union[int, UndefinedName]
            |}
           ["Unbound name [10]: Name `UndefinedName` is used but not defined in the current scope."];
      (* TODO (T61917464): Surface explicit type aliases registeration failures as type errors *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              import typing_extensions
              MyAlias: typing_extensions.TypeAlias = typing.Union[int, "UndefinedName"]
            |}
           [];
      (* Aliases to invalid types *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              MyAlias = typing.Union[int, 3]
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `MyAlias` has no type \
              specified.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import typing
              import typing_extensions
              MyAlias: typing_extensions.TypeAlias = typing.Union[int, 3]
            |}
           [];
      (* Handle quoted references to aliases. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Mapping

              Bar = int
              AliasWithStringAnnotation = Mapping[str, "Bar"]
              x: AliasWithStringAnnotation = 7
            |}
           [
             "Incompatible variable type [9]: x is declared to have type `Mapping[str, int]` but \
              is used as type `int`.";
           ];
    ]


let test_final_type =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final
              x: Final[int] = 3
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final, List
              x: List[Final[int]] = [3]
            |}
           [
             "Incompatible variable type [9]: x is declared to have type `List[Final[int]]` but is \
              used as type `List[int]`.";
             "Invalid type [31]: Expression `List[Final[int]]` is not a valid type. Final cannot \
              be nested.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final

              def foo(x: Final[int]) -> None:
                pass
            |}
           ["Invalid type [31]: Parameter `x` cannot be annotated with Final."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final
              x: Final[str] = 3
            |}
           [
             "Incompatible variable type [9]: x is declared to have type `str` but is used as type \
              `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final

              class Foo:
                uninitialized_attribute: Final[int]
                attribute: Final[int]

                def __init__(self) -> None:
                  self.attribute = 1
            |}
           [
             "Uninitialized attribute [13]: Attribute `uninitialized_attribute` is declared in \
              class `Foo` to have type `int` but is never initialized.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final

              class Foo:
                attribute: Final[int] = 1

              class Bar(Foo):
                attribute = 2

              def test(foo: Foo) -> int:
                foo.attribute = 2
                return foo.attribute
            |}
           [
             "Invalid assignment [41]: Cannot reassign final attribute `attribute`.";
             "Invalid assignment [41]: Cannot reassign final attribute `foo.attribute`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final

              def foo() -> int:
                x: Final[int] = 1
                return x
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final

              def foo() -> None:
                x: Final[int] = 1
                x = 2
            |}
           ["Invalid assignment [41]: Cannot reassign final attribute `x`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final
              x: Final[int] = 3

              def foo() -> None:
                global x
                x = 2
            |}
           ["Invalid assignment [41]: Cannot reassign final attribute `x`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final, List
              x: Final[List[int]] = [3]

              def foo() -> None:
                global x
                x.append(4)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Final
              x: Final[int] = 3

              def foo() -> int:
                return x
            |}
           [];
    ]


let test_check_invalid_inheritance =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable
              class MyCallable(Callable):
                pass
            |}
           [
             "Invalid type parameters [24]: Generic type `Callable` expects 2 type parameters.";
             "Invalid inheritance [39]: `typing.Callable[..., typing.Any]` is not a valid parent \
              class.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Any
              class MySpecialClass(Any, int):
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Dict, Union
              D = Dict[str, Union[str, "D"]]

              class Foo(D): ...
             |}
           [
             "Invalid inheritance [39]: `test.D (resolves to Dict[str, Union[D, str]])` is not a \
              valid parent class.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import List, Tuple

              class Foo(List[int]): ...

              class Bar(Tuple[int, int]): ...
             |}
           [];
    ]


let test_check_invalid_generic_inheritance =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                from typing import Generic, TypeVar

                T = TypeVar('T')
                class Base(Generic[T]):
                  def __init__(self, foo: T) -> None:
                    self.foo = foo
                class Child(Base[T]): pass
                class InstantiatedChild(Base[int]): pass

                y: Base[str] = Base(0)
                # Check __init__.
                InstantiatedChild("foo")

                x: Child[str] = Child(0)
                correct: Child[str] = Child("bar")
              |}
           [
             "Incompatible variable type [9]: y is declared to have type `Base[str]` but is used \
              as type `Base[int]`.";
             "Incompatible parameter type [6]: In call `Base.__init__`, for 1st positional \
              argument, expected `int` but got `str`.";
             "Incompatible variable type [9]: x is declared to have type `Child[str]` but is used \
              as type `Child[int]`.";
             "Incompatible variable type [9]: correct is declared to have type `Child[str]` but is \
              used as type `Child[typing_extensions.Literal['bar']]`.";
           ];
      (* Check __new__. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                from typing import Generic, TypeVar

                T = TypeVar('T')
                T2 = TypeVar('T2')
                class Base(Generic[T, T2]):
                  def __new__(cls, foo: T, bar: T2) -> Base[T, T2]:
                    self = super(Base, cls).__new__(cls)
                    self.foo = foo
                    self.bar = bar
                    return self
                class PartialChild(Base[int, T2], Generic[T2]): pass

                PartialChild("hello", "world")
              |}
           [
             "Incompatible parameter type [6]: In call `Base.__new__`, for 1st positional \
              argument, expected `int` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                from typing import Generic, TypeVar

                T = TypeVar('T')
                T2 = TypeVar('T2')
                class Base(Generic[T, T2]):
                  def __init__(self, foo: T, bar: T2) -> None:
                    self.foo = foo
                    self.bar = bar
                class PartialChild(Base[int, T2], Generic[T2]): pass

                PartialChild("hello", "world")
                y1: PartialChild[str] = PartialChild(0, "hello")
                y2: PartialChild[str] = PartialChild(0, 1)
                y3: PartialChild[str] = PartialChild("hello", 0)
                y4: PartialChild[int] = PartialChild(0, "hello")
              |}
           [
             "Incompatible parameter type [6]: In call `Base.__init__`, for 1st positional \
              argument, expected `int` but got `str`.";
             "Incompatible variable type [9]: y1 is declared to have type `PartialChild[str]` but \
              is used as type `PartialChild[typing_extensions.Literal['hello']]`.";
             "Incompatible variable type [9]: y2 is declared to have type `PartialChild[str]` but \
              is used as type `PartialChild[int]`.";
             "Incompatible variable type [9]: y3 is declared to have type `PartialChild[str]` but \
              is used as type `PartialChild[int]`.";
             "Incompatible parameter type [6]: In call `Base.__init__`, for 1st positional \
              argument, expected `int` but got `str`.";
             "Incompatible variable type [9]: y4 is declared to have type `PartialChild[int]` but \
              is used as type `PartialChild[str]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                from typing import Generic, TypeVar

                T = TypeVar('T')
                T2 = TypeVar('T2')
                class Base(Generic[T, T2]):
                  def __init__(self, foo: T, bar: T2) -> None:
                    self.foo = foo
                    self.bar = bar
                class PartialChildWithConstructor(Base[int, T2], Generic[T2]):
                  def __init__(self, first: T2, second: int, third: str) -> None:
                    self.foo: int = second
                    self.bar: T2 = first
                    self.third: str = third

                PartialChildWithConstructor("hello", 0, 0)
                y3: PartialChildWithConstructor[str] = PartialChildWithConstructor(0, 0, "world")
              |}
           [
             "Incompatible parameter type [6]: In call `PartialChildWithConstructor.__init__`, for \
              3rd positional argument, expected `str` but got `int`.";
             "Incompatible variable type [9]: y3 is declared to have type \
              `PartialChildWithConstructor[str]` but is used as type \
              `PartialChildWithConstructor[int]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                from typing import Generic, TypeVar

                T = TypeVar('T')
                T2 = TypeVar('T2')
                T3 = TypeVar('T3')
                class Base(Generic[T, T2]):
                  def __init__(self, foo: T, bar: T2) -> None:
                    self.foo = foo
                    self.bar = bar
                class TypeNotUsedInConstructor(Base[int, T2], Generic[T2, T3]):
                  def __init__(self, first: T2, second: int, third: str) -> None:
                    self.foo: int = second
                    self.bar: T2 = first
                    self.third: str = third

                  def identity(self, x: T3) -> T3: ...

                y1: TypeNotUsedInConstructor[str, int]
                reveal_type(y1.identity(0))
                reveal_type(y1.identity("hello"))
              |}
           [
             "Revealed type [-1]: Revealed type for `y1.identity(0)` is `int`.";
             "Revealed type [-1]: Revealed type for `y1.identity(\"hello\")` is `int`.";
             "Incompatible parameter type [6]: In call `TypeNotUsedInConstructor.identity`, for \
              1st positional argument, expected `int` but got `str`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                from typing import Generic, TypeVar

                T = TypeVar('T')
                T2 = TypeVar('T2')
                class Base(Generic[T, T2]):
                  def __init__(self, foo: T, bar: T2) -> None:
                    self.foo = foo
                    self.bar = bar
                  def generic_method(self, x: T, y: T2) -> None: ...

                class Child(Base[T, T2]): pass
                class PartialChild(Base[int, T2], Generic[T2]): pass

                y1: Base[str, int] = Base("hello", 1)
                y2: Child[str, int] = Child("hello", 1)
                y3: PartialChild[str] = PartialChild(0, "hello")
                def call_base(x: Base[str, int]) -> None:
                  x.generic_method("hello", 1)
                  x.generic_method("hello", "world")
                def call_child(x: Child[str, int]) -> None:
                  x.generic_method("hello", 1)
                  x.generic_method("hello", "world")
                def call_partial_child(x: PartialChild[str]) -> None:
                  x.generic_method(1, "world")
                  x.generic_method("hello", "world")
              |}
           [
             "Incompatible variable type [9]: y1 is declared to have type `Base[str, int]` but is \
              used as type `Base[typing_extensions.Literal['hello'], \
              typing_extensions.Literal[1]]`.";
             "Incompatible variable type [9]: y2 is declared to have type `Child[str, int]` but is \
              used as type `Child[typing_extensions.Literal['hello'], \
              typing_extensions.Literal[1]]`.";
             "Incompatible variable type [9]: y3 is declared to have type `PartialChild[str]` but \
              is used as type `PartialChild[typing_extensions.Literal['hello']]`.";
             "Incompatible parameter type [6]: In call `Base.generic_method`, for 2nd positional \
              argument, expected `int` but got `str`.";
             "Incompatible parameter type [6]: In call `Base.generic_method`, for 2nd positional \
              argument, expected `int` but got `str`.";
             "Incompatible parameter type [6]: In call `Base.generic_method`, for 1st positional \
              argument, expected `int` but got `str`.";
           ];
    ]


let test_check_literal_assignment =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing_extensions import Literal
              x: Literal["on", "off"] = "on"
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Generic, TypeVar

              T = TypeVar('T')
              class Foo(Generic[T]):
                foo: T
                def __init__(self, foo: T) -> None:
                  self.foo = foo

              def foo(s: str) -> None:
                string: Foo[str] = Foo(s)
                literal_string: Foo[str] = Foo("bar")
            |}
           [
             "Incompatible variable type [9]: literal_string is declared to have type `Foo[str]` \
              but is used as type `Foo[typing_extensions.Literal['bar']]`.";
           ];
      (* We should not sort the generic variables in alphabetical order. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import *

              T = TypeVar("T", bound=int)
              AU = TypeVar("AU")
              X = TypeVar("X", bound=int)
              AY = TypeVar("AY")

              class Base(Generic[T, AU]):
                  pass

              class Child(Base[X, AY]):
                  pass

              class GrandChild(Child[T, AU]):
                  pass
          |}
           [];
    ]


let test_check_pyre_extensions_generic =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from pyre_extensions import Generic
              from typing import TypeVar

              T = TypeVar("T")

              # Pyre should not complain about the `pyre_extensions` Generic not being
              # subscriptable.
              class Foo(Generic[T]): ...
            |}
           [];
    ]


let test_check_safe_cast =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              import pyre_extensions
              def foo(input: float) -> int:
                return pyre_extensions.safe_cast(int, input)
            |}
           [
             "Unsafe cast [49]: `safe_cast` is only permitted to widen the type of `input`. `int` \
              is not a super type of `input`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                import pyre_extensions
                def foo(input: int) -> float:
                  return pyre_extensions.safe_cast(float, input)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
                import pyre_extensions
                def foo(input: str) -> int:
                  return pyre_extensions.safe_cast(int, input)
            |}
           [
             "Unsafe cast [49]: `safe_cast` is only permitted to widen the type of `input`. `int` \
              is not a super type of `input`.";
           ];
    ]


let test_check_annotation_with_any =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import List, Any
              def foo(x: List[Any] = None) -> None:
                pass
            |}
           [
             "Incompatible variable type [9]: x is declared to have type `List[typing.Any]` but is \
              used as type `None`.";
             "Missing parameter annotation [2]: Parameter `x` is used as type `None` and must have \
              a type that does not contain `Any`.";
           ];
    ]


let test_check_variable_bounds_with_any =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeVar, Any, List
              T = TypeVar("T", bound=List[Any])
            |}
           ["Prohibited any [33]: Type variable `T` cannot have a bound containing `Any`."];
    ]


let test_check_variable_bounds_with_quoted_bound =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeVar, Any, List
              T = TypeVar("T", bound="List[Any]")
            |}
           ["Prohibited any [33]: Type variable `T` cannot have a bound containing `Any`."];
    ]


let test_check_variable_bounds_with_quoted_any =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeVar, Any, List
              T = TypeVar("T", bound=List["Any"])
            |}
           ["Prohibited any [33]: Type variable `T` cannot have a bound containing `Any`."];
    ]


let test_check_annotated =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Annotated

              def foo(x: str) -> str: ...

              class Foo:
                def __init__(self, x: str) -> None: ...

              valid_string_literal: Annotated["int", foo("hello")]
              valid_string_literal2: Annotated["int", Foo("hello")]
            |}
           [];
    ]


let test_check_subscripted_type_expression =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors {|
              x: type[list[str]] = list[str]
            |} [];
    ]


let test_check_union_shorthand =
  test_list
    [
      (* String annotations should work correctly with the union shorthand. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class Foo: ...

              x: int | "Foo"
              reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `typing.Union[Foo, int]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import List
              class Foo: ...

              x: int | List["Foo"]
              reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `typing.Union[List[Foo], int]`."];
    ]


let test_check_unpack =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import Generic
              from typing_extensions import TypeVarTuple, Unpack

              Ts = TypeVarTuple("Ts")
              Ts2 = TypeVarTuple("Ts2")

              class Array(Generic[Unpack[Ts, Ts2]]):
                  pass
            |}
           [
             "Invalid type [31]: Expression `typing.Generic[typing_extensions.Unpack[(Ts, Ts2)]]` \
              is not a valid type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing import Generic
              from typing_extensions import TypeVarTuple, Unpack

              Ts = TypeVarTuple("Ts")

              class Array(Generic[*Ts]):
                  pass

              Ts2 = TypeVarTuple("Ts2")

              class Array2(Generic[Unpack[Ts2]]):
                  pass

              class Array3(Generic[*tuple[int, ...]]):
                  pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_type_errors
           {|
              from typing_extensions import TypeVarTuple, Unpack

              Ts = TypeVarTuple("Ts")

              def foo(*args: Unpack[Ts]) -> None:
                pass

              Ts2 = TypeVarTuple("Ts2")

              def foo2(*args: *Ts2) -> None:
                pass

              Ts3 = TypeVarTuple("Ts3")

              def foo3(args: tuple[*Ts3]) -> None:
                pass

              def foo4(*args: *tuple[int, ...]) -> None:
                pass
            |}
           [];
    ]


let test_check_compose =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose

              class Foo:
                def __call__(self, x: str) -> bool: ...
              class Bar:
                def __call__(self, x: bool) -> int: ...

              x: Compose[Foo, Bar]
              res = x("hi")
              reveal_type(res)
            |}
           ["Revealed type [-1]: Revealed type for `res` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out]):
                def __call__(self, input: Tensor[T, In]) -> Tensor[T, Out]: ...

              x: Tensor[L[5], L[10]]
              layer: Compose[
                Linear[L[10], L[20]],
                Linear[L[20], L[30]]
              ]
              reveal_type(layer)
              res = layer(x)
              reveal_type(res)
            |}
           [
             "Revealed type [-1]: Revealed type for `layer` is \
              `pyre_extensions.Compose[Linear[typing_extensions.Literal[10], \
              typing_extensions.Literal[20]], Linear[typing_extensions.Literal[20], \
              typing_extensions.Literal[30]]]`.";
             "Revealed type [-1]: Revealed type for `res` is `Tensor[typing_extensions.Literal[5], \
              typing_extensions.Literal[30]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Tuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              R = TypeVar("R")

              class Foo:
                def __call__(self, x: Tuple[T, R]) -> T: ...
              class Bar:
                def __call__(self, x: R) -> R: ...

              layer: Compose[
                Foo,
                Bar
              ]
              res = layer((True, 3))
              reveal_type(res)
            |}
           ["Revealed type [-1]: Revealed type for `res` is `typing_extensions.Literal[True]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Tuple

              x: Compose[Tuple[int, str]]
            |}
           [
             "Invalid type [31]: Expression `pyre_extensions.Compose[typing.Tuple[(int, str)]]` is \
              not a valid type.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")
              N = TypeVar("N")
              M = TypeVar("M")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out]):
                def __call__(self, input: Tensor[T, In]) -> Tensor[T, Out]: ...

              def foo(n: N, m: M) -> None:
                x: Tensor[L[5], L[10]]
                layer: Compose[
                  Linear[L[10], N],
                  Linear[M, L[30]]
                ]
                reveal_type(layer)
                res = layer(x)

              input: Tensor[L[5], L[10]]
              layer2: Compose[Linear[L[10], L[20]], Linear[L[21], L[30]]]
              res = layer2(input)
            |}
           [
             "Revealed type [-1]: Revealed type for `layer` is \
              `pyre_extensions.Compose[Linear[typing_extensions.Literal[10], Variable[N]], \
              Linear[Variable[M], typing_extensions.Literal[30]]]`.";
             "Call error [29]: `pyre_extensions.Compose[Linear[int, Variable[N]], \
              Linear[Variable[M], int]]` is not a function.";
             "Call error [29]: `pyre_extensions.Compose[Linear[int, int], Linear[int, int]]` is \
              not a function.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Tuple

              class Foo:
                def __call__(self, x: int, y: str) -> Tuple[int, str]: ...

              layer: Compose[Foo, Foo]
              x = (1, "hi")
              result = layer( *x)
            |}
           ["Call error [29]: `pyre_extensions.Compose[Foo, Foo]` is not a function."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Tuple, overload, Union

              class Foo:
                @overload
                def __call__(self, x: int) -> int: ...
                @overload
                def __call__(self, x: str) -> str: ...
                def __call__(self, x: Union[int, str]) -> Union[int, str]: ...

              layer: Compose[Foo, Foo]
              result = layer(1)
              reveal_type(result)
            |}
           [
             "Call error [29]: `pyre_extensions.Compose[Foo, Foo]` is not a function.";
             "Revealed type [-1]: Revealed type for `result` is `typing.Any`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")
              N = TypeVar("N")
              M = TypeVar("M")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out]):
                def __call__(self, input: Tensor[T, In]) -> Tensor[T, Out]: ...

              def foo(n: N, m: M) -> None:
                x: Tensor[L[5], L[10]]
                layer: Compose[
                  Linear[L[10], N],
                  Linear[N, M],
                  Linear[M, L[30]]
                ]
                res = layer(x)
                reveal_type(res)
            |}
           [
             "Revealed type [-1]: Revealed type for `res` is `Tensor[typing_extensions.Literal[5], \
              typing_extensions.Literal[30]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out]):
                def __call__(self, input: Tensor[T, In]) -> Tensor[T, Out]: ...
              class Constant:
                def __call__(self, input: Tensor[L[5], L[20]]) -> Tensor[L[5], L[30]]: ...

              x: Tensor[L[5], L[10]]
              layer: Compose[
                Linear[L[10], L[20]],
                Constant
              ]
              res = layer(x)
            |}
           [
             "Call error [29]: `pyre_extensions.Compose[Linear[int, int], Constant]` is not a \
              function.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              R = TypeVar("R")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out, R]):
                def __call__(self, input: Tensor[T, In, R]) -> Tensor[T, Out, R]: ...

              def bar(z: T) -> None:
                def foo(input: Tensor[L[5], L[10], T]) -> None:
                  x: Compose[Linear[L[10], L[20], T], Linear[L[20], L[30], T]]
                  res = x(input)
                  reveal_type(res)
            |}
           [
             "Revealed type [-1]: Revealed type for `res` is `Tensor[typing_extensions.Literal[5], \
              typing_extensions.Literal[30], Variable[T]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              Ts = TypeVarTuple("Ts")
              In = TypeVar("In")
              Out = TypeVar("Out")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out]):
                def __call__(self, input: Tensor[Unpack[Ts], In]) -> Tensor[Unpack[Ts], Out]: ...

              x: Compose[
                Compose[
                  Linear[L[10], L[20]],
                  Linear[L[20], L[30]]
                ],
                Compose[
                  Linear[L[30], L[40]],
                  Linear[L[40], L[50]]
                ]
              ]
              input: Tensor[L[5], L[8], L[10]]
              res = x(input)
              reveal_type(res)
            |}
           [
             "Revealed type [-1]: Revealed type for `res` is `Tensor[typing_extensions.Literal[5], \
              typing_extensions.Literal[8], typing_extensions.Literal[50]]`.";
           ];
      (* This test should be rejected later once we have variadic bounds, since `bar` does not
         extend `nn.Module`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              DType = TypeVar("DType")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")
              Rs = TypeVarTuple("Rs")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Sequential(Generic[Unpack[Ts]]):
                def __init__(self, *layers: Unpack[Ts]) -> None: ...
                __call__: Compose[Unpack[Ts]] = ...

              class Linear(Generic[In, Out]):
                def __init__(self, x: In, y: Out) -> None: ...
                def __call__(self, input: Tensor[DType, Unpack[Ts], In]) -> Tensor[DType, Unpack[Ts], Out]: ...

              class Foo:
                def __call__(self, x: Tensor[DType, Unpack[Ts], L[20]]) -> Tensor[DType, Unpack[Ts], L[30]]: ...

              layer: Compose[Foo, Foo]
              x = (1, "hi")
              result = layer( *x)
            |}
           [
             "Inconsistent override [15]: `__call__` overrides attribute defined in `type` \
              inconsistently. Type `pyre_extensions.Compose[*test.Ts]` is not a subtype of the \
              overridden attribute `BoundMethod[typing.Callable(Sequential.__init__)[[Named(self, \
              Sequential[*test.Ts]), Variable(*test.Ts)], Sequential[*test.Ts]], \
              Sequential[*test.Ts]]`.";
             "Call error [29]: `pyre_extensions.Compose[Foo, Foo]` is not a function.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Tuple, overload, Union, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              DType = TypeVar("DType")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")
              Rs = TypeVarTuple("Rs")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Sequential(Generic[Unpack[Ts]]):
                def __init__(self, *layers: Unpack[Ts]) -> None: ...
                __call__: Compose[Unpack[Ts]] = ...

              class Linear(Generic[In, Out]):
                def __init__(self, x: In, y: Out) -> None: ...
                def __call__(self, input: Tensor[DType, Unpack[Ts], In]) -> Tensor[DType, Unpack[Ts], Out]: ...

              def bar(x: Tensor[DType, Unpack[Rs], L[40]]) -> Tensor[DType, Unpack[Rs], L[50]]: ...

              layer = Sequential(
                Linear(10, 20),
                Linear(20, 40),
                bar
              )
              x: Tensor[int, L[5], L[10]]
              result = layer(x)
              reveal_type(result)
            |}
           [
             "Inconsistent override [15]: `__call__` overrides attribute defined in `type` \
              inconsistently. Type `pyre_extensions.Compose[*test.Ts]` is not a subtype of the \
              overridden attribute `BoundMethod[typing.Callable(Sequential.__init__)[[Named(self, \
              Sequential[*test.Ts]), Variable(*test.Ts)], Sequential[*test.Ts]], \
              Sequential[*test.Ts]]`.";
             "Revealed type [-1]: Revealed type for `result` is `Tensor[int, \
              typing_extensions.Literal[5], typing_extensions.Literal[50]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")
              N = TypeVar("N")
              M = TypeVar("M")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out]):
                def __call__(self, input: Tensor[T, In]) -> Tensor[T, Out]: ...

              def foo(n: N, m: M) -> None:
                x: Tensor[L[5], L[10]]
                layer: Compose[
                  Linear[L[10], N],
                  Linear[N, M],
                  Linear[M, L[30]]
                ]
                res = layer(x)
                reveal_type(res)
            |}
           [
             "Revealed type [-1]: Revealed type for `res` is `Tensor[typing_extensions.Literal[5], \
              typing_extensions.Literal[30]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Linear(Generic[In, Out]):
                def __call__(self, input: Tensor[T, In]) -> Tensor[T, Out]: ...
              class Constant:
                def __call__(self, input: Tensor[L[5], L[20]]) -> Tensor[L[5], L[30]]: ...

              x: Tensor[L[5], L[10]]
              layer: Compose[
                Linear[L[10], L[20]],
                Constant
              ]
              res = layer(x)
            |}
           [
             "Call error [29]: `pyre_extensions.Compose[Linear[int, int], Constant]` is not a \
              function.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              R = TypeVar("R")
              DType = TypeVar("DType")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Sequential(Generic[Unpack[Ts]]):
                def __init__(self, *layers: Unpack[Ts]) -> None: ...
                __call__: Compose[Unpack[Ts]] = ...

              class Linear(Generic[In, Out]):
                def __init__(self, x: In, y: Out) -> None: ...
                def __call__(self, input: Tensor[DType, Unpack[Ts], In]) -> Tensor[DType, Unpack[Ts], Out]: ...

              layer = Sequential(
                        Sequential(
                          Sequential(
                            Linear(10, 20),
                            Linear(20, 30),
                          ),
                          Linear(30, 40),
                        ),
                        Sequential(
                          Linear(40, 50),
                          Linear(50, 60)
                        )
                      )
              x: Tensor[int, L[5], L[10]]
              result = layer(x)
              reveal_type(result)
            |}
           [
             "Inconsistent override [15]: `__call__` overrides attribute defined in `type` \
              inconsistently. Type `pyre_extensions.Compose[*test.Ts]` is not a subtype of the \
              overridden attribute `BoundMethod[typing.Callable(Sequential.__init__)[[Named(self, \
              Sequential[*test.Ts]), Variable(*test.Ts)], Sequential[*test.Ts]], \
              Sequential[*test.Ts]]`.";
             "Revealed type [-1]: Revealed type for `result` is `Tensor[int, \
              typing_extensions.Literal[5], typing_extensions.Literal[60]]`.";
           ];
      (* TODO (T96622059): Fix solving `Compose` or `Sequential` against `Callable` *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from pyre_extensions import Compose
              from typing import Generic, Callable, TypeVar, Unpack, TypeVarTuple
              from typing_extensions import Literal as L

              T = TypeVar("T")
              T2 = TypeVar("T2")
              DType = TypeVar("DType")
              In = TypeVar("In")
              Out = TypeVar("Out")
              Ts = TypeVarTuple("Ts")

              class Tensor(Generic[Unpack[Ts]]): ...

              class Sequential(Generic[Unpack[Ts]]):
                def __init__(self, *layers: Unpack[Ts]) -> None: ...
                __call__: Compose[Unpack[Ts]] = ...

              class Linear(Generic[In, Out]):
                def __init__(self, x: In, y: Out) -> None: ...
                def __call__(self, input: Tensor[DType, Unpack[Ts], In]) -> Tensor[DType, Unpack[Ts], Out]: ...

              def foo(x: Callable[[T], T2]) -> Callable[[T], T2]: ...
              x = Sequential(Linear(10, 20), Linear(20, 30))
              y: Compose[Linear[L[10], L[20]], Linear[L[20], L[30]]]
              result = foo(x)
              result2 = foo(y)
              reveal_type(result)
              reveal_type(result2)
            |}
           [
             "Inconsistent override [15]: `__call__` overrides attribute defined in `type` \
              inconsistently. Type `pyre_extensions.Compose[*test.Ts]` is not a subtype of the \
              overridden attribute `BoundMethod[typing.Callable(Sequential.__init__)[[Named(self, \
              Sequential[*test.Ts]), Variable(*test.Ts)], Sequential[*test.Ts]], \
              Sequential[*test.Ts]]`.";
             "Incompatible parameter type [6]: In call `foo`, for 1st positional argument, \
              expected `typing.Callable[[Variable[T]], Variable[T2]]` but got \
              `Sequential[Linear[int, int], Linear[int, int]]`.";
             "Incompatible parameter type [6]: In call `foo`, for 1st positional argument, \
              expected `typing.Callable[[Variable[T]], Variable[T2]]` but got \
              `pyre_extensions.Compose[Linear[int, int], Linear[int, int]]`.";
             "Revealed type [-1]: Revealed type for `result` is `typing.Callable[[typing.Any], \
              typing.Any]`.";
             "Revealed type [-1]: Revealed type for `result2` is `typing.Callable[[typing.Any], \
              typing.Any]`.";
           ];
    ]


let () =
  "annotation"
  >::: [
         test_check_tuple_typeform;
         test_check_undefined_type;
         test_check_invalid_type;
         test_check_illegal_annotation_target;
         test_check_invalid_type_variables;
         test_check_missing_type_parameters;
         test_check_analysis_failure;
         test_check_immutable_annotations;
         test_check_incomplete_annotations;
         test_check_incomplete_callable;
         test_check_refinement;
         test_check_aliases;
         test_final_type;
         test_check_invalid_inheritance;
         test_check_invalid_generic_inheritance;
         test_check_literal_assignment;
         test_check_pyre_extensions_generic;
         test_check_safe_cast;
         test_check_annotation_with_any;
         test_check_variable_bounds_with_any;
         test_check_variable_bounds_with_quoted_any;
         test_check_variable_bounds_with_quoted_bound;
         test_check_annotated;
         test_check_subscripted_type_expression;
         test_check_union_shorthand;
         test_check_compose;
         test_check_unpack;
       ]
  |> Test.run
