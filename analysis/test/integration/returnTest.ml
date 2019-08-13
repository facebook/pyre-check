(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_return context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors "def foo() -> None: pass" [];
  assert_type_errors "def foo() -> None: return" [];
  assert_type_errors "def foo() -> float: return 1.0" [];
  assert_type_errors "def foo() -> float: return 1" [];
  assert_type_errors
    "def foo() -> int: return 1.0"
    ["Incompatible return type [7]: Expected `int` but got `float`."];
  assert_type_errors
    "def foo() -> str: return 1.0"
    ["Incompatible return type [7]: Expected `str` but got `float`."];
  assert_type_errors
    "def foo() -> str: return"
    ["Incompatible return type [7]: Expected `str` but got `None`."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[str]:
          return 1
    |}
    ["Incompatible return type [7]: Expected `typing.List[str]` but got `int`."];
  assert_default_type_errors
    "def foo() -> int: return"
    ["Incompatible return type [7]: Expected `int` but got `None`."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[str]:
          return []
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Dict[str, int]:
          return {}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [ "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, use \
       `typing.Dict` to avoid runtime subscripting errors.";
      "Missing return annotation [3]: Return type must be specified as type "
      ^ "that does not contain `Any`." ];
  assert_type_errors
    {|
      import typing
      def f() -> dict:
        return {}
      def foo() -> typing.Dict[typing.Any]:
        return f()
    |}
    [ "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, use \
       `typing.Dict` to avoid runtime subscripting errors.";
      "Missing return annotation [3]: Return type must be specified as type that does "
      ^ "not contain `Any`.";
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, received 1, \
       use `typing.Dict` to avoid runtime subscripting errors." ];
  assert_type_errors
    {|
      import typing
      x: typing.Type
      def foo() -> typing.Type[typing.Any]:
        return x
    |}
    [ "Invalid type parameters [24]: Generic type `type` expects 1 type parameter, use \
       `typing.Type` to avoid runtime subscripting errors.";
      "Missing return annotation [3]: Return type must be specified as type "
      ^ "that does not contain `Any`." ];
  assert_type_errors
    {|
      import typing
      def derp()->typing.Union[str, None]:
          return None
    |}
    [];
  assert_type_errors
    "def foo() -> str: return 1.0\ndef bar() -> int: return ''"
    [ "Incompatible return type [7]: Expected `str` but got `float`.";
      "Incompatible return type [7]: Expected `int` but got `str`." ];
  assert_type_errors "class A: pass\ndef foo() -> A: return A()" [];
  assert_type_errors
    "class A: pass\ndef foo() -> A: return 1"
    ["Incompatible return type [7]: Expected `A` but got `int`."];
  assert_type_errors "def bar() -> str: return ''\ndef foo() -> str: return bar()" [];
  assert_type_errors
    "def foo() -> str: return not_annotated()"
    ["Incompatible return type [7]: Expected `str` but got `unknown`."];
  assert_type_errors
    {|
      def x()->int:
        return None
    |}
    ["Incompatible return type [7]: Expected `int` but got `None`."];
  assert_type_errors
    {|
      import typing
      def derp()->typing.Set[int]:
        return {1}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def derp()->typing.Set[int]:
        return {""}
    |}
    ["Incompatible return type [7]: Expected `typing.Set[int]` but got `typing.Set[str]`."];
  assert_type_errors
    {|
      def foo() -> str:
        if condition():
          return 1
        else:
          return 2
    |}
    [ "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible return type [7]: Expected `str` but got `int`." ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(x: list[T])-> T:
        return x

      def bar(x: list[int]) -> int:
        return foo(x)
    |}
    ["Incompatible return type [7]: Expected `Variable[T]` but got `typing.List[Variable[T]]`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(x: list[T])-> T:
        return x[0]
      def bar(x: list[int]) -> int:
        return foo(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', int, str)
      def foo(x: T)-> T:
        return "a"
      def bar() -> int:
        return foo(8) + 9
    |}
    ["Incompatible return type [7]: Expected `Variable[T <: [int, str]]` but got `str`."];
  assert_type_errors
    {|
      import typing
      class C:
        pass
      def foo() -> typing.Callable[[], C]:
        return C
    |}
    [];
  assert_type_errors
    {|
      import typing
      class C:
        pass
      def foo(x: typing.Optional[C]) -> C:
        irrelevant = x and 16
        return x
    |}
    ["Incompatible return type [7]: Expected `C` but got `typing.Optional[C]`."];
  assert_default_type_errors
    {|
      import typing
      def bar(x: typing.Any) -> int:
          return x
    |}
    [];
  assert_default_type_errors
    {|
      import typing
      def bar(x: typing.Union[int, typing.Any]) -> int:
          return x
    |}
    [];
  assert_default_type_errors
    {|
      import typing
      def bar(x: typing.Optional[typing.Any]) -> int:
          return x
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[typing.Any]`."];
  assert_default_type_errors
    {|
      import typing
      def bar(x: typing.Union[int, typing.Any, None]) -> int:
          return x
    |}
    [ "Incompatible return type [7]: Expected `int` but got \
       `typing.Optional[typing.Union[typing.Any, int]]`." ];
  ()


let test_check_return_control_flow context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      def x() -> int:
        if unknown_condition:
          return 1
    |}
    [ "Undefined name [18]: Global name `unknown_condition` is not defined, or there is at least \
       one control flow path that doesn't define `unknown_condition`.";
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`." ];
  assert_type_errors
    {|
      def foo() -> int:
        if unknown_condition:
          return 1
        else:
          x = 1
    |}
    [ "Undefined name [18]: Global name `unknown_condition` is not defined, or there is at least \
       one control flow path that doesn't define `unknown_condition`.";
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`." ];
  assert_type_errors
    {|
      import typing
      x: typing.List[int]
      def foo() -> list:
        return x
    |}
    [ "Invalid type parameters [24]: Generic type `list` expects 1 type parameter, use \
       `typing.List` to avoid runtime subscripting errors.";
      "Incompatible return type [7]: Expected `typing.List[typing.Any]` but got `typing.List[int]`."
    ];
  assert_type_errors
    {|
      import typing
      x: typing.List[typing.Any]
      def foo() -> list:
        return x
    |}
    [ "Missing global annotation [5]: Globally accessible variable `x` must be specified "
      ^ "as type that does not contain `Any`.";
      "Invalid type parameters [24]: Generic type `list` expects 1 type parameter, use \
       `typing.List` to avoid runtime subscripting errors." ];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Union[int, str]) -> int:
        if isinstance(x, str):
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          raise
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          x = 1
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          continue
        return x
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Optional[float]) -> typing.Optional[int]:
        if x is not None:
          return int(x)
        return x
    |}
    [];

  (* Type aliases in the class hierarchy are resolved. I.e. we follow the conditional `Collection`
     indirection in typeshed. *)
  assert_type_errors
    {|
      import typing
      def foo(input: typing.Sequence[int]) -> typing.Iterable[int]:
        return input
    |}
    [];
  assert_type_errors
    "def foo() -> str: return None"
    ["Incompatible return type [7]: Expected `str` but got `None`."];
  assert_type_errors "def foo() -> typing.Optional[str]: return None" [];
  assert_type_errors "def foo() -> typing.Optional[int]: return 1" [];
  assert_type_errors
    {|
      import typing
      def foo(flag: bool) -> typing.Optional[float]:
          a = 1.0
          if flag:
            a = None
          return a
    |}
    [];
  assert_type_errors
    {|
      import typing;
      def foo() -> typing.Optional[int]:
          return 1.0
    |}
    ["Incompatible return type [7]: Expected `typing.Optional[int]` but got `float`."];
  assert_type_errors
    {|
      import typing
      def foo(optional: typing.Optional[int]) -> int:
          if optional:
            return optional
          else:
            return -1
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo( **kwargs: int) -> None:
        return kwargs
    |}
    ["Incompatible return type [7]: Expected `None` but got `typing.Dict[str, int]`."];
  assert_type_errors
    {|
      def f( *args: int) -> None:
       pass
      def g( *args: int) -> None:
        return f( *args)
    |}
    [];

  (* Object methods are picked up for optionals. *)
  assert_type_errors {|
      def f() -> int:
        return None.__sizeof__()
    |} [];

  (* Builtins. *)
  assert_default_type_errors
    {|
      import typing
      def f() -> str:
        return __name__
      def g() -> str:
        return __file__
      def h() -> str:
        return typing.__name__
      def dict() -> typing.Dict[str, typing.Any]:
        return __dict__
      def i() -> str:
        return ...
    |}
    [];
  assert_type_errors
    {|
      import builtins
      def f() -> int:
        return builtins.__name__
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  (* Meta. *)
  assert_type_errors
    {|
      import typing
      def f(meta: typing.Type[int]) -> type[int]:
        return meta
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(meta: type[int]) -> typing.Type[int]:
        return meta
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(meta: type) -> typing.Type[int]:
        return meta
    |}
    [ "Invalid type parameters [24]: Generic type `type` expects 1 type parameter, use \
       `typing.Type` to avoid runtime subscripting errors.";
      "Incompatible return type [7]: Expected `typing.Type[int]` but got "
      ^ "`typing.Type[typing.Any]`." ];
  assert_type_errors
    {|
      class other(): pass
      def foo() -> other:
        result = 0
        if True:
          result = not_annotated()
        return result
    |}
    ["Incompatible return type [7]: Expected `other` but got `unknown`."];
  assert_type_errors
    {|
      def derp(x) -> None:
        y = {
            "a": x,
            "b": True,
            "c": False
        }
        return y
    |}
    [ "Missing parameter annotation [2]: Parameter `x` has no type specified.";
      "Incompatible return type [7]: Expected `None` but got `typing.Dict[str, typing.Any]`." ]


let test_check_collections context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def f(x: typing.List[int]) -> typing.Set[str]:
        return {1, *x}
    |}
    ["Incompatible return type [7]: Expected `typing.Set[str]` but got `typing.Set[int]`."];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.Optional[typing.List[int]]) -> typing.List[int]:
        return input or []
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
        return input or set()
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.Optional[typing.Dict[int, str]]) -> typing.Dict[int, str]:
        return input or {}
    |}
    []


let test_check_meta_annotations context =
  assert_type_errors
    ~context
    {|
      import typing
      class Class:
        pass
      def foo() -> typing.Type[Class]:
        return Class
    |}
    []


let test_check_noreturn context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def no_return() -> typing.NoReturn:
        return 0
    |}
    ["Incompatible return type [7]: Expected `typing.NoReturn` but got `int`."];
  assert_type_errors
    {|
      import typing
      def no_return() -> typing.NoReturn:
        # We implicitly return None, so have to accept this.
        return None
    |}
    [];
  assert_type_errors
    {|
      import typing
      def no_return(input: typing.Optional[int]) -> int:
        if input is None:
          sys.exit(-1)
        return input
    |}
    [];
  assert_type_errors
    {|
      import sys
      def no_return() -> str:
        sys.exit(0)  # once control flow terminates, we know input won't be returned.
    |}
    [];
  assert_type_errors
    {|
      def may_not_return() -> str:
        if condition():
          sys.exit(0)
        else:
          return ""
    |}
    [];
  assert_type_errors
    {|
      def no_return() -> int:
        if condition():
          return 1
        else:
          sys.exit(0)
    |}
    []


let () =
  "return"
  >::: [ "check_return" >:: test_check_return;
         "check_return_control_flow" >:: test_check_return_control_flow;
         "check_collections" >:: test_check_collections;
         "check_meta_annotations" >:: test_check_meta_annotations;
         "check_noreturn" >:: test_check_noreturn ]
  |> Test.run
