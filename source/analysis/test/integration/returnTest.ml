(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
    ["Incompatible return type [7]: Expected `List[str]` but got `int`."];
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
    [
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, use \
       `typing.Dict` to avoid runtime subscripting errors.";
      "Missing return annotation [3]: Return type must be specified as type "
      ^ "that does not contain `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      def f() -> dict:
        return {}
      def foo() -> typing.Dict[typing.Any]:
        return f()
    |}
    [
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, use \
       `typing.Dict` to avoid runtime subscripting errors.";
      "Missing return annotation [3]: Return type must be specified as type that does "
      ^ "not contain `Any`.";
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, received 1, \
       use `typing.Dict` to avoid runtime subscripting errors.";
    ];
  assert_type_errors
    {|
      import typing
      x: typing.Type
      def foo() -> typing.Type[typing.Any]:
        return x
    |}
    [
      "Invalid type parameters [24]: Generic type `type` expects 1 type parameter, use \
       `typing.Type` to avoid runtime subscripting errors.";
      "Missing return annotation [3]: Return type must be specified as type that does not contain \
       `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      def derp()->typing.Union[str, None]:
          return None
    |}
    [];
  assert_type_errors
    "def foo() -> str: return 1.0\ndef bar() -> int: return ''"
    [
      "Incompatible return type [7]: Expected `str` but got `float`.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
    ];
  assert_type_errors "class A: pass\ndef foo() -> A: return A()" [];
  assert_type_errors
    "class A: pass\ndef foo() -> A: return 1"
    ["Incompatible return type [7]: Expected `A` but got `int`."];
  assert_type_errors "def bar() -> str: return ''\ndef foo() -> str: return bar()" [];
  assert_type_errors
    "from builtins import not_annotated\ndef foo() -> str: return not_annotated()"
    [];
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
    ["Incompatible return type [7]: Expected `Set[int]` but got `Set[str]`."];
  assert_type_errors
    {|
      from builtins import condition
      def foo() -> str:
        if condition():
          return 1
        else:
          return 2
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T')
      def foo(x: list[T])-> T:
        return x

      def bar(x: list[int]) -> int:
        return foo(x)
    |}
    ["Incompatible return type [7]: Expected `Variable[T]` but got `List[Variable[T]]`."];
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
    ["Incompatible return type [7]: Expected `C` but got `Optional[C]`."];
  assert_type_errors
    {|
      class A:
        pass
      x = A()
      def foo() -> type(x):
        return x
    |}
    ["Invalid type [31]: Expression `type($local_test$x)` is not a valid type."];
  assert_type_errors
    {|
      from typing import Type
      class A:
        pass
      x = A()
      def foo() -> Type(x):
        return x
    |}
    ["Invalid type [31]: Expression `typing.Type($local_test$x)` is not a valid type."];
  assert_type_errors
    {|
      from typing import Tuple, Type
      class A:
        pass
      x = A()
      def foo() -> Tuple[Type(x)]:
        return (x,)
    |}
    [
      "Invalid type [31]: Expression `typing.Tuple[typing.Type($local_test$x)]` is not a valid type.";
    ];
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
    [];
  assert_default_type_errors
    {|
      import typing
      def bar(x: typing.Union[int, typing.Any, None]) -> int:
          return x
    |}
    [];
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
    [
      "Unbound name [10]: Name `unknown_condition` is used but not defined in the current scope.";
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`.";
    ];
  assert_type_errors
    {|
      def foo() -> int:
        if unknown_condition:
          return 1
        else:
          x = 1
    |}
    [
      "Unbound name [10]: Name `unknown_condition` is used but not defined in the current scope.";
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`.";
    ];
  assert_type_errors
    {|
      import typing
      x: typing.List[int]
      def foo() -> list:
        return x
    |}
    [
      "Invalid type parameters [24]: Generic type `list` expects 1 type parameter, use \
       `typing.List` to avoid runtime subscripting errors.";
    ];
  assert_type_errors
    {|
      import typing
      x: typing.List[typing.Any]
      def foo() -> list:
        return x
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` must be specified "
      ^ "as type that does not contain `Any`.";
      "Invalid type parameters [24]: Generic type `list` expects 1 type parameter, use \
       `typing.List` to avoid runtime subscripting errors.";
    ];
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
  assert_type_errors "import typing\ndef foo() -> typing.Optional[str]: return None" [];
  assert_type_errors "import typing\ndef foo() -> typing.Optional[int]: return 1" [];
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
    ["Incompatible return type [7]: Expected `Optional[int]` but got `float`."];
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
    ["Incompatible return type [7]: Expected `None` but got `Dict[str, int]`."];
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
      def doc() -> str:
        return __doc__
      def path() -> typing.Iterable[str]:
        return __path__
    |}
    [];
  assert_type_errors
    {|
      import builtins
      def f() -> int:
        return builtins.__name__
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      def f() -> int:
        return __doc__
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      def f() -> int:
        return __package__
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
    [
      "Invalid type parameters [24]: Generic type `type` expects 1 type parameter, use \
       `typing.Type` to avoid runtime subscripting errors.";
    ];
  assert_type_errors
    {|
      from builtins import not_annotated
      class other(): pass
      def foo() -> other:
        result = 0
        if True:
          result = not_annotated()
        return result
    |}
    [];
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
    [
      "Missing parameter annotation [2]: Parameter `x` has no type specified.";
      "Incompatible return type [7]: Expected `None` but got `Dict[str, typing.Any]`.";
    ]


let test_check_collections context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def f(x: typing.List[int]) -> typing.Set[str]:
        return {1, *x}
    |}
    ["Incompatible return type [7]: Expected `Set[str]` but got `Set[int]`."];
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
    ["Incompatible return type [7]: Expected `NoReturn` but got `int`."];
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
      import sys
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
      import sys
      from builtins import condition
      def may_not_return() -> str:
        if condition():
          sys.exit(0)
        else:
          return ""
    |}
    [];
  assert_type_errors
    {|
      import sys
      from builtins import condition
      def no_return() -> int:
        if condition():
          return 1
        else:
          sys.exit(0)
    |}
    []


let test_check_return_unimplemented context =
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_strict_type_errors
    {|
      def f() -> int:
        pass
    |}
    ["Incompatible return type [7]: Expected `int` but got implicit return value of `None`."];
  assert_strict_type_errors
    {|
      def f():
        pass
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_strict_type_errors
    {|
      class Foo():
        def run(self) -> int:
          pass
    |}
    ["Incompatible return type [7]: Expected `int` but got implicit return value of `None`."];
  assert_strict_type_errors
    {|
      from abc import ABC, ABCMeta, abstractmethod

      class MyABC(ABC):
        @abstractmethod
        def run(self) -> int:
              pass

      class MyABC2(metaclass=ABCMeta):
        @abstractmethod
        def run(self) -> int:
            pass
    |}
    [];
  assert_strict_type_errors {|
      def f() -> None:
        pass
    |} [];
  assert_default_type_errors {|
      def f() -> int:
        pass
    |} [];
  assert_default_type_errors
    {|
      def foo() -> int:
        return ''
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."]


let () =
  "return"
  >::: [
         "check_return" >:: test_check_return;
         "check_return_control_flow" >:: test_check_return_control_flow;
         "check_collections" >:: test_check_collections;
         "check_meta_annotations" >:: test_check_meta_annotations;
         "check_noreturn" >:: test_check_noreturn;
         "check_return_unimplemented" >:: test_check_return_unimplemented;
       ]
  |> Test.run
