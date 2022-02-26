(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_type_guard context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_default_type_errors
    {|
      from typing import Any, List, TypeGuard

      def is_str_list(val: List[Any]) -> TypeGuard[List[str]]:
          """Determines whether all objects in the list are strings"""
          return all(isinstance(x, str) for x in val)

      def foo(xs: List[int | str]) -> None:
          if is_str_list(xs):
              reveal_type(xs)
          else:
              reveal_type(xs)
    |}
    [
      "Revealed type [-1]: Revealed type for `xs` is `List[str]`.";
      "Revealed type [-1]: Revealed type for `xs` is `List[typing.Union[int, str]]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, TypeGuard, Tuple

      _T = TypeVar("_T")

      def is_two_element_tuple(val: Tuple[_T, ...]) -> TypeGuard[Tuple[_T, _T]]:
          return len(val) == 2

      def func(names: Tuple[str, ...]) -> None:
          if is_two_element_tuple(names):
              reveal_type(names)  # Tuple[str, str]
          else:
              reveal_type(names)  # Tuple[str, ...]
    |}
    [
      "Revealed type [-1]: Revealed type for `names` is `Tuple[str, str]`.";
      "Revealed type [-1]: Revealed type for `names` is `typing.Tuple[str, ...]`.";
    ];
  assert_type_errors
    {|
      from typing import List, TypeGuard

      def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
          return all(isinstance(x, str) for x in val)

      class Foo():
        x: List[int | str] = [5, "hello"]

      def bar() -> None:
        obj = Foo()
        if is_str_list(obj.x):
          reveal_type(obj.x)
    |}
    ["Revealed type [-1]: Revealed type for `obj.x` is `List[str]`."];
  assert_type_errors
    {|
      from typing import List, TypeGuard

      def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
          return all(isinstance(x, str) for x in val)

      x: List[int | str] = [5, "hello"]
      if is_str_list(x):
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `List[str]`."];
  assert_type_errors
    {|
      from typing import List, TypeGuard

      def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
          return all(isinstance(x, str) for x in val)

      class Foo():
        x: List[int | str] = [5, "hello"]

      obj = Foo()
      if is_str_list(obj.x):
        reveal_type(obj.x)
    |}
    ["Revealed type [-1]: Revealed type for `obj.x` is `List[str]`."];
  assert_type_errors
    {|
      from typing import TypeGuard, List

      def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
          return all(isinstance(x, str) for x in val)

      def bar(xs: List[int | str]) -> None:
          if is_str_list(val=xs):
              reveal_type(xs)
    |}
    ["Revealed type [-1]: Revealed type for `xs` is `List[typing.Union[int, str]]`."];
  assert_default_type_errors
    {|
      from typing import List, TypeGuard, TypedDict, Any, Dict

      class Person(TypedDict):
          name: str
          age: int

      def is_person(val: Dict[Any, Any]) -> "TypeGuard[Person]":
          try:
              return isinstance(val["name"], str) and isinstance(val["age"], int)
          except KeyError:
              return False

      def print_age(val: Dict[Any, Any]) -> None:
          if is_person(val):
              reveal_type(val)
              print(f"Age: {val['age']}")
          else:
              print("Not a person!")
    |}
    ["Revealed type [-1]: Revealed type for `val` is `Person`."];
  assert_type_errors
    {|
      from typing import List, TypeGuard, TypedDict, Any, Dict

      def all_values_int(d: Dict[str, Any]) -> TypeGuard[Dict[str,int]]:
          return all(isinstance(val, int) for val in d.values())

      def foo(x: Dict[str, Any]) -> None:
          if all_values_int(x):
              reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `Dict[str, int]`."];

  ()


let test_multiple_arguments context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      from typing import TypeGuard, List

      def is_str_list(val: List[int | str], allow_empty: bool) -> TypeGuard[List[str]]:
          if len(val) == 0:
              return allow_empty
          return all(isinstance(x, str) for x in val)

      def foo(xs: List[int | str]) -> None:
          if is_str_list(xs, True):
              reveal_type(xs)
          elif is_str_list(xs, False):
              reveal_type(xs)
    |}
    [
      "Revealed type [-1]: Revealed type for `xs` is `List[str]`.";
      "Revealed type [-1]: Revealed type for `xs` is `List[str]`.";
    ];
  assert_default_type_errors
    {|
      from typing import TypeGuard, List, TypeVar, Any, Type

      _T = TypeVar("_T")

      def is_list_of(val: List[Any], type: Type[_T]) -> TypeGuard[List[_T]]:
          return all(isinstance(x, type) for x in val)

      def foo(xs: List[int | str]) -> None:
          if is_list_of(xs, int):
              reveal_type(xs)
          elif is_list_of(xs, str):
              reveal_type(xs)
          else:
              reveal_type(xs)
    |}
    [
      "Revealed type [-1]: Revealed type for `xs` is `List[int]`.";
      "Revealed type [-1]: Revealed type for `xs` is `List[str]`.";
      "Revealed type [-1]: Revealed type for `xs` is `List[typing.Union[int, str]]`.";
    ];
  ()


let test_methods context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import List, TypeGuard

      class Foo:
        def is_str_list(self, val: List[int | str]) -> TypeGuard[List[str]]:
            return all(isinstance(x, str) for x in val)
        def bar(self, xs: List[int | str]) -> None:
            if self.is_str_list(xs):
                reveal_type(xs)
    |}
    ["Revealed type [-1]: Revealed type for `xs` is `List[str]`."];
  assert_type_errors
    {|
      from typing import List, TypeGuard

      class Foo:
        def is_str_list(self, val: List[int | str]) -> TypeGuard[List[str]]:
            return all(isinstance(x, str) for x in val)

      def bar(xs: List[int | str]) -> None:
          if Foo().is_str_list(xs):
              reveal_type(xs)
    |}
    ["Revealed type [-1]: Revealed type for `xs` is `List[str]`."];
  assert_type_errors
    {|
      from typing import TypeGuard, List

      class Foo:
        def is_str_list(self, val: List[int | str], allow_empty: bool) -> TypeGuard[List[str]]:
            if len(val) == 0:
                return allow_empty
            return all(isinstance(x, str) for x in val)
        def bar(self, xs: List[int | str]) -> None:
            if self.is_str_list(xs, True):
                reveal_type(xs)
            elif self.is_str_list(xs, False):
                reveal_type(xs)
    |}
    [
      "Revealed type [-1]: Revealed type for `xs` is `List[str]`.";
      "Revealed type [-1]: Revealed type for `xs` is `List[str]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeGuard, List

      class Foo:
        @classmethod
        def is_str_list(cls, val: List[int | str]) -> TypeGuard[List[str]]:
            return all(isinstance(x, str) for x in val)
      def bar(xs: List[int | str]) -> None:
          if Foo.is_str_list(xs):
              reveal_type(xs)
    |}
    ["Revealed type [-1]: Revealed type for `xs` is `List[str]`."];
  assert_type_errors
    {|
      from typing import TypeGuard, List

      class Foo:
        @staticmethod
        def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
            return all(isinstance(x, str) for x in val)
      def bar(xs: List[int | str]) -> None:
          if Foo.is_str_list(xs):
              reveal_type(xs)
    |}
    ["Revealed type [-1]: Revealed type for `xs` is `List[str]`."];
  ()


let test_callback context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors
    {|
      from typing import Callable, TypeGuard

      def simple_fn(x: bool, is_int: Callable[[object], TypeGuard[int]]) -> None:
          val: int | str = 5 if x else "foo"
          if is_int(val):
            reveal_type(val)
    |}
    ["Revealed type [-1]: Revealed type for `val` is `int`."];
  assert_default_type_errors
    {|
      from typing import Callable, TypeGuard, TypeVar, Any, overload

      _T = TypeVar("_T")

      def returns_typeguard() -> TypeGuard[int]:
          ...

      def returns_bool() -> bool:
          ...

      def simple_fn(callback: Callable[[], bool]) -> None:
          ...

      @overload
      def overloaded_fn(callback: Callable[[], TypeGuard[_T]]) -> _T:
          ...

      @overload
      def overloaded_fn(callback: Callable[[], bool]) -> None:
          ...

      def overloaded_fn(callback: Callable[[], Any]) -> Any:
          ...

      x1: None = simple_fn(returns_typeguard)
      x2: None = simple_fn(returns_bool)
      x3: int  = overloaded_fn(returns_typeguard)
      x4: None = overloaded_fn(returns_bool)
    |}
    [];
  ()


let test_return_type context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in

  assert_type_errors
    {|
      from typing import List, TypeGuard

      def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
        return all(isinstance(x, str) for x in val)
    |}
    [];
  assert_type_errors
    {|
      from typing import List, TypeGuard

      def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
        return 5
    |}
    ["Incompatible return type [7]: Expected `bool` but got `int`."];
  assert_type_errors
    {|
      from typing import TypeGuard

      x: int = 5

      def typeguard_parameter_should_be_a_type(x: int) -> TypeGuard[x]:
        return True
    |}
    ["Undefined or invalid type [11]: Annotation `x` is not defined as a type."];
  assert_type_errors
    {|
      from typing import TypeGuard

      def typeguard_takes_only_one_parameter(x: int) -> TypeGuard[int, int]:
        return True
    |}
    ["Invalid type parameters [24]: Generic type `TypeGuard` expects 1 type parameter, received 2."];
  assert_type_errors
    {|
      from typing import TypeGuard

      def typeguard_needs_a_parameter(x: int) -> TypeGuard:
        return True
    |}
    ["Invalid type parameters [24]: Generic type `TypeGuard` expects 1 type parameter."];
  assert_default_type_errors
    {|
      from typing import TypeGuard, List, TypeVar, Any, Type

      _T = TypeVar("_T")

      def is_list_of(val: List[Any], type: Type[_T]) -> TypeGuard[List[_T]]:
          return all(isinstance(x, type) for x in val)

      def is_str_list(val: List[Any]) -> TypeGuard[List[str]]:
        return is_list_of(val, str)
    |}
    [];
  ()


let test_walrus_operator context =
  let assert_type_errors = assert_type_errors ~context in
  (* TODO(T95581122): `bar` should be narrowed to `int`. *)
  assert_type_errors
    {|
      from typing import TypeGuard

      def typeguard_fn(val: int | str) -> TypeGuard[int]:
        return True

      def foo(x: int | str) -> None:
        if typeguard_fn(bar := x):
          reveal_type(bar)
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `typing.Union[int, str]`."];
  ()


let test_boolean_operators context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  (* We see only two revealed types because the `False` branches are unreachable. *)
  assert_type_errors
    {|
      from typing import TypeGuard

      def typeguard_fn(val: int | str) -> TypeGuard[int]:
        return True

      def foo(val: int | str) -> None:
        if True and typeguard_fn(val):
          reveal_type(val)
        if False and typeguard_fn(val):
          reveal_type(val)
        if typeguard_fn(val) and True:
          reveal_type(val)
        if typeguard_fn(val) and False:
          reveal_type(val)
    |}
    [
      "Revealed type [-1]: Revealed type for `val` is `int`.";
      "Revealed type [-1]: Revealed type for `val` is `int`.";
    ];
  assert_type_errors
    {|
      from typing import TypeGuard

      def int_or_str(val: int | str | bool) -> TypeGuard[int | str]:
        return True

      def str_or_bool(val: int | str | bool) -> TypeGuard[str | bool]:
        return True

      def foo(val: int | str | bool) -> None:
        if int_or_str(val) and str_or_bool(val):
          reveal_type(val)
    |}
    ["Revealed type [-1]: Revealed type for `val` is `str`."];
  assert_type_errors
    {|
      from typing import TypeGuard

      def is_int(val: int | str | bool) -> TypeGuard[int]:
        return True

      def is_str(val: int | str | bool) -> TypeGuard[str]:
        return True

      def foo(val: int | str | bool) -> None:
        if is_int(val) or is_str(val):
          reveal_type(val)
    |}
    ["Revealed type [-1]: Revealed type for `val` is `typing.Union[int, str]`."];
  assert_default_type_errors
    {|
      from typing import TypeGuard, List, TypeVar, Any, Type, Union

      _T = TypeVar("_T")

      def is_list_of(val: List[Any], type: Type[_T]) -> TypeGuard[List[_T]]:
          return all(isinstance(x, type) for x in val)

      def foo(val: List[Any]) -> None:
        if is_list_of(val, int) or is_list_of(val, str):
          reveal_type(val)
    |}
    ["Revealed type [-1]: Revealed type for `val` is `Union[List[int], List[str]]`."];
  assert_type_errors
    {|
      from typing import TypeGuard, Union

      def typeguard_fn(val: int | str) -> TypeGuard[int]:
        return True

      def foo(val: int | str) -> None:
        if True or typeguard_fn(val):
          reveal_type(val)
        if False or typeguard_fn(val):
          reveal_type(val)
        if typeguard_fn(val) or True:
          reveal_type(val)
        if typeguard_fn(val) or False:
          reveal_type(val)
    |}
    [
      "Revealed type [-1]: Revealed type for `val` is `Union[int, str]`.";
      "Revealed type [-1]: Revealed type for `val` is `Union[int, str]`.";
      "Revealed type [-1]: Revealed type for `val` is `Union[int, str]`.";
      "Revealed type [-1]: Revealed type for `val` is `Union[int, str]`.";
    ];
  assert_type_errors
    {|
      from typing import TypeGuard, Union

      def typeguard_fn(val: int | str) -> TypeGuard[int]:
        return True

      def foo(val1: int | str, val2: int | str) -> None:
        if typeguard_fn(val1) and typeguard_fn(val2):
          reveal_type(val1)
          reveal_type(val2)
        if typeguard_fn(val1) or typeguard_fn(val2):
          reveal_type(val1)
          reveal_type(val2)
    |}
    [
      "Revealed type [-1]: Revealed type for `val1` is `int`.";
      "Revealed type [-1]: Revealed type for `val2` is `int`.";
      "Revealed type [-1]: Revealed type for `val1` is `Union[int, str]`.";
      "Revealed type [-1]: Revealed type for `val2` is `Union[int, str]`.";
    ];
  ()


let test_misc context =
  let assert_type_errors = assert_type_errors ~context in
  (* Test ternary operator *)
  assert_type_errors
    {|
      from typing import TypeGuard, Union

      def typeguard_fn(val: int | str) -> TypeGuard[int]:
        return True

      def bool_fn(val: int | str) -> bool:
        return True

      def first_int(x: int | str) -> None:
        y = x if typeguard_fn(x) else 5
        reveal_type(y)
        z = x if bool_fn(x) else 5
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `z` is `Union[int, str]`.";
    ];
  (* Test narrowing in assert statement *)
  assert_type_errors
    {|
      from typing import TypeGuard

      def typeguard_fn(val: int | str) -> TypeGuard[int]:
        return True

      def foo(val: int | str) -> None:
        assert typeguard_fn(val)
        reveal_type(val)
    |}
    ["Revealed type [-1]: Revealed type for `val` is `int`."];
  (* Test frozen dataclass attributes *)
  assert_type_errors
    {|
      from typing import TypeGuard
      from dataclasses import dataclass

      def typeguard_fn(val: int | str) -> TypeGuard[int]:
        return True

      @dataclass(frozen=True)
      class Bar():
        field: int | str = 5

      def foo(obj: Bar) -> None:
        if typeguard_fn(obj.field):
          reveal_type(obj.field)
    |}
    ["Revealed type [-1]: Revealed type for `obj.field` is `int`."];
  (* Test supported from typing_extensions as well. *)
  assert_type_errors
    {|
      from typing import List
      from typing_extensions import TypeGuard

      def is_str_list(val: List[int | str]) -> TypeGuard[List[str]]:
        return all(isinstance(x, str) for x in val)
    |}
    [];
  ()


let () =
  "type_guard"
  >::: [
         "test_type_guard" >:: test_type_guard;
         "test_multiple_arguments" >:: test_multiple_arguments;
         "test_methods" >:: test_methods;
         "test_callback" >:: test_callback;
         "test_return_type" >:: test_return_type;
         "test_walrus_operator" >:: test_walrus_operator;
         "test_boolean_operators" >:: test_boolean_operators;
         "test_misc" >:: test_misc;
       ]
  |> Test.run
