(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_type_is =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import Any, Sequence, TypeIs

              def is_str_list(val: Sequence[Any]) -> TypeIs[Sequence[str]]:
                  """Determines whether all objects in the list are strings"""
                  return all(isinstance(x, str) for x in val)

              def foo(xs: Sequence[int | str]) -> None:
                  if is_str_list(xs):
                      reveal_type(xs)
                  else:
                      reveal_type(xs)
            |}
           [
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`.";
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[typing.Union[int, str]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Callable
              def typeguard_bool(x: object) -> TypeIs[bool]:
                  return True

              def typeguard_int(x: object) -> TypeIs[int]:
                  return True

              a: Callable[[object], TypeIs[int]] = typeguard_bool
              b: Callable[[object], TypeIs[bool]] = typeguard_bool
              c: Callable[[object], TypeIs[int]] = typeguard_int
              d: Callable[[object], TypeIs[bool]] = typeguard_int
            |}
           [
             (* TypeIs is invariant (unlike TypeGuard, which is covariant). *)
             "Incompatible variable type [9]: a is declared to have type \
              `typing.Callable[[object], TypeIs[int]]` but is used as type \
              `typing.Callable(typeguard_bool)[[Named(x, object)], TypeIs[bool]]`.";
             "Incompatible variable type [9]: d is declared to have type \
              `typing.Callable[[object], TypeIs[bool]]` but is used as type \
              `typing.Callable(typeguard_int)[[Named(x, object)], TypeIs[int]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def is_str() -> TypeIs[str]:
                  return True
            |}
           [
             "Invalid type guard [68]: User-defined type guard functions or methods must have at \
              least one input parameter.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def is_bytes_or_str(x: object) -> TypeIs[bytes | str]:
                  return isinstance(x, bytes) or isinstance(x, str)

              def func(x: int | bytes | str) -> None:
                  if is_bytes_or_str(x):
                      reveal_type(x)  # bytes | str
                  else:
                      reveal_type(x)  # int
            |}
           [
             (* TypeIs, unlike TypeGuard, narrows on the negative case. *)
             "Revealed type [-1]: Revealed type for `x` is `typing.Union[bytes, str]`.";
             "Revealed type [-1]: Revealed type for `x` is `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeVar, TypeIs, Tuple

              _T = TypeVar("_T")

              def is_two_element_tuple(val: Tuple[_T, ...]) -> TypeIs[Tuple[_T, _T]]:
                  return len(val) == 2

              def func(names: Tuple[str, ...]) -> None:
                  if is_two_element_tuple(names):
                      reveal_type(names)  # Tuple[str, str]
                  else:
                      reveal_type(names)  # Tuple[str, ...]
            |}
           [
             (* We don't have a closed-form representation for a Tuple with any number of exements
                *except* 2, so the negative case doesn't narrow here. *)
             "Revealed type [-1]: Revealed type for `names` is `Tuple[str, str]`.";
             (* In addition we have a bug... TODO(T195443878): gradual types should not narrow to
                unreachable in the negative case, and variadic tuples are gradual.

                "Revealed type [-1]: Revealed type for `names` is `typing.Tuple[str, ...]`."; *)
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs

              def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              class Foo():
                x: Sequence[int | str] = [5, "hello"]

              def bar() -> None:
                obj = Foo()
                if is_str_list(obj.x):
                  reveal_type(obj.x)
            |}
           ["Revealed type [-1]: Revealed type for `obj.x` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs

              def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              x: Sequence[int | str] = [5, "hello"]
              if is_str_list(x):
                reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs

              def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              class Foo():
                x: Sequence[int | str] = [5, "hello"]

              obj = Foo()
              if is_str_list(obj.x):
                reveal_type(obj.x)
            |}
           ["Revealed type [-1]: Revealed type for `obj.x` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Sequence

              def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              def bar(xs: Sequence[int | str]) -> None:
                  if is_str_list(val=xs):
                      reveal_type(xs)
            |}
           ["Revealed type [-1]: Revealed type for `xs` is `Sequence[typing.Union[int, str]]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import Sequence, TypeIs, TypedDict, Any, Dict

              class Person(TypedDict):
                  name: str
                  age: int

              def is_person(val: Dict[Any, Any]) -> "TypeIs[Person]":
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
           [
             (* This is a bug in TypedDict ordering, but the type guard validation is correct *)
             "Invalid type guard [68]: The narrowed type Person of this type guard is not a \
              subtype of the first positional parameter type Dict[typing.Any, typing.Any].";
             "Revealed type [-1]: Revealed type for `val` is `Person`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs, TypedDict, Any, Dict

              def all_values_int(d: Dict[str, Any]) -> TypeIs[Dict[str,int]]:
                  return all(isinstance(val, int) for val in d.values())

              def foo(x: Dict[str, Any]) -> None:
                  if all_values_int(x):
                      reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `Dict[str, int]`."];
    ]


let test_type_guard =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import Any, Sequence, TypeGuard

              def is_str_list(val: Sequence[Any]) -> TypeGuard[Sequence[str]]:
                  """Determines whether all objects in the list are strings"""
                  return all(isinstance(x, str) for x in val)

              def foo(xs: Sequence[int | str]) -> None:
                  if is_str_list(xs):
                      reveal_type(xs)
                  else:
                      reveal_type(xs)
            |}
           [
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`.";
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[typing.Union[int, str]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeGuard, Callable
              def typeguard_bool(x: object) -> TypeGuard[bool]:
                  return True

              def typeguard_int(x: object) -> TypeGuard[int]:
                  return True

              a: Callable[[object], TypeGuard[int]] = typeguard_bool
              b: Callable[[object], TypeGuard[bool]] = typeguard_bool
              c: Callable[[object], TypeGuard[int]] = typeguard_int
              d: Callable[[object], TypeGuard[bool]] = typeguard_int
            |}
           [
             (* TypeGuard is covariant (unlike TypeIs, which is invariant), so we only get errors if
                we try to go in a contravariant direction. *)
             "Incompatible variable type [9]: d is declared to have type \
              `typing.Callable[[object], TypeGuard[bool]]` but is used as type \
              `typing.Callable(typeguard_int)[[Named(x, object)], TypeGuard[int]]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeGuard

              def is_str() -> TypeGuard[str]:
                  return True
            |}
           [
             "Invalid type guard [68]: User-defined type guard functions or methods must have at \
              least one input parameter.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeGuard

              def is_bytes_or_str(x: object) -> TypeGuard[bytes | str]:
                  return isinstance(x, bytes) or isinstance(x, str)

              def func(x: int | bytes | str) -> None:
                  if is_bytes_or_str(x):
                      reveal_type(x)  # bytes | str
                  else:
                      reveal_type(x)  # int | bytes | str
            |}
           [
             (* TypeGuard, unlike TypeIs, does not narrow the negative case. *)
             "Revealed type [-1]: Revealed type for `x` is `typing.Union[bytes, str]`.";
             "Revealed type [-1]: Revealed type for `x` is `typing.Union[bytes, int, str]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeGuard

              def is_str_list(val: Sequence[int | str]) -> TypeGuard[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              class Foo():
                x: Sequence[int | str] = [5, "hello"]

              def bar() -> None:
                obj = Foo()
                if is_str_list(obj.x):
                  reveal_type(obj.x)
            |}
           ["Revealed type [-1]: Revealed type for `obj.x` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeGuard

              def is_str_list(val: Sequence[int | str]) -> TypeGuard[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              x: Sequence[int | str] = [5, "hello"]
              if is_str_list(x):
                reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeGuard

              def is_str_list(val: Sequence[int | str]) -> TypeGuard[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              class Foo():
                x: Sequence[int | str] = [5, "hello"]

              obj = Foo()
              if is_str_list(obj.x):
                reveal_type(obj.x)
            |}
           ["Revealed type [-1]: Revealed type for `obj.x` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeGuard, Sequence

              def is_str_list(val: Sequence[int | str]) -> TypeGuard[Sequence[str]]:
                  return all(isinstance(x, str) for x in val)

              def bar(xs: Sequence[int | str]) -> None:
                  if is_str_list(val=xs):
                      reveal_type(xs)
            |}
           ["Revealed type [-1]: Revealed type for `xs` is `Sequence[typing.Union[int, str]]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import Sequence, TypeGuard, TypedDict, Any, Dict

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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeGuard, TypedDict, Any, Dict

              def all_values_int(d: Dict[str, Any]) -> TypeGuard[Dict[str,int]]:
                  return all(isinstance(val, int) for val in d.values())

              def foo(x: Dict[str, Any]) -> None:
                  if all_values_int(x):
                      reveal_type(x)
            |}
           ["Revealed type [-1]: Revealed type for `x` is `Dict[str, int]`."];
    ]


let test_multiple_arguments =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Sequence

              def is_str_list(val: Sequence[int | str], allow_empty: bool) -> TypeIs[Sequence[str]]:
                  if len(val) == 0:
                      return allow_empty
                  return all(isinstance(x, str) for x in val)

              def foo(xs: Sequence[int | str]) -> None:
                  if is_str_list(xs, True):
                      reveal_type(xs)
                  elif is_str_list(xs, False):
                      reveal_type(xs)
            |}
           [
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`.";
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import TypeIs, Sequence, TypeVar, Any, Type

              _T = TypeVar("_T")

              def is_list_of(val: Sequence[Any], type: Type[_T]) -> TypeIs[Sequence[_T]]:
                  return all(isinstance(x, type) for x in val)

              def foo(xs: Sequence[int | str]) -> None:
                  if is_list_of(xs, int):
                      reveal_type(xs)
                  elif is_list_of(xs, str):
                      reveal_type(xs)
                  else:
                      reveal_type(xs)
            |}
           [
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[int]`.";
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`.";
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[typing.Union[int, str]]`.";
           ];
    ]


let test_methods =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              class Foo:
                def is_str(val: object) -> TypeIs[str]:
                    return isinstance(val, str)
            |}
           [
             "Invalid type guard [68]: User-defined type guard functions or methods must have at \
              least one input parameter.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              class Foo:
                @staticmethod
                def is_str(val: object) -> TypeIs[str]:
                    return isinstance(val, str)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              class Foo:
                def is_str(self, val: object) -> TypeIs[str]:
                    return isinstance(val, str)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              class Foo:
                @classmethod
                def is_str(cls, val: object) -> TypeIs[str]:
                    return isinstance(val, str)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              class Foo:
                @classmethod
                def is_str(val: object) -> TypeIs[str]:
                    return isinstance(val, str)
            |}
           [
             "Invalid type guard [68]: User-defined type guard functions or methods must have at \
              least one input parameter.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs

              class Foo:
                def is_str_list(self, val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                    return all(isinstance(x, str) for x in val)
                def bar(self, xs: Sequence[int | str]) -> None:
                    if self.is_str_list(xs):
                        reveal_type(xs)
            |}
           ["Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs

              class Foo:
                def is_str_list(self, val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                    return all(isinstance(x, str) for x in val)

              def bar(xs: Sequence[int | str]) -> None:
                  if Foo().is_str_list(xs):
                      reveal_type(xs)
            |}
           ["Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Sequence

              class Foo:
                def is_str_list(self, val: Sequence[int | str], allow_empty: bool) -> TypeIs[Sequence[str]]:
                    if len(val) == 0:
                        return allow_empty
                    return all(isinstance(x, str) for x in val)
                def bar(self, xs: Sequence[int | str]) -> None:
                    if self.is_str_list(xs, True):
                        reveal_type(xs)
                    elif self.is_str_list(xs, False):
                        reveal_type(xs)
            |}
           [
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`.";
             "Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Sequence

              class Foo:
                @classmethod
                def is_str_list(cls, val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                    return all(isinstance(x, str) for x in val)
              def bar(xs: Sequence[int | str]) -> None:
                  if Foo.is_str_list(xs):
                      reveal_type(xs)
            |}
           ["Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Sequence

              class Foo:
                @staticmethod
                def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                    return all(isinstance(x, str) for x in val)
              def bar(xs: Sequence[int | str]) -> None:
                  if Foo.is_str_list(xs):
                      reveal_type(xs)
            |}
           ["Revealed type [-1]: Revealed type for `xs` is `Sequence[str]`."];
    ]


let test_callback =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Callable, TypeIs

              def simple_fn(x: bool, is_int: Callable[[object], TypeIs[int]]) -> None:
                  val: int | str = 5 if x else "foo"
                  if is_int(val):
                    reveal_type(val)
            |}
           ["Revealed type [-1]: Revealed type for `val` is `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import Callable, TypeIs, TypeVar, Any, overload

              _T = TypeVar("_T")

              def returns_typeguard(val: int) -> TypeIs[int]:
                  ...

              def returns_bool(val: int) -> bool:
                  ...

              def simple_fn(callback: Callable[[int], bool]) -> None:
                  ...

              @overload
              def overloaded_fn(callback: Callable[[int], TypeIs[_T]]) -> _T:
                  ...

              @overload
              def overloaded_fn(callback: Callable[[int], bool]) -> None:
                  ...

              def overloaded_fn(callback: Callable[[int], Any]) -> Any:
                  ...

              x1: None = simple_fn(returns_typeguard)
              x2: None = simple_fn(returns_bool)
              x3: int  = overloaded_fn(returns_typeguard)
              x4: None = overloaded_fn(returns_bool)
            |}
           [];
    ]


let test_return_type =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs

              def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                return all(isinstance(x, str) for x in val)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence, TypeIs

              def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                return 5
            |}
           ["Incompatible return type [7]: Expected `bool` but got `int`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              x: int = 5

              def typeguard_parameter_should_be_a_type(x: int) -> TypeIs[x]:
                return True
            |}
           ["Undefined or invalid type [11]: Annotation `x` is not defined as a type."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def typeguard_takes_only_one_parameter(x: int) -> TypeIs[int, int]:
                return True
            |}
           [
             "Invalid type parameters [24]: Generic type `TypeIs` expects 1 type parameter, \
              received 2.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def typeguard_needs_a_parameter(x: int) -> TypeIs:
                return True
            |}
           ["Invalid type parameters [24]: Generic type `TypeIs` expects 1 type parameter."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import TypeIs, Sequence, TypeVar, Any, Type

              _T = TypeVar("_T")

              def is_list_of(val: Sequence[Any], type: Type[_T]) -> TypeIs[Sequence[_T]]:
                  return all(isinstance(x, type) for x in val)

              def is_str_list(val: Sequence[Any]) -> TypeIs[Sequence[str]]:
                return is_list_of(val, str)
            |}
           [];
    ]


let test_boolean_operators =
  test_list
    [
      (* We see only two revealed types because the `False` branches are unreachable. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def typeguard_fn(val: int | str) -> TypeIs[int]:
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def int_or_str(val: int | str | bool) -> TypeIs[int | str]:
                return True

              def str_or_bool(val: int | str | bool) -> TypeIs[str | bool]:
                return True

              def foo(val: int | str | bool) -> None:
                if int_or_str(val) and str_or_bool(val):
                  reveal_type(val)
            |}
           ["Revealed type [-1]: Revealed type for `val` is `typing.Union[bool, str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def is_int(val: int | str | bool) -> TypeIs[int]:
                return True

              def is_str(val: int | str | bool) -> TypeIs[str]:
                return True

              def foo(val: int | str | bool) -> None:
                if is_int(val) or is_str(val):
                  reveal_type(val)
            |}
           ["Revealed type [-1]: Revealed type for `val` is `typing.Union[int, str]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_default_type_errors
           {|
              from typing import TypeIs, List, TypeVar, Any, Type, Union

              _T = TypeVar("_T")

              def is_list_of(val: List[Any], type: Type[_T]) -> TypeIs[List[_T]]:
                  return all(isinstance(x, type) for x in val)

              def foo(val: List[Any]) -> None:
                if is_list_of(val, int) or is_list_of(val, str):
                  reveal_type(val)
            |}
           ["Revealed type [-1]: Revealed type for `val` is `Union[List[int], List[str]]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Union

              def typeguard_fn(val: int | str) -> TypeIs[int]:
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Union

              def typeguard_fn(val: int | str) -> TypeIs[int]:
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
    ]


let test_consistency_checks =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def custom_type_guard(val: list[int | str]) -> TypeIs[list[str]]:
                return all(isinstance(x, str) for x in val)
            |}
           [
             "Invalid type guard [68]: The narrowed type typing.List[str] of this type guard is \
              not a subtype of the first positional parameter type typing.List[typing.Union[int, \
              str]].";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeGuard

              def custom_type_guard(val: list[int | str]) -> TypeGuard[list[str]]:
                return all(isinstance(x, str) for x in val)
            |}
           [ (* The consistency check is applied only to `TypeIs`, not `TypeGuard`. See
                https://typing.readthedocs.io/en/latest/spec/narrowing.html. *) ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              class CustomTypeGuard:
                  def guard(self, val: list[int | str]) -> TypeIs[list[str]]:
                      return all(isinstance(x, str) for x in val)
            |}
           [
             "Invalid type guard [68]: The narrowed type typing.List[str] of this type guard is \
              not a subtype of the first positional parameter type typing.List[typing.Union[int, \
              str]].";
           ];
    ]


let test_misc =
  test_list
    [
      (* Test ternary operator *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs, Union

              def typeguard_fn(val: int | str) -> TypeIs[int]:
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs

              def typeguard_fn(val: int | str) -> TypeIs[int]:
                return True

              def foo(val: int | str) -> None:
                assert typeguard_fn(val)
                reveal_type(val)
            |}
           ["Revealed type [-1]: Revealed type for `val` is `int`."];
      (* Test frozen dataclass attributes *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import TypeIs
              from dataclasses import dataclass

              def typeguard_fn(val: int | str) -> TypeIs[int]:
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Sequence
              from typing_extensions import TypeIs

              def is_str_list(val: Sequence[int | str]) -> TypeIs[Sequence[str]]:
                return all(isinstance(x, str) for x in val)
            |}
           [];
    ]


let () =
  "type_guard"
  >::: [
         test_type_guard;
         test_type_is;
         test_multiple_arguments;
         test_methods;
         test_callback;
         test_return_type;
         test_boolean_operators;
         test_consistency_checks;
         test_misc;
       ]
  |> Test.run
