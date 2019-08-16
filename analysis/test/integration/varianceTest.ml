(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_variance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def narnia(): pass
      def foo() -> None:
        [narnia()] + [2]
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_type_errors
    {|
      def foo(input: str) -> typing.List[int]:
        return typing.cast(typing.List[float], input)
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[float]`."];
  assert_type_errors
    {|
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[unknown], input)
    |}
    [ "Missing parameter annotation [2]: Parameter `input` has no type specified.";
      "Incompatible return type [7]: Expected `typing.List[int]` but got `unknown`.";
      "Undefined type [11]: Type `unknown` is not defined." ];
  assert_type_errors
    {|
      def foo(a: typing.Mapping[str, float]) -> float:
        return a["a"]
      def bar(x: typing.Dict[str, int]) -> float:
        return foo(x)
    |}
    [];
  assert_type_errors
    {|
      def foo(d: typing.Dict[int, typing.Any]) -> None:
        d.update({ 1: 1 })
    |}
    [ "Missing parameter annotation [2]: Parameter `d` must have a type "
      ^ "that does not contain `Any`." ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      IV = TypeVar('IV')
      CV = TypeVar('CV', covariant=True)
      class A(Generic[IV]): pass
      class B(A[CV], Generic[CV]):pass
    |}
    [ "Invalid type variance [46]: The type variable `Variable[CV](covariant)` is incompatible \
       with parent class type variable `Variable[IV]` because subclasses cannot use more \
       permissive type variables than their superclasses." ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      IV = TypeVar('IV')
      CV = TypeVar('CV', contravariant=True)
      class A(Generic[IV]): pass
      class B(A[CV], Generic[CV]):pass
    |}
    [ "Invalid type variance [46]: The type variable `Variable[CV](contravariant)` is \
       incompatible with parent class type variable `Variable[IV]` because subclasses cannot use \
       more permissive type variables than their superclasses." ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      CV = TypeVar('CV', covariant=True)
      CNV = TypeVar('CNV', contravariant=True)
      class A(Generic[CV]): pass
      class B(A[CNV], Generic[CNV]):pass
    |}
    [ "Invalid type variance [46]: The type variable `Variable[CNV](contravariant)` is \
       incompatible with parent class type variable `Variable[CV](covariant)` because subclasses \
       cannot use more permissive type variables than their superclasses." ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      CV = TypeVar('CV', covariant=True)
      CNV = TypeVar('CNV', contravariant=True)
      class A(Generic[CNV]): pass
      class B(A[CV], Generic[CV]):pass
    |}
    [ "Invalid type variance [46]: The type variable `Variable[CV](covariant)` is incompatible \
       with parent class type variable `Variable[CNV](contravariant)` because subclasses cannot \
       use more permissive type variables than their superclasses." ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      IV = TypeVar('IV')
      CV = TypeVar('CV', covariant=True)
      class A(Generic[CV]): pass
      class B(A[IV], Generic[IV]):pass
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      IV = TypeVar('IV')
      CV = TypeVar('CV', contravariant=True)
      class A(Generic[CV]): pass
      class B(A[IV], Generic[IV]):pass
    |}
    [];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      T = TypeVar('T')
      U = TypeVar('U')
      V = TypeVar('V')
      class A(Generic[T]): pass
      class B(A[U, V], Generic[U, V]): pass
    |}
    ["Invalid type parameters [24]: Generic type `A` expects 1 type parameter, received 2."];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      T = TypeVar('T')
      U = TypeVar('U')
      V = TypeVar('V')
      class A(Generic[T, U]): pass
      class B(A[V], Generic[V]): pass
    |}
    ["Invalid type parameters [24]: Generic type `A` expects 2 type parameters, received 1."]


let test_check_literal_variance context =
  let assert_type_errors = assert_type_errors ~context in
  (* We special case literal lists and dicts for convenience, as they can never escape scope. *)
  assert_type_errors {|
      x: typing.List[float] = []
      x = [1]
    |} [];
  assert_type_errors {|
      x: typing.List[float] = []
      x = [y for y in [1,2,3,4]]
    |} [];

  (* Mutable default arguments may escape scope, and we shouldn't allow subtyping. *)
  assert_type_errors
    {|
      def foo(x: typing.List[float] = [1]) -> typing.List[float]:
        return x
    |}
    [ "Incompatible variable type [9]: x is declared to have type `typing.List[float]` but is "
      ^ "used as type `typing.List[int]`." ];
  assert_type_errors
    {|
      x: typing.List[float] = []
      y: typing.List[int] = [1]
      x = y
    |}
    [ "Incompatible variable type [9]: x is declared to have type `typing.List[float]` but is "
      ^ "used as type `typing.List[int]`." ];
  assert_type_errors {|
      x: typing.Dict[str, float] = {}
      x = { "s": 1 }
    |} [];
  assert_type_errors
    {|
      x: typing.Dict[str, float] = {}
      x = { "s": value for value in [1,2,3] }
    |}
    [];
  assert_type_errors
    {|
      x: typing.Dict[str, float] = {}
      x = { "s": "" }
    |}
    [ "Incompatible variable type [9]: x is declared to have type `typing.Dict[str, float]` but "
      ^ "is used as type `typing.Dict[str, str]`." ];
  assert_type_errors
    {|
      x: typing.Dict[str, float] = { "s": 1 }
      y: typing.Dict[str, int] = { "s": 1 }
      x = y
    |}
    [ "Incompatible variable type [9]: x is declared to have type `typing.Dict[str, float]` but "
      ^ "is used as type `typing.Dict[str, int]`." ];

  (* Returns. *)
  assert_type_errors {|
      def foo() -> typing.List[float]:
        return [1]
    |} [];
  assert_type_errors
    {|
      def foo() -> typing.List[float]:
        a = [1]
        return a
    |}
    ["Incompatible return type [7]: Expected `typing.List[float]` but got `typing.List[int]`."];
  assert_type_errors
    {|
      def foo() -> typing.Dict[float, float]:
        return {1: 1}
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Dict[float, float]:
        a = {1: 1}
        return a
    |}
    [ "Incompatible return type [7]: Expected `typing.Dict[float, float]` but got \
       `typing.Dict[int, int]`." ];
  assert_type_errors {|
      def foo() -> typing.Set[float]:
        return {1}
    |} [];
  assert_type_errors
    {|
      def foo() -> typing.Set[float]:
        return {x for x in [1,2,3]}
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Set[float]:
        a = {1}
        return a
    |}
    ["Incompatible return type [7]: Expected `typing.Set[float]` but got `typing.Set[int]`."];
  assert_type_errors
    {|
      def foo(a: typing.List[float]) -> float:
        return a[0]
      def bar() -> float:
        return foo([1,2,3])
    |}
    [];
  assert_type_errors
    {|
      def foo(a: typing.List[float]) -> float:
        return a[0]
      def bar() -> float:
        a = [1,2,3]
        return foo(a)
    |}
    [ "Incompatible parameter type [6]: Expected `typing.List[float]` "
      ^ "for 1st anonymous parameter to call `foo` but got `typing.List[int]`." ];
  assert_type_errors
    ~show_error_traces:true
    {|
      def foo(a: typing.List[float]) -> float:
        return a[0]
      def bar() -> float:
        a = [1,2,3]
        return foo(a)
    |}
    [ "Incompatible parameter type [6]: Expected `typing.List[float]` "
      ^ "for 1st anonymous parameter to call `foo` but got `typing.List[int]`. "
      ^ "This call might modify the type of the parameter. See https://pyre-check.org/docs/"
      ^ "error-types.html#list-and-dictionary-mismatches-with-subclassing "
      ^ "for mutable container errors." ];
  assert_type_errors
    {|
      def foo(a: typing.Dict[str, float]) -> float:
        return a["a"]
      def bar() -> float:
        return foo({ "a" : 1 })
    |}
    [];
  assert_type_errors
    {|
      def foo(a: typing.Dict[str, float]) -> float:
        return a["a"]
      def bar() -> float:
        a = { "a" : 1 }
        return foo(a)
    |}
    [ "Incompatible parameter type [6]: Expected `typing.Dict[str, float]` for "
      ^ "1st anonymous parameter to call `foo` but got `typing.Dict[str, int]`." ]


let () =
  "variance"
  >::: [ "check_variance" >:: test_check_variance;
         "check_literal_variance" >:: test_check_literal_variance ]
  |> Test.run
