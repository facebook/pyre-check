(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
      import typing
      def foo(input: str) -> typing.List[int]:
        return typing.cast(typing.List[float], input)
    |}
    ["Incompatible return type [7]: Expected `List[int]` but got `List[float]`."];
  assert_type_errors
    {|
      import typing
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[unknown], input)
    |}
    [
      "Missing parameter annotation [2]: Parameter `input` has no type specified.";
      "Incompatible return type [7]: Expected `List[int]` but got `unknown`.";
      "Unbound name [10]: Name `unknown` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Mapping[str, float]) -> float:
        return a["a"]
      def bar(x: typing.Dict[str, int]) -> float:
        return foo(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(d: typing.Dict[int, typing.Any]) -> None:
        d.update({ 1: 1 })
    |}
    [
      "Missing parameter annotation [2]: Parameter `d` must have a type "
      ^ "that does not contain `Any`.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      IV = TypeVar('IV')
      CV = TypeVar('CV', covariant=True)
      class A(Generic[IV]): pass
      class B(A[CV], Generic[CV]):pass
    |}
    [
      "Invalid type variance [46]: The type variable `Variable[CV](covariant)` is incompatible \
       with parent class type variable `Variable[IV]` because subclasses cannot use more \
       permissive type variables than their superclasses.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      IV = TypeVar('IV')
      CV = TypeVar('CV', contravariant=True)
      class A(Generic[IV]): pass
      class B(A[CV], Generic[CV]):pass
    |}
    [
      "Invalid type variance [46]: The type variable `Variable[CV](contravariant)` is incompatible \
       with parent class type variable `Variable[IV]` because subclasses cannot use more \
       permissive type variables than their superclasses.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      CV = TypeVar('CV', covariant=True)
      CNV = TypeVar('CNV', contravariant=True)
      class A(Generic[CV]): pass
      class B(A[CNV], Generic[CNV]):pass
    |}
    [
      "Invalid type variance [46]: The type variable `Variable[CNV](contravariant)` is \
       incompatible with parent class type variable `Variable[CV](covariant)` because subclasses \
       cannot use more permissive type variables than their superclasses.";
    ];
  assert_type_errors
    {|
      from typing import TypeVar, Generic
      CV = TypeVar('CV', covariant=True)
      CNV = TypeVar('CNV', contravariant=True)
      class A(Generic[CNV]): pass
      class B(A[CV], Generic[CV]):pass
    |}
    [
      "Invalid type variance [46]: The type variable `Variable[CV](covariant)` is incompatible \
       with parent class type variable `Variable[CNV](contravariant)` because subclasses cannot \
       use more permissive type variables than their superclasses.";
    ];
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
      import typing
      x: typing.List[float] = []
      x = [1]
    |} [];
  assert_type_errors
    {|
      import typing
      x: typing.List[float] = []
      x = [y for y in [1,2,3,4]]
    |}
    [];

  (* Mutable default arguments may escape scope, and we shouldn't allow subtyping. *)
  assert_type_errors
    {|
      import typing
      def foo(x: typing.List[float] = [1]) -> typing.List[float]:
        return x
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `List[float]` but is "
      ^ "used as type `List[int]`.";
    ];
  assert_type_errors
    {|
      import typing
      x: typing.List[float] = []
      y: typing.List[int] = [1]
      x = y
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `List[float]` but is "
      ^ "used as type `List[int]`.";
    ];
  assert_type_errors
    {|
      import typing
      x: typing.Dict[str, float] = {}
      x = { "s": 1 }
    |}
    [];
  assert_type_errors
    {|
      import typing
      x: typing.Dict[str, float] = {}
      x = { "s": value for value in [1,2,3] }
    |}
    [];
  assert_type_errors
    {|
      import typing
      x: typing.Dict[str, float] = {}
      x = { "s": "" }
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `Dict[str, float]` but "
      ^ "is used as type `Dict[str, str]`.";
    ];
  assert_type_errors
    {|
      import typing
      x: typing.Dict[str, float] = { "s": 1 }
      y: typing.Dict[str, int] = { "s": 1 }
      x = y
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `Dict[str, float]` but "
      ^ "is used as type `Dict[str, int]`.";
    ];

  (* Attributes. *)
  assert_type_errors
    {|
      import typing
      class Foo():
        x: typing.List[float] = []
      y: typing.List[int] = [1]
      z = Foo()
      z.x = y
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type \
       `List[float]` but is used as type `List[int]`.";
    ];

  (* Returns. *)
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[float]:
        return [1]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[float]:
        a = [1]
        return a
    |}
    ["Incompatible return type [7]: Expected `List[float]` but got `List[int]`."];

  assert_type_errors
    {|
    import typing
    def foo() -> typing.Dict[float, float]:
      return {1: 1}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Dict[float, float]:
        a = {1: 1}
        return a
    |}
    ["Incompatible return type [7]: Expected `Dict[float, float]` but got `Dict[int, int]`."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Set[float]:
        return {1}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Set[float]:
        return {x for x in [1,2,3]}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Set[float]:
        a = {1}
        return a
    |}
    ["Incompatible return type [7]: Expected `Set[float]` but got `Set[int]`."];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.List[float]) -> float:
        return a[0]
      def bar() -> float:
        return foo([1,2,3])
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.List[float]) -> float:
        return a[0]
      def bar() -> float:
        a = [1,2,3]
        return foo(a)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `List[float]` but got `List[int]`.";
    ];
  assert_type_errors
    ~show_error_traces:true
    {|
      import typing
      def foo(a: typing.List[float]) -> float:
        return a[0]
      def bar() -> float:
        a = [1,2,3]
        return foo(a)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `List[float]` but got `List[int]`. This call might modify the type of the parameter. See \
       https://pyre-check.org/docs/errors#covariance-and-contravariance for mutable container \
       errors.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, float]) -> float:
        return a["a"]
      def bar() -> float:
        return foo({ "a" : 1 })
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, float]) -> float:
        return a["a"]
      def bar() -> float:
        a = { "a" : 1 }
        return foo(a)
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Dict[str, float]` but got `Dict[str, int]`.";
    ]


let () =
  "variance"
  >::: [
         "check_variance" >:: test_check_variance;
         "check_literal_variance" >:: test_check_literal_variance;
       ]
  |> Test.run
