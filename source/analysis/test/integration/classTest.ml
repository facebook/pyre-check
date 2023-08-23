(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_inconsistent_mro context =
  assert_type_errors
    ~context
    {|
        class A(B): pass
        class B(A): pass
    |}
    [
      "Inconsistent method resolution order [64]: Class `A` does not have a consistent method \
       resolution order";
      "Inconsistent method resolution order [64]: Class `B` does not have a consistent method \
       resolution order";
    ];
  (* Inconsistent MRO would shadow other checks like override validity *)
  assert_type_errors
    ~context
    {|
        class A(B):
          def foo(self) -> int: ...
        class B(A):
          def foo(self) -> str: ...
    |}
    [
      "Inconsistent method resolution order [64]: Class `A` does not have a consistent method \
       resolution order";
      "Inconsistent method resolution order [64]: Class `B` does not have a consistent method \
       resolution order";
    ];
  assert_type_errors
    ~context
    {|
        class A: pass
        class B(A): pass
        class C(A, B): pass
    |}
    [
      "Inconsistent method resolution order [64]: Class `C` does not have a consistent method \
       resolution order";
    ];
  (* No MRO error due to special-handling of GenericMeta.__mro_entries__ *)
  assert_type_errors
    ~context
    {|
        from typing import Generic, TypeVar
        T1 = TypeVar("T1")
        T2 = TypeVar("T2")

        class Foo1(Generic[T1]): pass
        class Foo2(Generic[T1]): pass
        class Bar1(Generic[T1, T2], Foo1[T1], Foo2[T2]): pass
        class Bar2(Generic[T1, T2], Foo1, Foo2[T2]): pass
    |}
    ["Invalid type parameters [24]: Generic type `Foo1` expects 1 type parameter."];
  (* MRO error even in the presence of GenericMeta.__mro_entries__ *)
  assert_type_errors
    ~context
    {|
        from typing import Generic, TypeVar
        T1 = TypeVar("T1")
        T2 = TypeVar("T2")

        class Foo1(Generic[T1]): pass
        class Foo2(Generic[T1]): pass
        class Bar3(Generic[T1, T2], Foo1, Foo2): pass
    |}
    [
      "Inconsistent method resolution order [64]: Class `Bar3` does not have a consistent method \
       resolution order";
      "Invalid type parameters [24]: Generic type `Foo1` expects 1 type parameter.";
      "Invalid type parameters [24]: Generic type `Foo2` expects 1 type parameter.";
    ];
  ()


let () = "class" >::: ["inconsistent_mro" >:: test_inconsistent_mro] |> Test.run
