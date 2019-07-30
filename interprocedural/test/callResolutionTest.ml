(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Core
open Interprocedural
open Test
open Pyre

let test_get_property_callable context =
  let assert_callable ~source ~property:(base, attribute) ~expected =
    let sources, _ =
      ScratchProject.setup ~context ["x.py", source] |> ScratchProject.parse_sources
    in
    let resolution = Test.resolution ~sources () in
    CallResolution.resolve_property_targets
      ~resolution
      ~base:(Test.parse_single_expression base)
      ~attribute
    >>| (function
          | [(target, _)] -> Callable.show target
          | _ -> "bad")
    |> assert_equal
         ~cmp:(Option.equal String.equal)
         ~printer:(Option.value ~default:"None")
         expected
  in
  assert_callable
    ~source:{|
      class C:
        @property
        def foo() -> int:
          ...
    |}
    ~property:("x.C", "foo")
    ~expected:(Some "x.C::foo (method)");
  assert_callable
    ~source:
      {|
      class C:
        @property
        def foo() -> int:
          ...
      c: C = C()
    |}
    ~property:("x.c", "foo")
    ~expected:(Some "x.C::foo (method)");

  (* Subclasses evaluate to the right callable. *)
  assert_callable
    ~source:
      {|
      class C:
        @property
        def foo() -> int:
          ...
      class D(C):
        pass
    |}
    ~property:("x.D", "foo")
    ~expected:(Some "x.C::foo (method)");
  assert_callable
    ~source:
      {|
      class C:
        @property
        def foo() -> int:
          ...
      class D(C):
        pass
      d: D = D()
    |}
    ~property:("x.d", "foo")
    ~expected:(Some "x.C::foo (method)");

  (* Don't attempt to find callables for regular functions. *)
  assert_callable
    ~source:{|
      class C:
        def foo() -> int:
          ...
    |}
    ~property:("x.C", "foo")
    ~expected:None;
  assert_callable
    ~source:{|
      class C:
        foo: int = 1
    |}
    ~property:("x.C", "foo")
    ~expected:None


let () = "callResolution" >::: ["get_property_callable" >:: test_get_property_callable] |> Test.run
