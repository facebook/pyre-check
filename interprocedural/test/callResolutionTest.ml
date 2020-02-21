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
  let assert_callable ~setter ~source ~property:(base, attribute) ~expected =
    let resolution =
      ScratchProject.setup ~context ["x.py", source] |> ScratchProject.build_resolution
    in
    CallResolution.resolve_property_targets
      ~resolution
      ~base:(Test.parse_single_expression base)
      ~attribute
      ~setter
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
    ~setter:false
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
    ~setter:false
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
    ~setter:false
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
    ~setter:false
    ~expected:(Some "x.C::foo (method)");

  (* Don't attempt to find callables for regular functions. *)
  assert_callable
    ~source:{|
      class C:
        def foo() -> int:
          ...
    |}
    ~property:("x.C", "foo")
    ~setter:false
    ~expected:None;
  assert_callable
    ~source:{|
      class C:
        foo: int = 1
    |}
    ~property:("x.C", "foo")
    ~setter:false
    ~expected:None;

  (* Setters. *)
  assert_callable
    ~source:
      {|
      class C:
        @property
        def foo(self) -> int:
          ...
        @foo.setter
        def foo(self, value: int) -> None: ...
    |}
    ~property:("x.C", "foo")
    ~setter:true
    ~expected:(Some "x.C::foo$setter (method)");
  assert_callable
    ~source:
      {|
      class C:
        @property
        def foo(self) -> int:
          ...
        @foo.setter
        def foo(self, value: int) -> None: ...
      class D(C):
        pass
    |}
    ~property:("x.D", "foo")
    ~setter:true
    ~expected:(Some "x.C::foo$setter (method)")


let test_resolve_ignoring_optional context =
  let assert_resolved_without_optional ~source ~expression ~expected =
    let resolution =
      ScratchProject.setup ~context ["x.py", source] |> ScratchProject.build_resolution
    in
    CallResolution.resolve_ignoring_optional ~resolution (Test.parse_single_expression expression)
    |> assert_equal ~printer:Type.show expected
  in
  assert_resolved_without_optional
    ~source:{|
    class Data:
      def __init__(self, x: int) -> None: ...
  |}
    ~expression:"x.Data()"
    ~expected:(Type.Primitive "x.Data")


let () =
  "callResolution"
  >::: [
         "get_property_callable" >:: test_get_property_callable;
         "resolve_ignoring_optional" >:: test_resolve_ignoring_optional;
       ]
  |> Test.run
