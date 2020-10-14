(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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


let test_resolve_target context =
  let assert_resolved ~source ~expression ~expected =
    let resolution =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution
    in
    CallResolution.resolve_target ~resolution (Test.parse_single_expression expression)
    |> assert_equal
         ~cmp:(List.equal CallResolution.equal_target)
         ~printer:(List.to_string ~f:CallResolution.show_target)
         expected
  in
  assert_resolved
    ~source:
      {|
      from functools import lru_cache
      @lru_cache()
      def f() -> int:
        return 0
    |}
    ~expression:"test.f"
    ~expected:[`Function "test.f", None];

  assert_resolved
    ~source:
      {|
      from functools import lru_cache
      class C:
        @lru_cache()
        def m(self, x: int) -> int:
          return x
      c: C = C()
    |}
    ~expression:"test.c.m"
    ~expected:
      [
        `Method { Callable.class_name = "test.C"; method_name = "m" }, Some (Type.Primitive "test.C");
      ];
  assert_resolved
    ~source:
      {|
      from functools import lru_cache
      class C:
        @lru_cache()
        def m(self, x: int) -> int:
          return x
      c: C = C()
    |}
    ~expression:"test.C.m"
    ~expected:
      [
        ( `Method { Callable.class_name = "test.C"; method_name = "m" },
          Some (Type.meta (Type.Primitive "test.C")) );
      ];
  assert_resolved
    ~source:
      {|
      from functools import lru_cache
      class C:
        @classmethod
        @lru_cache()
        def m(cls, x: int) -> int:
          return x
    |}
    ~expression:"test.C.m"
    ~expected:
      [
        ( `Method { Callable.class_name = "test.C"; method_name = "m" },
          Some (Type.meta (Type.Primitive "test.C")) );
      ];
  assert_resolved
    ~source:
      {|
        class C:
          def __call__(self, arg):
            return arg
        c: C = C()
      |}
    ~expression:"test.c"
    ~expected:
      [
        ( `Method { Callable.class_name = "test.C"; method_name = "__call__" },
          Some (Type.Primitive "test.C") );
      ]


let () =
  "callResolution"
  >::: [
         "get_property_callable" >:: test_get_property_callable;
         "resolve_target" >:: test_resolve_target;
       ]
  |> Test.run
