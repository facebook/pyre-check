(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Test

let test_default_builder context =
  let assert_registers ~source ?target ~callables ~callee expected =
    Callgraph.DefaultBuilder.initialize ();
    let global_resolution =
      ScratchProject.setup ["test.py", source] ~include_typeshed_stubs:true ~context
      |> ScratchProject.build_global_resolution
    in
    Callgraph.DefaultBuilder.add_callee
      ~global_resolution
      ~target
      ~callables
      ~arguments:[]
      ~dynamic:false
      ~qualifier:(Reference.create "test")
      ~callee_type:Type.Any
      ~callee:(parse_single_expression callee);
    let printer = List.to_string ~f:Callgraph.show_callee in
    assert_equal
      ~printer
      expected
      (Callgraph.DefaultBuilder.get_all_callees ()
      |> List.map ~f:(fun { Callgraph.callee; _ } -> callee))
  in
  let make_named name =
    let name = Reference.create name in
    Type.Callable.create ~name ~annotation:Top ()
    |> function
    | Type.Callable callable -> callable
    | _ -> failwith "impossible"
  in
  assert_registers
    ~source:""
    ~target:Type.integer
    ~callables:[make_named "callable"]
    ~callee:"expression"
    [
      Method
        {
          class_name = Type.integer;
          direct_target = Reference.create "callable";
          dispatch = Static;
          is_optional_class_attribute = false;
        };
    ];
  assert_registers
    ~source:
      {|
      from typing import Callable
      class Foo:
        method: Callable("named")[[int], str]
    |}
    ~target:(Type.optional (Primitive "test.Foo"))
    ~callables:[]
    ~callee:"foo.method"
    [
      Method
        {
          class_name = Primitive "test.Foo";
          direct_target = Reference.create "named";
          dispatch = Static;
          is_optional_class_attribute = true;
        };
    ];
  assert_registers
    ~source:
      {|
      from typing import Callable
      class Foo:
        bound_method: BoundMethod[Callable("named")[[int], str], Foo]
    |}
    ~target:(Type.optional (Primitive "test.Foo"))
    ~callables:[]
    ~callee:"foo.bound_method"
    [
      Method
        {
          class_name = Primitive "test.Foo";
          direct_target = Reference.create "named";
          dispatch = Static;
          is_optional_class_attribute = true;
        };
    ];
  assert_registers
    ~source:{|
        def a():
          pass
        def b():
          pass
      |}
    ~callables:[make_named "foo.a"; make_named "foo.b"]
    ~callee:"foo.a if 1 else foo.b"
    [Function !&"foo.a"; Function !&"foo.b"];
  assert_registers
    ~source:{|
      from typing import Callable
      class Foo:
        not_callable: int
    |}
    ~target:(Type.optional (Primitive "test.Foo"))
    ~callables:[]
    ~callee:"foo.not_callable"
    [];
  assert_registers
    ~source:{|
      from typing import Callable
      class Foo: ...
    |}
    ~target:(Type.optional (Primitive "test.Foo"))
    ~callables:[]
    ~callee:"foo.non_existent"
    [];
  ()


let () = "callgraph" >::: ["default builder" >:: test_default_builder] |> Test.run
