(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Ast
open Test

let test_find_globals context =
  let assert_found_globals ~source ~expected =
    let project =
      ScratchProject.setup
        ~context
        ["test.py", source]
        ~include_helper_builtins:false
        ~include_typeshed_stubs:false
    in
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.build_type_environment project
    in
    let actual =
      TaintModelQuery.ModelQuery.GlobalVariableQueries.get_globals_and_annotations
        ~environment:type_environment
    in
    let expected = List.map ~f:Reference.create expected in
    assert_equal
      ~cmp:(List.equal Reference.equal)
      ~printer:(List.to_string ~f:Reference.show)
      expected
      actual
  in
  assert_found_globals ~source:{|
      foo = []
    |} ~expected:["test.foo"];
  (* Note that functions are not selected *)
  assert_found_globals ~source:{|
      def foo():
        pass
    |} ~expected:[];
  assert_found_globals
    ~source:{|
      foo = []
      bar = {}
    |}
    ~expected:["test.foo"; "test.bar"];
  (* TODO T132423781: Classes are not recognized as globals *)
  assert_found_globals
    ~source:{|
      class C:
        def f():
          pass
      C.bar = 1
    |}
    ~expected:[];
  assert_found_globals
    ~source:{|
      class C:
        def f():
          pass
      c = C()
    |}
    ~expected:["test.c"];
  assert_found_globals ~source:{|
      x, y = [], {}
    |} ~expected:["test.x"; "test.y"];
  ()


let () = "globalVarQuery" >::: ["find_globals" >:: test_find_globals] |> Test.run
