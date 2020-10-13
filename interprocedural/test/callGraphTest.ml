(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Test

let test_call_graph_of_define context =
  let assert_call_graph_of_define ~source ~define_name ~expected =
    let define, environment =
      let find_define = function
        | { Node.value = Statement.Statement.Define define; _ }
          when String.equal (Reference.show (Node.value (Statement.Define.name define))) define_name
          ->
            Some define
        | _ -> None
      in
      let project = Test.ScratchProject.setup ~context ["test.py", source] in
      ScratchProject.build_type_environment project
      |> fun { ScratchProject.BuiltTypeEnvironment.type_environment; sources } ->
      ( List.find_map_exn
          ~f:find_define
          (List.find_map_exn
             sources
             ~f:(fun { Source.source_path = { SourcePath.qualifier; _ }; statements; _ } ->
               Option.some_if (String.equal (Reference.show qualifier) "test") statements)),
        Analysis.TypeEnvironment.read_only type_environment )
    in
    assert_equal
      ~cmp:(Location.Map.equal (List.equal Interprocedural.Callable.equal))
      ~printer:(fun map ->
        Location.Map.sexp_of_t (List.sexp_of_t Interprocedural.Callable.sexp_of_t) map
        |> Sexp.to_string_hum)
      expected
      (Interprocedural.CallGraph.call_graph_of_define ~environment ~define)
  in
  assert_call_graph_of_define
    ~source:{|
     def foo():
         bar()

     def bar():
         pass
  |}
    ~define_name:"test.foo"
    ~expected:
      (Location.Map.of_alist_exn
         [
           ( {
               Location.start = { Location.line = 3; column = 4 };
               stop = { Location.line = 3; column = 9 };
             },
             [`Function "test.bar"] );
         ])


let () =
  "interproceduralCallGraph" >::: ["call_graph_of_define" >:: test_call_graph_of_define] |> Test.run
