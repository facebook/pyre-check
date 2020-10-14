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
    let expected =
      let parse_location location =
        let parse_position position =
          let line_and_column = String.split ~on:':' position in
          {
            Location.line = Int.of_string (List.nth_exn line_and_column 0);
            column = Int.of_string (List.nth_exn line_and_column 1);
          }
        in
        let positions = String.split ~on:'-' location in
        {
          Location.start = parse_position (List.nth_exn positions 0);
          stop = parse_position (List.nth_exn positions 1);
        }
      in
      List.map expected ~f:(fun (key, value) -> parse_location key, value)
      |> Location.Map.of_alist_exn
    in
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
      ~cmp:(Location.Map.equal Interprocedural.CallGraph.equal_callees)
      ~printer:(fun map ->
        map
        |> Location.Map.to_alist
        |> List.map ~f:(fun (key, value) ->
               Format.sprintf
                 "%s: %s"
                 (Location.show key)
                 (Interprocedural.CallGraph.show_callees value))
        |> String.concat ~sep:"\n")
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
      [
        ( "3:4-3:9",
          Interprocedural.CallGraph.RegularTargets
            { implicit_self = false; targets = [`Function "test.bar"] } );
      ]


let () =
  "interproceduralCallGraph" >::: ["call_graph_of_define" >:: test_call_graph_of_define] |> Test.run
