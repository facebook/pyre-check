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
    let define, test_source, environment =
      let find_define = function
        | { Node.value = Statement.Statement.Define define; _ }
          when String.equal (Reference.show (Node.value (Statement.Define.name define))) define_name
          ->
            Some define
        | _ -> None
      in
      let project = Test.ScratchProject.setup ~context ["test.py", source] in
      let { ScratchProject.BuiltTypeEnvironment.type_environment; sources } =
        ScratchProject.build_type_environment project
      in
      let test_source =
        List.find_map_exn
          sources
          ~f:(fun ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) ->
            Option.some_if (String.equal (Reference.show qualifier) "test") source)
      in
      ( List.find_map_exn (Ast.Source.statements test_source) ~f:find_define,
        test_source,
        Analysis.TypeEnvironment.read_only type_environment )
    in
    let overrides =
      Interprocedural.DependencyGraph.create_overrides ~environment ~source:test_source
    in
    Interprocedural.DependencyGraphSharedMemory.record_overrides overrides;
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
      (Interprocedural.CallGraph.call_graph_of_define ~environment ~define);
    Interprocedural.DependencyGraphSharedMemory.remove_overriding_types
      (Reference.Map.keys overrides)
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
      ];
  assert_call_graph_of_define
    ~source:
      {|
     def foo(c: C):
         c.m()

     class C:
       def m(self):
         pass
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "3:4-3:9",
          Interprocedural.CallGraph.RegularTargets
            {
              implicit_self = true;
              targets = [Interprocedural.Callable.create_method (Reference.create "test.C.m")];
            } );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     def foo():
       if 1 > 2:
         f = bar
       else:
         f = baz
       f()
     def baz(): ...
     def bar(): ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "7:2-7:5",
          Interprocedural.CallGraph.RegularTargets
            { implicit_self = false; targets = [`Function "test.bar"; `Function "test.baz"] } );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     def foo():
       if 1 > 2:
         f = bar
       else:
         f = None
       f()
     def bar(): ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "7:2-7:5",
          Interprocedural.CallGraph.RegularTargets
            { implicit_self = false; targets = [`Function "test.bar"] } );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     from typing import Optional

     def foo(c: Optional[C]):
       c.m()
     class C:
       def m():
         ...

      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:7",
          Interprocedural.CallGraph.RegularTargets
            {
              implicit_self = true;
              targets = [Interprocedural.Callable.create_method (Reference.create "test.C.m")];
            } );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     from typing import Optional

     def foo(c: C):
       c.m()
     class C:
       def m():
         ...
     class D(C):
       def m():
         ...
     class E(D):
       def m():
         ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:7",
          Interprocedural.CallGraph.RegularTargets
            {
              implicit_self = true;
              targets = [Interprocedural.Callable.create_override (Reference.create "test.C.m")];
            } );
      ];
  assert_call_graph_of_define
    ~source:
      {|
     from typing import Optional

     def foo(d: D):
       d.m()
     class C:
       def m():
         ...
     class D(C):
       pass
     class E(D):
       def m():
         ...
      |}
    ~define_name:"test.foo"
    ~expected:
      [
        ( "5:2-5:7",
          Interprocedural.CallGraph.RegularTargets
            {
              implicit_self = true;
              targets =
                [
                  Interprocedural.Callable.create_method (Reference.create "test.C.m");
                  Interprocedural.Callable.create_method (Reference.create "test.E.m");
                ];
            } );
      ]


let test_resolve_ignoring_optional context =
  let assert_resolved_without_optional ~source ~expression ~expected =
    let resolution =
      ScratchProject.setup ~context ["x.py", source] |> ScratchProject.build_resolution
    in
    Interprocedural.CallGraph.resolve_ignoring_optional
      ~resolution
      (Test.parse_single_expression expression)
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
  "interproceduralCallGraph"
  >::: [
         "call_graph_of_define" >:: test_call_graph_of_define;
         "resolve_ignoring_optional" >:: test_resolve_ignoring_optional;
       ]
  |> Test.run
