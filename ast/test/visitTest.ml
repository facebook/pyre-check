(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Expression
open Statement
open Test

let test_collect _ =
  let assert_collect statements expected =
    let collect =
      let module ExpressionPredicate = struct
        type t = Expression.t

        let predicate expression = Some expression
      end
      in
      let module StatementPredicate = struct
        type t = Statement.t

        let visit_children _ = true

        let predicate statement = Some statement
      end
      in
      let module NodePredicate = struct
        type t = Visit.node

        let predicate node = Some node
      end
      in
      let module Collector =
        Visit.Collector (ExpressionPredicate) (StatementPredicate) (NodePredicate)
      in
      let { Collector.expressions; statements; _ } =
        Collector.collect (Source.create statements)
      in
      expressions, statements
    in
    let equal left right =
      List.equal Expression.equal (fst left) (fst right)
      && List.equal Statement.equal (snd left) (snd right)
    in
    let printer (expressions, statements) =
      Format.asprintf
        "%a | %a"
        Sexp.pp
        [%message (expressions : Expression.t list)]
        Sexp.pp
        [%message (statements : Statement.t list)]
    in
    assert_equal ~cmp:equal ~printer expected collect
  in
  assert_collect
    [+Expression (+Float 1.0); +Expression (+Float 2.0)]
    ([+Float 2.0; +Float 1.0], [+Expression (+Float 2.0); +Expression (+Float 1.0)]);
  assert_collect
    [ +If
         {
           If.test = +Float 2.0;
           body = [+Expression (+Float 3.0)];
           orelse = [+Expression (+Float 4.0)];
         } ]
    ( [+Float 4.0; +Float 3.0; +Float 2.0],
      [ +If
           {
             If.test = +Float 2.0;
             body = [+Expression (+Float 3.0)];
             orelse = [+Expression (+Float 4.0)];
           };
        +Expression (+Float 4.0);
        +Expression (+Float 3.0) ] );
  assert_collect
    [ +If
         {
           If.test = +Float 1.0;
           body =
             [ +If
                  {
                    If.test = +Float 2.0;
                    body = [+Expression (+Float 3.0)];
                    orelse = [+Expression (+Float 4.0)];
                  } ];
           orelse = [+Expression (+Float 5.0)];
         } ]
    ( [+Float 5.0; +Float 4.0; +Float 3.0; +Float 2.0; +Float 1.0],
      [ +If
           {
             If.test = +Float 1.0;
             body =
               [ +If
                    {
                      If.test = +Float 2.0;
                      body = [+Expression (+Float 3.0)];
                      orelse = [+Expression (+Float 4.0)];
                    } ];
             orelse = [+Expression (+Float 5.0)];
           };
        +Expression (+Float 5.0);
        +If
           {
             If.test = +Float 2.0;
             body = [+Expression (+Float 3.0)];
             orelse = [+Expression (+Float 4.0)];
           };
        +Expression (+Float 4.0);
        +Expression (+Float 3.0) ] )


let test_collect_location _ =
  let assert_collect_location source expected_locations =
    let source = parse ~handle:"test.py" source in
    let actual_locations = Visit.collect_locations source in
    let expected_locations =
      let create_location (start_line, start_column, end_line, end_column) =
        {
          Location.path = !&"test";
          start = { Ast.Location.line = start_line; column = start_column };
          stop = { Ast.Location.line = end_line; column = end_column };
        }
      in
      List.map ~f:create_location expected_locations
    in
    let equal left right = List.equal Location.equal left right in
    let printer locations =
      Format.asprintf "%a" Sexp.pp [%message (locations : Location.t list)]
    in
    assert_equal ~cmp:equal ~printer expected_locations actual_locations
  in
  assert_collect_location
    {|
      if test:
        1
      else:
        2
    |}
    [ (* Entire if statement. *)
      2, 0, 5, 3;
      (* Integer 2 expression *)
      5, 2, 5, 3;
      (* orelse statement *)
      5, 2, 5, 3;
      (* Integer 1 expression *)
      3, 2, 3, 3;
      (* body statement *)
      3, 2, 3, 3;
      (* test expression *)
      2, 3, 2, 7 ]


let test_node_visitor _ =
  let module Visitor = struct
    type t = int String.Table.t

    let node state node =
      let increment hash_table key =
        match Hashtbl.find hash_table key with
        | None -> Hashtbl.set hash_table ~key ~data:1
        | Some value -> Hashtbl.set hash_table ~key ~data:(value + 1)
      in
      match node with
      | Visit.Expression _ ->
          increment state "expression";
          state
      | Visit.Statement _ ->
          increment state "statement";
          state
      | Visit.Identifier _ ->
          increment state "identifier";
          state
      | Visit.Parameter _ ->
          increment state "parameter";
          state
      | Visit.Substring _ ->
          increment state "substring";
          state
  end
  in
  let module Visit = Visit.MakeNodeVisitor (Visitor) in
  let assert_counts source expected_counts =
    let table = Visit.visit (String.Table.create ()) source in
    List.iter
      ~f:(fun (key, expected_value) ->
        assert_equal
          ~printer:(fun value -> Format.sprintf "%s -> %d" key (Option.value value ~default:0))
          (Some expected_value)
          (Hashtbl.find table key))
      expected_counts
  in
  let source =
    parse
      {|
        def foo(x: int) -> int:
          return x
        foo(x = 1)
        foo(x = 2)
      |}
  in
  assert_counts source ["expression", 9; "statement", 4; "parameter", 1; "identifier", 2];
  let source = parse {|
        f"foo"
        f'foo' 'bar'
      |} in
  assert_counts source ["expression", 2; "statement", 2; "substring", 3]


let test_statement_visitor _ =
  let module StatementVisitor = struct
    type t = int String.Table.t

    let visit_children _ = true

    let statement _ visited statement =
      let increment hash_table key =
        match Hashtbl.find hash_table key with
        | None -> Hashtbl.set hash_table ~key ~data:1
        | Some value -> Hashtbl.set hash_table ~key ~data:(value + 1)
      in
      match Node.value statement with
      | Assign _ ->
          increment visited "assign";
          visited
      | Import _ ->
          increment visited "import";
          visited
      | Return _ ->
          increment visited "return";
          visited
      | _ -> visited
  end
  in
  let module Visit = Visit.MakeStatementVisitor (StatementVisitor) in
  let assert_counts source expected_counts =
    let table = Visit.visit (String.Table.create ()) source in
    List.iter
      ~f:(fun (key, expected_value) -> assert_equal (Some expected_value) (Hashtbl.find table key))
      expected_counts
  in
  let source =
    parse
      {|
      from b import c
      def f():
        a = 1
        return
      b = 2
      if x < 3:
        import a
        c = 3
  |}
  in
  assert_counts source ["assign", 3; "return", 1; "import", 2];
  ()


let test_statement_visitor_source _ =
  let module StatementVisitor = struct
    type t = string (* Last source *)

    let visit_children _ = true

    let statement { Source.relative; _ } _ _ = relative
  end
  in
  let module Visit = Visit.MakeStatementVisitor (StatementVisitor) in
  let handle = Visit.visit "" (parse ~handle:"test.py" "a = 1") in
  assert_equal "test.py" handle;
  let handle = Visit.visit "" (parse ~handle:"test2.py" "b = 2") in
  assert_equal "test2.py" handle;
  ()


let () =
  "visit"
  >::: [ "collect" >:: test_collect;
         "collect_location" >:: test_collect_location;
         "node_visitor" >:: test_node_visitor;
         "statement_visitor" >:: test_statement_visitor;
         "statement_visitor_source" >:: test_statement_visitor_source ]
  |> Test.run
