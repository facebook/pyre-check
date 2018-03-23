(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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
        let predicate expression =
          Some expression
      end in
      let module StatementPredicate = struct
        type t = Statement.t
        let predicate statement =
          Some statement
      end in
      let module Collector =
        Visit.Collector(ExpressionPredicate)(StatementPredicate) in
      Collector.collect (Source.create statements) in
    let equal left right =
      List.equal (fst left) (fst right) ~equal:Expression.equal &&
      List.equal (snd left) (snd right) ~equal:Statement.equal in
    let printer (expressions, statements) =
      Format.asprintf
        "%a | %a"
        Sexp.pp (sexp_of_list Expression.sexp_of_t expressions)
        Sexp.pp (sexp_of_list Statement.sexp_of_t statements)
    in
    assert_equal ~cmp:equal ~printer collect expected in

  assert_collect
    [+Expression (+Float 1.0); +Expression (+Float 2.0)]
    ([
      +Float 2.0;
      +Float 1.0;
    ],
      [
        +Expression (+Float 2.0);
        +Expression (+Float 1.0);
      ]);

  assert_collect
    [
      +If {
        If.test = +Float 2.0;
        body = [+Expression (+Float 3.0)];
        orelse = [+Expression (+Float 4.0)];
      };
    ]
    ([
      +Float 2.0;
      +Float 3.0;
      +Float 4.0;
    ],
      [
        +If {
          If.test = +Float 2.0;
          body = [+Expression (+Float 3.0)];
          orelse = [+Expression (+Float 4.0)];
        };
        +Expression (+Float 3.0);
        +Expression (+Float 4.0);
      ]);

  assert_collect
    [
      +If {
        If.test = +Float 1.0;
        body = [
          +If {
            If.test = +Float 2.0;
            body = [+Expression (+Float 3.0)];
            orelse = [+Expression (+Float 4.0)];
          };
        ];
        orelse = [+Expression (+Float 5.0)];
      };
    ]
    ([
      +Float 1.0;
      +Float 2.0;
      +Float 3.0;
      +Float 4.0;
      +Float 5.0;
    ],
      [
        +If {
          If.test = +Float 1.0;
          body = [
            +If {
              If.test = +Float 2.0;
              body = [+Expression (+Float 3.0)];
              orelse = [+Expression (+Float 4.0)];
            };
          ];
          orelse = [+Expression (+Float 5.0)];
        };
        +If {
          If.test = +Float 2.0;
          body = [+Expression (+Float 3.0)];
          orelse = [+Expression (+Float 4.0)];
        };
        +Expression (+Float 3.0);
        +Expression (+Float 4.0);
        +Expression (+Float 5.0);
      ])


let test_collect_accesses_in_position _ =
  let source =
    {|
       s = ham.egg(cheese).bake
    |}
    |> parse_single_statement
  in
  let assert_collected_accesses source line column expected_accesses =
    let position = { Location.line; column } in
    assert_equal
      ~printer:(String.concat ~sep:", ")
      (List.map ~f:Node.value (Visit.collect_accesses_in_position source position)
       |> List.map ~f:Access.show)
      expected_accesses
  in
  assert_collected_accesses source 2 0 ["s"];
  assert_collected_accesses source 2 4 ["ham.egg(...).bake"];
  assert_collected_accesses source 2 2 [];
  assert_collected_accesses source 2 8 ["ham.egg(...).bake"; "egg"]

let test_statement_visitor _ =
  let module StatementVisitor =
  struct
    type t = int String.Table.t

    let statement _ visited statement =
      let increment hash_table key =
        match Hashtbl.find hash_table key with
        | None ->
            Hashtbl.set hash_table ~key ~data:1
        | Some value ->
            Hashtbl.set hash_table ~key ~data:(value + 1)
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
      | _ ->
          visited

  end
  in
  let module Visit = Visit.MakeStatementVisitor(StatementVisitor) in
  let assert_counts source expected_counts =
    let table = Visit.visit (String.Table.create ()) source in
    List.iter
      ~f:(fun (key, expected_value) -> assert_equal (Hashtbl.find table key) (Some expected_value))
      expected_counts
  in
  let source = parse {|
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
  assert_counts source [
    "assign", 3;
    "return", 1;
    "import", 2;
  ];
  ()


let test_statement_visitor_source _ =
  let module StatementVisitor =
  struct
    type t = string (* Last source *)

    let statement { Source.path; _ } _ _ =
      path
  end
  in
  let module Visit = Visit.MakeStatementVisitor(StatementVisitor) in
  let path = Visit.visit "" (parse ~path:"test.py" "a = 1") in
  assert_equal path "test.py";

  let path = Visit.visit "" (parse ~path:"test2.py" "b = 2") in
  assert_equal path "test2.py";
  ()

let () =
  "visit">:::[
    "collect">::test_collect;
    "collect_accesses_in_position">::test_collect_accesses_in_position;
    "statement_visitor">::test_statement_visitor;
    "statement_visitor_source">::test_statement_visitor_source;
  ]
  |> run_test_tt_main
