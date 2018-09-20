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

        let visit_children _ =
          true

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
        Sexp.pp [%message (expressions: Expression.t list)]
        Sexp.pp [%message (statements: Statement.t list)]
    in
    assert_equal ~cmp:equal ~printer expected collect in

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
      +Float 4.0;
      +Float 3.0;
      +Float 2.0;
    ],
      [
        +If {
          If.test = +Float 2.0;
          body = [+Expression (+Float 3.0)];
          orelse = [+Expression (+Float 4.0)];
        };
        +Expression (+Float 4.0);
        +Expression (+Float 3.0);
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
      +Float 5.0;
      +Float 4.0;
      +Float 3.0;
      +Float 2.0;
      +Float 1.0;
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
        +Expression (+Float 5.0);
        +If {
          If.test = +Float 2.0;
          body = [+Expression (+Float 3.0)];
          orelse = [+Expression (+Float 4.0)];
        };
        +Expression (+Float 4.0);
        +Expression (+Float 3.0);
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
      expected_accesses
      (Visit.collect_accesses_in_position source position
       |> List.map ~f:Node.value
       |> List.map ~f:Access.show)
  in
  assert_collected_accesses source 2 0 ["s"];
  assert_collected_accesses source 2 4 ["ham.egg.(...).bake"];
  assert_collected_accesses source 2 2 [];
  assert_collected_accesses source 2 8 ["ham.egg.(...).bake"]


let test_collect_accesses_with_location _ =
  let source =
    {|
       s = ham.egg(cheese).bake
    |}
    |> parse_single_statement
  in
  let instantiate location =
    let lookup_table = Int.Table.of_alist_exn [String.hash "test.py", "test.py"] in
    Location.instantiate ~lookup:(Hashtbl.find lookup_table) location
  in
  let assert_collected_accesses source expected_accesses =
    assert_equal
      ~printer:(String.concat ~sep:", ")
      expected_accesses
      (List.map
         ~f:(fun node ->
             Format.sprintf "%s|%s"
               (Node.location node |> instantiate |> Location.Instantiated.show)
               (Node.value node |> Access.show))
         (Visit.collect_accesses_with_location source))
  in
  assert_collected_accesses
    source
    [
      "test.py:2:4-2:24|ham.egg.(...).bake";
      "test.py:2:12-2:18|cheese";
      "test.py:2:0-2:1|s";
    ]


let test_statement_visitor _ =
  let module StatementVisitor =
  struct
    type t = int String.Table.t

    let visit_children _ =
      true

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
      ~f:(fun (key, expected_value) -> assert_equal (Some expected_value) (Hashtbl.find table key))
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

    let visit_children _ =
      true

    let statement { Source.handle; _ } _ _ =
      File.Handle.show handle
  end
  in
  let module Visit = Visit.MakeStatementVisitor(StatementVisitor) in
  let handle = Visit.visit "" (parse ~handle:"test.py" "a = 1") in
  assert_equal "test.py" handle;

  let handle = Visit.visit "" (parse ~handle:"test2.py" "b = 2") in
  assert_equal "test2.py" handle;
  ()

let () =
  "visit">:::[
    "collect">::test_collect;
    "collect_accesses_in_position">::test_collect_accesses_in_position;
    "collect_accesses_with_location">::test_collect_accesses_with_location;
    "statement_visitor">::test_statement_visitor;
    "statement_visitor_source">::test_statement_visitor_source;
  ]
  |> Test.run
