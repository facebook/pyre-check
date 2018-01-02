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

let () =
  "visit">:::[
    "collect">::test_collect;
    "collect_accesses_in_position">::test_collect_accesses_in_position;
  ]
  |> run_test_tt_main
