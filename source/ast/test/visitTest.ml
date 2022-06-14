(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

        let visit_children _ = true

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
      let { Collector.expressions; statements; _ } = Collector.collect (Source.create statements) in
      expressions, statements
    in
    let equal left right =
      List.equal [%compare.equal: Expression.t] (fst left) (fst right)
      && List.equal [%compare.equal: Statement.t] (snd left) (snd right)
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
    [
      +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
    ]
    ( [+Expression.Constant (Constant.Float 2.0); +Expression.Constant (Constant.Float 1.0)],
      [
        +Statement.Expression (+Expression.Constant (Constant.Float 2.0));
        +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
      ] );
  assert_collect
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Float 2.0);
           body = [+Statement.Expression (+Expression.Constant (Constant.Float 3.0))];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Float 4.0))];
         };
    ]
    ( [
        +Expression.Constant (Constant.Float 4.0);
        +Expression.Constant (Constant.Float 3.0);
        +Expression.Constant (Constant.Float 2.0);
      ],
      [
        +Statement.If
           {
             If.test = +Expression.Constant (Constant.Float 2.0);
             body = [+Statement.Expression (+Expression.Constant (Constant.Float 3.0))];
             orelse = [+Statement.Expression (+Expression.Constant (Constant.Float 4.0))];
           };
        +Statement.Expression (+Expression.Constant (Constant.Float 4.0));
        +Statement.Expression (+Expression.Constant (Constant.Float 3.0));
      ] );
  assert_collect
    [
      +Statement.If
         {
           If.test = +Expression.Constant (Constant.Float 1.0);
           body =
             [
               +Statement.If
                  {
                    If.test = +Expression.Constant (Constant.Float 2.0);
                    body = [+Statement.Expression (+Expression.Constant (Constant.Float 3.0))];
                    orelse = [+Statement.Expression (+Expression.Constant (Constant.Float 4.0))];
                  };
             ];
           orelse = [+Statement.Expression (+Expression.Constant (Constant.Float 5.0))];
         };
    ]
    ( [
        +Expression.Constant (Constant.Float 5.0);
        +Expression.Constant (Constant.Float 4.0);
        +Expression.Constant (Constant.Float 3.0);
        +Expression.Constant (Constant.Float 2.0);
        +Expression.Constant (Constant.Float 1.0);
      ],
      [
        +Statement.If
           {
             If.test = +Expression.Constant (Constant.Float 1.0);
             body =
               [
                 +Statement.If
                    {
                      If.test = +Expression.Constant (Constant.Float 2.0);
                      body = [+Statement.Expression (+Expression.Constant (Constant.Float 3.0))];
                      orelse = [+Statement.Expression (+Expression.Constant (Constant.Float 4.0))];
                    };
               ];
             orelse = [+Statement.Expression (+Expression.Constant (Constant.Float 5.0))];
           };
        +Statement.Expression (+Expression.Constant (Constant.Float 5.0));
        +Statement.If
           {
             If.test = +Expression.Constant (Constant.Float 2.0);
             body = [+Statement.Expression (+Expression.Constant (Constant.Float 3.0))];
             orelse = [+Statement.Expression (+Expression.Constant (Constant.Float 4.0))];
           };
        +Statement.Expression (+Expression.Constant (Constant.Float 4.0));
        +Statement.Expression (+Expression.Constant (Constant.Float 3.0));
      ] );
  assert_collect
    [
      +Statement.Match
         {
           Match.subject = +Expression.Constant (Constant.Integer 0);
           cases =
             [
               {
                 Match.Case.pattern = +Match.Pattern.MatchSingleton (Constant.Integer 2);
                 guard = Some (+Expression.Constant (Constant.Integer 4));
                 body = [];
               };
             ];
         };
    ]
    ( [
        +Expression.Constant (Constant.Integer 4);
        +Expression.Constant (Constant.Integer 2);
        +Expression.Constant (Constant.Integer 0);
      ],
      [
        +Statement.Match
           {
             Match.subject = +Expression.Constant (Constant.Integer 0);
             cases =
               [
                 {
                   Match.Case.pattern = +Match.Pattern.MatchSingleton (Constant.Integer 2);
                   guard = Some (+Expression.Constant (Constant.Integer 4));
                   body = [];
                 };
               ];
           };
      ] );

  assert_collect
    [
      +Statement.Expression
         (+Expression.WalrusOperator
             { target = !"a"; value = +Expression.Constant (Constant.Integer 1) });
    ]
    ( [
        +Expression.WalrusOperator
           { target = !"a"; value = +Expression.Constant (Constant.Integer 1) };
        +Expression.Constant (Constant.Integer 1);
        +Expression.Name (Identifier "a");
      ],
      [
        +Statement.Expression
           (+Expression.WalrusOperator
               { target = !"a"; value = +Expression.Constant (Constant.Integer 1) });
      ] )


let test_collect_location _ =
  let assert_collect_location source expected_locations =
    let source = parse ~handle:"test.py" source in
    let actual_locations = Visit.collect_locations source in
    let expected_locations =
      let create_location (start_line, start_column, end_line, end_column) =
        {
          Location.start = { Ast.Location.line = start_line; column = start_column };
          stop = { Ast.Location.line = end_line; column = end_column };
        }
      in
      List.map ~f:create_location expected_locations
    in
    let equal left right = List.equal Location.equal left right in
    let printer locations = Format.asprintf "%a" Sexp.pp [%message (locations : Location.t list)] in
    assert_equal ~cmp:equal ~printer expected_locations actual_locations
  in
  assert_collect_location
    {|
      if test:
        1
      else:
        2
    |}
    [
      (* Entire if statement. *)
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
      2, 3, 2, 7;
    ]


let test_collect_non_generic_type_names _ =
  let assert_non_generic_type_names expression expected =
    assert_equal
      ~cmp:[%equal: string list]
      ~printer:[%show: string list]
      expected
      (Visit.collect_non_generic_type_names (parse_single_expression ~preprocess:true expression))
  in
  assert_non_generic_type_names "typing.Tuple" ["typing.Tuple"];
  assert_non_generic_type_names "typing.Tuple[int]" ["int"];
  assert_non_generic_type_names "typing.Mapping[str, typing.Any]" ["str"; "typing.Any"];
  assert_non_generic_type_names "typing.Literal['typing.Any']" [];
  assert_non_generic_type_names "typing.Callable[[int], str]" ["int"; "str"];
  assert_non_generic_type_names "typing.Union[typing.Callable[[int], str]]" ["int"; "str"];
  assert_non_generic_type_names "typing.Union[typing.Tuple]" ["typing.Tuple"];
  ()


let test_collect_format_strings_with_ignores _ =
  let assert_format_strings_with_ignores source expected =
    let ({ Source.typecheck_flags = { ignore_lines; _ }; _ } as source) = parse source in
    let ignore_line_map =
      List.map ignore_lines ~f:(fun ({ Ignore.ignored_line; _ } as ignore) -> ignored_line, ignore)
      |> Int.Map.of_alist_multi
    in
    let format_strings_with_ignores =
      Visit.collect_format_strings_with_ignores ~ignore_line_map source
      |> List.map ~f:snd
      |> List.map
           ~f:
             (List.map ~f:(fun { Ignore.codes; ignored_line; kind; _ } -> ignored_line, kind, codes))
    in
    assert_equal
      ~cmp:[%compare.equal: (int * Ignore.kind * int list) list list]
      ~printer:[%show: (int * Ignore.kind * int list) list list]
      expected
      format_strings_with_ignores
  in
  let open Ignore in
  assert_format_strings_with_ignores
    {|
      def foo() -> None:
        # pyre-ignore[7]
        # pyre-fixme[58, 42]
        # Some comment.
        f"""
        foo
        bar
        {1 + "hello"}
        baz
        """
    |}
    [[6, PyreIgnore, [7]; 6, PyreFixme, [58; 42]]];
  assert_format_strings_with_ignores
    {|
      def foo() -> None:
        # pyre-fixme[58]
        # Some comment.
        f"""
        {1 + "hello2"}
        """ f"""
        {1 + "hello3"}
        """ f"""
        {1 + "hello4"}
        """
    |}
    [[5, PyreFixme, [58]]];
  assert_format_strings_with_ignores
    {|
      def foo() -> None:
        # type: ignore
        f"{1 + 'hello'}"

        # pyre-fixme[58]
        f"{1 + 'hello'}"
    |}
    [[7, PyreFixme, [58]]; [4, TypeIgnore, []]];
  assert_format_strings_with_ignores
    {|
      def foo() -> None:
        # Unignored format string
        f"""
        {1 + "world"}
        """
    |}
    [];
  ()


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
      | Visit.Argument _ ->
          increment state "identifier";
          state
      | Visit.Parameter _ ->
          increment state "parameter";
          state
      | Visit.Reference _ ->
          increment state "reference";
          state
      | Visit.Substring _ ->
          increment state "substring";
          state
      | Visit.Generator _ ->
          increment state "generator";
          state


    let visit_statement_children _ _ = true

    let visit_expression_children _ _ = true

    let visit_format_string_children _ _ = true
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
  assert_counts
    source
    ["expression", 9; "statement", 4; "parameter", 1; "identifier", 2; "reference", 1];
  let source = parse {|
        f"foo"
        f'foobar'
      |} in
  assert_counts source ["expression", 2; "statement", 2; "substring", 2];
  let source = parse {|
        f"foo {bar}"
      |} in
  assert_counts source ["expression", 2; "substring", 2];
  let source = parse {|
        class C:
          x = 1
      |} in
  assert_counts source ["expression", 2; "reference", 1];
  ()


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
      | Statement.Assign _ ->
          increment visited "assign";
          visited
      | Import _ ->
          increment visited "import";
          visited
      | Match _ ->
          increment visited "match";
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
      match x:
        case _:
          import b
  |}
  in
  assert_counts source ["assign", 3; "return", 1; "import", 3; "match", 1];
  ()


let test_statement_visitor_source _ =
  let module StatementVisitor = struct
    type t = string (* Last source *)

    let visit_children _ = true

    let statement { Source.module_path = { ModulePath.relative; _ }; _ } _ _ = relative
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
  >::: [
         "collect" >:: test_collect;
         "collect_location" >:: test_collect_location;
         "collect_non_generic_type_names" >:: test_collect_non_generic_type_names;
         "collect_format_strings_with_ignores" >:: test_collect_format_strings_with_ignores;
         "node_visitor" >:: test_node_visitor;
         "statement_visitor" >:: test_statement_visitor;
         "statement_visitor_source" >:: test_statement_visitor_source;
       ]
  |> Test.run
