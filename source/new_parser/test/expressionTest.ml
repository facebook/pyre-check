(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
open Test
open Ast.Expression

let expression_location_insensitive_equal left right =
  Expression.location_insensitive_compare left right |> Int.equal 0


let expression_print_to_sexp expression = Sexp.to_string_hum (Expression.sexp_of_t expression)

let assert_parsed ~context ~expected text =
  match PyreNewParser.parse_expression ~context text with
  | Result.Error { PyreNewParser.Error.message; _ } ->
      let message = Format.sprintf "Unexpected parsing failure: %s" message in
      assert_failure message
  | Result.Ok actual ->
      assert_equal
        ~cmp:expression_location_insensitive_equal
        ~printer:expression_print_to_sexp
        expected
        actual


let assert_not_parsed ~context text =
  match PyreNewParser.parse_expression ~context text with
  | Result.Ok actual ->
      let message =
        Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (Expression.sexp_of_t actual)
      in
      assert_failure message
  | Result.Error _ -> ()


let test_name _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed "x" ~expected:(+Expression.Name (Name.Identifier "x"));
    assert_parsed "x1" ~expected:(+Expression.Name (Name.Identifier "x1"));
    assert_parsed "x_y" ~expected:(+Expression.Name (Name.Identifier "x_y"));
    assert_not_parsed "$x";
    assert_not_parsed "1x";
    ()
  in
  PyreNewParser.with_context do_test


let test_attribute _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed
      "a.b"
      ~expected:
        (+Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false }));
    assert_parsed
      "a  .  b"
      ~expected:
        (+Expression.Name (Name.Attribute { base = !"a"; attribute = "b"; special = false }));
    assert_parsed
      "a.b.c"
      ~expected:
        (+Expression.Name
            (Name.Attribute
               {
                 base =
                   +Expression.Name
                      (Name.Attribute { base = !"a"; attribute = "b"; special = false });
                 attribute = "c";
                 special = false;
               }));
    assert_parsed
      "1.0.b"
      ~expected:
        (+Expression.Name
            (Name.Attribute
               {
                 base = +Expression.Constant (Constant.Float 1.0);
                 attribute = "b";
                 special = false;
               }));

    assert_not_parsed "a.async";
    assert_not_parsed "a.1";
    assert_not_parsed "1 .0 .a";
    ()
  in
  PyreNewParser.with_context do_test


let test_constant _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in

    assert_parsed "None" ~expected:(+Expression.Constant Constant.NoneLiteral);
    assert_parsed "False" ~expected:(+Expression.Constant Constant.False);
    assert_parsed "True" ~expected:(+Expression.Constant Constant.True);
    assert_parsed "..." ~expected:(+Expression.Constant Constant.Ellipsis);

    assert_parsed "1" ~expected:(+Expression.Constant (Constant.Integer 1));
    assert_parsed "0" ~expected:(+Expression.Constant (Constant.Integer 0));
    assert_parsed "00" ~expected:(+Expression.Constant (Constant.Integer 0));
    assert_parsed "00_0" ~expected:(+Expression.Constant (Constant.Integer 0));
    assert_parsed "0x1" ~expected:(+Expression.Constant (Constant.Integer 0x1));
    assert_parsed "1_01" ~expected:(+Expression.Constant (Constant.Integer 101));
    assert_parsed "(1)" ~expected:(+Expression.Constant (Constant.Integer 1));
    assert_parsed "((1))" ~expected:(+Expression.Constant (Constant.Integer 1));
    assert_parsed "0XaBc" ~expected:(+Expression.Constant (Constant.Integer 0xABC));
    assert_parsed "0o13" ~expected:(+Expression.Constant (Constant.Integer 0o13));
    assert_parsed "0b01" ~expected:(+Expression.Constant (Constant.Integer 0b01));
    assert_parsed "0b0_1" ~expected:(+Expression.Constant (Constant.Integer 0b01));
    assert_parsed "0b_0_1" ~expected:(+Expression.Constant (Constant.Integer 0b01));
    assert_not_parsed "1L";
    assert_not_parsed "01";

    assert_parsed "1.0" ~expected:(+Expression.Constant (Constant.Float 1.0));
    assert_parsed "1_0.1_01" ~expected:(+Expression.Constant (Constant.Float 10.101));
    assert_parsed ".1" ~expected:(+Expression.Constant (Constant.Float 0.1));
    assert_parsed "1." ~expected:(+Expression.Constant (Constant.Float 1.0));
    assert_parsed "1e10" ~expected:(+Expression.Constant (Constant.Float 1e10));
    assert_parsed "0.1j" ~expected:(+Expression.Constant (Constant.Complex 0.1));
    assert_parsed "1e10j" ~expected:(+Expression.Constant (Constant.Complex 1e10));
    assert_parsed "1e1_0j" ~expected:(+Expression.Constant (Constant.Complex 1e10));
    assert_parsed "2j" ~expected:(+Expression.(Constant (Constant.Complex 2.0)));
    assert_parsed "2J" ~expected:(+Expression.(Constant (Constant.Complex 2.0)));
    assert_not_parsed "0xZ";
    assert_not_parsed "0_1";
    assert_not_parsed "0o9";
    assert_not_parsed "1a3";

    assert_parsed
      "'foo'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
    assert_parsed
      "\"foo\""
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
    assert_parsed
      "'''foo'''"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
    assert_parsed
      "\"\"\"foo\"\"\""
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
    assert_parsed
      "r'foo'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
    assert_parsed
      "R'foo'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
    assert_parsed
      "b'foo'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
    assert_parsed
      "u'foo'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foo")));
    assert_parsed
      "bR'foo'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create ~bytes:true "foo")));
    assert_parsed
      "'foo' 'bar'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "foobar")));
    assert_parsed
      "\"'\""
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "'")));
    assert_parsed
      "'\"'"
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "\"")));
    assert_parsed
      "\"\\\"\""
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "\"")));
    assert_parsed
      "\"\\'\""
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "'")));
    assert_parsed
      "\"\"\"\nfoo\"\"\""
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "\nfoo")));
    assert_parsed
      "\"f.o\\no\""
      ~expected:(+Expression.Constant (Constant.String (StringLiteral.create "f.o\no")));
    assert_not_parsed "ub'foo'";
    assert_not_parsed "ur'foo'";

    ()
  in
  PyreNewParser.with_context do_test


let test_fstring _ =
  let do_test context =
    let assert_parsed ~expected text =
      assert_parsed ~context ~expected:(+Expression.FormatString expected) text
    in
    let assert_not_parsed = assert_not_parsed ~context in
    let literal value = Substring.Literal (+value) in
    let format value = Substring.Format (+value) in
    assert_parsed "f'foo'" ~expected:[literal "foo"];
    assert_parsed "F'foo'" ~expected:[literal "foo"];
    assert_parsed "f'foo' f'bar'" ~expected:[literal "foobar"];
    assert_parsed "f'foo' 'bar'" ~expected:[literal "foobar"];
    assert_parsed "f'{  3.14  }'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
    assert_parsed "f'{3.14:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
    assert_parsed "f'{3.14:.10f}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
    assert_parsed "f'{3.14!s:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
    assert_parsed "f'{3.14!r:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
    assert_parsed "f'{3.14!a:10.10}'" ~expected:[format (Expression.Constant (Constant.Float 3.14))];
    assert_parsed "f'a{{'" ~expected:[literal "a{"];
    assert_parsed "f'a{{b}}c'" ~expected:[literal "a{b}c"];
    assert_parsed
      "f'{\"{{}}\"}'"
      ~expected:[format (Expression.Constant (Constant.String (StringLiteral.create "{{}}")))];
    assert_parsed
      "f\"{'''x'''}\""
      ~expected:[format (Expression.Constant (Constant.String (StringLiteral.create "x")))];
    (* The `=` syntax was added in Python 3.8. *)
    assert_parsed "f'{x=}'" ~expected:[literal "x="; format (Expression.Name (Name.Identifier "x"))];
    assert_parsed
      "f'{x=!r:^20}'"
      ~expected:[literal "x="; format (Expression.Name (Name.Identifier "x"))];
    assert_parsed
      "f'{3.14!a:{w}.{p}}'"
      ~expected:[format (Expression.Constant (Constant.Float 3.14))];
    assert_parsed
      "f'a{b}c'"
      ~expected:[literal "a"; format (Expression.Name (Name.Identifier "b")); literal "c"];
    assert_parsed
      "f'a{\"b\"}c'"
      ~expected:
        [
          literal "a";
          format (Expression.Constant (Constant.String (StringLiteral.create "b")));
          literal "c";
        ];
    assert_parsed
      "f'{f\"a{b}c\"}'"
      ~expected:
        [
          format
            (Expression.FormatString
               [literal "a"; format (Expression.Name (Name.Identifier "b")); literal "c"]);
        ];
    assert_parsed
      "f'''\n{b}\nc'''"
      ~expected:[literal "\n"; format (Expression.Name (Name.Identifier "b")); literal "\nc"];

    assert_not_parsed "f'' b''";
    assert_not_parsed "f'{\"x'";
    assert_not_parsed "f'{\"x}'";
    assert_not_parsed "f'{(\"x'";
    assert_not_parsed "f'{(\"x)'";
    assert_not_parsed "f'{((}'";
    assert_not_parsed "f'{a[4)}'";
    assert_not_parsed "f'{a(4]}'";
    assert_not_parsed "f'{a[4}'";
    assert_not_parsed "f'{a(4}'";
    assert_not_parsed "f'{1#}'";
    assert_not_parsed "f'{#}'";
    assert_not_parsed "f'{}'";
    assert_not_parsed "f'{,}'";
    assert_not_parsed "f'{\n}'";
    assert_not_parsed "f'{\\'a\\'}'";
    ()
  in
  PyreNewParser.with_context do_test


let test_await_yield _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed "await x" ~expected:(+Expression.Await !"x");
    assert_parsed "(yield)" ~expected:(+Expression.Yield None);
    assert_parsed
      "(yield 1)"
      ~expected:(+Expression.Yield (Some (+Expression.Constant (Constant.Integer 1))));
    assert_parsed "(yield from x)" ~expected:(+Expression.YieldFrom !"x");
    assert_parsed "(yield (await x))" ~expected:(+Expression.Yield (Some (+Expression.Await !"x")));
    assert_parsed "await (yield from x)" ~expected:(+Expression.Await (+Expression.YieldFrom !"x"));

    assert_not_parsed "await";
    (* Standalone yield/yield from expressions are required to be protected with parenthesis. *)
    assert_not_parsed "yield";
    assert_not_parsed "yield from";
    ()
  in
  PyreNewParser.with_context do_test


let test_ternary_walrus _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed
      "1 if 2 else 3"
      ~expected:
        (+Expression.Ternary
            {
              Ternary.target = +Expression.Constant (Constant.Integer 1);
              test = +Expression.Constant (Constant.Integer 2);
              alternative = +Expression.Constant (Constant.Integer 3);
            });
    assert_parsed
      "1 if 2 else 3 if 4 else 5"
      ~expected:
        (+Expression.Ternary
            {
              Ternary.target = +Expression.Constant (Constant.Integer 1);
              test = +Expression.Constant (Constant.Integer 2);
              alternative =
                +Expression.Ternary
                   {
                     Ternary.target = +Expression.Constant (Constant.Integer 3);
                     test = +Expression.Constant (Constant.Integer 4);
                     alternative = +Expression.Constant (Constant.Integer 5);
                   };
            });
    assert_parsed
      "(a := 1)"
      ~expected:
        (+Expression.WalrusOperator
            { target = !"a"; value = +Expression.Constant (Constant.Integer 1) });
    assert_parsed
      "(a := (b := 1))"
      ~expected:
        (+Expression.WalrusOperator
            {
              target = !"a";
              value =
                +Expression.WalrusOperator
                   { target = !"b"; value = +Expression.Constant (Constant.Integer 1) };
            });

    (* Standalone assignment expressions are required to be protected with parenthesis. *)
    assert_not_parsed "a := 1";
    assert_not_parsed "(a := 1) := 2";
    assert_not_parsed "(a.b := 1)";
    assert_not_parsed "(a[b] := 1)";
    assert_not_parsed "((a, b) := 1)";
    assert_not_parsed "([a, b] := 1)";
    ()
  in
  PyreNewParser.with_context do_test


let test_container_literals _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed "()" ~expected:(+Expression.Tuple []);
    (* The outer parens will *NOT* be interpreted as another layer of tuple. *)
    assert_parsed "(())" ~expected:(+Expression.Tuple []);
    assert_parsed "((),)" ~expected:(+Expression.Tuple [+Expression.Tuple []]);
    assert_parsed "1," ~expected:(+Expression.Tuple [+Expression.Constant (Constant.Integer 1)]);
    assert_parsed "(1,)" ~expected:(+Expression.Tuple [+Expression.Constant (Constant.Integer 1)]);
    assert_parsed
      "1,2"
      ~expected:
        (+Expression.Tuple
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    assert_parsed
      "1,2,"
      ~expected:
        (+Expression.Tuple
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    assert_parsed
      "(1,2)"
      ~expected:
        (+Expression.Tuple
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    assert_parsed
      "(\n\t1,\n\t2\n)"
      ~expected:
        (+Expression.Tuple
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);

    assert_parsed "[]" ~expected:(+Expression.List []);
    assert_parsed "[[]]" ~expected:(+Expression.List [+Expression.List []]);
    assert_parsed "[1]" ~expected:(+Expression.List [+Expression.Constant (Constant.Integer 1)]);
    assert_parsed "[1,]" ~expected:(+Expression.List [+Expression.Constant (Constant.Integer 1)]);
    assert_parsed
      "[1,2]"
      ~expected:
        (+Expression.List
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    assert_parsed
      "[1,2,]"
      ~expected:
        (+Expression.List
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    assert_parsed
      "[\n\t1,\n\t2\n]"
      ~expected:
        (+Expression.List
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);

    assert_parsed "{1}" ~expected:(+Expression.Set [+Expression.Constant (Constant.Integer 1)]);
    assert_parsed "{1,}" ~expected:(+Expression.Set [+Expression.Constant (Constant.Integer 1)]);
    assert_parsed
      "{1,2}"
      ~expected:
        (+Expression.Set
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    assert_parsed
      "{1,2,}"
      ~expected:
        (+Expression.Set
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);
    assert_parsed
      "{\n\t1,\n\t2\n}"
      ~expected:
        (+Expression.Set
            [+Expression.Constant (Constant.Integer 1); +Expression.Constant (Constant.Integer 2)]);

    assert_parsed "{}" ~expected:(+Expression.Dictionary { Dictionary.entries = []; keywords = [] });
    assert_parsed
      "{{}}"
      ~expected:
        (+Expression.Set [+Expression.Dictionary { Dictionary.entries = []; keywords = [] }]);
    assert_parsed
      "{1:2}"
      ~expected:
        (+Expression.Dictionary
            {
              Dictionary.entries =
                [
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                    value = +Expression.Constant (Constant.Integer 2);
                  };
                ];
              keywords = [];
            });
    assert_parsed
      "{1:2,}"
      ~expected:
        (+Expression.Dictionary
            {
              Dictionary.entries =
                [
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                    value = +Expression.Constant (Constant.Integer 2);
                  };
                ];
              keywords = [];
            });
    assert_parsed
      "{1:2,3:4}"
      ~expected:
        (+Expression.Dictionary
            {
              Dictionary.entries =
                [
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                    value = +Expression.Constant (Constant.Integer 2);
                  };
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 3);
                    value = +Expression.Constant (Constant.Integer 4);
                  };
                ];
              keywords = [];
            });
    assert_parsed
      "{1:2,3:4,}"
      ~expected:
        (+Expression.Dictionary
            {
              Dictionary.entries =
                [
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                    value = +Expression.Constant (Constant.Integer 2);
                  };
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 3);
                    value = +Expression.Constant (Constant.Integer 4);
                  };
                ];
              keywords = [];
            });
    assert_parsed
      "{\n\t1:2,\n\t3:4\n}"
      ~expected:
        (+Expression.Dictionary
            {
              Dictionary.entries =
                [
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                    value = +Expression.Constant (Constant.Integer 2);
                  };
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 3);
                    value = +Expression.Constant (Constant.Integer 4);
                  };
                ];
              keywords = [];
            });
    assert_parsed
      "{**foo}"
      ~expected:(+Expression.Dictionary { Dictionary.entries = []; keywords = [!"foo"] });
    assert_parsed
      "{**foo, **bar}"
      ~expected:(+Expression.Dictionary { Dictionary.entries = []; keywords = [!"foo"; !"bar"] });
    assert_parsed
      "{**foo, 1:2}"
      ~expected:
        (+Expression.Dictionary
            {
              Dictionary.entries =
                [
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                    value = +Expression.Constant (Constant.Integer 2);
                  };
                ];
              keywords = [!"foo"];
            });
    assert_parsed
      "{1:2,**foo}"
      ~expected:
        (+Expression.Dictionary
            {
              Dictionary.entries =
                [
                  {
                    Dictionary.Entry.key = +Expression.Constant (Constant.Integer 1);
                    value = +Expression.Constant (Constant.Integer 2);
                  };
                ];
              keywords = [!"foo"];
            });

    assert_not_parsed "(1,2";
    assert_not_parsed "[1,2";
    assert_not_parsed "{1,2";
    ()
  in
  PyreNewParser.with_context do_test


let test_comprehensions _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in
    assert_parsed
      "(a for a in b)"
      ~expected:
        (+Expression.Generator
            {
              Comprehension.element = !"a";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions = [];
                    async = false;
                  };
                ];
            });
    assert_parsed
      "(a for a in (b for b in c))"
      ~expected:
        (+Expression.Generator
            {
              Comprehension.element = !"a";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator =
                      +Expression.Generator
                         {
                           Comprehension.element = !"b";
                           generators =
                             [
                               {
                                 Comprehension.Generator.target = !"b";
                                 iterator = !"c";
                                 conditions = [];
                                 async = false;
                               };
                             ];
                         };
                    conditions = [];
                    async = false;
                  };
                ];
            });
    assert_parsed
      "(c for a in b for c in a)"
      ~expected:
        (+Expression.Generator
            {
              Comprehension.element = !"c";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions = [];
                    async = false;
                  };
                  {
                    Comprehension.Generator.target = !"c";
                    iterator = !"a";
                    conditions = [];
                    async = false;
                  };
                ];
            });
    assert_parsed
      "(a for a in b if True)"
      ~expected:
        (+Expression.Generator
            {
              Comprehension.element = !"a";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions = [+Expression.Constant Constant.True];
                    async = false;
                  };
                ];
            });
    assert_parsed
      "(a for a in b if True if False)"
      ~expected:
        (+Expression.Generator
            {
              Comprehension.element = !"a";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions =
                      [+Expression.Constant Constant.True; +Expression.Constant Constant.False];
                    async = false;
                  };
                ];
            });
    assert_parsed
      "(a async for a in b)"
      ~expected:
        (+Expression.Generator
            {
              Comprehension.element = !"a";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions = [];
                    async = true;
                  };
                ];
            });
    assert_parsed
      "[a for a in b]"
      ~expected:
        (+Expression.ListComprehension
            {
              Comprehension.element = !"a";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions = [];
                    async = false;
                  };
                ];
            });
    assert_parsed
      "{a for a in b}"
      ~expected:
        (+Expression.SetComprehension
            {
              Comprehension.element = !"a";
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions = [];
                    async = false;
                  };
                ];
            });
    assert_parsed
      "{a:0 for a in b}"
      ~expected:
        (+Expression.DictionaryComprehension
            {
              Comprehension.element =
                { Dictionary.Entry.key = !"a"; value = +Expression.Constant (Constant.Integer 0) };
              generators =
                [
                  {
                    Comprehension.Generator.target = !"a";
                    iterator = !"b";
                    conditions = [];
                    async = false;
                  };
                ];
            });

    assert_not_parsed "(*a for a in b)";
    assert_not_parsed "[*a for a in b]";
    assert_not_parsed "{*a for a in b}";
    assert_not_parsed "{**a for a in b}";
    ()
  in
  PyreNewParser.with_context do_test


let test_starred _ =
  let do_test context =
    let assert_parsed = assert_parsed ~context in
    let assert_not_parsed = assert_not_parsed ~context in

    assert_parsed "( *a, )" ~expected:(+Expression.Tuple [+Expression.Starred (Starred.Once !"a")]);
    assert_parsed
      "(a, *b)"
      ~expected:(+Expression.Tuple [!"a"; +Expression.Starred (Starred.Once !"b")]);
    assert_parsed
      "( *a, b )"
      ~expected:(+Expression.Tuple [+Expression.Starred (Starred.Once !"a"); !"b"]);
    assert_parsed
      "( *a, *b )"
      ~expected:
        (+Expression.Tuple
            [+Expression.Starred (Starred.Once !"a"); +Expression.Starred (Starred.Once !"b")]);

    assert_parsed "[*a]" ~expected:(+Expression.List [+Expression.Starred (Starred.Once !"a")]);
    assert_parsed "[*a,]" ~expected:(+Expression.List [+Expression.Starred (Starred.Once !"a")]);
    assert_parsed
      "[*a, b]"
      ~expected:(+Expression.List [+Expression.Starred (Starred.Once !"a"); !"b"]);
    assert_parsed
      "[a, *b]"
      ~expected:(+Expression.List [!"a"; +Expression.Starred (Starred.Once !"b")]);
    assert_parsed
      "[*a, *b]"
      ~expected:
        (+Expression.List
            [+Expression.Starred (Starred.Once !"a"); +Expression.Starred (Starred.Once !"b")]);

    assert_parsed "{*a}" ~expected:(+Expression.Set [+Expression.Starred (Starred.Once !"a")]);
    assert_parsed "{*a,}" ~expected:(+Expression.Set [+Expression.Starred (Starred.Once !"a")]);
    assert_parsed
      "{*a, b}"
      ~expected:(+Expression.Set [+Expression.Starred (Starred.Once !"a"); !"b"]);
    assert_parsed
      "{a, *b}"
      ~expected:(+Expression.Set [!"a"; +Expression.Starred (Starred.Once !"b")]);
    assert_parsed
      "{*a, *b}"
      ~expected:
        (+Expression.Set
            [+Expression.Starred (Starred.Once !"a"); +Expression.Starred (Starred.Once !"b")]);

    (* Star expressions cannot appear out-of-context. *)
    assert_not_parsed "*x";
    assert_not_parsed "**x";
    assert_not_parsed "*(x)";
    assert_not_parsed "*[x]";
    assert_not_parsed "( *x )";
    assert_not_parsed "( **x )";
    assert_not_parsed "[**x]";
    ()
  in
  PyreNewParser.with_context do_test


let () =
  "parse_expression"
  >::: [
         "name" >:: test_name;
         "attribute" >:: test_attribute;
         "constant" >:: test_constant;
         "fstring" >:: test_fstring;
         "await_yield" >:: test_await_yield;
         "ternary_walrus" >:: test_ternary_walrus;
         "container_literals" >:: test_container_literals;
         "comprehensions" >:: test_comprehensions;
         "starred" >:: test_starred;
       ]
  |> Test.run
