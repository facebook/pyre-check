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

let assert_statement_location
    ~statement
    ~start:(start_line, start_column)
    ~stop:(stop_line, stop_column)
  =
  let actual_location = statement.Node.location in
  let expected_location =
    { Location.path = String.hash "test.py";
      start = { Location.line = start_line; Location.column = start_column };
      stop = { Location.line = stop_line; Location.column = stop_column }
    }
  in
  assert_equal
    ~cmp:Location.Reference.equal
    ~printer:(fun location -> Format.asprintf "%a" Location.Reference.pp location)
    ~pp_diff:(diff ~print:Location.Reference.pp)
    expected_location
    actual_location


let test_string_locations _ =
  let test_one source_code ~start ~stop =
    let statement = parse_single_statement source_code in
    assert_statement_location ~statement ~start ~stop
  in
  test_one "'literal'" ~start:(1, 0) ~stop:(1, 9);
  test_one "\"literal\"" ~start:(1, 0) ~stop:(1, 9);
  test_one "'''multiline\nliteral'''\n" ~start:(1, 0) ~stop:(2, 10);
  test_one "\"\"\"multiline\nliteral\"\"\"\n" ~start:(1, 0) ~stop:(2, 10)


let test_multiline_strings_locations _ =
  let test_one source_code =
    let statement = parse_last_statement source_code in
    assert_statement_location ~statement ~start:(5, 0) ~stop:(5, 4)
  in
  (* variations of the multiline string: ''' AAA BBB ''' pass *)
  test_one "'''\nAAA\nBBB\n'''\npass";
  test_one "\"\"\"\nAAA\nBBB\n\"\"\"\npass";

  (* variations of the multiline string: (note the backslash in line 2) ''' AAA \ BBB ''' pass *)
  test_one "'''\nAAA \\\nBBB\n'''\npass";
  test_one "\"\"\"\nAAA \\\nBBB\n\"\"\"\npass"


let test_define_locations _ =
  let source_code = parse_single_statement "def a():\n    return None" in
  assert_statement_location ~statement:source_code ~start:(1, 0) ~stop:(2, 15)


let assert_source_locations source statements =
  let parsed_source = parse source in
  let expected_source = { parsed_source with Source.statements } in
  assert_source_equal_with_locations expected_source parsed_source


let node ~start:(start_line, start_column)
         ~stop:(stop_line, stop_column) =
  let location =
    { Location.path = String.hash "test.py";
      start = { Location.line = start_line; Location.column = start_column };
      stop = { Location.line = stop_line; Location.column = stop_column }
    }
  in
  Node.create ~location


let test_call_locations _ =
  assert_source_locations
    "a[1 < 2]"
    [ +Expression
         (+Call
             { callee =
                 node
                   ~start:(1, 0)
                   ~stop:(1, 1)
                   (Name
                      (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = true }));
               arguments =
                 [ { Call.Argument.name = None;
                     value =
                       node
                         ~start:(1, 2)
                         ~stop:(1, 3) (* TODO(T45713676): This should be (1, 7). *)
                         (ComparisonOperator
                            { ComparisonOperator.left = +Integer 1;
                              operator = ComparisonOperator.LessThan;
                              right = +Integer 2
                            })
                   } ]
             }) ];
  assert_source_locations
    "a.__getitem__(argument)"
    [ +Expression
         (+Call
             { callee =
                 node
                   ~start:(1, 0)
                   ~stop:(1, 13)
                   (Name
                      (Name.Attribute { base = !"a"; attribute = "__getitem__"; special = false }));
               arguments =
                 [ { Call.Argument.name = None;
                     value = node ~start:(1, 14) ~stop:(1, 22) (Name (Name.Identifier "argument"))
                   } ]
             }) ];
  assert_source_locations
    "a(arg1,  arg2,)"
    [ +Expression
         (+Call
             { callee = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
               arguments =
                 [ { Call.Argument.name = None;
                     value = node ~start:(1, 2) ~stop:(1, 6) (Name (Name.Identifier "arg1"))
                   };
                   { Call.Argument.name = None;
                     value = node ~start:(1, 9) ~stop:(1, 13) (Name (Name.Identifier "arg2"))
                   } ]
             }) ];
  assert_source_locations
    "a(arg1)(arg2)"
    [ +Expression
         (+Call
             { callee =
                 +Call
                    { callee = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                      arguments =
                        [ { Call.Argument.name = None;
                            value = node ~start:(1, 2) ~stop:(1, 6) (Name (Name.Identifier "arg1"))
                          } ]
                    };
               arguments =
                 [ { Call.Argument.name = None;
                     value = node ~start:(1, 8) ~stop:(1, 12) (Name (Name.Identifier "arg2"))
                   } ]
             }) ];
  assert_source_locations
    "a(  arg1)((arg2)  )"
    [ +Expression
         (+Call
             { callee =
                 +Call
                    { callee = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                      arguments =
                        [ { Call.Argument.name = None;
                            value = node ~start:(1, 4) ~stop:(1, 8) (Name (Name.Identifier "arg1"))
                          } ]
                    };
               arguments =
                 [ { Call.Argument.name = None;
                     value = node ~start:(1, 11) ~stop:(1, 15) (Name (Name.Identifier "arg2"))
                   } ]
             }) ];
  assert_source_locations
    "foo(1, a = 2, *args, **kwargs)"
    [ +Expression
         (+Call
             { callee = !"foo";
               arguments =
                 [ { Call.Argument.name = None; value = +Integer 1 };
                   { Call.Argument.name = Some ~+"a"; value = +Integer 2 };
                   { Call.Argument.name = None;
                     value = node ~start:(1, 14) ~stop:(1, 19) (Starred (Starred.Once !"args"))
                   };
                   { Call.Argument.name = None;
                     value = node ~start:(1, 21) ~stop:(1, 29) (Starred (Starred.Twice !"kwargs"))
                   } ]
             }) ];
  assert_source_locations
    "foo(1, second = 2)"
    [ +Expression
         (+Call
             { callee = +Name (Name.Identifier "foo");
               arguments =
                 [ { Call.Argument.name = None; value = +Integer 1 };
                   { Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 13) "second");
                     value = node ~start:(1, 16) ~stop:(1, 17) (Integer 2)
                   } ]
             }) ];
  assert_source_locations
    "foo(1, second = \n2)"
    [ +Expression
         (+Call
             { callee = +Name (Name.Identifier "foo");
               arguments =
                 [ { Call.Argument.name = None; value = +Integer 1 };
                   { Call.Argument.name = Some (node ~start:(1, 7) ~stop:(1, 13) "second");
                     value = node ~start:(2, 0) ~stop:(2, 1) (Integer 2)
                   } ]
             }) ]


let test_name_locations _ =
  assert_source_locations
    "a.b.c"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 5)
            (Name
               (Name.Attribute
                  { base =
                      node
                        ~start:(1, 0)
                        ~stop:(1, 3)
                        (Name
                           (Name.Attribute
                              { base = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                                attribute = "b";
                                special = false
                              }));
                    attribute = "c";
                    special = false
                  }))) ];
  assert_source_locations
    "((a)).b"
    [ +Expression
         (node
            ~start:
              (1, 2) (* TODO(T45713676): Start should take parens into account if stop does. *)
            ~stop:(1, 7)
            (Name
               (Name.Attribute
                  { base = node ~start:(1, 2) ~stop:(1, 3) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false
                  }))) ];
  assert_source_locations
    "(a  \n).b"
    [ +Expression
         (node
            ~start:(1, 1) (* TODO(T45713676): Same as above. *)
            ~stop:(2, 3)
            (Name
               (Name.Attribute
                  { base = node ~start:(1, 1) ~stop:(1, 2) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false
                  }))) ];
  assert_source_locations
    {|
      a. \
      b
    |}
    [ +Expression
         (node
            ~start:(2, 0)
            ~stop:(3, 1)
            (Name
               (Name.Attribute
                  { base = node ~start:(2, 0) ~stop:(2, 1) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false
                  }))) ];
  assert_source_locations
    "a.b;"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 3)
            (Name
               (Name.Attribute
                  { base = node ~start:(1, 0) ~stop:(1, 1) (Name (Name.Identifier "a"));
                    attribute = "b";
                    special = false
                  }))) ];
  assert_source_locations
    "a(arg).b"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 8)
            (Name
               (Name.Attribute
                  { base =
                      node
                        ~start:(1, 0)
                        ~stop:(1, 1) (* TODO(T45713676): Should be (1, 6). *)
                        (Call
                           { callee = !"a";
                             arguments = [{ Call.Argument.name = None; value = !"arg" }]
                           });
                    attribute = "b";
                    special = false
                  }))) ]


let test_tuple_locations _ =
  assert_source_locations
    {|
      (1, 2) = a
    |}
    [ +Assign
         { Assign.target = node ~start:(2, 1) ~stop:(2, 5) (Tuple [+Integer 1; +Integer 2]);
           annotation = None;
           value = !"a";
           parent = None
         } ]


let () =
  "parsed_locations"
  >::: [ "string_locations" >:: test_string_locations;
         "multiline_strings_positions" >:: test_multiline_strings_locations;
         "define_locations" >:: test_define_locations;
         "call_locations" >:: test_call_locations;
         "name_locations" >:: test_name_locations;
         "tuple_locations" >:: test_tuple_locations ]
  |> Test.run
