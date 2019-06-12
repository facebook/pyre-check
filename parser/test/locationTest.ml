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
  let expected_source = Source.create ~handle:(File.Handle.create "test.py") statements in
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


let test_call_arguments_locations _ =
  assert_source_locations
    {|
      fun(1, second = 2)
    |}
    [ +Expression
         (+Call
             { callee = +Name (Name.Identifier "fun");
               arguments =
                 [ { Call.Argument.name = None; value = +Integer 1 };
                   { Call.Argument.name = Some (node ~start:(2, 7) ~stop:(2, 13) "second");
                     value = +Integer 2
                   } ]
             }) ]


let () =
  "parsing"
  >::: [ "string_locations" >:: test_string_locations;
         "multiline_strings_positions" >:: test_multiline_strings_locations;
         "define_locations" >:: test_define_locations;
         "tuple_locations" >:: test_tuple_locations;
         "call_arguments_locations" >:: test_call_arguments_locations ]
  |> Test.run
