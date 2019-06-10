(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Expression
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


(* Tests below need more granualer helper function for testing locations *)

let test_tuple_locations _ =
  let parsed_source = parse_single_statement "(1, 2) = a" in
  match parsed_source with
  | { Node.value = Statement.Assign { target = { Node.location; _ }; _ }; _ } ->
      let expected_location =
        { Ast.Location.path = Location.path location;
          start = { Ast.Location.line = 1; column = 1 };
          stop = { Ast.Location.line = 1; column = 5 }
        }
      in
      assert_equal ~cmp:Location.equal ~printer:Location.show expected_location location
  | _ -> assert_unreached ()


let test_call_arguments_locations _ =
  let source_code = "fun(1, second = 2)" in
  let statement = parse_single_statement source_code in
  let arguments =
    let print_argument { Call.Argument.name; value } =
      Format.asprintf
        "name=%s value=%a"
        ( Option.map name ~f:(fun { Node.value; location } ->
              Format.asprintf "%a/%s" String.pp value (Location.Reference.show location))
        |> Option.value ~default:"(none)" )
        Expression.pp
        value
    in
    Visit.collect_calls statement
    |> List.map ~f:Node.value
    |> List.map ~f:(fun { Call.arguments; _ } -> arguments)
    |> List.concat
    |> List.map ~f:print_argument
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    [ "name=(none) value=1";
      Format.sprintf "name=second/%d:1:7-1:13 value=2" (String.hash "test.py") ]
    arguments


let () =
  "parsing"
  >::: [ "string_locations" >:: test_string_locations;
         "multiline_strings_positions" >:: test_multiline_strings_locations;
         "define_locations" >:: test_define_locations;
         "tuple_locations" >:: test_tuple_locations;
         "call_arguments_locations" >:: test_call_arguments_locations ]
  |> Test.run
