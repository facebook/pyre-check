(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Core
open Ast
open Server
open Protocol
open Pyre
open Test

let test_parse_query context =
  let { ScratchProject.configuration; _ } = ScratchProject.setup ~context [] in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let assert_parses serialized query =
    let type_query_request_equal left right =
      let expression_equal left right = Expression.location_insensitive_compare left right = 0 in
      match left, right with
      | ( TypeQuery.IsCompatibleWith (left_first, left_second),
          TypeQuery.IsCompatibleWith (right_first, right_second) )
      | Join (left_first, left_second), Join (right_first, right_second)
      | LessOrEqual (left_first, left_second), LessOrEqual (right_first, right_second)
      | Meet (left_first, left_second), Meet (right_first, right_second) ->
          expression_equal left_first right_first && expression_equal left_second right_second
      | Methods left, Methods right
      | NormalizeType left, NormalizeType right ->
          expression_equal left right
      | Superclasses left, Superclasses right ->
          List.for_all2_exn ~f:(fun left right -> expression_equal left right) left right
      | Type left, Type right -> expression_equal left right
      | _ -> TypeQuery.equal_request left right
    in

    let cmp left right =
      match left, right with
      | Request.TypeQueryRequest left, Request.TypeQueryRequest right ->
          type_query_request_equal left right
      | _ -> Request.equal left right
    in
    assert_equal
      ~cmp
      ~printer:Request.show
      (Request.TypeQueryRequest query)
      (Query.parse_query ~configuration serialized)
  in
  let assert_fails_to_parse serialized =
    try
      Query.parse_query ~configuration serialized |> ignore;
      assert_unreached ()
    with
    | Query.InvalidQuery _ -> ()
  in
  let ( ! ) name =
    let open Expression in
    Expression.Name (Name.Identifier name) |> Node.create_with_default_location
  in
  assert_parses "less_or_equal(int, bool)" (LessOrEqual (!"int", !"bool"));
  assert_parses "less_or_equal (int, bool)" (LessOrEqual (!"int", !"bool"));
  assert_parses "less_or_equal(  int, int)" (LessOrEqual (!"int", !"int"));
  assert_parses "Less_Or_Equal(  int, int)" (LessOrEqual (!"int", !"int"));
  assert_parses "is_compatible_with(int, bool)" (IsCompatibleWith (!"int", !"bool"));
  assert_parses "is_compatible_with (int, bool)" (IsCompatibleWith (!"int", !"bool"));
  assert_parses "is_compatible_with(  int, int)" (IsCompatibleWith (!"int", !"int"));
  assert_parses "Is_Compatible_With(  int, int)" (IsCompatibleWith (!"int", !"int"));
  assert_parses "meet(int, bool)" (Meet (!"int", !"bool"));
  assert_parses
    "qualified_names(path='a.py')"
    (NamesInFiles [Path.create_relative ~root:local_root ~relative:"a.py"]);
  assert_parses
    "qualified_names('a.py')"
    (NamesInFiles [Path.create_relative ~root:local_root ~relative:"a.py"]);
  assert_parses
    "qualified_names('a.py', 'b.py')"
    (NamesInFiles
       [
         Path.create_relative ~root:local_root ~relative:"a.py";
         Path.create_relative ~root:local_root ~relative:"b.py";
       ]);
  assert_fails_to_parse "qualified_names(a.py)";
  assert_parses "join(int, bool)" (Join (!"int", !"bool"));
  assert_fails_to_parse "less_or_equal()";
  assert_fails_to_parse "less_or_equal(int, int, int)";
  assert_fails_to_parse "less_or_eq(int, bool)";
  assert_fails_to_parse "is_compatible_with()";
  assert_fails_to_parse "is_compatible_with(int, int, int)";
  assert_fails_to_parse "iscompatible(int, bool)";
  assert_fails_to_parse "IsCompatibleWith(int, bool)";
  assert_fails_to_parse "meet(int, int, int)";
  assert_fails_to_parse "meet(int)";
  assert_fails_to_parse "join(int)";
  assert_parses "superclasses(int)" (Superclasses [!"int"]);
  assert_parses "superclasses(int, bool)" (Superclasses [!"int"; !"bool"]);
  assert_parses "normalize_type(int)" (NormalizeType !"int");
  assert_fails_to_parse "normalizeType(int, str)";
  assert_parses
    "type_check('derp/fiddle.py')"
    (RunCheck
       {
         check_name = "typeCheck";
         paths = [Path.create_relative ~root:local_root ~relative:"derp/fiddle.py"];
       });
  assert_parses "type(C)" (Type !"C");
  assert_parses "type((C,B))" (Type (+Expression.Expression.Tuple [!"C"; !"B"]));
  assert_fails_to_parse "type(a.b, c.d)";
  assert_fails_to_parse "typecheck(1+2)";
  assert_parses
    "type_at_position('a.py', 1, 2)"
    (TypeAtPosition
       {
         path = Path.create_relative ~root:local_root ~relative:"a.py";
         position = { Ast.Location.line = 1; column = 2 };
       });
  assert_fails_to_parse "type_at_position(a.py:1:2)";
  assert_fails_to_parse "type_at_position('a.py', 1, 2, 3)";
  assert_parses
    "types(path='a.py')"
    (TypesInFiles [Path.create_relative ~root:local_root ~relative:"a.py"]);
  assert_parses
    "types('a.py')"
    (TypesInFiles [Path.create_relative ~root:local_root ~relative:"a.py"]);
  assert_fails_to_parse "types(a.py:1:2)";
  assert_fails_to_parse "types(a.py)";
  assert_fails_to_parse "types('a.py', 1, 2)";
  assert_parses "attributes(C)" (Attributes !&"C");
  assert_fails_to_parse "attributes(C, D)";
  assert_parses "signature(a.b)" (Signature [!&"a.b"]);
  assert_parses "signature(a.b, a.c)" (Signature [!&"a.b"; !&"a.c"]);
  assert_parses
    "save_server_state('state')"
    (SaveServerState (Path.create_absolute ~follow_symbolic_links:false "state"));
  assert_fails_to_parse "save_server_state(state)";
  assert_parses
    "dump_memory_to_sqlite()"
    (DumpMemoryToSqlite (Path.create_relative ~root:local_root ~relative:".pyre/memory.sqlite"));
  let memory_file, _ = bracket_tmpfile context in
  assert_parses
    (Format.sprintf "dump_memory_to_sqlite('%s')" memory_file)
    (DumpMemoryToSqlite (Path.create_absolute memory_file));
  assert_parses
    (Format.sprintf "dump_memory_to_sqlite('a.sqlite')")
    (DumpMemoryToSqlite
       (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.sqlite"));
  assert_parses
    (Format.sprintf
       "dump_memory_to_sqlite('%s/%s')"
       (Path.absolute (Path.current_working_directory ()))
       "absolute.sqlite")
    (DumpMemoryToSqlite
       (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"absolute.sqlite"));
  assert_parses "path_of_module(a.b.c)" (PathOfModule !&"a.b.c");
  assert_fails_to_parse "path_of_module('a.b.c')";
  assert_fails_to_parse "path_of_module(a.b, b.c)";
  assert_parses "compute_hashes_to_keys()" ComputeHashesToKeys;
  assert_fails_to_parse "compute_hashes_to_keys(foo)";
  assert_parses "decode_ocaml_values()" (DecodeOcamlValues []);
  assert_parses
    "decode_ocaml_values(('first_key', 'first_value'))"
    (DecodeOcamlValues
       [
         TypeQuery.SerializedValue { serialized_key = "first_key"; serialized_value = "first_value" };
       ]);
  assert_parses
    "decode_ocaml_values(('first_key', 'first_value'), ('second_key', 'second_value', \
     'third_value'))"
    (DecodeOcamlValues
       [
         TypeQuery.SerializedValue { serialized_key = "first_key"; serialized_value = "first_value" };
         TypeQuery.SerializedPair
           {
             serialized_key = "second_key";
             first_serialized_value = "second_value";
             second_serialized_value = "third_value";
           };
       ]);
  assert_fails_to_parse "decode_ocaml_values('a', 'b')";
  let path =
    let path = Path.create_relative ~root:local_root ~relative:"decode.me" in
    File.write (File.create path ~content:"key,value\nsecond_key,second_value,third_value");
    path
  in
  assert_parses
    (Format.sprintf "decode_ocaml_values_from_file('%s')" (Path.absolute path))
    (DecodeOcamlValues
       [
         TypeQuery.SerializedValue { serialized_key = "key"; serialized_value = "value" };
         TypeQuery.SerializedPair
           {
             serialized_key = "second_key";
             first_serialized_value = "second_value";
             second_serialized_value = "third_value";
           };
       ]);
  assert_parses "validate_taint_models()" (ValidateTaintModels None);
  assert_parses
    (Format.sprintf "validate_taint_models('%s')" (Path.absolute path))
    (ValidateTaintModels (Some path));
  assert_parses
    (Format.sprintf "run_check('cool_static_analysis', '%s')" (Path.absolute path))
    (RunCheck { check_name = "cool_static_analysis"; paths = [path] });
  assert_parses "defines(a.b)" (Defines [Reference.create "a.b"]);
  assert_parses "batch()" (Batch []);
  assert_fails_to_parse "batch(batch())";
  assert_fails_to_parse "batch(defines(a.b), invalid(a))";
  assert_parses "batch(defines(a.b))" (Batch [Defines [Reference.create "a.b"]]);
  assert_parses
    "batch(defines(a.b), types(path='a.py'))"
    (Batch
       [
         Defines [Reference.create "a.b"];
         TypesInFiles [Path.create_relative ~root:local_root ~relative:"a.py"];
       ])


let test_to_yojson _ =
  let open Server.Protocol in
  let assert_yojson response json =
    assert_equal
      ~printer:Yojson.Safe.pretty_to_string
      (Yojson.Safe.from_string json)
      (TypeQuery.response_to_yojson response)
  in
  assert_yojson
    (TypeQuery.Response
       (TypeQuery.Decoded
          {
            decoded =
              [
                TypeQuery.DecodedValue
                  {
                    serialized_key = "first_encoded";
                    kind = "Type";
                    actual_key = "first";
                    actual_value = Some "int";
                  };
                TypeQuery.DecodedValue
                  {
                    serialized_key = "first_encoded";
                    kind = "Type";
                    actual_key = "first";
                    actual_value = Some "str";
                  };
                TypeQuery.DecodedPair
                  {
                    serialized_key = "first_encoded";
                    kind = "Type";
                    actual_key = "first";
                    first_value = Some "str";
                    second_value = Some "int";
                    equal = false;
                  };
                TypeQuery.DecodedPair
                  {
                    serialized_key = "first_encoded";
                    kind = "Type";
                    actual_key = "first";
                    first_value = None;
                    second_value = Some "int";
                    equal = false;
                  };
                TypeQuery.DecodedPair
                  {
                    serialized_key = "first_encoded";
                    kind = "Type";
                    actual_key = "first";
                    first_value = Some "str";
                    second_value = None;
                    equal = false;
                  };
              ];
            undecodable_keys = ["no"];
          }))
    {|
      {
       "response": {
         "decoded": [
           {
             "serialized_key": "first_encoded",
             "kind": "Type",
             "key": "first",
             "value": "int"
           },
           {
             "serialized_key": "first_encoded",
             "kind": "Type",
             "key": "first",
             "value": "str"
           },
           {
             "serialized_key": "first_encoded",
             "kind": "Type",
             "key": "first",
             "equal": false,
             "first_value": "str",
             "second_value": "int"
           },
           {
             "serialized_key": "first_encoded",
             "kind": "Type",
             "key": "first",
             "equal": false,
             "second_value": "int"
           },
           {
             "serialized_key": "first_encoded",
             "kind": "Type",
             "key": "first",
             "equal": false,
             "first_value": "str"
           }
         ],
         "undecodable_keys": [ "no" ]
       }
     }
   |}


let () =
  "query" >::: ["parse_query" >:: test_parse_query; "to_yojson" >:: test_to_yojson] |> Test.run
