(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let test_parser _ =
  let string_of_ast ast = Format.asprintf "%a" JsonParsing.JsonAst.Json.pp_internal ast in
  let json_string =
    {|
    {
      "a": [],
      "b": {
        "c": "",
        "d": 2001,
        "e": "f"
      },
      "g": [{
        "h": "i"
      }]
    }
  |}
  in
  assert_equal
    ~printer:string_of_ast
    (JsonParsing.JsonAst.Json.from_string_exn json_string)
    {
      JsonParsing.JsonAst.Node.location =
        {
          JsonParsing.JsonAst.Location.start =
            { JsonParsing.JsonAst.Location.line = 11; column = 8 };
          stop = { JsonParsing.JsonAst.Location.line = 11; column = 8 };
        };
      value =
        `Assoc
          [
            ( "a",
              {
                JsonParsing.JsonAst.Node.location =
                  {
                    JsonParsing.JsonAst.Location.start =
                      { JsonParsing.JsonAst.Location.line = 3; column = 12 };
                    stop = { JsonParsing.JsonAst.Location.line = 3; column = 12 };
                  };
                value = `List [];
              } );
            ( "b",
              {
                JsonParsing.JsonAst.Node.location =
                  {
                    JsonParsing.JsonAst.Location.start =
                      { JsonParsing.JsonAst.Location.line = 7; column = 14 };
                    stop = { JsonParsing.JsonAst.Location.line = 7; column = 16 };
                  };
                value =
                  `Assoc
                    [
                      ( "c",
                        {
                          JsonParsing.JsonAst.Node.location =
                            {
                              JsonParsing.JsonAst.Location.start =
                                { JsonParsing.JsonAst.Location.line = 5; column = 14 };
                              stop = { JsonParsing.JsonAst.Location.line = 5; column = 15 };
                            };
                          value = `String "";
                        } );
                      ( "d",
                        {
                          JsonParsing.JsonAst.Node.location =
                            {
                              JsonParsing.JsonAst.Location.start =
                                { JsonParsing.JsonAst.Location.line = 6; column = 14 };
                              stop = { JsonParsing.JsonAst.Location.line = 6; column = 17 };
                            };
                          value = `Float 2001.;
                        } );
                      ( "e",
                        {
                          JsonParsing.JsonAst.Node.location =
                            {
                              JsonParsing.JsonAst.Location.start =
                                { JsonParsing.JsonAst.Location.line = 7; column = 14 };
                              stop = { JsonParsing.JsonAst.Location.line = 7; column = 16 };
                            };
                          value = `String "f";
                        } );
                    ];
              } );
            ( "g",
              {
                JsonParsing.JsonAst.Node.location =
                  {
                    JsonParsing.JsonAst.Location.start =
                      { JsonParsing.JsonAst.Location.line = 11; column = 7 };
                    stop = { JsonParsing.JsonAst.Location.line = 11; column = 7 };
                  };
                value =
                  `List
                    [
                      {
                        JsonParsing.JsonAst.Node.location =
                          {
                            JsonParsing.JsonAst.Location.start =
                              { JsonParsing.JsonAst.Location.line = 10; column = 14 };
                            stop = { JsonParsing.JsonAst.Location.line = 10; column = 16 };
                          };
                        value =
                          `Assoc
                            [
                              ( "h",
                                {
                                  JsonParsing.JsonAst.Node.location =
                                    {
                                      JsonParsing.JsonAst.Location.start =
                                        { JsonParsing.JsonAst.Location.line = 10; column = 14 };
                                      stop = { JsonParsing.JsonAst.Location.line = 10; column = 16 };
                                    };
                                  value = `String "i";
                                } );
                            ];
                      };
                    ];
              } );
          ];
    }


let test_pretty_print _ =
  let assert_equal ~expected ~actual =
    assert_equal
      ~cmp:String.equal
      ~printer:Fn.id
      expected
      (Format.asprintf "%a" JsonParsing.JsonAst.Json.pp_internal actual)
  in
  let line_1 =
    {
      JsonParsing.JsonAst.Location.start = { JsonParsing.JsonAst.Location.line = 1; column = 12 };
      stop = { JsonParsing.JsonAst.Location.line = 1; column = 13 };
    }
  in
  let line_2 =
    {
      JsonParsing.JsonAst.Location.start = { JsonParsing.JsonAst.Location.line = 2; column = 12 };
      stop = { JsonParsing.JsonAst.Location.line = 2; column = 14 };
    }
  in
  let line_3 =
    {
      JsonParsing.JsonAst.Location.start = { JsonParsing.JsonAst.Location.line = 3; column = 12 };
      stop = { JsonParsing.JsonAst.Location.line = 3; column = 15 };
    }
  in
  assert_equal
    ~expected:"Null (loc: 1:12-1:13)"
    ~actual:{ JsonParsing.JsonAst.Node.location = line_1; value = `Null };
  assert_equal
    ~expected:"true (loc: 1:12-1:13)"
    ~actual:{ JsonParsing.JsonAst.Node.location = line_1; value = `Bool true };
  assert_equal
    ~expected:"\"test\" (loc: 1:12-1:13)"
    ~actual:{ JsonParsing.JsonAst.Node.location = line_1; value = `String "test" };
  assert_equal
    ~expected:"1.200000 (loc: 1:12-1:13)"
    ~actual:{ JsonParsing.JsonAst.Node.location = line_1; value = `Float 1.2 };
  assert_equal
    ~expected:"5 (loc: 1:12-1:13)"
    ~actual:{ JsonParsing.JsonAst.Node.location = line_1; value = `Int 5 };
  let json_list =
    [
      { JsonParsing.JsonAst.Node.location = line_1; value = `Int 1 };
      { JsonParsing.JsonAst.Node.location = line_2; value = `Int 2 };
    ]
  in
  assert_equal
    ~expected:{|[
   1 (loc: 1:12-1:13)
   2 (loc: 2:12-2:14)
] (loc: 3:12-3:15)|}
    ~actual:{ JsonParsing.JsonAst.Node.location = line_3; value = `List json_list };
  assert_equal
    ~expected:{|{
   "a" -> 1 (loc: 1:12-1:13)
   "b" -> 2 (loc: 2:12-2:14)
} (loc: 3:12-3:15)|}
    ~actual:
      {
        JsonParsing.JsonAst.Node.location = line_3;
        value =
          `Assoc
            [
              "a", { JsonParsing.JsonAst.Node.location = line_1; value = `Int 1 };
              "b", { JsonParsing.JsonAst.Node.location = line_2; value = `Int 2 };
            ];
      };
  assert_equal
    ~expected:
      {|[
   [
      1 (loc: 1:12-1:13)
      2 (loc: 2:12-2:14)
   ] (loc: 1:12-1:13)
   [
      1 (loc: 1:12-1:13)
      2 (loc: 2:12-2:14)
   ] (loc: 2:12-2:14)
] (loc: 3:12-3:15)|}
    ~actual:
      {
        JsonParsing.JsonAst.Node.location = line_3;
        value =
          `List
            [
              { JsonParsing.JsonAst.Node.location = line_1; value = `List json_list };
              { JsonParsing.JsonAst.Node.location = line_2; value = `List json_list };
            ];
      };
  assert_equal
    ~expected:
      {|[
   {"a" -> 1 (loc: 1:12-1:13)} (loc: 1:12-1:13)
   {"b" -> 2 (loc: 1:12-1:13)} (loc: 1:12-1:13)
] (loc: 3:12-3:15)|}
    ~actual:
      {
        JsonParsing.JsonAst.Node.location = line_3;
        value =
          `List
            [
              {
                JsonParsing.JsonAst.Node.location = line_1;
                value = `Assoc ["a", { JsonParsing.JsonAst.Node.location = line_1; value = `Int 1 }];
              };
              {
                JsonParsing.JsonAst.Node.location = line_1;
                value = `Assoc ["b", { JsonParsing.JsonAst.Node.location = line_1; value = `Int 2 }];
              };
            ];
      };
  assert_equal
    ~expected:
      {|{
   "a" -> [
             1 (loc: 1:12-1:13)
             2 (loc: 2:12-2:14)
   ] (loc: 1:12-1:13)
   "b" -> [
             1 (loc: 1:12-1:13)
             2 (loc: 2:12-2:14)
   ] (loc: 2:12-2:14)
} (loc: 3:12-3:15)|}
    ~actual:
      {
        JsonParsing.JsonAst.Node.location = line_3;
        value =
          `Assoc
            [
              "a", { JsonParsing.JsonAst.Node.location = line_1; value = `List json_list };
              "b", { JsonParsing.JsonAst.Node.location = line_2; value = `List json_list };
            ];
      };
  assert_equal
    ~expected:
      {|{
   "a1" -> {"a2" -> 1 (loc: 1:12-1:13)} (loc: 1:12-1:13)
   "b1" -> {"a2" -> 3 (loc: 1:12-1:13)} (loc: 1:12-1:13)
} (loc: 3:12-3:15)|}
    ~actual:
      {
        JsonParsing.JsonAst.Node.location = line_3;
        value =
          `Assoc
            [
              ( "a1",
                {
                  JsonParsing.JsonAst.Node.location = line_1;
                  value =
                    `Assoc ["a2", { JsonParsing.JsonAst.Node.location = line_1; value = `Int 1 }];
                } );
              ( "b1",
                {
                  JsonParsing.JsonAst.Node.location = line_1;
                  value =
                    `Assoc ["a2", { JsonParsing.JsonAst.Node.location = line_1; value = `Int 3 }];
                } );
            ];
      }


let () =
  "jsonParsing" >::: ["parser" >:: test_parser; "pretty_print" >:: test_pretty_print] |> Test.run
