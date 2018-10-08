(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Test
open Ast
open Pyre
open OUnit2


let test_codex_format context =
  let source =
    {|
    def foo(a):
        return 1

    class Boo():
        def a():
            return foo(1)
    |}
    |> Test.trim_extra_indentation in
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  Path.create_relative ~root:local_root ~relative:"test.py"
  |> File.create ~content:"\n"
  |> File.write;
  let configuration = Configuration.Analysis.create ~local_root () in
  let parsed = parse_untrimmed ~handle:"test.py" source in
  Ast.SharedMemory.Sources.add (File.Handle.create "test.py") parsed;
  let (name, json) = Codex.source_to_json ~configuration parsed in
  let expected_json_string =
    Format.sprintf
      {|
      {
        "name": "test",
        "docstring": null,
        "rank": 0,
        "filename": "%s",
        "members": {
          "foo": {
            "docstring": null,
            "name": "foo",
            "rank": 0,
            "comments": null,
            "location": [
              2,
              0
            ],
            "type": "function",
            "source": "\n",
            "symbols": {

            },
            "decorators": [

            ],
            "arguments": {
              "args": [
                "a"
              ],
              "type": "arguments",
              "defaults": [

              ],
              "kwarg": null,
              "vararg": null
            }
          },
          "Boo": {
            "docstring": null,
            "name": "Boo",
            "rank": 0,
            "comments": null,
            "location": [
              5,
              0
            ],
            "members": {
              "a": {
                "docstring": null,
                "name": "Boo.a",
                "rank": 0,
                "comments": null,
                "location": [
                  6,
                  4
                ],
                "type": "function",
                "source": "\n",
                "symbols": {
                },
                "decorators": [
                ],
                "arguments": {
                  "args": [
                  ],
                  "type": "arguments",
                  "defaults": [
                  ],
                  "kwarg": null,
                  "vararg": null
                }
              }
            },
            "supers": [
            ],
            "mro": [
            ],
            "type": "class"
          }
        }
      }
      |}
      (Path.create_relative ~root:local_root ~relative:"test.py"
       |> Pyre.Path.absolute)
  in
  let expected_json = Yojson.Safe.from_string expected_json_string in
  assert_equal ~printer:ident name "test";
  assert_equal ~pp_diff:(diff ~print:Yojson.Safe.pretty_print) ~printer:Yojson.Safe.pretty_to_string expected_json json


let () =
  CommandTest.run_command_tests
    "codex_generator"
    [
      "codex_format", test_codex_format;
    ]
