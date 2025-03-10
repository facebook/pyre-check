(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Server
open ServerTest

let cannonicalize_json raw_json = Yojson.Safe.from_string raw_json |> Yojson.Safe.pretty_to_string

let test_handle_query_with_build_system context =
  let custom_source_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let module_a_original = "x: int = 42" in
  let module_a_updated = "x: str = 'x'" in
  let module_b = "from a import x; y = x" in
  let test_query client =
    let open Lwt.Infix in
    (* Check original types for a.py *)
    Client.send_request client (Request.Query "types('a.py')")
    >>= fun actual_response ->
    let expected_response =
      {|
    [
      "Query",
      {
        "response": [
          {
            "path": "a.py",
            "types": [
              {
                "location": {
                  "start": {
                    "line": 1,
                    "column": 0
                  },
                  "stop": {
                    "line": 1,
                    "column": 1
                  }
                },
                "annotation": "int"
              },
              {
                "location": {
                  "start": {
                    "line": 1,
                    "column": 3
                  },
                  "stop": {
                    "line": 1,
                    "column": 6
                  }
                },
                "annotation": "typing.Type[int]"
              },
              {
                "location": {
                  "start": {
                    "line": 1,
                    "column": 9
                  },
                  "stop": {
                    "line": 1,
                    "column": 11
                  }
                },
                "annotation": "typing_extensions.Literal[42]"
              }
            ]
          }
        ]
      }
    ]
    |}
    in
    assert_equal
      ~ctxt:context
      ~cmp:String.equal
      ~pp_diff:(Test.diff ~print:String.pp)
      ~printer:Fn.id
      (cannonicalize_json expected_response)
      (cannonicalize_json actual_response);
    (* Check original types for b.py *)
    Client.send_request client (Request.Query "types('b.py')")
    >>= fun actual_response ->
    let expected_response =
      {|
      [
        "Query",
        {
          "response": [
            {
              "path": "b.py",
              "types": [
                {
                  "location": {
                    "start": { "line": 1, "column": 14 },
                    "stop": { "line": 1, "column": 15 }
                  },
                  "annotation": "int"
                },
                {
                  "location": {
                    "start": { "line": 1, "column": 17 },
                    "stop": { "line": 1, "column": 18 }
                  },
                  "annotation": "int"
                },
                {
                  "location": {
                    "start": { "line": 1, "column": 21 },
                    "stop": { "line": 1, "column": 22 }
                  },
                  "annotation": "int"
                }
              ]
            }
          ]
        }
      ]
    |}
    in
    assert_equal
      ~ctxt:context
      ~cmp:String.equal
      ~pp_diff:(Test.diff ~print:String.pp)
      ~printer:Fn.id
      (cannonicalize_json expected_response)
      (cannonicalize_json actual_response);
    (* Update the environment *)
    let path_to_a = PyrePath.append custom_source_root ~element:"a.py" in
    File.create ~content:module_a_updated path_to_a |> File.write;
    Client.send_request client (Request.IncrementalUpdate [PyrePath.show path_to_a])
    >>= fun update_result ->
    assert_equal update_result {|["Ok"]|};
    (* Check updated types for a.py *)
    Client.send_request client (Request.Query "types('a.py')")
    >>= fun actual_response ->
    let expected_response =
      {|
    [
      "Query",
      {
        "response": [
          {
            "path": "a.py",
            "types": [
              {
                "location": {
                  "start": {
                    "line": 1,
                    "column": 0
                  },
                  "stop": {
                    "line": 1,
                    "column": 1
                  }
                },
                "annotation": "str"
              },
              {
                "location": {
                  "start": {
                    "line": 1,
                    "column": 3
                  },
                  "stop": {
                    "line": 1,
                    "column": 6
                  }
                },
                "annotation": "typing.Type[str]"
              },
              {
                "location": {
                  "start": {
                    "line": 1,
                    "column": 9
                  },
                  "stop": {
                    "line": 1,
                    "column": 12
                  }
                },
                "annotation": "typing_extensions.Literal['x']"
              }
            ]
          }
        ]
      }
    ]
    |}
    in
    assert_equal
      ~ctxt:context
      ~cmp:String.equal
      ~pp_diff:(Test.diff ~print:String.pp)
      ~printer:Fn.id
      (cannonicalize_json expected_response)
      (cannonicalize_json actual_response);
    (* Check updated types for b.py *)
    Client.send_request client (Request.Query "types('b.py')")
    >>= fun actual_response ->
    let expected_response =
      {|
      [
        "Query",
        {
          "response": [
            {
              "path": "b.py",
              "types": [
                {
                  "location": {
                    "start": { "line": 1, "column": 14 },
                    "stop": { "line": 1, "column": 15 }
                  },
                  "annotation": "str"
                },
                {
                  "location": {
                    "start": { "line": 1, "column": 17 },
                    "stop": { "line": 1, "column": 18 }
                  },
                  "annotation": "str"
                },
                {
                  "location": {
                    "start": { "line": 1, "column": 21 },
                    "stop": { "line": 1, "column": 22 }
                  },
                  "annotation": "str"
                }
              ]
            }
          ]
        }
      ]
    |}
    in
    assert_equal
      ~ctxt:context
      ~cmp:String.equal
      ~pp_diff:(Test.diff ~print:String.pp)
      ~printer:Fn.id
      (cannonicalize_json expected_response)
      (cannonicalize_json actual_response);
    Lwt.return_unit
  in
  ScratchProject.setup
    ~context
    ~include_helper_builtins:false
    ~custom_source_root
    ["a.py", module_a_original; "b.py", module_b]
  |> ScratchProject.test_server_with ~f:test_query


let () =
  "query"
  >::: [
         "handle_query_with_build_system"
         >:: OUnitLwt.lwt_wrapper test_handle_query_with_build_system;
       ]
  |> Test.run
