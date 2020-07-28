(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Newserver

let test_json_parsing context =
  let assert_parsed ~expected json_string =
    let json = Yojson.Safe.from_string json_string in
    match ServerConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        let actual_json = ServerConfiguration.to_yojson actual in
        List.iter expected ~f:(fun (key, value) ->
            assert_equal
              ~ctxt:context
              ~cmp:Yojson.Safe.equal
              ~printer:Yojson.Safe.pretty_to_string
              value
              (Yojson.Safe.Util.member key actual_json))
  in
  let assert_not_parsed json_string =
    let json = Yojson.Safe.from_string json_string in
    match ServerConfiguration.of_yojson json with
    | Result.Ok _ -> assert_failure "Unexpected JSON parsing success"
    | Result.Error _ -> ()
  in

  (* Empty. *)
  assert_not_parsed "[]";
  assert_not_parsed "{}";

  (* Mandatory fields must exist with the right type. *)
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "log_path": "/foo/bar" } |};
  assert_not_parsed {| { "source_path": ["/foo/bar"] } |};
  assert_not_parsed {| { "global_root": "/foo/bar" } |};
  assert_not_parsed {| { "log_path": 42, "source_path": ["/foo/bar"], "global_root": "/foo/bar" } |};
  assert_not_parsed
    {| { "log_path": "/foo/bar", "source_path": "/foo/bar", "global_root": "/foo/bar" } |};
  assert_not_parsed {| { "log_path": "/foo/bar", "source_path": ["/foo/bar"], "global_root": [] } |};

  let mandatory_fileds =
    {|
    "log_path": "/log",
    "source_paths": ["/source"],
    "global_root": "/project"
  |}
  in

  (* Default values *)
  assert_parsed
    (Format.sprintf "{%s}" mandatory_fileds)
    ~expected:
      [
        "log_path", `String "/log";
        "source_paths", `List [`String "/source"];
        "global_root", `String "/project";
        "excludes", `List [];
        "checked_directory_allowlist", `List [];
        "checked_directory_blocklist", `List [];
        "extensions", `List [];
        "debug", `Bool false;
        "strict", `Bool false;
        "show_error_traces", `Bool false;
        "store_type_check_resolution", `Bool false;
        "critical_files", `List [];
        "parallel", `Bool false;
        "number_of_workers", `Int 1;
      ];

  (* Individual values *)
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "excludes": ["/excludes"],
            "checked_directory_allowlist": ["/allows"],
            "checked_directory_blocklist": ["/blocks"],
            "extensions": [".typsy"],
            "taint_model_paths": ["/taint/model"]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        "excludes", `List [`String "/excludes"];
        "checked_directory_allowlist", `List [`String "/allows"];
        "checked_directory_blocklist", `List [`String "/blocks"];
        "extensions", `List [`String ".typsy"];
        "taint_model_paths", `List [`String "/taint/model"];
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "debug": true,
            "strict": true,
            "show_error_traces": true,
            "store_type_check_resolution": true,
            "critical_files": ["foo.py", "bar.txt"]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        "debug", `Bool true;
        "strict", `Bool true;
        "show_error_traces", `Bool true;
        "store_type_check_resolution", `Bool true;
        "critical_files", `List [`String "foo.py"; `String "bar.txt"];
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "parallel": true,
            "number_of_workers": 20
          }
       |}
       mandatory_fileds)
    ~expected:["parallel", `Bool true; "number_of_workers", `Int 20];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "local_root": "/project/local",
            "watchman_root": "/project"
          }
       |}
       mandatory_fileds)
    ~expected:["local_root", `String "/project/local"; "watchman_root", `String "/project"];
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
