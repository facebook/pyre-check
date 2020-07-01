(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open OUnit2

let test_json_parsing context =
  let assert_parsed ~expected json_string =
    let json = Yojson.Safe.from_string json_string in
    match Newserver.ServerConfiguration.of_yojson json with
    | Result.Error _ -> assert_failure "Unexpected JSON parsing failure"
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: Newserver.ServerConfiguration.t]
          ~printer:(fun configuration ->
            Newserver.ServerConfiguration.sexp_of_t configuration |> Sexp.to_string_hum)
          expected
          actual
  in
  let assert_not_parsed json_string =
    let json = Yojson.Safe.from_string json_string in
    match Newserver.ServerConfiguration.of_yojson json with
    | Result.Ok _ -> assert_failure "Unexpected JSON parsing success"
    | Result.Error _ -> ()
  in

  assert_not_parsed "[]";
  assert_not_parsed "{}";
  assert_not_parsed {| { "foo": 42 } |};

  assert_parsed
    {| { "log_path": "/foo/bar" } |}
    ~expected:
      {
        Newserver.ServerConfiguration.log_path =
          Path.create_absolute ~follow_symbolic_links:false "/foo/bar";
      };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
