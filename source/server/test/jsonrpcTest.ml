(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let test_format_request _ =
  let assert_formatted ~expected request =
    Yojson.Safe.from_string request
    |> Server.Jsonrpc.Request.format_request
         ~configuration:(Configuration.Analysis.create ~source_path:[] ())
    |> assert_equal
         ~cmp:Server.Protocol.Request.equal
         ~printer:Server.Protocol.Request.show
         expected
  in
  assert_formatted
    ~expected:
      (Server.Protocol.Request.UnparsableQuery
         { query = "less_or_equal(a, b, c)"; reason = "unexpected query" })
    {|{"method": "typeQuery", "params": {"query": "less_or_equal(a, b, c)"}}|}


let () = "jsonrpc" >::: ["format_request" >:: test_format_request] |> Test.run
