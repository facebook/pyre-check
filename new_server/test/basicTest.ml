(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Newserver
open NewServerTest
module Path = Pyre.Path

let test_basic client =
  let open Lwt.Infix in
  (* Test if the `GetInfo` request works properly. *)
  let request = Request.GetInfo in
  RequestHandler.process_request ~state:(Client.current_server_state client) request
  >>= fun (_, expected) ->
  Client.assert_response client ~request ~expected
  >>= fun () ->
  (* Test if we can get the initial type errors. *)
  let error_in_test =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 3;
          "column", `Int 2;
          "stop_line", `Int 3;
          "stop_column", `Int 14;
          "path", `String "test.py";
          "code", `Int 7;
          "name", `String "Incompatible return type";
          "description", `String "Incompatible return type [7]: Expected `str` but got `int`.";
          ( "long_description",
            `String
              "Incompatible return type [7]: Expected `str` but got `int`.\n\
               Type `str` expected on line 3, specified on line 2." );
          ( "concise_description",
            `String "Incompatible return type [7]: Expected `str` but got `int`." );
          "inference", `Assoc [];
          "define", `String "test.foo";
        ])
    |> Result.ok_or_failwith
  in
  let error_in_test2 =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 2;
          "column", `Int 0;
          "stop_line", `Int 2;
          "stop_column", `Int 3;
          "path", `String "test2.py";
          "code", `Int 9;
          "name", `String "Incompatible variable type";
          ( "description",
            `String
              "Incompatible variable type [9]: bar is declared to have type `str` but is used as \
               type `int`." );
          ( "long_description",
            `String
              "Incompatible variable type [9]: bar is declared to have type `str` but is used as \
               type `int`.\n\
               Redeclare `bar` on line 2 if you wish to override the previously declared type." );
          ( "concise_description",
            `String "Incompatible variable type [9]: bar has type `str`; used as `int`." );
          "inference", `Assoc [];
          "define", `String "test2.$toplevel";
        ])
    |> Result.ok_or_failwith
  in
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [error_in_test; error_in_test2])
  >>= fun () ->
  let test_path =
    Client.current_server_state client
    |> fun { ServerState.server_configuration = { ServerConfiguration.global_root; _ }; _ } ->
    Path.create_relative ~root:global_root ~relative:"test.py"
  in
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [Path.absolute test_path])
    ~expected:(Response.TypeErrors [error_in_test])


let test_basic context =
  ScratchProject.setup
    ~context
    ~include_helper_builtins:false
    [
      "test.py", {|
          def foo(x: int) -> str:
            return x + 1
        |};
      "test2.py", {|
          bar: str = 42
        |};
    ]
  |> ScratchProject.test_server_with ~f:test_basic


let () = "basic_test" >::: ["basic" >:: OUnitLwt.lwt_wrapper test_basic] |> Test.run
