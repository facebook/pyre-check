(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Newserver

let test_low_level_apis _ =
  let open Lwt.Infix in
  let open Watchman.Raw in
  (* A `.watchmanconfig` file is required at watchman root directory. *)
  let test_connection connection =
    let assert_member_exists ~key json =
      match Yojson.Safe.Util.member key json with
      | `Null ->
          let message = Format.sprintf "Failed to find required JSON member: %s" key in
          assert_failure message
      | _ -> ()
    in
    (* Send a simple `watchman version` request. *)
    let version_request = `List [`String "version"] in
    Connection.send connection version_request
    >>= fun () ->
    Connection.receive connection ()
    >>= function
    | Response.EndOfStream -> assert_failure "Unexpected end-of-stream response from watchman"
    | Response.Error message ->
        let message = Format.sprintf "Unexpected failure response from watchman: %s" message in
        assert_failure message
    | Response.Ok response ->
        assert_member_exists response ~key:"version";
        Lwt.return_unit
  in
  Lwt.catch
    (fun () -> create_exn () >>= with_connection ~f:test_connection)
    (function
      | OUnitTest.OUnit_failure _ as exn ->
          (* We need to re-raise OUnit test failures since OUnit relies on it for error reporting. *)
          raise exn
      | _ as exn ->
          Format.printf
            "Skipping low-level watchman API test due to exception: %s\n"
            (Exn.to_string exn);
          Lwt.return_unit)


let () = "watchman_test" >::: ["low_level" >:: OUnitLwt.lwt_wrapper test_low_level_apis] |> Test.run
