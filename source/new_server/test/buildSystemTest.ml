(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Newserver
open NewServerTest
module Path = Pyre.Path

let test_initialize context =
  let internal_state = ref "uninitiailzed" in
  let build_system =
    let initialize () =
      internal_state := "initialized";
      Lwt.return_unit
    in
    BuildSystem.create_for_testing ~initialize ()
  in
  let test_initialize _ =
    (* Verify that the build system has indeed been initiailzed. *)
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id "initialized" !internal_state;
    Lwt.return_unit
  in
  ScratchProject.setup
    ~context
    ~include_typeshed_stubs:false
    ~include_helper_builtins:false
    ~build_system
    []
  |> ScratchProject.test_server_with ~f:test_initialize


let test_cleanup context =
  let internal_state = ref "uncleaned" in
  let build_system =
    let cleanup () =
      internal_state := "cleaned";
      Lwt.return_unit
    in
    BuildSystem.create_for_testing ~cleanup ()
  in
  let open Lwt.Infix in
  ScratchProject.setup ~context ~include_typeshed_stubs:false ~include_helper_builtins:false []
  |> fun { ScratchProject.server_configuration; _ } ->
  Caml.Filename.set_temp_dir_name "/tmp";
  Start.start_server
    server_configuration
    ~build_system
    ~on_exception:(fun exn -> raise exn)
    ~on_started:(fun _ ->
      (* Shutdown the server immediately after it is started. *)
      Lwt.return Start.ExitStatus.Ok)
  >>= fun _ ->
  (* Verify that the build system has indeed been cleaned up. *)
  assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id "cleaned" !internal_state;
  Lwt.return_unit


let () =
  "build_system_test"
  >::: [
         "initialize" >:: OUnitLwt.lwt_wrapper test_initialize;
         "cleanup" >:: OUnitLwt.lwt_wrapper test_cleanup;
       ]
  |> Test.run
