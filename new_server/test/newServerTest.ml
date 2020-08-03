(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Newserver
module Path = Pyre.Path

module Client = struct
  type t = {
    context: test_ctxt;
    server_state: ServerState.t ref;
    input_channel: Lwt_io.input_channel;
    output_channel: Lwt_io.output_channel;
  }

  let current_server_state { server_state; _ } = !server_state

  let send_raw_request { input_channel; output_channel; _ } raw_request =
    let open Lwt in
    Lwt_io.write_line output_channel raw_request >>= fun _ -> Lwt_io.read_line input_channel


  let send_request client request =
    let open Lwt in
    Request.to_yojson request
    |> Yojson.Safe.to_string
    |> send_raw_request client
    >>= fun raw_response ->
    try
      match Response.of_yojson (Yojson.Safe.from_string raw_response) with
      | Ok response -> return (Result.Ok response)
      | Error _ -> return (Result.Error raw_response)
    with
    | _ -> return (Result.Error raw_response)


  let assert_response ~request ~expected ({ context; _ } as client) =
    let open Lwt in
    send_request client request
    >>= function
    | Result.Error raw_response ->
        let message =
          Format.sprintf "Cannot decode the received JSON from server: %s" raw_response
        in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: Response.t]
          ~printer:(fun response -> Format.asprintf "%a" Sexp.pp_hum (Response.sexp_of_t response))
          expected
          actual;
        return_unit
end

module ScratchProject = struct
  type t = {
    context: test_ctxt;
    server_configuration: ServerConfiguration.t;
    watchman: Watchman.Raw.t option;
  }

  let setup
      ~context
      ?(external_sources = [])
      ?(include_typeshed_stubs = true)
      ?(include_helper_builtins = true)
      ?watchman
      sources
    =
    let add_source ~root (relative, content) =
      let content = Test.trim_extra_indentation content in
      let file = File.create ~content (Path.create_relative ~root ~relative) in
      File.write file
    in
    (* We assume that there's only one checked source directory that acts as the global root as
       well. *)
    let source_root = bracket_tmpdir context |> Path.create_absolute in
    (* We assume that there's only one external source directory. *)
    let external_root = bracket_tmpdir context |> Path.create_absolute in
    let external_sources =
      if include_typeshed_stubs then
        Test.typeshed_stubs ~include_helper_builtins () @ external_sources
      else
        external_sources
    in
    let log_root = bracket_tmpdir context |> Path.create_absolute in
    List.iter sources ~f:(add_source ~root:source_root);
    List.iter external_sources ~f:(add_source ~root:external_root);
    (* We assume that watchman root is the same as global root. *)
    let watchman_root = Option.map watchman ~f:(fun _ -> source_root) in
    let server_configuration =
      {
        ServerConfiguration.source_paths = [source_root];
        search_paths = [SearchPath.Root external_root];
        excludes = [];
        checked_directory_allowlist = [source_root];
        checked_directory_blocklist = [];
        extensions = [];
        log_path = log_root;
        global_root = source_root;
        local_root = None;
        watchman_root;
        taint_model_paths = [];
        debug = false;
        strict = false;
        show_error_traces = false;
        store_type_check_resolution = false;
        critical_files = [];
        saved_state_action = None;
        parallel = false;
        number_of_workers = 1;
      }
    in
    { context; server_configuration; watchman }


  let test_server_with ~f { context; server_configuration; watchman } =
    Memory.reset_shared_memory ();
    Start.start_server
      server_configuration
      ?watchman
      ~on_exception:(function
        | OUnitTest.OUnit_failure _ as exn ->
            (* We need to re-raise OUnit test failures since OUnit relies on it for error reporting. *)
            raise exn
        | exn ->
            let message = Format.sprintf "Uncaught exception: %s" (Exn.to_string exn) in
            assert_failure message)
      ~on_started:(fun server_state ->
        (* Open a connection to the started server and send some test messages. *)
        let socket_address =
          let { ServerState.socket_path; _ } = !server_state in
          Lwt_unix.ADDR_UNIX (Pyre.Path.absolute socket_path)
        in
        let test_client (input_channel, output_channel) =
          f { Client.context; server_state; input_channel; output_channel }
        in
        Lwt_io.with_connection socket_address test_client)
end
