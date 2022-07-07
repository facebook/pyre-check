(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Server

module Client = struct
  type t = {
    context: test_ctxt;
    server_properties: ServerProperties.t;
    server_state: ServerState.t;
    input_channel: Lwt_io.input_channel;
    output_channel: Lwt_io.output_channel;
  }

  let get_server_properties { server_properties; _ } = server_properties

  let current_server_state { server_state; _ } = server_state

  let send_raw_request { input_channel; output_channel; _ } raw_request =
    let open Lwt in
    Lwt_io.write_line output_channel raw_request >>= fun _ -> Lwt_io.read_line input_channel


  let send_request client request =
    Request.to_yojson request |> Yojson.Safe.to_string |> send_raw_request client


  let assert_response_equal ~expected ~actual { context; _ } =
    let expected = Response.to_yojson expected |> Yojson.Safe.to_string in
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected actual


  let assert_response ~request ~expected client =
    let open Lwt in
    send_request client request
    >>= fun actual ->
    assert_response_equal client ~expected ~actual;
    return_unit


  let subscribe ~subscription ~expected_response client =
    let open Lwt in
    send_raw_request client (Subscription.Request.to_yojson subscription |> Yojson.Safe.to_string)
    >>= fun actual_response ->
    assert_response_equal client ~expected:expected_response ~actual:actual_response;
    return_unit


  let assert_subscription_response ~expected { context; input_channel; _ } =
    let open Lwt in
    Lwt_io.read_line input_channel
    >>= fun actual ->
    let expected = Subscription.Response.to_yojson expected |> Yojson.Safe.to_string in
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected actual;
    return_unit


  let close { input_channel; output_channel; _ } =
    let open Lwt in
    Lwt_io.close input_channel >>= fun () -> Lwt_io.close output_channel
end

module ScratchProject = struct
  type t = {
    context: test_ctxt;
    configuration: Configuration.Analysis.t;
    start_options: StartOptions.t;
    watchman: Watchman.Raw.t option;
    build_system_initializer: BuildSystem.Initializer.t;
  }

  let setup
      ~context
      ?(external_sources = [])
      ?(include_typeshed_stubs = true)
      ?(include_helper_builtins = true)
      ?custom_source_root
      ?watchman
      ?build_system_initializer
      sources
    =
    (* MacOS tends to use very long directory name as the default `temp_dir`. This unfortunately
       would make the filename of temporary socket files exceed the default Unix limit. Hard-coding
       temp dir to `/tmp` to avoid the issue for now. *)
    Caml.Filename.set_temp_dir_name "/tmp";

    let add_source ~root (relative, content) =
      let content = Test.trim_extra_indentation content in
      let file = File.create ~content (PyrePath.create_relative ~root ~relative) in
      File.write file
    in
    (* We assume that there's only one checked source directory that acts as the global root as
       well. *)
    let source_root =
      match custom_source_root with
      | Some source_root -> source_root
      | None -> bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    (* We assume that there's only one external source directory. *)
    let external_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_sources =
      if include_typeshed_stubs then
        Test.typeshed_stubs ~include_helper_builtins () @ external_sources
      else
        external_sources
    in
    let log_root = bracket_tmpdir context in
    List.iter sources ~f:(add_source ~root:source_root);
    List.iter external_sources ~f:(add_source ~root:external_root);
    (* We assume that watchman root is the same as global root. *)
    let watchman_root = Option.map watchman ~f:(fun _ -> source_root) in
    let configuration =
      Configuration.Analysis.create
        ~parallel:false
        ~analyze_external_sources:false
        ~filter_directories:[source_root]
        ~ignore_all_errors:[]
        ~number_of_workers:1
        ~local_root:source_root
        ~project_root:source_root
        ~search_paths:[SearchPath.Root external_root]
        ~strict:false
        ~debug:false
        ~show_error_traces:false
        ~excludes:[]
        ~extensions:[]
        ~store_type_check_resolution:false
        ~incremental_style:Configuration.Analysis.FineGrained
        ~log_directory:log_root
        ~source_paths:[SearchPath.Root source_root]
        ()
    in
    let start_options =
      {
        StartOptions.source_paths = Configuration.SourcePaths.Simple [SearchPath.Root source_root];
        socket_path =
          PyrePath.create_relative
            ~root:(PyrePath.create_absolute (bracket_tmpdir context))
            ~relative:"pyre_server_hash.sock";
        watchman_root;
        critical_files = [];
        saved_state_action = None;
        skip_initial_type_check = false;
      }
    in
    {
      context;
      configuration;
      start_options;
      watchman;
      build_system_initializer =
        Option.value build_system_initializer ~default:BuildSystem.Initializer.null;
    }


  let configuration_of { configuration; _ } = configuration

  let start_options_of { start_options; _ } = start_options

  let test_server_with
      ?(expected_exit_status = Start.ExitStatus.Ok)
      ?on_server_socket_ready
      ~f
      { context; configuration; start_options; watchman; build_system_initializer }
    =
    let open Lwt.Infix in
    Memory.reset_shared_memory ();
    Start.start_server
      start_options
      ~configuration
      ?watchman
      ~build_system_initializer
      ?on_server_socket_ready
      ~on_exception:(function
        | OUnitTest.OUnit_failure _ as exn ->
            (* We need to re-raise OUnit test failures since OUnit relies on it for error reporting. *)
            raise exn
        | exn ->
            Log.error "Uncaught exception: %s" (Exn.to_string exn);
            Lwt.return Start.ExitStatus.Error)
      ~on_started:(fun ({ ServerProperties.socket_path; _ } as server_properties) server_state ->
        (* Open a connection to the started server and send some test messages. *)
        ExclusiveLock.Lazy.read server_state ~f:Lwt.return
        >>= fun server_state ->
        let socket_address = Lwt_unix.ADDR_UNIX (PyrePath.absolute socket_path) in
        let test_client (input_channel, output_channel) =
          f { Client.context; server_properties; server_state; input_channel; output_channel }
          >>= fun () -> Lwt.return Start.ExitStatus.Ok
        in
        Lwt_io.with_connection socket_address test_client)
    >>= fun actual_exit_status ->
    assert_equal
      ~ctxt:context
      ~printer:(fun status -> Sexp.to_string (Start.ExitStatus.sexp_of_t status))
      ~cmp:[%compare.equal: Start.ExitStatus.t]
      expected_exit_status
      actual_exit_status;
    Lwt.return_unit
end
