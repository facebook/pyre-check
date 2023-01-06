(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Base
open OUnit2
open CodeNavigationServer
module Request = CodeNavigationServer.Testing.Request
module Response = CodeNavigationServer.Testing.Response

module ClientConnection = struct
  module Style = struct
    type t =
      | Sequential
      | Concurrent
  end

  type t = {
    context: test_ctxt;
    configuration: Configuration.Analysis.t;
    server_state: State.t Server.ExclusiveLock.t;
    input_channel: Lwt_io.input_channel;
    output_channel: Lwt_io.output_channel;
  }

  let get_context { context; _ } = context

  let send_raw_request { input_channel; output_channel; _ } raw_request =
    let%lwt () = Lwt_io.write_line output_channel raw_request in
    Lwt_io.read_line input_channel


  let send_request client request =
    Request.to_yojson request |> Yojson.Safe.to_string |> send_raw_request client


  let assert_response_equal ~expected ~actual { context; _ } =
    let expected = Response.to_yojson expected |> Yojson.Safe.to_string in
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected actual


  let assert_subscription_response ~expected ({ input_channel; _ } as client) =
    let%lwt actual = Lwt_io.read_line input_channel in
    assert_response_equal client ~expected ~actual;
    Lwt.return_unit


  let assert_response ~request ~expected client =
    let%lwt actual = send_request client request in
    assert_response_equal client ~expected ~actual;
    Lwt.return_unit


  let assert_error_response ~request ~kind client =
    let%lwt actual = send_request client request in
    match Yojson.Safe.from_string actual with
    | `List [`String "Error"; `List (`String actual_kind :: _)] ->
        if String.equal kind actual_kind then
          Lwt.return_unit
        else
          let message = Format.sprintf "Expected error kind `%s` but got: `%s`" kind actual_kind in
          assert_failure message
    | json ->
        let message =
          Format.sprintf "Expected error response but got: `%s`" (Yojson.Safe.to_string json)
        in
        assert_failure message
end

type t = {
  context: test_ctxt;
  start_options: StartOptions.t;
}

let setup ~context ?(include_typeshed_stubs = true) ?(critical_files = []) ?watchman sources =
  (* MacOS tends to use very long directory name as the default `temp_dir`. This unfortunately would
     make the filename of temporary socket files exceed the default Unix limit. Hard-coding temp dir
     to `/tmp` to avoid the issue for now. *)
  Caml.Filename.set_temp_dir_name "/tmp";

  (* We assume that there's only one checked source directory that acts as the global root as
     well. *)
  let source_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  (* We assume that there's only one external source directory. *)
  let external_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let external_sources =
    if include_typeshed_stubs then
      Test.typeshed_stubs ~include_helper_builtins:false ()
    else
      []
  in
  let log_root = bracket_tmpdir context in
  let add_source ~root (relative, content) =
    let content = Test.trim_extra_indentation content in
    let file = File.create ~content (PyrePath.create_relative ~root ~relative) in
    File.write file
  in
  List.iter sources ~f:(add_source ~root:source_root);
  List.iter external_sources ~f:(add_source ~root:external_root);
  let environment_controls =
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
      ~store_type_check_resolution:true
      ~track_dependencies:true
      ~log_directory:log_root
      ~source_paths:[SearchPath.Root source_root]
      ()
    |> Analysis.EnvironmentControls.create ~populate_call_graph:false ~use_lazy_module_tracking:true
  in
  let start_options =
    let watchman =
      Option.map watchman ~f:(fun raw ->
          (* We assume that watchman root is the same as global root. *)
          { Server.StartOptions.Watchman.root = source_root; raw })
    in
    {
      StartOptions.environment_controls;
      source_paths = Configuration.SourcePaths.Simple [SearchPath.Root source_root];
      socket_path =
        PyrePath.create_relative
          ~root:(PyrePath.create_absolute (bracket_tmpdir context))
          ~relative:"pyre_server_hash.sock";
      watchman;
      critical_files;
    }
  in
  { context; start_options }


let start_options_of { start_options; _ } = start_options

let configuration_of project =
  let { StartOptions.environment_controls; _ } = start_options_of project in
  Analysis.EnvironmentControls.configuration environment_controls


let socket_address_of project =
  let { StartOptions.socket_path; _ } = start_options_of project in
  Lwt_unix.ADDR_UNIX (PyrePath.absolute socket_path)


let source_root_of project =
  let { Configuration.Analysis.project_root; _ } = configuration_of project in
  project_root


let test_server_with ~style ~clients { context; start_options } =
  Memory.reset_shared_memory ();
  try%lwt
    Start.start_server
      start_options
      ~on_started:(fun { Server.ServerProperties.socket_path; configuration; _ } server_state ->
        let socket_address = Lwt_unix.ADDR_UNIX (PyrePath.absolute socket_path) in
        let test_client client =
          let run_on_connection (input_channel, output_channel) =
            client
              {
                ClientConnection.context;
                configuration;
                server_state;
                input_channel;
                output_channel;
              }
          in
          Lwt_io.with_connection socket_address run_on_connection
        in
        let iterate_list =
          match style with
          | ClientConnection.Style.Sequential -> Lwt_list.iter_s
          | ClientConnection.Style.Concurrent -> Lwt_list.iter_p
        in
        iterate_list test_client clients)
  with
  | Server.Start.ServerStopped _ -> Lwt.return_unit


let test_server_with_one_connection ~f =
  test_server_with ~style:ClientConnection.Style.Sequential ~clients:[f]
