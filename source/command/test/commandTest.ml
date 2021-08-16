(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Pyre
open Test

let clean_environment () =
  (* Clean up: hack library modifies the environment, causing OUnit to scream. Unset the variables
     that the library modifies. *)
  Unix.unsetenv "HH_SERVER_DAEMON_PARAM";
  Unix.unsetenv "HH_SERVER_DAEMON";
  ()


let mock_analysis_configuration
    ?(local_root = Path.current_working_directory ())
    ?expected_version
    ()
  =
  Configuration.Analysis.create
    ~debug:false
    ~parallel:false
    ?expected_version
    ~local_root
    ~source_path:[SearchPath.Root local_root]
    ()


let mock_server_configuration ~local_root ?expected_version () =
  let temporary = Filename.temp_file "" "" in
  Server.Operations.create_configuration
    ~log_path:(Path.create_absolute temporary)
    (mock_analysis_configuration ~local_root ?expected_version ())


let start_server ~local_root ?expected_version () =
  Commands.Start.run (mock_server_configuration ~local_root ?expected_version ())


(* NOTE: This function runs a standalone type-check pass and therefore needs to nuke shared memory. *)
let make_errors ~context ?(handle = "test.py") ?(show_error_traces = false) source =
  let project =
    let builtins_source =
      {|
          class int(float): pass
          class object():
            def __init__(self) -> None: pass
            def __new__(self) -> typing.Any: pass
            def __sizeof__(self) -> int: pass
            def __init_subclass__(cls) -> None: pass
          class str(object): pass
        |}
    in
    let typing_source = {|
          class Mapping:
            pass
    |} in
    ScratchProject.setup
      ~context
      ~show_error_traces
      ~include_typeshed_stubs:false
      ~external_sources:["builtins.pyi", builtins_source; "typing.pyi", typing_source]
      [handle, source]
  in
  let { ScratchProject.BuiltTypeEnvironment.type_environment; _ }, errors =
    ScratchProject.build_type_environment_and_postprocess project
  in
  let errors =
    let { Configuration.Analysis.show_error_traces; _ } = ScratchProject.configuration_of project in
    List.map
      errors
      ~f:
        (AnalysisError.instantiate
           ~show_error_traces
           ~lookup:
             (AstEnvironment.ReadOnly.get_real_path_relative
                ~configuration:(ScratchProject.configuration_of project)
                (TypeEnvironment.ast_environment type_environment |> AstEnvironment.read_only)))
  in
  Memory.reset_shared_memory ();
  errors


let run_command_tests test_category tests =
  (* We need this to fork off processes *)
  Scheduler.Daemon.check_entry_point ();
  Hh_logger.Level.set_min_level Hh_logger.Level.Fatal;
  let ( ! ) f context = with_bracket_chdir context (bracket_tmpdir context) f in
  test_category
  >::: List.map ~f:(fun (name, test_function) -> name >:: !test_function) tests
  |> Test.run


let protect ~f ~cleanup =
  try f () with
  | caught_exception ->
      cleanup ();
      raise caught_exception


exception Timeout

let with_timeout ~seconds f x =
  let timeout_option ~seconds f x =
    let signal = Signal.Expert.signal Signal.alrm (`Handle (fun _ -> raise Timeout)) in
    let cleanup () =
      let _ = Unix.alarm 0 in
      Signal.Expert.set Signal.alrm signal
    in
    try
      let _ = Unix.alarm seconds in
      let result = f x in
      cleanup ();
      Some result
    with
    | Timeout ->
        cleanup ();
        None
    | _ ->
        cleanup ();
        failwith "Timeout function failed"
  in
  match timeout_option ~seconds f x with
  | Some x -> x
  | None -> raise Timeout


let poll_for_deletion path =
  let rec poll () =
    if Path.file_exists path then (
      Unix.nanosleep 0.1 |> ignore;
      poll ())
    else
      ()
  in
  poll ()


let stop_server
    {
      Configuration.Server.configuration = { Configuration.Analysis.local_root; _ };
      socket = { path = socket_path; _ };
      _;
    }
  =
  Commands.Stop.stop ~log_directory:None ~local_root:(Path.absolute local_root) |> ignore;
  with_timeout ~seconds:3 poll_for_deletion socket_path;
  clean_environment ()


module ScratchServer = struct
  type t = {
    configuration: Configuration.Analysis.t;
    server_configuration: Configuration.Server.t;
    state: Server.State.t;
  }

  let start
      ?(incremental_style = Configuration.Analysis.FineGrained)
      ?(show_error_traces = false)
      ~context
      ?(external_sources = [])
      sources
    =
    let configuration, environment, type_errors =
      let ({ ScratchProject.configuration; _ } as project) =
        ScratchProject.setup
          ~context
          ~external_sources
          ~include_helper_builtins:false
          ~show_error_traces
          sources
      in
      let { ScratchProject.BuiltTypeEnvironment.type_environment; _ }, type_errors =
        ScratchProject.build_type_environment_and_postprocess project
      in
      { configuration with incremental_style }, type_environment, type_errors
    in
    (* Associate the new errors with new files *)
    let errors = Ast.Reference.Table.create () in
    List.iter type_errors ~f:(fun error ->
        let key = AnalysisError.path error in
        Hashtbl.add_multi errors ~key ~data:error);
    let server_configuration =
      Server.Operations.create_configuration
        ~log_path:(Path.create_absolute "/dev/null")
        configuration
    in
    let () =
      let set_up_shared_memory _ = () in
      let tear_down_shared_memory () _ = () in
      OUnit2.bracket set_up_shared_memory tear_down_shared_memory context
    in
    let state =
      {
        Server.State.environment;
        errors;
        symlink_targets_to_sources = String.Table.create ();
        last_request_time = Unix.time ();
        last_integrity_check = Unix.time ();
        connections =
          {
            lock = Error_checking_mutex.create ();
            connections =
              ref
                {
                  Server.State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
                  json_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
                  adapter_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
                  persistent_clients = Network.Socket.Map.empty;
                  json_sockets = [];
                  sockets_to_close = [];
                  adapter_sockets = [];
                };
          };
        scheduler = Scheduler.create_sequential ();
        open_documents = Ast.Reference.Table.create ();
        server_uuid = None;
      }
    in
    { configuration; server_configuration; state }
end
