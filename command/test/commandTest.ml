(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
  Worker.killall ()


let mock_analysis_configuration
    ?(local_root = Path.current_working_directory ())
    ?expected_version
    ()
  =
  Configuration.Analysis.create ~debug:false ~parallel:false ?expected_version ~local_root ()


let mock_server_configuration ~local_root ?expected_version () =
  let temporary = Filename.temp_file "" "" in
  Server.Operations.create_configuration
    ~log_path:(Path.create_absolute temporary)
    (mock_analysis_configuration ~local_root ?expected_version ())


let start_server ~local_root ?expected_version () =
  Commands.Start.run (mock_server_configuration ~local_root ?expected_version ())


let environment () =
  let configuration = Configuration.Analysis.create () in
  Test.environment
    ~configuration
    ~sources:
      [ parse
          {|
        class int(float): pass
        class object():
          def __init__(self) -> None: pass
          def __new__(self) -> typing.Any: pass
          def __sizeof__(self) -> int: pass
        class str(object): pass
      |}
        |> Preprocessing.qualify ]
    ()


let make_errors ?handle source =
  let configuration = Configuration.Analysis.create () in
  let source = parse ?handle source |> Preprocessing.preprocess in
  let environment = environment () in
  Service.Environment.populate environment ~configuration ~scheduler:(Scheduler.mock ()) [source];
  let configuration = mock_analysis_configuration () in
  let global_resolution = Environment.resolution environment () in
  TypeCheck.run ~configuration ~global_resolution ~source


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
      poll () )
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
  Commands.Stop.stop ~local_root:(Path.absolute local_root) |> ignore;
  with_timeout ~seconds:3 poll_for_deletion socket_path;
  clean_environment ()


module ScratchServer = struct
  type t = {
    configuration: Configuration.Analysis.t;
    server_configuration: Configuration.Server.t;
    state: Server.State.t;
  }

  let local_root_of { configuration = { Configuration.Analysis.local_root; _ }; _ } = local_root

  let start
      ?(incremental_transitive_dependencies = false)
      ~context
      ?(external_sources = [])
      sources
    =
    let configuration, module_tracker, ast_environment, sources =
      let ({ ScratchProject.module_tracker; configuration } as project) =
        ScratchProject.setup ~context ~external_sources sources
      in
      let sources, ast_environment = ScratchProject.parse_sources project in
      ( { configuration with incremental_transitive_dependencies },
        module_tracker,
        ast_environment,
        sources )
    in
    let external_sources = typeshed_stubs ~include_helper_builtins:false () in
    let environment = Test.environment ~configuration ~sources:(sources @ external_sources) () in
    let global_resolution = Environment.resolution environment () in
    let errors =
      let table = Ast.Reference.Table.create () in
      List.iter sources ~f:(fun ({ Ast.Source.qualifier; _ } as source) ->
          let errors = Analysis.TypeCheck.run ~configuration ~global_resolution ~source in
          Hashtbl.set table ~key:qualifier ~data:errors);
      table
    in
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
        Server.State.module_tracker;
        ast_environment;
        environment;
        errors;
        symlink_targets_to_sources = String.Table.create ();
        last_request_time = Unix.time ();
        last_integrity_check = Unix.time ();
        lookups = String.Table.create ();
        connections =
          {
            lock = Mutex.create ();
            connections =
              ref
                {
                  Server.State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
                  json_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
                  persistent_clients = Network.Socket.Map.empty;
                  file_notifiers = [];
                };
          };
        scheduler = Scheduler.mock ();
        open_documents = Path.Map.empty;
      }
    in
    { configuration; server_configuration; state }
end
