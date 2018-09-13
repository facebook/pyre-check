(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Pyre
open Test


let clean_environment () =
  (* Clean up: hack library modifies the environment, causing OUnit to
      scream. Unset the variables that the library modifies. *)
  Unix.unsetenv "HH_SERVER_DAEMON_PARAM";
  Unix.unsetenv "HH_SERVER_DAEMON";
  Worker.killall ()


let mock_analysis_configuration
    ?(local_root = Path.current_working_directory ())
    ?expected_version
    () =
  Configuration.create
    ~debug:false
    ~parallel:false
    ?expected_version
    ~local_root
    ()


let mock_server_configuration
    ?(local_root = Path.current_working_directory ())
    ?expected_version
    () =
  let temporary = Filename.temp_file "" "" in
  Server.ServerConfiguration.create
    ~log_path:(Path.create_absolute temporary)
    (mock_analysis_configuration ~local_root ?expected_version ())


let start_server ?(local_root = Path.current_working_directory ()) ?expected_version () =
  Commands.Server.start (mock_server_configuration ~local_root ?expected_version ())


let environment () =
  let configuration = Configuration.create () in
  let environment = Environment.Builder.create () in
  Service.Environment.populate
    (Environment.handler ~configuration environment)
    [
      parse {|
        class int(float): pass
      |}
    ];
  environment


let make_errors source =
  let configuration = Configuration.create () in
  let source = Preprocessing.preprocess (parse source) in
  let environment_handler = Environment.handler ~configuration (environment ()) in
  Service.Environment.populate environment_handler [source];
  let configuration = mock_analysis_configuration () in
  (TypeCheck.check configuration environment_handler source).TypeCheck.Result.errors


let run_command_tests test_category tests =
  (* We need this to fork off processes *)
  Scheduler.Daemon.check_entry_point ();
  Hh_logger.Level.set_min_level Hh_logger.Level.Fatal;
  let (!) f context = with_bracket_chdir context (bracket_tmpdir context) f in
  test_category>:::(List.map ~f:(fun (name, test_function) -> name>::(!test_function)) tests)
  |> run_test_tt_main


let protect ~f ~cleanup =
  try
    f ()
  with caught_exception ->
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
    | Timeout -> cleanup (); None
    | _ -> cleanup (); failwith "Timeout function failed" in
  match timeout_option ~seconds f x with
  | Some x -> x
  | None -> raise Timeout


let poll_for_deletion path =
  let rec poll () =
    if Path.file_exists path then
      (Unix.nanosleep 0.1 |> ignore; poll ())
    else
      ()
  in
  poll ()


let stop_server {
    Server.ServerConfiguration.configuration = {
      Configuration.local_root;
      _;
    };
    socket_path;
    _;
  } =
  Command.run ~argv:["_"; "-graceful"; Path.absolute local_root] Commands.Server.stop_command;
  with_timeout ~seconds:3 poll_for_deletion socket_path;
  clean_environment ()
