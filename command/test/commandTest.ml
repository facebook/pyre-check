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
  Configuration.Analysis.create
    ~debug:false
    ~parallel:false
    ?expected_version
    ~local_root
    ()


let mock_server_configuration
    ~local_root
    ?expected_version
    () =
  let temporary = Filename.temp_file "" "" in
  Server.Operations.create_configuration
    ~log_path:(Path.create_absolute temporary)
    (mock_analysis_configuration ~local_root ?expected_version ())


let start_server ~local_root ?expected_version () =
  Commands.Start.run (mock_server_configuration ~local_root ?expected_version ())


let environment () =
  let configuration = Configuration.Analysis.create () in
  let environment = Environment.Builder.create () in
  Service.Environment.populate
    (Environment.handler ~configuration environment)
    ~configuration
    [
      parse {|
        class int(float): pass
        class object():
          def __init__(self) -> None: pass
          def __new__(self) -> typing.Any: pass
          def __sizeof__(self) -> int: pass
      |}
      |> Preprocessing.qualify;
    ];
  environment


let make_errors ?handle ?qualifier source =
  let configuration = Configuration.Analysis.create () in
  let source = Preprocessing.preprocess (parse ?handle ?qualifier source) in
  let environment = Environment.handler ~configuration (environment ()) in
  Service.Environment.populate environment ~configuration [source];
  let configuration = mock_analysis_configuration () in
  let { TypeCheck.Result.errors; _ } =
    TypeCheck.check
      ~configuration
      ~environment
      ~source
  in
  errors


let associate_errors_and_filenames error_list =
  let error_file error =
    File.Handle.create (Error.path error), error
  in
  List.map ~f:error_file error_list
  |> (List.fold
        ~init:File.Handle.Map.empty
        ~f:(fun map (handle, error) -> Map.add_multi map ~key:handle ~data:error))
  |> Map.to_alist


let run_command_tests test_category tests =
  (* We need this to fork off processes *)
  Scheduler.Daemon.check_entry_point ();
  Hh_logger.Level.set_min_level Hh_logger.Level.Fatal;
  let (!) f context = with_bracket_chdir context (bracket_tmpdir context) f in
  test_category>:::(List.map ~f:(fun (name, test_function) -> name>::(!test_function)) tests)
  |> Test.run


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
    Configuration.Server.configuration = {
      Configuration.Analysis.local_root;
      _;
    };
    socket_path;
    _;
  } =
  Commands.Stop.stop ~local_root:(Path.absolute local_root)
  |> ignore;
  with_timeout ~seconds:3 poll_for_deletion socket_path;
  clean_environment ()
