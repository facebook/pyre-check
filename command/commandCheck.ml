(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Configuration
open Pyre

module Socket = CommandSocket
module Scheduler = Service.Scheduler
module Handshake = CommandHandshake


let server_not_running = 2


type result = {
  handles: File.Handle.t list;
  environment: (module Environment.Handler);
  errors: Error.t list
}


let check
    {
      start_time = _;
      verbose;
      version = _;
      sections;
      debug;
      infer;
      recursive_infer;
      strict;
      declare;
      show_error_traces;
      log_identifier;
      parallel;
      project_root;
      stub_roots;
      source_root;
    }
    original_scheduler
    () =
  Log.initialize ~verbose ~sections;

  if not (Path.is_directory source_root) then
    raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp source_root));
  if not (Path.is_directory project_root) then
    raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp project_root));
  List.iter
    ~f:(fun stub_root ->
        if not (Path.is_directory stub_root) then
          raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp stub_root)))
    stub_roots;

  let bucket_multiplier =
    try Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> (fun value -> Option.value_exn value))
    with _ -> 10
  in
  let scheduler =
    match original_scheduler with
    | None -> Scheduler.create ~is_parallel:parallel ~bucket_multiplier ()
    | Some scheduler -> scheduler
  in
  let configuration =
    Configuration.create
      ~verbose
      ~sections
      ~source_root
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ~project_root
      ~stub_roots
      ~infer
      ~recursive_infer
      ()
  in

  (* Parsing. *)
  let stubs = Service.Parser.parse_stubs scheduler ~configuration in
  let sources = Service.Parser.parse_sources scheduler ~configuration in

  (* Build environment. *)
  let environment =
    let handler =
      if Scheduler.is_parallel scheduler then
        Service.Environment.shared_memory_handler
      else
        Service.Environment.in_process_handler
    in
    handler scheduler ~configuration ~stubs ~sources
  in

  (* Run type checker. *)
  let errors, _, { TypeCheck.Coverage.full; partial; untyped; ignore } =
    Service.TypeCheck.analyze_sources scheduler configuration environment sources
  in
  (* Log coverage results *)
  let path_to_files =
    Path.get_relative_to_root ~root:project_root ~path:source_root
    |> Option.value ~default:(Path.absolute source_root)
  in
  Statistics.coverage
    ~coverage:[
      "full_type_coverage", full;
      "partial_type_coverage", partial;
      "no_type_coverage", untyped;
      "ignore_coverage", ignore;
      "total_errors", List.length errors;
    ]
    ~configuration
    ~normals:[
      "file_name", path_to_files;
    ]
    ();
  (* Only destroy the scheduler if the check command created it. *)
  begin
    match original_scheduler with
    | None -> Scheduler.destroy scheduler
    | Some _ -> ()
  end;
  { handles = stubs @ sources; environment; errors }


(** run_command prints out the errors, for a Check run *)
let run_check
    verbose
    version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    log_identifier
    project_root
    stub_roots
    source_root
    () =
  let configuration =
    Configuration.create
      ~verbose
      ?version
      ~sections
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ~infer
      ~recursive_infer
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ~stub_roots:(List.map ~f:Path.create_absolute stub_roots)
      ~source_root:(Path.create_absolute source_root)
      ()
  in

  let timer = Timer.start () in
  let { errors; _ } = check configuration None () in
  Statistics.performance
    ~name:"check"
    ~timer
    ~configuration
    ~normals:["request kind", "FullCheck"]
    ();
  (* Print results. *)
  Log.print
    "%s"
    (Yojson.Safe.to_string (
        `List (List.map
                 ~f:(fun error -> Error.to_json ~detailed:show_error_traces error)
                 errors)));
  Statistics.flush ()


let spec =
  Command.Spec.(
    empty
    +> flag "-verbose" no_arg ~doc:"Turn on verbose logging"
    +> flag "-version" (optional string) ~doc:"Pyre version"
    +> flag
      "-logging-sections"
      (optional_with_default [] (Arg_type.comma_separated string))
      ~doc:"Comma-separated list of logging sections."
    +> flag "-debug" no_arg ~doc:"Turn on debug mode"
    +> flag "-strict" no_arg ~doc:"Turn on strict mode"
    +> flag "-declare" no_arg ~doc:"Turn on declare mode"
    +> flag "-show-error-traces" no_arg ~doc:"Outputs additional error information"
    +> flag "-infer" no_arg ~doc:"Outputs extra information and errors for inference purposes"
    +> flag
      "-recursive-infer"
      no_arg
      ~doc:"Recursively run infer until no new annotations are generated."
    +> flag "-sequential" no_arg ~doc:"Turn off parallel processing (parallel on by default)."
    +> flag
      "-log-identifier"
      (optional_with_default "" string)
      ~doc:"Add given identifier to logged samples."
    +> flag
      "-project-root"
      (optional_with_default "/" string)
      ~doc:"Only check sources under this root directory."
      ~aliases:["-type-check-root"]
    +> flag
      "-stub-roots"
      (optional_with_default [] (Arg_type.comma_separated string))
      ~doc:"Directory containing stubs to include"
    +> anon (maybe_with_default "." ("source-root" %: string)))


let check_command =
  Command.basic_spec
    ~summary:"Runs a full check without a server (default)"
    spec
    run_check


let run_incremental
    recheck_all
    verbose
    version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    log_identifier
    project_root
    stub_roots
    source_root
    () =
  try
    Log.initialize ~verbose ~sections;
    let configuration =
      Configuration.create
        ~verbose
        ?version
        ~sections
        ~debug
        ~strict
        ~declare
        ~show_error_traces
        ~log_identifier
        ~infer
        ~recursive_infer
        ~parallel:(not sequential)
        ~stub_roots:(List.map ~f:Path.create_absolute stub_roots)
        ~project_root:(Path.create_absolute project_root)
        ~source_root:(Path.create_absolute source_root)
        ()
    in

    let socket =
      let socket =
        match Socket.open_connection (ServerConfiguration.socket_path configuration) with
        | `Success socket ->
            Log.log ~section:`Server "Connected to server";
            socket
        | `Failure ->
            raise ServerConfiguration.ServerNotRunning
      in
      let in_channel, _ = socket in
      Log.log ~section:`Server "Waiting for server response...";
      let socket = Unix.descr_of_in_channel in_channel in
      Socket.read socket
      |> fun (Handshake.ServerConnected _) ->
      Socket.write socket Handshake.ClientConnected;
      socket
    in

    if recheck_all then
      Socket.write socket ServerProtocol.Request.ReinitializeStateRequest
    else
      Socket.write
        socket
        (ServerProtocol.Request.TypeCheckRequest
           { ServerProtocol.files = []; check_dependents = false });

    let response_json =
      match Socket.read socket with
      | ServerProtocol.TypeCheckResponse errors ->
          errors
          |> List.map ~f:snd
          |> List.concat
          |> (fun errors ->
              `List (List.map
                       ~f:(fun error -> Error.to_json ~detailed:show_error_traces error)
                       errors))
      | _ -> failwith "Unexpected response in incremental check."
    in
    Log.print "%s" (Yojson.Safe.to_string response_json);
    Statistics.flush ()
  with ServerConfiguration.ServerNotRunning ->
    Log.error "Server is not running";
    exit server_not_running


let incremental_command =
  Command.basic_spec
    ~summary:"Shows current errors by asking the server. \
              Starts a daemon server in the current directory if it does not exist."
    Command.Spec.(
      empty
      +> flag "-recheck-all" no_arg ~doc:"Recheck the entire project on the server"
      ++ spec)
    run_incremental
