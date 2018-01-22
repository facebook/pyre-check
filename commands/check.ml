(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Configuration
open Pyre

module Error = PyreError
module Socket = PyreSocket


let server_not_running = 2


type result = {
  handles: File.Handle.t list;
  environment: (module Environment.Reader);
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
      parallel;
      project_root;
      stub_roots;
      source_root;
      report_undefined_attributes;
    }
    original_service
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
  let service =
    match original_service with
    | None -> Service.create ~is_parallel:parallel ~bucket_multiplier ()
    | Some service -> service
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
      ~project_root
      ~stub_roots
      ~infer
      ~recursive_infer
      ~report_undefined_attributes
      ()
  in

  (* Parsing. *)
  let stubs = ParseService.parse_stubs service ~configuration in
  let sources = ParseService.parse_sources service ~configuration in

  (* Build environment. *)
  let environment =
    let reader =
      if Service.is_parallel service then
        EnvironmentService.shared_memory_reader
      else
        EnvironmentService.in_process_reader
    in
    reader service ~configuration ~stubs ~sources
  in

  (* Run type checker. *)
  let errors, _ =
    TypeCheckService.analyze_sources service configuration environment sources
  in
  (* Only destroy the service if the check command created it. *)
  (match original_service with
   | None -> Service.destroy service
   | Some _ -> ());
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
    project_root
    stub_roots
    source_root
    () =
  let report_undefined_attributes =
    (* TODO(T24330702): remove this once the feature is complete enough. *)
    Sys.getenv "PYRE_REPORT_UNDEFINED_ATTRIBUTES"
    |> Option.is_some
  in
  let configuration =
    Configuration.create
      ~verbose
      ?version
      ~sections
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~infer
      ~recursive_infer
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ~stub_roots:(List.map ~f:Path.create_absolute stub_roots)
      ~source_root:(Path.create_absolute source_root)
      ~report_undefined_attributes
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
  Command.basic
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
      Socket.write socket Protocol.Request.ReinitializeStateRequest
    else
      Socket.write
        socket
        (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = false });

    let response_json =
      match Socket.read socket with
      | Protocol.TypeCheckResponse errors ->
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
  Command.basic
    ~summary:"Shows current errors by asking the server. \
              Starts a daemon server in the current directory if it does not exist."
    Command.Spec.(
      empty
      +> flag "-recheck-all" no_arg ~doc:"Recheck the entire project on the server"
      ++ spec)
    run_incremental
