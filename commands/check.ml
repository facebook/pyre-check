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
      verbose;
      version = _;
      sections;
      gradual;
      debug;
      infer;
      recursive_infer;
      strict;
      declare;
      show_error_traces;
      parallel;
      stub_roots;
      project_root
    }
    original_service
    () =
  Log.initialize ~verbose ~sections;

  if not (Path.is_directory project_root) then
    raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp project_root));
  List.iter
    ~f:(fun stub_root ->
        if not (Path.is_directory stub_root) then
          raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp stub_root)))
    stub_roots;

  let service =
    match original_service with
    | None -> Service.create ~is_parallel:parallel ()
    | Some service -> service
  in
  let configuration =
    Configuration.create
      ~verbose
      ~sections
      ~gradual
      ~project_root
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~stub_roots
      ~infer
      ~recursive_infer ()
  in

  (* Parsing. *)
  let stubs = ParseService.parse_stubs service ~roots:(project_root :: stub_roots) in
  let sources = ParseService.parse_sources service ~root:project_root in

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
    check_unannotated
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    stub_roots
    project_root
    () =
  let configuration =
    Configuration.create
      ~verbose
      ?version
      ~sections
      ~gradual:(not check_unannotated)
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~infer
      ~recursive_infer
      ~parallel:(not sequential)
      ~stub_roots:(List.map ~f:Path.create_absolute stub_roots)
      ~project_root:(Path.create_absolute project_root)
      ()
  in

  let timer = Timer.start () in
  let { errors; _ } = check configuration None () in
  Log.performance
    ~flush:true
    ~name:"check"
    ~timer
    ~labels:[
      "root", configuration.Configuration.project_root |> Path.show;
      "request_kind", "FullCheck";
    ];
  (* Print results. *)
  Log.print
    "%s"
    (Yojson.Safe.to_string (
        `List (List.map
                 ~f:(fun error -> Error.to_json ~detailed:show_error_traces error)
                 errors)))


let spec =
  Command.Spec.(
    empty
    +> flag "-verbose" no_arg ~doc:"Turn on verbose logging"
    +> flag "-version" (optional string) ~doc:"Pyre version"
    +> flag
      "-logging-sections"
      (optional_with_default [] (Arg_type.comma_separated string))
      ~doc:"Comma-separated list of logging sections."
    +> flag
      "-check-unannotated"
      no_arg
      ~doc:("Run typechecking on the whole codebase, including untyped functions.")
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
      "-stub-roots"
      (optional_with_default [] (Arg_type.comma_separated string))
      ~doc:"Directory containing stubs to include"
    +> anon (maybe_with_default "." ("project-root" %: string)))


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
    check_unannotated
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    stub_roots
    project_root
    () =
  try
    Log.initialize ~verbose ~sections;
    let configuration =
      Configuration.create
        ~verbose
        ?version
        ~sections
        ~gradual:(not check_unannotated)
        ~debug
        ~strict
        ~declare
        ~show_error_traces
        ~infer
        ~recursive_infer
        ~parallel:(not sequential)
        ~stub_roots:(List.map ~f:Path.create_absolute stub_roots)
        ~project_root:(Path.create_absolute project_root)
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
    Log.print "%s" (Yojson.Safe.to_string response_json)
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
