(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = Pyre.Path

module ServerEvent = struct
  type t =
    | SocketCreated of Path.t
    | ServerInitialized
    | Exception of string
  [@@deriving sexp, compare, hash, to_yojson]

  let serialize event = to_yojson event |> Yojson.Safe.to_string

  let write ~output_channel event =
    let open Lwt.Infix in
    serialize event |> Lwt_io.fprintl output_channel >>= fun () -> Lwt_io.flush output_channel
end

module ExitStatus = struct
  type t =
    | Ok
    | Error
  [@@deriving sexp, compare, hash]

  let exit_code = function
    | Ok -> 0
    | Error -> 1
end

(* Socket paths in most Unixes are limited to a length of +-100 characters, whereas `log_path` might
   exceed that limit. We have to work around this by shortening the original log path into
   `/tmp/pyre_server_XXX.sock`, where XXX is obtained by computing an MD5 hash of `log_path`. *)
(* Note that creating socket path this way implicitly assumes that `log_path` uniquely determines
   server instances. *)
let socket_path_of log_path =
  let socket_directory = Path.create_absolute ~follow_symbolic_links:false Filename.temp_dir_name in
  let log_path_digest = Path.absolute log_path |> Digest.string |> Digest.to_hex in
  Path.create_relative
    ~root:socket_directory
    ~relative:(Format.sprintf "pyre_server_%s.sock" log_path_digest)


let request_from_string request_string =
  try
    let json = Yojson.Safe.from_string request_string in
    match Request.of_yojson json with
    | Result.Error _ -> Result.Error "Malformed JSON request"
    | Result.Ok request -> Result.Ok request
  with
  | Yojson.Json_error message -> Result.Error message


let handle_request ~server_state request =
  let open Lwt.Infix in
  Lazy.force server_state
  >>= fun server_state ->
  let on_uncaught_server_exception exn =
    Log.info "Uncaught server exception: %s" (Exn.to_string exn);
    let () =
      let { ServerState.server_configuration; _ } = !server_state in
      StartupNotification.produce_for_configuration
        ~server_configuration
        "Restarting Pyre server due to unexpected crash"
    in
    Stop.stop_waiting_server ()
  in
  Lwt.catch
    (fun () -> RequestHandler.process_request ~state:!server_state request)
    on_uncaught_server_exception
  >>= fun (new_state, response) ->
  server_state := new_state;
  Lwt.return response


let handle_connection ~server_state _client_address (input_channel, output_channel) =
  let open Lwt.Infix in
  (* Raw request messages are processed line-by-line. *)
  let rec handle_line () =
    Lwt_io.read_line_opt input_channel
    >>= function
    | None ->
        Log.info "Connection closed";
        Lwt.return_unit
    | Some message ->
        let response =
          match request_from_string message with
          | Result.Error message -> Lwt.return (Response.Error message)
          | Result.Ok request -> handle_request ~server_state request
        in
        response
        >>= fun response ->
        Response.to_yojson response
        |> Yojson.Safe.to_string
        |> Lwt_io.write_line output_channel
        >>= handle_line
  in
  handle_line ()


let initialize_server_state
    ?watchman_subscriber
    ({ ServerConfiguration.log_path; saved_state_action; critical_files; _ } as server_configuration)
  =
  let configuration = ServerConfiguration.analysis_configuration_of server_configuration in
  (* This is needed to initialize shared memory. *)
  let _ = Memory.get_heap_handle configuration in
  let start_from_scratch () =
    Log.info "Initializing server state from scratch...";
    let { Service.Check.environment; errors } =
      Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
          Service.Check.check
            ~scheduler
            ~configuration
            ~call_graph_builder:(module Analysis.Callgraph.DefaultBuilder))
    in
    let error_table =
      let table = Ast.Reference.Table.create () in
      let add_error error =
        let key = Analysis.AnalysisError.path error in
        Hashtbl.add_multi table ~key ~data:error
      in
      List.iter errors ~f:add_error;
      table
    in
    {
      ServerState.socket_path = socket_path_of log_path;
      server_configuration;
      configuration;
      type_environment = environment;
      error_table;
    }
  in
  let fetch_saved_state_from_files ~shared_memory_path ~changed_files_path () =
    try
      let open Pyre in
      let changed_files =
        changed_files_path
        >>| File.create
        >>= File.content
        >>| String.split_lines
        >>| List.map ~f:(Path.create_absolute ~follow_symbolic_links:false)
        |> Option.value ~default:[]
      in
      Lwt.return (Result.Ok { SavedState.Fetched.path = shared_memory_path; changed_files })
    with
    | exn ->
        let message =
          Format.sprintf
            "Cannot fetch saved state from file due to exception: %s"
            (Exn.to_string exn)
        in
        Lwt.return (Result.Error message)
  in
  let fetch_saved_state_from_project ~project_name ~project_metadata () =
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        match watchman_subscriber with
        | None -> failwith "Watchman is not enabled"
        | Some watchman_subscriber ->
            let { Watchman.Subscriber.Setting.root = watchman_root; filter = watchman_filter; _ } =
              Watchman.Subscriber.setting_of watchman_subscriber
            in
            let watchman_connection = Watchman.Subscriber.connection_of watchman_subscriber in
            let target = Path.create_relative ~root:log_path ~relative:"server/server.state" in
            SavedState.query_and_fetch_exn
              {
                SavedState.Setting.watchman_root;
                watchman_filter;
                watchman_connection;
                project_name;
                project_metadata;
                critical_files;
                target;
              }
            >>= fun fetched -> Lwt.return (Result.Ok fetched))
      (fun exn ->
        let message =
          Format.sprintf
            "Cannot fetch saved state from project due to exception: %s"
            (Exn.to_string exn)
        in
        Lwt.return (Result.Error message))
  in
  let load_from_saved_state = function
    | Result.Error message ->
        Log.warning "%s" message;
        Lwt.return (start_from_scratch ())
    | Result.Ok { SavedState.Fetched.path; changed_files } ->
        Log.info "Restoring environments from saved state...";
        let loaded_state =
          Memory.load_shared_memory ~path:(Path.absolute path) ~configuration;
          let module_tracker = Analysis.ModuleTracker.SharedMemory.load () in
          let ast_environment = Analysis.AstEnvironment.load module_tracker in
          let type_environment =
            Analysis.AnnotatedGlobalEnvironment.create ast_environment
            |> Analysis.TypeEnvironment.create
          in
          Analysis.SharedMemoryKeys.DependencyKey.Registry.load ();
          let error_table = Server.SavedState.ServerErrors.load () in
          {
            ServerState.socket_path = socket_path_of log_path;
            server_configuration;
            configuration;
            type_environment;
            error_table;
          }
        in
        let open Lwt.Infix in
        Log.info "Processing recent updates not included in saved state...";
        Request.IncrementalUpdate (List.map changed_files ~f:Path.absolute)
        |> RequestHandler.process_request ~state:loaded_state
        >>= fun (new_state, _) -> Lwt.return new_state
  in
  let open Lwt.Infix in
  let get_initial_state () =
    match saved_state_action with
    | None -> Lwt.return (start_from_scratch ())
    | Some
        (ServerConfiguration.SavedStateAction.LoadFromFile
          { shared_memory_path; changed_files_path }) ->
        fetch_saved_state_from_files ~shared_memory_path ~changed_files_path ()
        >>= load_from_saved_state
    | Some (ServerConfiguration.SavedStateAction.LoadFromProject { project_name; project_metadata })
      ->
        fetch_saved_state_from_project ~project_name ~project_metadata () >>= load_from_saved_state
  in
  get_initial_state ()
  >>= fun state ->
  Log.info "Server state initialized.";
  Lwt.return (ref state)


let get_watchman_subscriber
    ?watchman
    { ServerConfiguration.watchman_root; critical_files; extensions; _ }
  =
  let open Lwt.Infix in
  match watchman_root with
  | None -> Lwt.return_none
  | Some root ->
      let get_raw_watchman = function
        | Some watchman -> Lwt.return watchman
        | None -> Watchman.Raw.create_exn ()
      in
      get_raw_watchman watchman
      >>= fun raw ->
      let subscriber_setting =
        let filter =
          let base_names =
            List.map critical_files ~f:ServerConfiguration.CriticalFile.base_name_of
            |> String.Set.of_list
            |> fun set ->
            Set.add set ".pyre_configuration"
            |> fun set -> Set.add set ".pyre_configuration.local" |> Set.to_list
          in
          let suffixes =
            String.Set.of_list extensions
            |> fun set -> Set.add set "py" |> fun set -> Set.add set "pyi" |> Set.to_list
          in
          { Watchman.Filter.base_names; suffixes }
        in
        { Watchman.Subscriber.Setting.raw; root; filter }
      in
      Watchman.Subscriber.subscribe subscriber_setting >>= Lwt.return_some


let on_watchman_update ~server_state paths =
  let open Lwt.Infix in
  let update_request = Request.IncrementalUpdate (List.map paths ~f:Path.absolute) in
  handle_request ~server_state update_request
  >>= fun _ok_response ->
  (* File watcher does not care about the content of the the response. *)
  Lwt.return_unit


let with_server ?watchman ~f ({ ServerConfiguration.log_path; _ } as server_configuration) =
  let open Lwt in
  let socket_path = socket_path_of log_path in
  (* Watchman connection needs to be up before server can start -- otherwise we risk missing
     filesystem updates during server establishment. *)
  get_watchman_subscriber ?watchman server_configuration
  >>= fun watchman_subscriber ->
  let server_state =
    (* We do not want the expensive server initialization to happen before we start to listen on the
       socket. Hence the use of `lazy` here to delay the initialization. *)
    lazy (initialize_server_state ?watchman_subscriber server_configuration)
  in
  Lwt_io.establish_server_with_client_address
    (Lwt_unix.ADDR_UNIX (Path.absolute socket_path))
    (handle_connection ~server_state)
  >>= fun server ->
  let server_waiter () = f (socket_path, server_state) in
  let server_destructor () =
    Log.info "Server is going down. Cleaning up...";
    Lwt_io.shutdown_server server
  in
  finalize
    (fun () ->
      Log.info "Server has started listening on socket `%a`" Path.pp socket_path;
      match watchman_subscriber with
      | None ->
          (* Only wait for the server if we do not have a watchman subscriber. *)
          server_waiter ()
      | Some subscriber ->
          let watchman_waiter =
            Watchman.Subscriber.listen ~f:(on_watchman_update ~server_state) subscriber
            >>= fun () ->
            (* Lost watchman connection is considered an error. *)
            return ExitStatus.Error
          in
          (* Make sure when the watchman subscriber crashes, the server would go down as well. *)
          Lwt.choose [server_waiter (); watchman_waiter])
    server_destructor


(* Create a promise that only gets fulfilled when given unix signals are received. *)
let wait_on_signals fatal_signals =
  let open Lwt in
  let waiter, resolver = wait () in
  List.iter fatal_signals ~f:(fun signal ->
      let signal = Signal.to_caml_int signal in
      Lwt_unix.on_signal signal (wakeup resolver) |> ignore);
  waiter
  >>= fun signal ->
  Log.info "Server interrupted with signal %d" signal;
  return_unit


let start_server
    ?watchman
    ?(on_server_socket_ready = fun _ -> Lwt.return_unit)
    ~on_started
    ~on_exception
    server_configuration
  =
  let open Lwt in
  let f (socket_path, uninitialized_server_state) =
    on_server_socket_ready socket_path
    >>= fun _ -> Lazy.force uninitialized_server_state >>= on_started
  in
  catch (fun () -> with_server ?watchman server_configuration ~f) on_exception


let start_server_and_wait ?event_channel server_configuration =
  let open Lwt in
  let write_event event =
    match event_channel with
    | None -> return_unit
    | Some output_channel ->
        catch
          (fun () -> ServerEvent.write ~output_channel event)
          (function
            | Lwt_io.Channel_closed _
            | Caml.Unix.Unix_error (Caml.Unix.EPIPE, _, _) ->
                return_unit
            | exn -> Lwt.fail exn)
  in
  start_server
    server_configuration
    ~on_server_socket_ready:(fun socket_path ->
      (* An empty message signals that server socket has been created. *)
      write_event (ServerEvent.SocketCreated socket_path))
    ~on_started:(fun _ ->
      write_event ServerEvent.ServerInitialized
      >>= fun () -> wait_on_signals [Signal.int] >>= fun () -> return ExitStatus.Ok)
    ~on_exception:(fun exn ->
      Log.error "Exception thrown from Pyre server.";
      let message = Exn.to_string exn in
      Log.error "%s" message;
      (* A non-empty message signals that an error has occurred. *)
      write_event (ServerEvent.Exception message) >>= fun () -> return ExitStatus.Error)
