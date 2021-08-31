(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = Pyre.Path

module ServerEvent = struct
  module ErrorKind = struct
    type t =
      | Watchman
      | BuckInternal
      | BuckUser
      | Pyre
      | Unknown
    [@@deriving sexp, compare, hash, to_yojson]
  end

  type t =
    | SocketCreated of Path.t
    | ServerInitialized
    | Exception of string * ErrorKind.t
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
  let socket_directory = Path.create_absolute (Caml.Filename.get_temp_dir_name ()) in
  let log_path_digest = Path.absolute log_path |> Digest.string |> Digest.to_hex in
  Path.create_relative
    ~root:socket_directory
    ~relative:(Format.sprintf "pyre_server_%s.sock" log_path_digest)


let with_performance_logging ?(normals = []) ~name f =
  let open Lwt.Infix in
  let timer = Timer.start () in
  f ()
  >>= fun result ->
  let normals =
    let version =
      (* HACK: Use `Version.version ()` directly when all servers are migrated. *)
      Format.sprintf "newserver-%s" (Version.version ())
    in
    ("binary_version", version) :: normals
  in
  Statistics.performance ~name ~timer ~normals ();
  Lwt.return result


module ClientRequest = struct
  type t =
    | Request of Request.t
    | Subscription of Subscription.Request.t
    | Error of string
  [@@deriving sexp, compare]

  let of_string input_string =
    try
      let json = Yojson.Safe.from_string input_string in
      match Subscription.Request.of_yojson json with
      | Result.Ok subscription -> Subscription subscription
      | Result.Error _ -> (
          match Request.of_yojson json with
          | Result.Ok request -> Request request
          | Result.Error _ -> Error "Malformed JSON request")
    with
    | Yojson.Json_error message -> Error message
end

let handle_request ~state request =
  let open Lwt.Infix in
  let on_uncaught_server_exception exn =
    Log.info "Uncaught server exception: %s" (Exn.to_string exn);
    let () =
      let { ServerState.configuration; _ } = state in
      StartupNotification.produce
        ~log_path:configuration.log_directory
        "Restarting Pyre server due to unexpected crash"
    in
    let origin =
      match exn with
      | Buck.Raw.BuckError _
      | Buck.Builder.JsonError _
      | Buck.Builder.LinkTreeConstructionError _ ->
          "buck"
      | Watchman.ConnectionError _
      | Watchman.SubscriptionError _
      | Watchman.QueryError _ ->
          "watchman"
      | _ -> "server"
    in
    Statistics.log_exception exn ~fatal:true ~origin;
    Stop.log_and_stop_waiting_server ~reason:"uncaught exception" ~state ()
  in
  Lwt.catch
    (fun () ->
      Log.log ~section:`Server "Processing request %a..." Sexp.pp (Request.sexp_of_t request);
      with_performance_logging
        ~normals:["request kind", Request.name_of request]
        ~name:"server request"
        (fun () -> RequestHandler.process_request ~state request))
    on_uncaught_server_exception
  >>= fun (new_state, response) ->
  Log.log ~section:`Server "Request `%a` processed" Sexp.pp (Request.sexp_of_t request);
  Lwt.return (new_state, response)


let handle_subscription ~state:{ ServerState.subscriptions; _ } ~output_channel request =
  match request with
  | Subscription.Request.SubscribeToTypeErrors subscriber_name ->
      let subscription = Subscription.create ~name:subscriber_name ~output_channel () in
      ServerState.Subscriptions.add subscriptions ~name:subscriber_name ~subscription;
      subscription


module ConnectionState = struct
  (* Keep track of the subscriptions created from each connection, so when it is closed we could
     remove those subscriptions from the server state automatically. *)
  type t = { subscription_names: string list }

  let create () = { subscription_names = [] }

  let add_subscription ~name { subscription_names } =
    Log.log ~section:`Server "Subscription added: %s" name;
    { subscription_names = name :: subscription_names }


  let cleanup ~server_state:{ ServerState.subscriptions; _ } { subscription_names } =
    List.iter subscription_names ~f:(fun name ->
        Log.log ~section:`Server "Subscription removed: %s" name;
        ServerState.Subscriptions.remove ~name subscriptions)
end

let handle_connection ~server_state _client_address (input_channel, output_channel) =
  let open Lwt.Infix in
  Log.log ~section:`Server "Connection established";
  (* Raw request messages are processed line-by-line. *)
  let rec handle_line connection_state =
    Lwt_io.read_line_opt input_channel
    >>= function
    | None ->
        Log.log ~section:`Server "Connection closed";
        ExclusiveLock.write server_state ~f:(fun server_state ->
            ConnectionState.cleanup ~server_state connection_state;
            Lwt.return (server_state, ()))
    | Some message ->
        let result =
          match ClientRequest.of_string message with
          | ClientRequest.Error message -> Lwt.return (connection_state, Response.Error message)
          | ClientRequest.Request request ->
              ExclusiveLock.write server_state ~f:(fun state ->
                  handle_request ~state request
                  >>= fun (new_state, response) ->
                  Lwt.return (new_state, (connection_state, response)))
          | ClientRequest.Subscription subscription ->
              ExclusiveLock.write server_state ~f:(fun state ->
                  let subscription = handle_subscription ~state ~output_channel subscription in
                  (* We send back the initial set of type errors when a subscription first gets
                     established. *)
                  handle_request ~state (Request.DisplayTypeError [])
                  >>= fun (new_state, response) ->
                  Lwt.return
                    ( new_state,
                      ( ConnectionState.add_subscription
                          ~name:(Subscription.name_of subscription)
                          connection_state,
                        response ) ))
        in
        result
        >>= fun (new_connection_state, response) ->
        Response.to_yojson response
        |> Yojson.Safe.to_string
        |> Lwt_io.write_line output_channel
        >>= fun () -> handle_line new_connection_state
  in
  ConnectionState.create () |> handle_line


let initialize_server_state
    ?watchman_subscriber
    ?build_system_initializer
    ~configuration:({ Configuration.Analysis.log_directory; _ } as configuration)
    { StartOptions.source_paths; saved_state_action; critical_files; _ }
  =
  (* This is needed to initialize shared memory. *)
  let _ = Memory.get_heap_handle configuration in
  let start_from_scratch ~build_system () =
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
    ServerState.create
      ~socket_path:(socket_path_of log_directory)
      ~critical_files
      ~configuration
      ~build_system
      ~type_environment:environment
      ~error_table
      ()
  in
  let build_and_start_from_scratch ~build_system_initializer () =
    let open Lwt.Infix in
    BuildSystem.Initializer.run build_system_initializer
    >>= fun build_system -> Lwt.return (start_from_scratch ~build_system ())
  in
  let fetch_saved_state_from_files ~shared_memory_path ~changed_files_path () =
    try
      let open Pyre in
      let changed_files =
        changed_files_path
        >>| File.create
        >>= File.content
        >>| String.split_lines
        >>| List.map ~f:Path.create_absolute
        |> Option.value ~default:[]
      in
      Lwt.return (Result.Ok { SavedState.Fetched.path = shared_memory_path; changed_files })
    with
    | exn ->
        let message =
          let detailed_message =
            match exn with
            | Watchman.ConnectionError message
            | Watchman.QueryError message ->
                message
            | _ -> Exn.to_string exn
          in
          Format.sprintf "Cannot fetch saved state from file: %s" detailed_message
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
            let {
              Watchman.Subscriber.Setting.root = watchman_root;
              filter = watchman_filter;
              raw;
              _;
            }
              =
              Watchman.Subscriber.setting_of watchman_subscriber
            in
            Watchman.Raw.with_connection raw ~f:(fun watchman_connection ->
                let target =
                  Path.create_relative ~root:log_directory ~relative:"new_server/server.state"
                in
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
                >>= fun fetched -> Lwt.return (Result.Ok fetched)))
      (fun exn ->
        let message =
          let detailed_message =
            match exn with
            | Watchman.ConnectionError message
            | Watchman.QueryError message
            | SavedState.SavedStateQueryFailure message ->
                message
            | _ -> Exn.to_string exn
          in
          Format.sprintf "Cannot fetch saved state from project: %s" detailed_message
        in
        Lwt.return (Result.Error message))
  in
  (* Note that this function contains some heuristics: it only attempts to perform cheap checks on
     what might affect type checking result. Do *NOT* use it as a general-purpose configuration
     comparator. *)
  let configuration_equal
      {
        Configuration.Analysis.analyze_external_sources = left_analyze_external_sources;
        filter_directories = left_filter_directories;
        ignore_all_errors = left_ignore_all_errors;
        source_path = left_source_path;
        search_path = left_search_path;
        taint_model_paths = left_taint_model_paths;
        strict = left_strict;
        excludes = left_excludes;
        extensions = left_extensions;
        _;
      }
      {
        Configuration.Analysis.analyze_external_sources = right_analyze_external_sources;
        filter_directories = right_filter_directories;
        ignore_all_errors = right_ignore_all_errors;
        source_path = right_source_path;
        search_path = right_search_path;
        taint_model_paths = right_taint_model_paths;
        strict = right_strict;
        excludes = right_excludes;
        extensions = right_extensions;
        _;
      }
    =
    let list_length_equal left right = Int.equal (List.length left) (List.length right) in
    let optional_list_length_equal left right =
      match left, right with
      | None, None -> true
      | Some left, Some right when list_length_equal left right -> true
      | _, _ -> false
    in
    Bool.equal left_analyze_external_sources right_analyze_external_sources
    && optional_list_length_equal left_filter_directories right_filter_directories
    && optional_list_length_equal left_ignore_all_errors right_ignore_all_errors
    && list_length_equal left_source_path right_source_path
    && list_length_equal left_search_path right_search_path
    && list_length_equal left_taint_model_paths right_taint_model_paths
    && Bool.equal left_strict right_strict
    && list_length_equal left_excludes right_excludes
    && list_length_equal left_extensions right_extensions
  in
  let load_from_shared_memory path =
    try Result.Ok (Memory.load_shared_memory ~path:(Path.absolute path) ~configuration) with
    | Memory.SavedStateLoadingFailure message -> Result.Error message
  in
  let load_from_saved_state ~build_system_initializer = function
    | Result.Error message ->
        Log.warning "%s" message;
        Statistics.event ~name:"saved state failure" ~normals:["reason", message] ();
        build_and_start_from_scratch ~build_system_initializer ()
    | Result.Ok { SavedState.Fetched.path; changed_files } -> (
        Log.info "Restoring environments from saved state...";
        match load_from_shared_memory path with
        | Result.Error message ->
            Log.warning "%s" message;
            Statistics.event
              ~name:"saved state failure"
              ~normals:["reason", "shared memory loading failure"]
              ();
            Memory.reset_shared_memory ();
            build_and_start_from_scratch ~build_system_initializer ()
        | Result.Ok () -> (
            match
              configuration_equal configuration (Server.SavedState.StoredConfiguration.load ())
            with
            | false ->
                (* Although this is a rare occurrence, it *is* possible for the provided
                   `Configuration.Analysis.t` to be different from what's stored in the saved state
                   even if the configuration file remained the same. If that happens, we cannot
                   reuse the saved state as it may lead to a server crash later. *)
                Log.warning
                  "Cannot load saved state due to unexpected configuration change. Falling back to \
                   cold start...";
                Statistics.event
                  ~name:"saved state failure"
                  ~normals:["reason", "configuration change"]
                  ();
                Memory.reset_shared_memory ();
                build_and_start_from_scratch ~build_system_initializer ()
            | true ->
                let open Lwt.Infix in
                BuildSystem.Initializer.load build_system_initializer
                >>= fun build_system ->
                let loaded_state =
                  let module_tracker = Analysis.ModuleTracker.SharedMemory.load () in
                  let ast_environment = Analysis.AstEnvironment.load module_tracker in
                  let type_environment =
                    Analysis.AnnotatedGlobalEnvironment.create ast_environment
                    |> Analysis.TypeEnvironment.create
                  in
                  Analysis.SharedMemoryKeys.DependencyKey.Registry.load ();
                  let error_table = Server.SavedState.ServerErrors.load () in
                  ServerState.create
                    ~socket_path:(socket_path_of log_directory)
                    ~critical_files
                    ~configuration
                    ~build_system
                    ~type_environment
                    ~error_table
                    ()
                in
                Log.info "Processing recent updates not included in saved state...";
                Statistics.event ~name:"saved state success" ();
                Request.IncrementalUpdate (List.map changed_files ~f:Path.absolute)
                |> RequestHandler.process_request ~state:loaded_state
                >>= fun (new_state, _) -> Lwt.return new_state))
  in
  let open Lwt.Infix in
  let get_initial_state ~build_system_initializer () =
    match saved_state_action with
    | Some (SavedStateAction.LoadFromFile { shared_memory_path; changed_files_path }) ->
        with_performance_logging
          ~normals:["initialization method", "saved state"]
          ~name:"initialization"
          (fun _ ->
            fetch_saved_state_from_files ~shared_memory_path ~changed_files_path ()
            >>= load_from_saved_state ~build_system_initializer)
    | Some (SavedStateAction.LoadFromProject { project_name; project_metadata }) ->
        let normals =
          let normals =
            ["initialization method", "saved state"; "saved_state_project", project_name]
          in
          match project_metadata with
          | None -> normals
          | Some metadata -> ("saved_state_metadata", metadata) :: normals
        in
        with_performance_logging ~normals ~name:"initialization" (fun _ ->
            fetch_saved_state_from_project ~project_name ~project_metadata ()
            >>= load_from_saved_state ~build_system_initializer)
    | _ ->
        with_performance_logging
          ~normals:["initialization method", "cold start"]
          ~name:"initialization"
          (fun _ -> build_and_start_from_scratch ~build_system_initializer ())
  in
  let store_initial_state
      { ServerState.configuration; type_environment; error_table; build_system; _ }
    =
    match saved_state_action with
    | Some (SavedStateAction.SaveToFile { shared_memory_path }) ->
        Memory.SharedMemory.collect `aggressive;
        Analysis.TypeEnvironment.module_tracker type_environment
        |> Analysis.ModuleTracker.SharedMemory.store;
        Analysis.TypeEnvironment.ast_environment type_environment |> Analysis.AstEnvironment.store;
        Server.SavedState.StoredConfiguration.store configuration;
        Server.SavedState.ServerErrors.store error_table;
        BuildSystem.store build_system;
        Analysis.SharedMemoryKeys.DependencyKey.Registry.store ();
        Memory.save_shared_memory ~path:(Path.absolute shared_memory_path) ~configuration;
        Log.info "Initial server state written to %a" Path.pp shared_memory_path
    | _ -> ()
  in
  let build_system_initializer =
    let from_source_paths = function
      | Configuration.SourcePaths.Simple _ -> BuildSystem.Initializer.null
      | Configuration.SourcePaths.Buck buck_options ->
          let raw = Buck.Raw.create () in
          BuildSystem.Initializer.buck ~raw buck_options
    in
    (* If not specified, auto-determine which build system to use based on server configuration *)
    Option.value build_system_initializer ~default:(from_source_paths source_paths)
  in
  get_initial_state ~build_system_initializer ()
  >>= fun state ->
  Log.info "Server state initialized.";
  if configuration.debug then
    Memory.report_statistics ();
  store_initial_state state;
  Lwt.return (ExclusiveLock.create state)


let get_watchman_subscriber ?watchman ~watchman_root ~critical_files ~extensions ~source_paths () =
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
        {
          Watchman.Subscriber.Setting.raw;
          root;
          filter =
            Watchman.Filter.from_server_configurations ~critical_files ~extensions ~source_paths ();
        }
      in
      Watchman.Subscriber.subscribe subscriber_setting >>= Lwt.return_some


let on_watchman_update ~server_state paths =
  let open Lwt.Infix in
  let update_request = Request.IncrementalUpdate (List.map paths ~f:Path.absolute) in
  ExclusiveLock.write server_state ~f:(fun state ->
      handle_request ~state update_request
      >>= fun (new_state, _ok_response) ->
      (* File watcher does not care about the content of the the response. *)
      Lwt.return (new_state, ()))


let with_server
    ?watchman
    ?build_system_initializer
    ~configuration:({ Configuration.Analysis.log_directory; extensions; _ } as configuration)
    ~f
    ({ StartOptions.source_paths; watchman_root; critical_files; _ } as start_options)
  =
  let open Lwt in
  let socket_path = socket_path_of log_directory in
  (* Watchman connection needs to be up before server can start -- otherwise we risk missing
     filesystem updates during server establishment. *)
  get_watchman_subscriber ?watchman ~watchman_root ~critical_files ~extensions ~source_paths ()
  >>= fun watchman_subscriber ->
  LwtSocketServer.PreparedSocket.create_from_path socket_path
  >>= fun prepared_socket ->
  (* We do not want the expensive server initialization to happen before we start to accept client
     requests. *)
  initialize_server_state
    ?watchman_subscriber
    ?build_system_initializer
    ~configuration
    start_options
  >>= fun server_state ->
  LwtSocketServer.establish prepared_socket ~f:(handle_connection ~server_state)
  >>= fun server ->
  let server_waiter () = f (socket_path, server_state) in
  let server_destructor () =
    Log.info "Server is going down. Cleaning up...";
    let build_system_cleanup () =
      (* We can't use `ExclusiveLock.read` here because when we reach this point, some other thread
         might still hold the lock on the server state (e.g. this can happen when the server
         destructor is initiated from a request handler that received a `stop` request). In those
         cases, trying to grab the lock on the server state would cause a deadlock. *)
      let { ServerState.build_system; _ } = ExclusiveLock.unsafe_read server_state in
      BuildSystem.cleanup build_system
    in
    build_system_cleanup () >>= fun () -> LwtSocketServer.shutdown server
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


(* Invoke `on_caught` when given unix signals are received. *)
let wait_on_signals ~on_caught signals =
  let open Lwt in
  let waiter, resolver = wait () in
  List.iter signals ~f:(fun signal ->
      let signal = Signal.to_system_int signal in
      Lwt_unix.on_signal signal (wakeup resolver) |> ignore);
  waiter
  >>= fun signal ->
  let signal = Signal.of_system_int signal in
  Log.info "Server interrupted with signal `%s`" (Signal.to_string signal);
  on_caught signal


let start_server
    ?watchman
    ?build_system_initializer
    ?(on_server_socket_ready = fun _ -> Lwt.return_unit)
    ~on_started
    ~on_exception
    ~configuration
    start_options
  =
  let open Lwt in
  let f (socket_path, server_state) =
    on_server_socket_ready socket_path >>= fun _ -> on_started server_state
  in
  catch
    (fun () -> with_server ?watchman ?build_system_initializer ~configuration start_options ~f)
    on_exception


let start_server_and_wait ?event_channel ~configuration start_options =
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
    start_options
    ~configuration
    ~on_server_socket_ready:(fun socket_path ->
      (* An empty message signals that server socket has been created. *)
      write_event (ServerEvent.SocketCreated socket_path))
    ~on_started:(fun state ->
      write_event ServerEvent.ServerInitialized
      >>= fun () ->
      choose
        [
          (* We rely on SIGINT for normal server shutdown. *)
          wait_on_signals [Signal.int] ~on_caught:(fun _ -> return ExitStatus.Ok);
          (* Getting these signals usually indicates something serious went wrong. *)
          wait_on_signals
            [Signal.abrt; Signal.term; Signal.pipe; Signal.quit; Signal.segv]
            ~on_caught:(fun signal ->
              let { ServerState.start_time; _ } =
                (* The use of `unsafe_read` is justified because (1) we really do not want to block
                   here, and (2) `start_time` is immutable anyway, which means no race condition can
                   occur in the first place. *)
                ExclusiveLock.unsafe_read state
              in
              Stop.log_stopped_server ~reason:(Signal.to_string signal) ~start_time ();
              return ExitStatus.Error);
        ])
    ~on_exception:(fun exn ->
      let kind, message =
        match exn with
        | Buck.Raw.BuckError { arguments; description; exit_code } ->
            (* Buck exit code >=10 are considered internal:
               https://buck.build/command/exit_codes.html *)
            let kind =
              match exit_code with
              | Some exit_code when exit_code < 10 -> ServerEvent.ErrorKind.BuckUser
              | _ -> ServerEvent.ErrorKind.BuckInternal
            in
            ( kind,
              Format.sprintf
                "Cannot build the project: %s. To reproduce this error, run `%s`."
                description
                (Buck.Raw.ArgumentList.to_buck_command arguments) )
        | Buck.Builder.JsonError message ->
            ( ServerEvent.ErrorKind.Pyre,
              Format.sprintf
                "Cannot build the project because Buck returns malformed JSON: %s"
                message )
        | Buck.Builder.LinkTreeConstructionError message ->
            ( ServerEvent.ErrorKind.Pyre,
              Format.sprintf
                "Cannot build the project because Pyre encounters a fatal error while constructing \
                 a link tree: %s"
                message )
        | Watchman.ConnectionError message ->
            ServerEvent.ErrorKind.Watchman, Format.sprintf "Watchman connection error: %s" message
        | Watchman.SubscriptionError message ->
            ServerEvent.ErrorKind.Watchman, Format.sprintf "Watchman subscription error: %s" message
        | Watchman.QueryError message ->
            ServerEvent.ErrorKind.Watchman, Format.sprintf "Watchman query error: %s" message
        | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
            ( ServerEvent.ErrorKind.Pyre,
              "A Pyre server is already running for the current project. Use `pyre stop` to stop \
               it before starting another one." )
        | _ -> ServerEvent.ErrorKind.Unknown, Exn.to_string exn
      in
      Log.info "%s" message;
      write_event (ServerEvent.Exception (message, kind)) >>= fun () -> return ExitStatus.Error)
