(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

exception ServerStopped of Stop.Reason.t option

exception ServerInterrupted of Signal.t

let with_performance_logging ?(normals = []) ~name f =
  let open Lwt.Infix in
  let timer = Timer.start () in
  f ()
  >>= fun result ->
  let normals = ("binary_version", Version.version ()) :: normals in
  Statistics.performance ~name ~timer ~normals ();
  Lwt.return result


module ClientRequest = struct
  type t =
    | GetInfo
    | StopServer
    | Request of Request.t
    | Subscription of Subscription.Request.t
    | Error of string
  [@@deriving sexp, compare]

  let of_string input_string =
    try
      let json = Yojson.Safe.from_string input_string in
      match json with
      | `List [`String "GetInfo"] -> GetInfo
      | `List [`String "Stop"] -> StopServer
      | _ -> (
          match Subscription.Request.of_yojson json with
          | Result.Ok subscription -> Subscription subscription
          | Result.Error _ -> (
              match Request.of_yojson json with
              | Result.Ok request -> Request request
              | Result.Error _ -> Error "Malformed JSON request"))
    with
    | Yojson.Json_error message -> Error message
end

let handle_request ~properties ~state request =
  let open Lwt.Infix in
  let on_uncaught_server_exception exn =
    Log.info "Uncaught server exception: %s" (Exn.to_string exn);
    let () =
      let { ServerProperties.configuration; _ } = properties in
      StartupNotification.produce
        ~log_path:configuration.log_directory
        "Restarting Pyre server due to unexpected crash"
    in
    let reason = Stop.Reason.UncaughtException exn in
    Statistics.log_exception exn ~fatal:true ~origin:(Stop.Reason.origin_of_exception exn);
    let subscriptions =
      let { ServerState.subscriptions; _ } = state in
      ServerState.Subscriptions.all subscriptions
    in
    Subscription.batch_send
      subscriptions
      ~response:(lazy (Response.Error (Stop.Reason.message_of reason)))
    >>= fun () -> Stop.stop_waiting_server reason
  in
  Lwt.catch
    (fun () ->
      Log.log ~section:`Server "Processing request %a..." Sexp.pp (Request.sexp_of_t request);
      with_performance_logging
        ~normals:["request kind", Request.name_of request]
        ~name:"server request"
        (fun () -> RequestHandler.process_request ~properties ~state request))
    on_uncaught_server_exception
  >>= fun (new_state, response) ->
  Log.log ~section:`Server "Request `%a` processed" Sexp.pp (Request.sexp_of_t request);
  Lwt.return (new_state, response)


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

let handle_subscription_request
    ~server_properties
    ~server_state
    ~connection_state
    ~output_channel
    subscription_request
  =
  let open Lwt.Infix in
  ExclusiveLock.Lazy.write
    server_state
    ~f:(fun ({ ServerState.subscriptions; _ } as old_server_state) ->
      let subscription = Subscription.create ~subscription_request ~output_channel () in
      let () = ServerState.Subscriptions.add subscriptions ~subscription in
      let new_connection_state =
        ConnectionState.add_subscription ~name:(Subscription.name_of subscription) connection_state
      in
      match subscription_request with
      | Subscription.Request.SubscribeToTypeErrors _ ->
          (* We send back the initial set of type errors when a subscription first gets
             established. *)
          handle_request
            ~properties:server_properties
            ~state:old_server_state
            (Request.DisplayTypeError [])
          >>= fun (new_server_state, response) ->
          Lwt.return (new_server_state, (new_connection_state, response))
      | Subscription.Request.SubscribeToStateChanges _ ->
          Lwt.return (old_server_state, (new_connection_state, Response.Ok)))


let handle_connection
    ~server_properties
    ~server_state
    _client_address
    (input_channel, output_channel)
  =
  let open Lwt.Infix in
  Log.log ~section:`Server "Connection established";
  (* Raw request messages are processed line-by-line. *)
  let rec handle_line connection_state =
    Lwt_io.read_line_opt input_channel
    >>= function
    | None ->
        Log.log ~section:`Server "Connection closed";
        ExclusiveLock.Lazy.write server_state ~f:(fun server_state ->
            ConnectionState.cleanup ~server_state connection_state;
            Lwt.return (server_state, ()))
    | Some message ->
        let result =
          match ClientRequest.of_string message with
          | ClientRequest.Error message -> Lwt.return (connection_state, Response.Error message)
          | ClientRequest.GetInfo ->
              let response = RequestHandler.create_info_response server_properties in
              Lwt.return (connection_state, response)
          | ClientRequest.StopServer ->
              let reason = Stop.Reason.ExplicitRequest in
              let subscriptions =
                (* HACK(grievejia): Bypass the lock here because we do not want to block stop
                   request on other kinds of requests. It's ok in this case because we are only
                   interested in retrieving the subscription list for notification purpose and we
                   don't care about potential concurrent modifications. We might also consider using
                   separate locks for subscriptions and the rest of the state to avoid this unsafe
                   read in the future. *)
                match ExclusiveLock.Lazy.unsafe_read server_state with
                | None -> []
                | Some { ServerState.subscriptions; _ } ->
                    ServerState.Subscriptions.all subscriptions
              in
              Subscription.batch_send
                subscriptions
                ~response:(lazy (Response.Error (Stop.Reason.message_of reason)))
              >>= fun () -> Stop.stop_waiting_server reason
          | ClientRequest.Request request ->
              ExclusiveLock.Lazy.write server_state ~f:(fun state ->
                  handle_request ~properties:server_properties ~state request
                  >>= fun (new_state, response) ->
                  Lwt.return (new_state, (connection_state, response)))
          | ClientRequest.Subscription subscription_request ->
              handle_subscription_request
                ~server_properties
                ~server_state
                ~connection_state
                ~output_channel
                subscription_request
        in
        result
        >>= fun (new_connection_state, response) ->
        let on_io_exception exn =
          Log.log
            ~section:`Server
            "Exception occurred while sending responses: %s"
            (Exn.to_string exn);
          Lwt.return_unit
        in
        let raw_response = Yojson.Safe.to_string (Response.to_yojson response) in
        Lwt.catch (fun () -> Lwt_io.write_line output_channel raw_response) on_io_exception
        >>= fun () -> handle_line new_connection_state
  in
  ConnectionState.create () |> handle_line


let initialize_server_state
    ?watchman_subscriber
    ~build_system_initializer
    ~saved_state_action
    ~skip_initial_type_check
    ~environment_controls
    ({ ServerProperties.critical_files; _ } as server_properties)
  =
  (* This is needed to initialize shared memory. *)
  let ({ Configuration.Analysis.log_directory; _ } as configuration) =
    Analysis.EnvironmentControls.configuration environment_controls
  in
  let _ = Memory.get_heap_handle configuration in
  let start_from_scratch ~build_system () =
    Log.info "Initializing server state from scratch...";
    let overlaid_environment =
      Scheduler.with_scheduler
        ~configuration
        ~should_log_exception:(fun _ -> true)
        ~f:(fun scheduler ->
          let environment = Analysis.ErrorsEnvironment.create environment_controls in
          let () =
            if skip_initial_type_check then
              ()
            else
              Analysis.ErrorsEnvironment.project_qualifiers environment
              |> Analysis.ErrorsEnvironment.check_and_preprocess environment ~scheduler
          in
          Analysis.OverlaidEnvironment.create environment)
    in
    ServerState.create ~build_system ~overlaid_environment ()
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
        >>| List.map ~f:PyrePath.create_absolute
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
                  PyrePath.create_relative ~root:log_directory ~relative:"classic/server.state"
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
        source_paths = left_source_paths;
        search_paths = left_search_paths;
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
        source_paths = right_source_paths;
        search_paths = right_search_paths;
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
    && list_length_equal left_source_paths right_source_paths
    && list_length_equal left_search_paths right_search_paths
    && list_length_equal left_taint_model_paths right_taint_model_paths
    && Bool.equal left_strict right_strict
    && list_length_equal left_excludes right_excludes
    && list_length_equal left_extensions right_extensions
  in
  let load_from_shared_memory path =
    try Result.Ok (Memory.load_shared_memory ~path:(PyrePath.absolute path) ~configuration) with
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
            match configuration_equal configuration (ServerState.load_stored_configuration ()) with
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
                let loaded_state = ServerState.load ~configuration ~build_system () in
                Log.info "Processing recent updates not included in saved state...";
                Statistics.event ~name:"saved state success" ();
                Request.IncrementalUpdate (List.map changed_files ~f:PyrePath.absolute)
                |> RequestHandler.process_request ~properties:server_properties ~state:loaded_state
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
  let store_initial_state state =
    match saved_state_action with
    | Some (SavedStateAction.SaveToFile { shared_memory_path }) ->
        ServerState.store ~path:shared_memory_path ~configuration state;
        Log.info "Initial server state written to %a" PyrePath.pp shared_memory_path
    | _ -> ()
  in
  get_initial_state ~build_system_initializer ()
  >>= fun state ->
  Log.info "Server state initialized.";
  if configuration.debug then
    Memory.report_statistics ();
  store_initial_state state;
  Lwt.return state


let get_watchman_subscriber
    ~critical_files
    ~extensions
    ~source_paths
    { StartOptions.Watchman.root; raw }
  =
  let subscriber_setting =
    {
      Watchman.Subscriber.Setting.raw;
      root;
      filter =
        Watchman.Filter.from_server_configurations ~critical_files ~extensions ~source_paths ();
    }
  in
  Watchman.Subscriber.subscribe subscriber_setting


let get_optional_watchman_subscriber ~critical_files ~extensions ~source_paths = function
  | None -> Lwt.return_none
  | Some watchman_option ->
      let open Lwt.Infix in
      get_watchman_subscriber ~critical_files ~extensions ~source_paths watchman_option
      >>= Lwt.return_some


let on_watchman_update ~server_properties ~server_state paths =
  let open Lwt.Infix in
  let update_request = Request.IncrementalUpdate (List.map paths ~f:PyrePath.absolute) in
  ExclusiveLock.Lazy.write server_state ~f:(fun state ->
      handle_request ~properties:server_properties ~state update_request
      >>= fun (new_state, _ok_response) ->
      (* File watcher does not care about the content of the the response. *)
      Lwt.return (new_state, ()))


let wait_for_signal ~on_caught signals =
  let open Lwt in
  let waiter, resolver = wait () in
  List.iter signals ~f:(fun signal ->
      let signal = Signal_unix.to_system_int signal in
      Lwt_unix.on_signal signal (wakeup resolver) |> ignore);
  waiter
  >>= fun signal ->
  let signal = Signal_unix.of_system_int signal in
  Log.info "Server interrupted with signal `%s`" (Signal.to_string signal);
  on_caught signal


let with_server
    ~when_started
    {
      StartOptions.environment_controls;
      socket_path;
      source_paths;
      watchman;
      build_system_initializer;
      critical_files;
      saved_state_action;
      skip_initial_type_check;
    }
  =
  let open Lwt in
  let ({ Configuration.Analysis.extensions; _ } as configuration) =
    Analysis.EnvironmentControls.configuration environment_controls
  in
  (* Watchman connection needs to be up before server can start -- otherwise we risk missing
     filesystem updates during server establishment. *)
  get_optional_watchman_subscriber ~critical_files ~extensions ~source_paths watchman
  >>= fun watchman_subscriber ->
  let server_properties = ServerProperties.create ~socket_path ~critical_files ~configuration () in
  let server_state =
    (* Use a lazy lock so we do not initialize the expensive server until we know server can be
       established (without conflicting with a pre-existing server). *)
    ExclusiveLock.Lazy.create (fun () ->
        initialize_server_state
          ?watchman_subscriber
          ~build_system_initializer
          ~saved_state_action
          ~skip_initial_type_check
          ~environment_controls
          server_properties)
  in
  let after_server_starts () =
    Log.info "Server has started listening on socket `%a`" PyrePath.pp socket_path;
    let waiters =
      let server_waiter () = when_started (socket_path, server_properties, server_state) in
      let signal_waiters =
        [
          (* We rely on SIGINT for normal server shutdown. *)
          wait_for_signal [Signal.int] ~on_caught:(fun _ ->
              let stop_reason = Stop.get_last_server_stop_reason () in
              Lwt.fail (ServerStopped stop_reason));
          (* Getting these signals usually indicates something serious went wrong. *)
          wait_for_signal
            [Signal.abrt; Signal.term; Signal.quit; Signal.segv]
            ~on_caught:(fun signal -> Lwt.fail (ServerInterrupted signal));
        ]
      in
      let watchman_waiter =
        Option.map watchman_subscriber ~f:(fun subscriber ->
            Watchman.Subscriber.listen
              ~f:(on_watchman_update ~server_properties ~server_state)
              subscriber
            >>= fun () ->
            Lwt.fail (Watchman.SubscriptionError "Lost subscription connection to watchman"))
      in
      List.concat_no_order [[server_waiter ()]; signal_waiters; Option.to_list watchman_waiter]
    in
    Lwt.choose waiters
  in
  let after_server_stops () =
    Log.info "Server is going down. Cleaning up...";
    BuildSystem.Initializer.cleanup build_system_initializer
  in
  LwtSocketServer.SocketAddress.create_from_path socket_path
  |> LwtSocketServer.with_server
       ~handle_connection:(handle_connection ~server_properties ~server_state)
       ~f:(fun () -> Lwt.finalize after_server_starts after_server_stops)


let start_server
    ?(on_server_socket_ready = fun _ -> Lwt.return_unit)
    ~on_started
    ~on_exception
    start_options
  =
  let open Lwt in
  let when_started (socket_path, server_properties, server_state) =
    on_server_socket_ready socket_path >>= fun _ -> on_started server_properties server_state
  in
  catch (fun () -> with_server ~when_started start_options) on_exception
