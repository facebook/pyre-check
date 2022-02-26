(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

exception ConnectionError of string

exception SubscriptionError of string

exception QueryError of string

module Raw = struct
  module Response = struct
    type t =
      | Ok of Yojson.Safe.t
      | EndOfStream
      | Error of string
  end

  module Connection = struct
    type t = {
      send: Yojson.Safe.t -> unit Lwt.t;
      receive: unit -> Response.t Lwt.t;
      shutdown: unit -> unit Lwt.t;
    }

    let send { send; _ } = send

    let receive { receive; _ } = receive

    let shutdown { shutdown; _ } = shutdown
  end

  type t = { open_connection: unit -> Connection.t Lwt.t }

  let open_connection { open_connection } = open_connection ()

  let shutdown_connection connection = Connection.shutdown connection ()

  let with_connection ~f { open_connection } =
    let open Lwt.Infix in
    open_connection ()
    >>= fun connection -> Lwt.finalize (fun () -> f connection) (Connection.shutdown connection)


  let create_for_testing ~send ~receive () =
    let receive () =
      let open Lwt.Infix in
      receive ()
      >>= function
      | Some json -> Lwt.return (Response.Ok json)
      | None -> Lwt.return Response.EndOfStream
    in
    let shutdown () = Lwt.return_unit in
    let mock_connection = { Connection.send; receive; shutdown } in
    { open_connection = (fun () -> Lwt.return mock_connection) }


  let get_watchman_socket_name () =
    let open Lwt.Infix in
    LwtSubprocess.run "watchman" ~arguments:["--no-pretty"; "get-sockname"]
    >>= fun { LwtSubprocess.Completed.status; stdout; stderr } ->
    match status with
    | Caml.Unix.WEXITED 0 ->
        let socket_name =
          try
            Yojson.Safe.from_string stdout
            |> Yojson.Safe.Util.member "sockname"
            |> Yojson.Safe.Util.to_string
          with
          | Yojson.Json_error message ->
              let message =
                Format.sprintf "Cannot parse JSON result from watchman getsockname: %s" message
              in
              raise (ConnectionError message)
        in
        Lwt.return socket_name
    | WEXITED 127 ->
        let message =
          Format.sprintf
            "Cannot find watchman exectuable under PATH: %s"
            (Option.value (Sys_utils.getenv_path ()) ~default:"(not set)")
        in
        raise (ConnectionError message)
    | WEXITED code ->
        let message = Format.sprintf "Watchman exited code %d, stderr = %S" code stderr in
        raise (ConnectionError message)
    | WSIGNALED signal ->
        let message =
          Format.sprintf "watchman signaled with %s signal" (PrintSignal.string_of_signal signal)
        in
        raise (ConnectionError message)
    | WSTOPPED signal ->
        let message =
          Format.sprintf "watchman stopped with %s signal" (PrintSignal.string_of_signal signal)
        in
        raise (ConnectionError message)


  let create_exn () =
    let open Lwt.Infix in
    Log.info "Initializing file watching service...";
    get_watchman_socket_name ()
    >>= fun socket_name ->
    let open_connection () =
      Log.info "Connecting to watchman...";
      Lwt_io.open_connection (Lwt_unix.ADDR_UNIX socket_name)
      >>= fun (input_channel, output_channel) ->
      Log.info "Established watchman connection.";
      let send json = Yojson.Safe.to_string json |> Lwt_io.write_line output_channel in
      let receive () =
        Lwt_io.read_line_opt input_channel
        >>= function
        | None -> Lwt.return Response.EndOfStream
        | Some line -> (
            try
              let json = Yojson.Safe.from_string line in
              Lwt.return (Response.Ok json)
            with
            | Yojson.Json_error message ->
                let message =
                  Format.sprintf "Cannot parse JSON from watchman response: %s" message
                in
                Lwt.return (Response.Error message))
      in
      let shutdown () =
        Log.info "Shutting down watchman connection...";
        Lwt_io.close input_channel >>= fun () -> Lwt_io.close output_channel
      in
      Lwt.return { Connection.send; receive; shutdown }
    in
    Lwt.return { open_connection }


  let create () =
    let open Lwt.Infix in
    Lwt.catch
      (fun () -> create_exn () >>= fun raw -> Lwt.return (Result.Ok raw))
      (fun exn ->
        let message =
          Format.sprintf "Cannot initialize watchman due to exception: %s" (Exn.to_string exn)
        in
        Lwt.return (Result.Error message))
end

module Filter = struct
  type t = {
    base_names: string list;
    whole_names: string list;
    suffixes: string list;
  }
  [@@deriving sexp, compare, hash]

  let from_server_configurations ~critical_files ~extensions ~source_paths () =
    let base_name_of = function
      | CriticalFile.BaseName name -> Some name
      | CriticalFile.FullPath path -> Some (PyrePath.last path)
      | CriticalFile.Extension _ -> None
    in
    let base_names =
      List.filter_map critical_files ~f:base_name_of
      |> String.Set.of_list
      |> fun set ->
      Set.add set ".pyre_configuration"
      |> fun set ->
      Set.add set ".pyre_configuration.local"
      |> fun set ->
      (match source_paths with
      | Configuration.SourcePaths.Buck _ ->
          let set = Set.add set "TARGETS" in
          Set.add set "BUCK"
      | Configuration.SourcePaths.Simple _
      | Configuration.SourcePaths.WithUnwatchedDependency _ ->
          set)
      |> Set.to_list
    in
    let whole_names =
      match source_paths with
      | Configuration.SourcePaths.WithUnwatchedDependency
          {
            unwatched_dependency =
              {
                Configuration.UnwatchedDependency.change_indicator =
                  { Configuration.ChangeIndicator.relative; _ };
                _;
              };
            _;
          } ->
          (* Change indicator file needs to be watched, since we rely on it to detect changes in
             unwatched dependencies. *)
          [relative]
      | _ -> []
    in
    let extension_of = function
      | CriticalFile.BaseName _
      | CriticalFile.FullPath _ ->
          (* We do not need to track these files by extensions since they are already tracked by
             base_names. *)
          None
      | CriticalFile.Extension suffix -> Some suffix
    in
    let suffixes =
      List.map ~f:Configuration.Extension.suffix extensions
      |> List.map ~f:(String.lstrip ~drop:(Char.equal '.'))
      |> String.Set.of_list
      |> fun set ->
      List.filter_map critical_files ~f:extension_of
      |> List.fold ~init:set ~f:String.Set.add
      |> fun set -> Set.add set "py" |> fun set -> Set.add set "pyi" |> Set.to_list
    in
    { base_names; whole_names; suffixes }


  let watchman_expression_of { base_names; whole_names; suffixes } =
    let base_names =
      List.map base_names ~f:(fun base_name ->
          `List [`String "match"; `String base_name; `String "basename"])
    in
    let whole_names =
      List.map whole_names ~f:(fun base_name ->
          `List [`String "match"; `String base_name; `String "wholename"])
    in
    let suffixes = List.map suffixes ~f:(fun suffix -> `List [`String "suffix"; `String suffix]) in
    `List
      [
        `String "allof";
        `List [`String "type"; `String "f"];
        `List (`String "anyof" :: List.concat [suffixes; base_names; whole_names]);
      ]
end

module Subscriber = struct
  module Setting = struct
    type t = {
      raw: Raw.t;
      root: PyrePath.t;
      filter: Filter.t;
    }
  end

  type t = {
    setting: Setting.t;
    connection: Raw.Connection.t;
    initial_clock: string;
  }

  let send_request ~connection request =
    let open Lwt.Infix in
    Raw.Connection.send connection request >>= fun () -> Raw.Connection.receive connection ()


  let create_subscribe_request ~root ~filter () =
    `List
      [
        `String "subscribe";
        `String (PyrePath.absolute root);
        `String "pyre_file_change_subscription";
        `Assoc
          [
            "empty_on_fresh_instance", `Bool true;
            "expression", Filter.watchman_expression_of filter;
            "fields", `List [`String "name"];
          ];
      ]


  let create_watch_project_rquest ~root () =
    `List [`String "watch-project"; `String (PyrePath.absolute root)]


  let handle_subscribe_response = function
    | Raw.Response.Error message -> raise (SubscriptionError message)
    | Raw.Response.EndOfStream ->
        raise (SubscriptionError "Cannot get the initial response from `watchman subscribe`")
    | Raw.Response.Ok initial_response -> (
        match Yojson.Safe.Util.member "error" initial_response with
        | `Null -> (
            match Yojson.Safe.Util.member "clock" initial_response with
            | `String initial_clock -> Lwt.return initial_clock
            | _ as error ->
                let message =
                  Format.sprintf
                    "Cannot determinte the initial clock from response %s"
                    (Yojson.Safe.to_string error)
                in
                raise (SubscriptionError message))
        | _ as error ->
            let message =
              Format.sprintf
                "Subscription rejected by watchman. Response: %s"
                (Yojson.Safe.to_string error)
            in
            raise (SubscriptionError message))


  let handle_watch_project_response = function
    | Raw.Response.Error message -> raise (SubscriptionError message)
    | Raw.Response.EndOfStream ->
        raise (SubscriptionError "Cannot get the initial response from `watchman watch-project`")
    | Raw.Response.Ok initial_response -> (
        match Yojson.Safe.Util.member "error" initial_response with
        | `Null -> Lwt.return_unit
        | _ as error ->
            let message =
              Format.sprintf
                "Watch-project request rejected by watchman. Response: %s"
                (Yojson.Safe.to_string error)
            in
            raise (SubscriptionError message))


  let subscribe ({ Setting.raw; root; filter } as setting) =
    let open Lwt.Infix in
    Raw.open_connection raw
    >>= fun connection ->
    let do_subscribe () =
      Log.info "Request watchman subscription at %a" PyrePath.pp root;
      send_request ~connection (create_watch_project_rquest ~root ())
      >>= handle_watch_project_response
      >>= fun () ->
      send_request ~connection (create_subscribe_request ~root ~filter ())
      >>= handle_subscribe_response
      >>= fun initial_clock -> Lwt.return { setting; connection; initial_clock }
    in
    Lwt.catch do_subscribe (fun exn ->
        (* Make sure the connection is properly shut down when an exception is raised. *)
        Raw.shutdown_connection connection >>= fun () -> raise exn)


  let setting_of { setting; _ } = setting

  let listen ~f { connection; initial_clock; setting = _ } =
    let open Lwt.Infix in
    let rec do_listen () =
      Raw.Connection.receive connection ()
      >>= function
      | Raw.Response.Error message -> raise (SubscriptionError message)
      | Raw.Response.EndOfStream -> Lwt.return_unit
      | Raw.Response.Ok response -> (
          match
            ( Yojson.Safe.Util.member "is_fresh_instance" response,
              Yojson.Safe.Util.member "clock" response )
          with
          | `Bool true, `String update_clock when String.equal initial_clock update_clock ->
              (* This is the initial `is_fresh_instance` message, which can be safely ignored. *)
              do_listen ()
          | `Bool true, _ ->
              (* This is not the initial `is_fresh_instance` message, which usually indicates that
                 our current view of the filesystem may not be accurate anymore. *)
              raise (SubscriptionError "Received `is_fresh_instance` message from watchman")
          | _, _ -> (
              match Yojson.Safe.Util.member "canceled" response with
              | `Bool true ->
                  (* This means the susbscription is cancelled by watchman. We should not keep
                     going. *)
                  raise (SubscriptionError "Subscription is cancelled by watchman")
              | _ -> (
                  let () =
                    match Yojson.Safe.Util.member "warning" response with
                    | `String message -> Log.warning "Received watchman warning: %s" message
                    | _ -> ()
                  in
                  match Yojson.Safe.Util.member "files" response with
                  | `Null ->
                      (* This could be a "state-enter"/"state-leave" message which can also be
                         safely ignored. *)
                      do_listen ()
                  | files_json -> (
                      try
                        let root =
                          Yojson.Safe.Util.(member "root" response |> to_string)
                          |> PyrePath.create_absolute
                        in
                        let changed_paths =
                          Yojson.Safe.Util.(convert_each to_string files_json)
                          |> List.map ~f:(fun relative -> PyrePath.create_relative ~root ~relative)
                        in
                        f changed_paths >>= fun () -> do_listen ()
                      with
                      | Yojson.Json_error message ->
                          let message =
                            Format.sprintf
                              "Cannot parse JSON result from watchman subscription: %s"
                              message
                          in
                          raise (SubscriptionError message)
                      | Yojson.Safe.Util.Type_error (message, json)
                      | Yojson.Safe.Util.Undefined (message, json) ->
                          let message =
                            Format.sprintf
                              "Unexpected JSON format for watchman subscription: %s. %s."
                              (Yojson.Safe.to_string json)
                              message
                          in
                          raise (SubscriptionError message)))))
    in
    Lwt.finalize do_listen (fun () -> Raw.Connection.shutdown connection ())


  let with_subscription ~f config =
    let open Lwt.Infix in
    subscribe config >>= fun subscriber -> listen ~f subscriber
end

module SinceQuery = struct
  module Since = struct
    module SavedState = struct
      type t = {
        storage: string;
        project_name: string;
        project_metadata: string option;
      }
      [@@deriving sexp, compare, hash]

      let watchman_expression_of { storage; project_name; project_metadata } =
        let storage_entry = "storage", `String storage in
        let configuration_entry =
          ( "config",
            let project_name_entry = "project", `String project_name in
            match project_metadata with
            | None -> `Assoc [project_name_entry]
            | Some project_metadata ->
                let project_metadata_entry = "project-metadata", `String project_metadata in
                `Assoc [project_name_entry; project_metadata_entry] )
        in
        `Assoc [storage_entry; configuration_entry]
    end

    type t =
      | Clock of string
      | SourceControlAware of {
          mergebase_with: string;
          saved_state: SavedState.t option;
        }
    [@@deriving sexp, compare, hash]

    let watchman_expression_of = function
      | Clock clock -> `String clock
      | SourceControlAware { mergebase_with; saved_state } ->
          `Assoc
            [
              ( "scm",
                let mergebase_with_entry = "mergebase-with", `String mergebase_with in
                match saved_state with
                | None -> `Assoc [mergebase_with_entry]
                | Some saved_state ->
                    let saved_state_entry =
                      "saved-state", SavedState.watchman_expression_of saved_state
                    in
                    `Assoc [mergebase_with_entry; saved_state_entry] );
            ]
  end

  module Response = struct
    module SavedState = struct
      type t = {
        bucket: string;
        path: string;
        commit_id: string option;
      }
      [@@deriving sexp, compare, hash]

      let of_watchman_response_exn response =
        let open Yojson.Safe.Util in
        let bucket = member "manifold-bucket" response |> to_string in
        let path = member "manifold-path" response |> to_string in
        let commit_id =
          match member "commit-id" response with
          | `String id -> Some id
          | _ -> None
        in
        { bucket; path; commit_id }
    end

    type t = {
      relative_paths: string list;
      saved_state: SavedState.t option;
    }
    [@@deriving sexp, compare, hash]

    let of_watchman_response_exn response =
      let open Yojson.Safe.Util in
      let relative_paths = member "files" response |> to_list |> filter_string in
      let saved_state =
        match member "saved-state-info" response with
        | `Null -> None
        | _ as response -> SavedState.of_watchman_response_exn response |> Option.some
      in
      { relative_paths; saved_state }


    let of_watchman_response response =
      try Some (of_watchman_response_exn response) with
      | _ -> None
  end

  type t = {
    root: PyrePath.t;
    filter: Filter.t;
    since: Since.t;
  }
  [@@deriving sexp, compare, hash]

  let watchman_request_of { root; filter; since } =
    `List
      [
        `String "query";
        `String (PyrePath.absolute root);
        `Assoc
          [
            "fields", `List [`String "name"];
            "expression", Filter.watchman_expression_of filter;
            "since", Since.watchman_expression_of since;
          ];
      ]


  let query_exn ~connection since_query =
    let open Lwt.Infix in
    let request = watchman_request_of since_query in
    Raw.Connection.send connection request
    >>= fun () ->
    Raw.Connection.receive connection ()
    >>= function
    | Raw.Response.Ok response -> Lwt.return (Response.of_watchman_response_exn response)
    | Error message -> raise (QueryError message)
    | EndOfStream ->
        let message = "Failed to receive any response from watchman server" in
        raise (QueryError message)


  let query ~connection since_query =
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        query_exn ~connection since_query >>= fun response -> Lwt.return (Result.Ok response))
      (fun exn ->
        let message = Format.sprintf "Watchman query failed. Exception: %s" (Exn.to_string exn) in
        Lwt.return (Result.Error message))
end
