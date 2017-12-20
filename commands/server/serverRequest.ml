(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

open State
open Configuration
open ServerConfiguration
open Protocol
open Protocol.Request

open Pyre

exception InvalidRequest

let rec process_request
    new_socket
    state
    ({ configuration = { project_root; _ } as configuration; _ } as server_configuration)
    request =
  let timer = Timer.start () in
  let build_file_to_error_map ?(checked_files = None) error_list =
    let initial_files = Option.value ~default:(Hashtbl.keys state.errors) checked_files in
    let error_file { Error.location = { Ast.Location.path; _ }; _ } =
      File.Handle.create path
    in
    List.fold
      ~init:File.Handle.Map.empty
      ~f:(fun map key -> Map.add map ~key ~data:[])
      initial_files
    |> (fun map ->
        List.fold
          ~init:map
          ~f:(fun map error -> Map.add_multi map ~key:(error_file error) ~data:error)
          error_list)
    |> Map.to_alist
  in
  let display_cached_type_errors state files =
    let errors =
      match files with
      | [] ->
          Hashtbl.data state.errors
          |> List.concat
      | _ ->
          List.filter_map ~f:(File.handle ~root:project_root) files
          |> List.filter_map ~f:(Hashtbl.find state.errors)
          |> List.concat
    in
    state, Some (TypeCheckResponse (build_file_to_error_map errors))
  in
  let handle_type_check state = function
    | { Protocol.files = []; _ } ->
        Log.log ~section:`Server "Handling type check request";
        let state =
          let deferred_requests = state.deferred_requests in
          let state = { state with deferred_requests = [] } in
          let update_state state request =
            let state, _ = process_request new_socket state server_configuration request in
            state
          in
          List.fold ~init:state ~f:update_state deferred_requests
        in
        let errors =
          Hashtbl.data state.errors
          |> List.concat
        in
        state, Some (TypeCheckResponse (build_file_to_error_map errors))

    | { Protocol.files; check_dependents } ->
        let deferred_requests =
          if check_dependents then
            let files =
              let dependents =
                let paths =
                  List.filter_map
                    ~f:(fun file ->
                        Path.get_relative_to_root ~root:project_root ~path:(File.path file))
                    files
                in
                Log.log
                  ~section:`Server
                  "Handling type check request for files %a"
                  Sexp.pp (sexp_of_list sexp_of_string paths);
                let (module Reader: Environment.Reader) = state.environment in
                Dependencies.of_list ~get_dependencies:(Reader.dependencies) ~paths
                |> Set.to_list
              in
              Log.log
                ~section:`Server
                "Inferred affected files: %a"
                Sexp.pp
                (sexp_of_list sexp_of_string dependents);
              List.map
                ~f:(fun path ->
                    Path.create_relative ~root:configuration.project_root ~relative:path
                    |> File.create)
                dependents
            in
            if List.is_empty files then
              state.deferred_requests
            else
              (TypeCheckRequest { Protocol.files; check_dependents = false })
              :: state.deferred_requests
          else
            state.deferred_requests
        in
        let service = Service.with_parallel state.service ~is_parallel:(List.length files > 5) in
        let repopulate_handles, new_source_handles =
          if check_dependents then
            List.filter_map ~f:(File.handle ~root:project_root) files,
            ParseService.parse_sources_list
              service
              files
              ~root:project_root
            |> fst
          else
            [], List.filter_map ~f:(File.handle ~root:project_root) files
        in
        let new_errors, lookups =
          let errors, lookups =
            TypeCheckService.analyze_sources
              ~repopulate_handles
              service
              configuration
              state.environment
              new_source_handles
          in
          errors, lookups
        in
        Map.iteri
          ~f:(fun ~key:name ~data:map -> Hashtbl.set ~key:name ~data:map state.State.lookups)
          lookups;
        (* Kill all previous errors for new files we just checked *)
        List.iter ~f:(Hashtbl.remove state.errors) new_source_handles;
        (* Associate the new errors with new files *)
        List.iter
          new_errors
          ~f:(fun error ->
              let { Ast.Location.path; _ } = Error.location error in
              Hashtbl.add_multi
                state.errors
                ~key:(File.Handle.create path)
                ~data:error);
        let new_files = File.Handle.Set.of_list new_source_handles in
        let checked_files =
          List.filter_map
            ~f:(fun file -> File.path file |> Path.relative >>| File.Handle.create)
            files
          |> fun handles -> Some handles
        in
        { state with handles = Set.union state.handles new_files; deferred_requests },
        Some (TypeCheckResponse (build_file_to_error_map ~checked_files new_errors))
  in
  let handle_type_query state request =
    let (module Reader: Environment.Reader) = state.environment in
    let order = (module Reader.TypeOrderReader : TypeOrder.Reader) in
    match request with
    | LessOrEqual (left, right) ->
        let response =
          TypeOrder.less_or_equal order ~left ~right
          |> Bool.to_string
        in
        state,
        (Some (TypeQueryResponse response))
    | Join (left, right) ->
        let response =
          TypeOrder.join order left right
          |> Type.show
        in
        state,
        (Some (TypeQueryResponse response))
    | Meet (left, right) ->
        let response =
          TypeOrder.meet order left right
          |> Type.show
        in
        state,
        (Some (TypeQueryResponse response))
  in
  let handle_client_shutdown_request id =
    let response = LanguageServerProtocol.ShutdownResponse.default id in
    state,
    Some (LanguageServerProtocolResponse (
        Yojson.Safe.to_string (LanguageServerProtocol.ShutdownResponse.to_yojson response)))
  in
  let result =
    match request with
    | TypeCheckRequest request -> handle_type_check state request
    | TypeQueryRequest request -> handle_type_query state request
    | DisplayTypeErrors request -> display_cached_type_errors state request
    | StopRequest ->
        Log.log ~section:`Server "Stopping the server";
        Socket.write new_socket Protocol.StopResponse;
        Mutex.critical_section
          state.lock
          ~f:(fun () ->
              ServerOperations.stop_server
                server_configuration
                !(state.connections).socket);
        state, None
    | LanguageServerProtocolRequest request ->
        Log.log ~section:`Server "Server received LSP request %s" request;
        LanguageServerProtocolRequestParser.parse
          ~root:configuration.project_root
          (Yojson.Safe.from_string request)
        >>= (function
            | TypeCheckRequest files -> Some (handle_type_check state files)
            | ClientShutdownRequest id -> Some (handle_client_shutdown_request id)
            | ClientExitRequest Persistent ->
                Log.log ~section:`Server "Stopping persistent client";
                Some (state, Some (ClientExitResponse Persistent))
            | GetDefinitionRequest { DefinitionRequest.id; path; position } ->
                let definition =
                  Hashtbl.find state.lookups path
                  >>= fun lookup -> Lookup.get_definition lookup position
                in
                Some
                  (state,
                   Some
                     (Protocol.LanguageServerProtocolResponse
                        (LanguageServerProtocol.TextDocumentDefinitionResponse.create
                           ~root:project_root
                           ~id
                           ~location:definition
                         |> LanguageServerProtocol.TextDocumentDefinitionResponse.to_yojson
                         |> Yojson.Safe.to_string)))
            | RageRequest id ->
                let items = Rage.get_logs configuration in
                Some
                  (state,
                   Some (Protocol.LanguageServerProtocolResponse
                           (LanguageServerProtocol.RageResponse.create ~items ~id
                            |> LanguageServerProtocol.RageResponse.to_yojson
                            |> Yojson.Safe.to_string)))
            | _ -> None)
        |> Option.value ~default:(state, None)

    | ClientShutdownRequest id -> handle_client_shutdown_request id

    | ClientExitRequest client ->
        Log.log ~section:`Server "Stopping %s client" (Protocol.show_client client);
        state, Some (ClientExitResponse client)

    | RageRequest id ->
        let items = Rage.get_logs configuration in
        state,
        Some
          (Protocol.LanguageServerProtocolResponse
             (LanguageServerProtocol.RageResponse.create ~items ~id
              |> LanguageServerProtocol.RageResponse.to_yojson
              |> Yojson.Safe.to_string))
    | ReinitializeStateRequest ->
        let state =
          ServerOperations.initialize
            ~old_state:state
            state.lock
            state.connections
            server_configuration
        in
        handle_type_check state { Protocol.files = []; check_dependents = false }

    | GetDefinitionRequest { DefinitionRequest.path; position; _ } ->
        state, Some (Protocol.GetDefinitionResponse (
            Hashtbl.find state.lookups path
            >>= fun lookup -> Lookup.get_definition lookup position))

    | ClientConnectionRequest _ ->
        raise InvalidRequest
  in
  Log.performance
    ~flush:false
    ~name:"Server request"
    ~timer
    ~root:(Path.last configuration.project_root)
    ~labels:["request_kind", Protocol.Request.name request];
  result
