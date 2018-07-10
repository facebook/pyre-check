(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis

open ServerState
open Configuration
open ServerConfiguration
open ServerProtocol
open Request

open Pyre

module Rage = CommandRage
module Scheduler = Service.Scheduler


exception InvalidRequest


let rec process_request
    new_socket
    state
    ({ configuration = { source_root; _ } as configuration; _ } as server_configuration)
    request =
  let timer = Timer.start () in
  let build_file_to_error_map ?(checked_files = None) error_list =
    let initial_files = Option.value ~default:(Hashtbl.keys state.errors) checked_files in
    let error_file error = File.Handle.create (Error.path error) in
    List.fold
      ~init:File.Handle.Map.empty
      ~f:(fun map key -> Map.set map ~key ~data:[])
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
          List.filter_map ~f:(File.handle ~root:source_root) files
          |> List.filter_map ~f:(Hashtbl.find state.errors)
          |> List.concat
    in
    state, Some (TypeCheckResponse (build_file_to_error_map errors))
  in
  let flush_type_errors state =
    begin
      let state =
        let deferred_requests = Request.flatten state.deferred_requests in
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
    end
  in
  let handle_type_check state { TypeCheckRequest.update_environment_with; check} =
    if Scheduler.Memory.heap_use_ratio () > 0.5 then
      begin
        let previous_use_ratio = Scheduler.Memory.heap_use_ratio () in
        SharedMem.collect `aggressive;
        Log.log
          ~section:`Server
          "Garbage collected due to a previous heap use ratio of %f. New ratio is %f."
          previous_use_ratio
          (Scheduler.Memory.heap_use_ratio ())
      end;
    let (module Handler: Environment.Handler) = state.environment in
    let deferred_requests =
      if not (List.is_empty update_environment_with) then
        let files =
          let dependents =
            let paths =
              List.filter_map
                ~f:(fun file ->
                    Path.get_relative_to_root ~root:source_root ~path:(File.path file))
                update_environment_with
            in
            let check_paths =
              List.filter_map
                ~f:(fun file ->
                    Path.get_relative_to_root ~root:source_root ~path:(File.path file))
                check
            in
            Log.log
              ~section:`Server
              "Handling type check request for files %a"
              Sexp.pp (sexp_of_list sexp_of_string paths);
            Dependencies.of_list ~get_dependencies:(Handler.dependencies) ~paths
            |> (fun dependency_set -> Set.diff dependency_set (String.Set.of_list check_paths))
            |> Set.to_list
          in

          Log.log
            ~section:`Server
            "Inferred affected files: %a"
            Sexp.pp
            (sexp_of_list sexp_of_string dependents);
          List.map
            ~f:(fun path ->
                Path.create_relative ~root:configuration.source_root ~relative:path
                |> File.create)
            dependents
        in

        if List.is_empty files then
          state.deferred_requests
        else
          (TypeCheckRequest (TypeCheckRequest.create ~check:files ()))
          :: state.deferred_requests
      else
        state.deferred_requests
    in
    let scheduler =
      Scheduler.with_parallel
        state.scheduler
        ~is_parallel:(List.length check > 5)
    in
    let repopulate_handles =
      let is_stub file =
        file
        |> File.path
        |> Path.absolute
        |> String.is_suffix ~suffix:".pyi"
      in
      let () =
        (* Clean up all data related to updated files. *)
        let handles =
          List.filter_map ~f:(File.handle ~root:source_root) update_environment_with
        in
        AstSharedMemory.remove_paths handles;
        let (module Handler: Analysis.Environment.Handler) = state.environment in
        Handler.purge handles
      in
      let stubs, sources = List.partition_tf ~f:is_stub update_environment_with in
      let stubs = Service.Parser.parse_sources ~configuration ~scheduler ~files:stubs in
      let sources =
        let keep file =
          (File.handle ~root:source_root file
           >>= fun path -> Some (Source.qualifier ~path:(File.Handle.show path))
           >>= Handler.module_definition
           >>= Module.path
           >>| (fun existing_path -> File.Handle.show path = existing_path))
          |> Option.value ~default:true
        in
        List.filter ~f:keep sources
      in
      let sources = Service.Parser.parse_sources ~configuration ~scheduler ~files:sources in
      stubs @ sources
    in
    let new_source_handles = List.filter_map ~f:(File.handle ~root:source_root) check in
    Annotated.Class.AttributesCache.clear ();
    let () =
      Log.log
        ~section:`Debug
        "Repopulating the environment with %s"
        (List.to_string ~f:File.Handle.show repopulate_handles);
      let repopulate_path handle =
        match AstSharedMemory.get_source handle with
        | Some source -> [source]
        | None -> []
      in
      List.concat_map ~f:repopulate_path repopulate_handles
      |> Service.Environment.populate state.environment ~source_root;
      Statistics.event
        ~section:`Memory
        ~name:"Shared memory size"
        ~integers:["size", Service.EnvironmentSharedMemory.heap_size ()]
        ();
    in
    Service.Ignore.register ~configuration scheduler repopulate_handles;

    (* Clear all type resolution info from shared memory for all affected sources. *)
    List.filter_map ~f:AstSharedMemory.get_source new_source_handles
    |> List.concat_map ~f:Preprocessing.defines
    |> List.map ~f:(fun { Node.value = { Statement.Define.name; _ }; _ } -> name)
    |> TypeResolutionSharedMemory.remove;

    let new_errors, _ =
      Service.TypeCheck.analyze_sources
        scheduler
        configuration
        state.environment
        new_source_handles
    in
    (* Kill all previous errors for new files we just checked *)
    List.iter ~f:(Hashtbl.remove state.errors) new_source_handles;
    (* Associate the new errors with new files *)
    List.iter
      new_errors
      ~f:(fun error ->
          let path = Error.path error in
          Hashtbl.add_multi
            state.errors
            ~key:(File.Handle.create path)
            ~data:error);
    let new_files = File.Handle.Set.of_list new_source_handles in
    let checked_files =
      List.filter_map
        ~f:(fun file -> File.path file |> Path.relative >>| File.Handle.create)
        check
      |> fun handles -> Some handles
    in
    { state with handles = Set.union state.handles new_files; deferred_requests },
    Some (TypeCheckResponse (build_file_to_error_map ~checked_files new_errors))
  in
  let handle_type_query state request =
    let (module Handler: Environment.Handler) = state.environment in
    let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
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
    | Superclasses annotation ->
        let resolution = Environment.resolution state.environment () in
        let response =
          Handler.class_definition annotation
          >>| (fun { Analysis.Environment.class_definition; _ } -> class_definition)
          >>| Annotated.Class.create
          >>| Annotated.Class.superclasses ~resolution
          >>| List.map ~f:(Annotated.Class.annotation ~resolution)
          >>| List.map ~f:Type.show
          >>| String.concat ~sep:", "
          >>| (fun response -> TypeQueryResponse response)
        in
        state, response
  in
  let handle_client_shutdown_request id =
    let response = LanguageServer.Protocol.ShutdownResponse.default id in
    state,
    Some (LanguageServerProtocolResponse (
        Yojson.Safe.to_string (LanguageServer.Protocol.ShutdownResponse.to_yojson response)))
  in
  let result =
    match request with
    | TypeCheckRequest request -> handle_type_check state request
    | TypeQueryRequest request -> handle_type_query state request
    | DisplayTypeErrors request -> display_cached_type_errors state request
    | FlushTypeErrorsRequest -> flush_type_errors state
    | StopRequest ->
        Log.info "Stopping the server";
        Socket.write new_socket StopResponse;
        Mutex.critical_section
          state.lock
          ~f:(fun () ->
              ServerOperations.stop_server
                ~reason:"explicit request"
                server_configuration
                !(state.connections).socket);
        state, None
    | LanguageServerProtocolRequest request ->
        let check_on_save =
          Mutex.critical_section
            state.lock
            ~f:(fun () ->
                let { file_notifiers; _ } = !(state.connections) in
                List.is_empty file_notifiers)
        in
        LanguageServer.RequestParser.parse
          ~root:configuration.source_root
          ~check_on_save
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
                  >>= (fun lookup -> Lookup.get_definition lookup position)
                in
                Some
                  (state,
                   Some
                     (LanguageServerProtocolResponse
                        (LanguageServer.Protocol.TextDocumentDefinitionResponse.create
                           ~root:source_root
                           ~id
                           ~location:definition
                         |> LanguageServer.Protocol.TextDocumentDefinitionResponse.to_yojson
                         |> Yojson.Safe.to_string)))
            | HoverRequest { DefinitionRequest.id; path; position } ->
                let relative_path =
                  Path.from_uri path
                  >>= (fun path ->
                      Path.get_relative_to_root
                        ~root:configuration.project_root
                        ~path)
                  |> Option.value ~default:path
                in

                let open LanguageServer.Protocol in
                let result =
                  File.Handle.create relative_path
                  |> AstSharedMemory.get_source
                  >>| Lookup.create_of_source state.environment
                  >>= Lookup.get_annotation ~position
                  >>| (fun (location, annotation) ->
                      {
                        HoverResponse.location;
                        contents =
                          Type.show annotation
                          |> String.substr_replace_all ~pattern:"`" ~with_:"";
                      })
                in
                Some
                  (state,
                   Some
                     (LanguageServerProtocolResponse
                        (HoverResponse.create ~id ~result
                         |> HoverResponse.to_yojson
                         |> Yojson.Safe.to_string)))
            | RageRequest id ->
                let items = Rage.get_logs configuration in
                Some
                  (state,
                   Some (LanguageServerProtocolResponse
                           (LanguageServer.Protocol.RageResponse.create ~items ~id
                            |> LanguageServer.Protocol.RageResponse.to_yojson
                            |> Yojson.Safe.to_string)))
            | _ -> None)
        |> Option.value ~default:(state, None)

    | ClientShutdownRequest id -> handle_client_shutdown_request id

    | ClientExitRequest client ->
        Log.log ~section:`Server "Stopping %s client" (show_client client);
        state, Some (ClientExitResponse client)

    | RageRequest id ->
        let items = Rage.get_logs configuration in
        state,
        Some
          (LanguageServerProtocolResponse
             (LanguageServer.Protocol.RageResponse.create ~items ~id
              |> LanguageServer.Protocol.RageResponse.to_yojson
              |> Yojson.Safe.to_string))

    | GetDefinitionRequest { DefinitionRequest.path; position; _ } ->
        state, Some (GetDefinitionResponse (
            Hashtbl.find state.lookups path
            >>= (fun lookup -> Lookup.get_definition lookup position)))

    | HoverRequest { DefinitionRequest.path; position; _ } ->
        state, Some (HoverResponse (
            Hashtbl.find state.lookups path
            >>= fun lookup -> Lookup.get_definition lookup position))

    | ClientConnectionRequest _ ->
        raise InvalidRequest
  in
  Statistics.performance
    ~name:"server request"
    ~timer
    ~normals:["request_kind", Request.name request]
    ();
  result
