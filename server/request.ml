(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Network

open State
open Configuration
open ServerConfiguration
open Protocol
open Request

open Pyre


exception InvalidRequest



let parse ~root ~request =
  let open LanguageServer.Types in
  let log_method_error method_name =
    Log.error
      "Error for method %s: %s does not have required parameters"
      method_name
      (Yojson.Safe.pretty_to_string request)
  in
  let uri_to_contained_relative_path ~root ~uri =
    let to_relative_path ~root ~path =
      String.chop_prefix ~prefix:(root ^ "/") path
      |> Option.value ~default:path
    in
    String.chop_prefix ~prefix:"file://" uri
    >>= (fun path ->
        if String.is_prefix ~prefix:root path then
          Some (to_relative_path ~root ~path)
        else
          None)
    |> Option.value ~default:uri
  in
  let handle_request request_method =
    match request_method with
    | "textDocument/definition" ->
        begin
          match TextDocumentDefinitionRequest.of_yojson request with
          | Ok {
              TextDocumentDefinitionRequest.parameters = Some {
                  TextDocumentPositionParams.textDocument = {
                    TextDocumentIdentifier.uri;
                    _;
                  };
                  position = { Position.line; character };
                };
              id;
              _;
            } ->
              let file =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
                |> fun relative ->
                Path.create_relative ~root ~relative
                |> File.create
              in
              Some (GetDefinitionRequest {
                  DefinitionRequest.id;
                  file;
                  (* The LSP protocol starts a file at line 0, column 0.
                     Pyre starts a file at line 1, column 0. *)
                  position = { Ast.Location.line = line + 1; column = character };
                })
          | Ok _ ->
              None
          | Error yojson_error ->
              Log.dump "%s" yojson_error;
              None
        end
    | "textDocument/didClose" ->
        begin
          match DidCloseTextDocument.of_yojson request with
          | Ok {
              DidCloseTextDocument.parameters = Some {
                  DidCloseTextDocumentParams.textDocument = {
                    TextDocumentIdentifier.uri;
                    _;
                  };
                  _
                };
              _;
            } ->
              let file =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
                |> fun relative ->
                Path.create_relative ~root ~relative
                |> File.create
              in
              Log.log ~section:`Server "Closed file %a" File.pp file;
              Some (CloseDocument file)
          | Ok _ ->
              log_method_error request_method;
              None
          | Error yojson_error ->
              Log.log ~section:`Server "Error: %s" yojson_error;
              None
        end

    | "textDocument/didOpen" ->
        begin
          match DidOpenTextDocument.of_yojson request with
          | Ok {
              DidOpenTextDocument.parameters = Some {
                  DidOpenTextDocumentParams.textDocument = {
                    TextDocumentItem.uri;
                    _;
                  };
                  _;
                };
              _;
            } ->
              let file =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
                |> fun relative ->
                Path.create_relative ~root ~relative
                |> File.create
              in
              Log.log ~section:`Server "Opened file %a" File.pp file;
              Some (OpenDocument file)
          | Ok _ ->
              log_method_error request_method;
              None
          | Error yojson_error ->
              Log.log ~section:`Server "Error: %s" yojson_error;
              None
        end

    | "textDocument/didSave" ->
        begin
          match DidSaveTextDocument.of_yojson request with
          | Ok {
              DidSaveTextDocument.parameters = Some {
                  DidSaveTextDocumentParams.textDocument = {
                    TextDocumentIdentifier.uri;
                    _;
                  };
                  text;
                };
              _;
            } ->
              let file =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
                |> fun relative ->
                Path.create_relative ~root ~relative
                |> File.create ~content:text
              in
              Some (SaveDocument file)
          | Ok _ ->
              log_method_error request_method;
              None
          | Error yojson_error ->
              Log.log ~section:`Server "Error: %s" yojson_error;
              None
        end

    | "textDocument/hover" ->
        begin
          match HoverRequest.of_yojson request with
          | Ok {
              HoverRequest.parameters = Some {
                  TextDocumentPositionParams.textDocument = {
                    TextDocumentIdentifier.uri;
                    _;
                  };
                  position = { Position.line; character };
                };
              id;
              _;
            } ->
              let file =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
                |> fun relative ->
                Path.create_relative ~root ~relative
                |> File.create
              in
              Some (HoverRequest {
                  DefinitionRequest.id;
                  file;
                  (* The LSP protocol starts a file at line 0, column 0.
                     Pyre starts a file at line 1, column 0. *)
                  position = { Ast.Location.line = line + 1; column = character };
                })
          | Ok _ ->
              None
          | Error yojson_error ->
              Log.log ~section:`Server "Error: %s" yojson_error;
              None
        end

    | "shutdown" ->
        begin
          match ShutdownRequest.of_yojson request with
          | Ok { ShutdownRequest.id; _ } -> Some (ClientShutdownRequest id)
          | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
        end

    | "exit" -> Some (ClientExitRequest Persistent)
    | "telemetry/rage" ->
        begin
          match RageRequest.of_yojson request with
          | Ok { RageRequest.id; _ } -> Some (Request.RageRequest id)
          | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
        end
    | unmatched_method ->
        Log.log ~section:`Server "Unhandled %s" unmatched_method; None
  in
  try
    let request_method = Yojson.Safe.Util.member "method" request in
    handle_request (Yojson.Safe.Util.to_string request_method)
  with Yojson.Safe.Util.Type_error _ -> None


module LookupCache = struct
  let relative_path file =
    File.path file
    |> Path.relative


  let get ~state:{ lookups; environment; _ } ~configuration:{ local_root; _ } file =
    let find_or_add path =
      let construct_lookup path =
        let add_source table =
          let source =
            Path.create_relative ~root:local_root ~relative:path
            |> File.create
            |> File.content
            |> Option.value ~default:""
          in
          { table; source }
        in
        File.Handle.create path
        |> Ast.SharedMemory.get_source
        >>| Lookup.create_of_source environment
        >>| add_source
      in
      let cache_read = String.Table.find lookups path in
      match cache_read with
      | Some _ ->
          cache_read
      | None ->
          let lookup = construct_lookup path in
          lookup
          >>| (fun data -> String.Table.set lookups ~key:path ~data)
          |> ignore;
          lookup
    in
    relative_path file
    >>= find_or_add


  let evict ~state:{ lookups; _ } file =
    relative_path file
    >>| String.Table.remove lookups
    |> ignore


  let find_annotation ~state ~configuration file position =
    get ~state ~configuration file
    >>= fun { table; source } ->
    Lookup.get_annotation table ~position ~source_text:source


  let find_definition ~state ~configuration file position =
    get ~state ~configuration file
    >>= fun { table; _ } ->
    Lookup.get_definition table ~position
end


let handle_client_shutdown_request ~state ~id =
  let open LanguageServer.Protocol in
  let response =
    ShutdownResponse.default id
    |> ShutdownResponse.to_yojson
    |> Yojson.Safe.to_string
  in
  state, Some (LanguageServerProtocolResponse response)


let handle_type_query_request ~state:({ State.environment; _ } as state) ~local_root ~request =
  let (module Handler: Environment.Handler) = environment in
  let handle_request () =
    let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
    let resolution = Environment.resolution state.environment () in
    let parse_and_validate unparsed_annotation =
      let annotation = Resolution.parse_annotation resolution unparsed_annotation in
      if TypeOrder.is_instantiated order annotation then
        annotation
      else
        raise (TypeOrder.Untracked annotation)
    in
    match request with
    | TypeQuery.Attributes annotation ->
        let to_attribute {
            Node.value = { Annotated.Class.Attribute.name; annotation; _ };
            _;
          } =
          let annotation = Annotation.annotation annotation in
          {
            TypeQuery.name = Expression.show (Node.create_with_default_location name);
            annotation;
          }
        in
        parse_and_validate annotation
        |> Handler.class_definition
        >>| (fun { Analysis.Resolution.class_definition; _ } -> class_definition)
        >>| Annotated.Class.create
        >>| (fun annotated_class -> Annotated.Class.attributes ~resolution annotated_class)
        >>| List.map ~f:to_attribute
        >>| (fun attributes -> TypeQuery.Response (TypeQuery.FoundAttributes attributes))

        |> Option.value
          ~default:(
            TypeQuery.Error (
              Format.sprintf
                "No class definition found for %s"
                (Expression.show annotation)))

    | TypeQuery.Join (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        TypeOrder.join order left right
        |> (fun annotation -> TypeQuery.Response (TypeQuery.Type annotation))

    | TypeQuery.LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        TypeOrder.less_or_equal order ~left ~right
        |> (fun response -> TypeQuery.Response (TypeQuery.Boolean response))

    | TypeQuery.Meet (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        TypeOrder.meet order left right
        |> (fun annotation -> TypeQuery.Response (TypeQuery.Type annotation))

    | TypeQuery.Methods annotation ->
        let to_method annotated_method =
          let open Annotated.Class.Method in
          let name =
            name annotated_method
            |> List.last
            >>| (fun name -> Expression.Access.show [name])
            |> Option.value ~default:""
          in
          let annotations = parameter_annotations_positional ~resolution annotated_method in
          let parameters =
            Map.keys annotations
            |> List.sort ~compare:Int.compare
            |> Fn.flip List.drop 1 (* Drop the self argument *)
            |> List.map ~f:(Map.find_exn annotations)
            |> fun parameters -> (Type.primitive "self") :: parameters
          in
          let return_annotation = return_annotation ~resolution annotated_method in
          { TypeQuery.name; parameters; return_annotation }
        in
        parse_and_validate annotation
        |> Handler.class_definition
        >>| (fun { Analysis.Resolution.class_definition; _ } -> class_definition)
        >>| Annotated.Class.create
        >>| Annotated.Class.methods
        >>| List.map ~f:to_method
        >>| (fun methods -> TypeQuery.Response (TypeQuery.FoundMethods methods))
        |> Option.value
          ~default:(
            TypeQuery.Error
              (Format.sprintf
                 "No class definition found for %s"
                 (Expression.show annotation)))
    | TypeQuery.NormalizeType expression ->
        parse_and_validate expression
        |> (fun annotation -> TypeQuery.Response (TypeQuery.Type annotation))

    | TypeQuery.Signature function_name ->
        let keep_known_annotation annotation =
          match annotation with
          | Type.Top ->
              None
          | _ ->
              Some annotation
        in
        begin
          match Resolution.global resolution function_name with
          | Some { Node.value; _ } ->
              begin
                match Annotation.annotation value with
                | Type.Callable { Type.Callable.overloads; _ } ->
                    let overload_signature { Type.Callable.annotation; parameters } =
                      match parameters with
                      | Type.Callable.Defined parameters ->
                          let format parameter =
                            match parameter with
                            | Type.Callable.Parameter.Named
                                { Type.Callable.Parameter.name; annotation; _ } ->
                                let name = Expression.Access.sanitized name in
                                Some {
                                  TypeQuery.parameter_name = Expression.Access.show name;
                                  annotation = keep_known_annotation annotation;
                                }
                            | _ ->
                                None
                          in
                          let parameters = List.filter_map ~f:format parameters in
                          Some {
                            TypeQuery.return_type = keep_known_annotation annotation;
                            parameters;
                          }
                      | _ ->
                          None
                    in
                    TypeQuery.Response
                      (TypeQuery.FoundSignature
                         (List.filter_map overloads ~f:overload_signature))
                | _ ->
                    TypeQuery.Error
                      (Format.sprintf
                         "%s is not a callable"
                         (Expression.Access.show function_name))
              end

          | None ->
              TypeQuery.Error
                (Format.sprintf
                   "No signature found for %s"
                   (Expression.Access.show function_name))
        end
    | TypeQuery.Superclasses annotation ->
        parse_and_validate annotation
        |> Handler.class_definition
        >>| (fun { Analysis.Resolution.class_definition; _ } -> class_definition)
        >>| Annotated.Class.create
        >>| Annotated.Class.superclasses ~resolution
        >>| List.map ~f:(Annotated.Class.annotation ~resolution)
        >>| (fun classes -> TypeQuery.Response (TypeQuery.Superclasses classes))
        |> Option.value
          ~default:(
            TypeQuery.Error
              (Format.sprintf "No class definition found for %s" (Expression.show annotation)))
    | TypeQuery.TypeAtLocation {
        Ast.Location.path;
        start = ({ Ast.Location.line; column} as start);
        _;
      } ->
        let source_text =
          Path.create_relative
            ~root:local_root
            ~relative:path
          |> File.create
          |> File.content
          |> Option.value ~default:""
        in
        File.Handle.create path
        |> Ast.SharedMemory.get_source
        >>| Lookup.create_of_source state.environment
        >>= Lookup.get_annotation ~position:start ~source_text
        >>| (fun (_, annotation) -> TypeQuery.Response (TypeQuery.Type annotation))
        |> Option.value ~default:(
          TypeQuery.Error (
            Format.sprintf
              "Not able to get lookup at %s:%d:%d"
              path
              line
              column))
  in
  let response =
    try
      handle_request ()
    with TypeOrder.Untracked untracked ->
      let untracked_response =
        Format.asprintf "Type `%a` was not found in the type order." Type.pp untracked
      in
      TypeQuery.Error untracked_response
  in
  TypeQueryResponse response


let build_file_to_error_map ?(checked_files = None) ~state error_list =
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


let handle_display_type_errors_request ~state ~local_root ~files =
  let errors =
    match files with
    | [] ->
        Hashtbl.data state.errors
        |> List.concat
    | _ ->
        List.filter_map ~f:(File.handle ~root:local_root) files
        |> List.filter_map ~f:(Hashtbl.find state.errors)
        |> List.concat
  in
  state, Some (TypeCheckResponse (build_file_to_error_map ~state errors))


let rec process_request
    ~new_socket
    ~state
    ~configuration:({
        configuration = { local_root; _ } as configuration;
        _;
      } as server_configuration)
    ~request =
  let timer = Timer.start () in
  let (module Handler: Environment.Handler) = state.environment in
  let flush_type_errors state =
    begin
      let state =
        let deferred_requests = Request.flatten state.deferred_requests in
        let state = { state with deferred_requests = [] } in
        let update_state state request =
          let state, _ =
            process_request
              ~new_socket
              ~state
              ~configuration:server_configuration
              ~request
          in
          state
        in
        List.fold ~init:state ~f:update_state deferred_requests
      in
      let errors =
        Hashtbl.data state.errors
        |> List.concat
      in
      state, Some (TypeCheckResponse (build_file_to_error_map ~state errors))
    end
  in
  let handle_type_check state { TypeCheckRequest.update_environment_with; check} =
    let deferred_requests =
      if not (List.is_empty update_environment_with) then
        let files =
          let dependents =
            let relative_path file =
              Path.get_relative_to_root ~root:local_root ~path:(File.path file)
            in
            let update_environment_with =
              List.filter_map update_environment_with ~f:relative_path
            in
            let check = List.filter_map check ~f:relative_path in
            Log.log
              ~section:`Server
              "Handling type check request for files %a"
              Sexp.pp [%message (update_environment_with: string list)];
            let get_dependencies path =
              let qualifier = Ast.Source.qualifier ~path in
              Handler.dependencies qualifier
            in
            Dependencies.of_list
              ~get_dependencies
              ~paths:update_environment_with
            |> Fn.flip Set.diff (String.Set.of_list check)
            |> Set.to_list
          in

          Log.log
            ~section:`Server
            "Inferred affected files: %a"
            Sexp.pp [%message (dependents: string list)];
          List.map
            ~f:(fun path ->
                Path.create_relative ~root:local_root ~relative:path
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
    let scheduler = Scheduler.with_parallel state.scheduler ~is_parallel:(List.length check > 5) in
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
          List.filter_map ~f:(File.handle ~root:local_root) update_environment_with
        in
        Ast.SharedMemory.remove_paths handles;
        Handler.purge handles;
        (* Evict entries from the lookup cache. The next lookup (if
           any) will freshen the cache. *)
        update_environment_with
        |> List.iter ~f:(LookupCache.evict ~state)
      in
      let stubs, sources = List.partition_tf ~f:is_stub update_environment_with in
      let stubs = Service.Parser.parse_sources ~configuration ~scheduler ~files:stubs in
      let sources =
        let keep file =
          (File.handle ~root:local_root file
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
    let new_source_handles = List.filter_map ~f:(File.handle ~root:local_root) check in
    Annotated.Class.AttributesCache.clear ();
    let () =
      Log.log
        ~section:`Debug
        "Repopulating the environment with %a"
        Sexp.pp [%message (repopulate_handles: File.Handle.t list)];

      List.filter_map ~f:Ast.SharedMemory.get_source repopulate_handles
      |> Service.Environment.populate state.environment;
      let classes_to_infer =
        let get_class_keys handle =
          Handler.DependencyHandler.get_class_keys ~path:(File.Handle.show handle)
        in
        List.concat_map repopulate_handles ~f:get_class_keys
      in
      Analysis.Environment.infer_protocols ~handler:state.environment ~classes_to_infer ();
      Statistics.event
        ~section:`Memory
        ~name:"shared memory size"
        ~integers:["size", Service.EnvironmentSharedMemory.heap_size ()]
        ();
    in
    Service.Postprocess.register_ignores ~configuration scheduler repopulate_handles;

    (* Clear all type resolution info from shared memory for all affected sources. *)
    List.filter_map ~f:Ast.SharedMemory.get_source new_source_handles
    |> List.concat_map ~f:(Preprocessing.defines ~extract_into_toplevel:true)
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
          Hashtbl.add_multi state.errors ~key:(File.Handle.create (Error.path error)) ~data:error);
    let new_files = File.Handle.Set.of_list new_source_handles in
    let checked_files =
      List.filter_map
        ~f:(fun file -> File.path file |> Path.relative >>| File.Handle.create)
        check
      |> fun handles -> Some handles
    in
    { state with handles = Set.union state.handles new_files; deferred_requests },
    Some (TypeCheckResponse (build_file_to_error_map ~checked_files ~state new_errors))
  in
  let handle_lsp_request ~check_on_save lsp_request =
    match lsp_request with
    | TypeCheckRequest files -> Some (handle_type_check state files)
    | ClientShutdownRequest id -> Some (handle_client_shutdown_request ~state ~id)
    | ClientExitRequest Persistent ->
        Log.log ~section:`Server "Stopping persistent client";
        Some (state, Some (ClientExitResponse Persistent))
    | GetDefinitionRequest { DefinitionRequest.id; file; position } ->
        let open LanguageServer.Protocol in
        let definition = LookupCache.find_definition ~state ~configuration file position in
        Some
          (state,
           Some
             (LanguageServerProtocolResponse
                (TextDocumentDefinitionResponse.create
                   ~root:local_root
                   ~id
                   ~location:definition
                 |> TextDocumentDefinitionResponse.to_yojson
                 |> Yojson.Safe.to_string)))
    | HoverRequest { DefinitionRequest.id; file; position } ->
        let open LanguageServer.Protocol in
        let result =
          LookupCache.find_annotation ~state ~configuration file position
          >>| fun (location, annotation) ->
          {
            HoverResponse.location;
            contents = Type.show annotation;
          }
        in
        Some
          (state,
           Some
             (LanguageServerProtocolResponse
                (HoverResponse.create ~id ~result
                 |> HoverResponse.to_yojson
                 |> Yojson.Safe.to_string)))
    | RageRequest id ->
        let items = Service.Rage.get_logs configuration in
        Some
          (state,
           Some (LanguageServerProtocolResponse
                   (LanguageServer.Protocol.RageResponse.create ~items ~id
                    |> LanguageServer.Protocol.RageResponse.to_yojson
                    |> Yojson.Safe.to_string)))

    | OpenDocument file ->
        (* Make sure cache is fresh. We might not have received a close notification. *)
        LookupCache.evict ~state file;
        LookupCache.get ~state ~configuration file
        |> ignore;
        None
    | CloseDocument file ->
        LookupCache.evict ~state file;
        None
    | SaveDocument file ->
        (* On save, evict entries from the lookup cache. The updated
           source will be picked up at the next lookup (if any). *)
        LookupCache.evict ~state file;
        if check_on_save then
          Some
            (handle_type_check
               state
               { TypeCheckRequest.update_environment_with = [file]; check = [file]; })
        else
          begin
            Log.log ~section:`Server "Explicitly ignoring didSave request";
            None
          end

    | _ ->
        Log.log
          ~section:`Server
          "Ignoring request of type `%s` wrapped inside LSP request"
          (name lsp_request);
        None
  in
  let result =
    match request with
    | TypeCheckRequest request ->
        if Memory.heap_use_ratio () > 0.5 then
          begin
            let previous_use_ratio = Memory.heap_use_ratio () in
            SharedMem.collect `aggressive;
            Log.log
              ~section:`Server
              "Garbage collected due to a previous heap use ratio of %f. New ratio is %f."
              previous_use_ratio
              (Memory.heap_use_ratio ())
          end;
        handle_type_check state request
    | TypeQueryRequest request ->
        state, Some (handle_type_query_request ~state ~local_root ~request)
    | DisplayTypeErrors files ->
        handle_display_type_errors_request ~state ~local_root ~files
    | FlushTypeErrorsRequest ->
        flush_type_errors state
    | StopRequest ->
        Socket.write new_socket StopResponse;
        Mutex.critical_section
          state.lock
          ~f:(fun () ->
              Operations.stop_server
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
        parse
          ~root:configuration.local_root
          ~request:(Yojson.Safe.from_string request)
        >>= handle_lsp_request ~check_on_save
        |> Option.value ~default:(state, None)

    | ClientShutdownRequest id -> handle_client_shutdown_request ~state ~id

    | ClientExitRequest client ->
        Log.log ~section:`Server "Stopping %s client" (show_client client);
        state, Some (ClientExitResponse client)

    | RageRequest id ->
        let items = Service.Rage.get_logs configuration in
        state,
        Some
          (LanguageServerProtocolResponse
             (LanguageServer.Protocol.RageResponse.create ~items ~id
              |> LanguageServer.Protocol.RageResponse.to_yojson
              |> Yojson.Safe.to_string))

    (* Requests that can only be fulfilled if wrapped in a LanguageServerProtocolRequest. *)
    | GetDefinitionRequest _
    | HoverRequest _
    | OpenDocument _
    | CloseDocument _
    | SaveDocument _ ->
        Log.warning "Request of type `%s` received in the wrong state" (name request);
        state, None

    (* Requests that cannot be fulfilled here. *)
    | ClientConnectionRequest _ ->
        raise InvalidRequest
  in
  Statistics.performance
    ~name:"server request"
    ~timer
    ~normals:["request_kind", Request.name request]
    ();
  result
