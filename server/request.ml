(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Network

open State
open Configuration.Analysis
open Configuration.Server
open Protocol
open Request

open Pyre


let parse_lsp ~configuration ~request =
  let open LanguageServer.Types in
  let log_method_error method_name =
    Log.error
      "Error for method %s: %s does not have required parameters"
      method_name
      (Yojson.Safe.pretty_to_string request)
  in
  let uri_to_path ~uri =
    let search_path = Configuration.Analysis.search_path configuration in
    Path.from_uri uri
    >>= fun path ->
    match Path.search_for_path ~search_path ~path with
    | Some path ->
        Some path
    | None ->
        Ast.SharedMemory.SymlinksToPaths.get (Path.absolute path)
        >>= fun path -> Path.search_for_path ~search_path ~path
  in
  let process_request request_method =
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
              uri_to_path ~uri
              >>| File.create
              >>| fun file ->
              GetDefinitionRequest {
                DefinitionRequest.id;
                file;
                (* The LSP protocol starts a file at line 0, column 0.
                   Pyre starts a file at line 1, column 0. *)
                position = { Ast.Location.line = line + 1; column = character };
              }
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
              uri_to_path ~uri
              >>| File.create
              >>| fun file ->
              Log.log ~section:`Server "Closed file %a" File.pp file;
              CloseDocument file
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
              uri_to_path ~uri
              >>| File.create
              >>| fun file ->
              Log.log ~section:`Server "Opened file %a" File.pp file;
              OpenDocument file
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
              uri_to_path ~uri
              >>| File.create ?content:text
              >>| fun file ->
              SaveDocument file
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
              uri_to_path ~uri
              >>| File.create
              >>| fun file ->
              HoverRequest {
                DefinitionRequest.id;
                file;
                (* The LSP protocol starts a file at line 0, column 0.
                   Pyre starts a file at line 1, column 0. *)
                position = { Ast.Location.line = line + 1; column = character };
              }
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
    process_request (Yojson.Safe.Util.to_string request_method)
  with Yojson.Safe.Util.Type_error _ -> None


type response = {
  state: State.t;
  response: Protocol.response option;
}


module LookupCache = struct
  let handle ~configuration file =
    try
      File.handle ~configuration file
      |> Option.some
    with File.NonexistentHandle error ->
      Log.info "%s" error;
      None


  let get_by_handle ~state:{ lookups; environment; _ } ~file ~handle =
    let cache_read = String.Table.find lookups (File.Handle.show handle) in
    match cache_read with
    | Some _ ->
        cache_read
    | None ->
        let lookup =
          let content =
            File.content file
            |> Option.value ~default:""
          in
          Ast.SharedMemory.Sources.get handle
          >>| Lookup.create_of_source environment
          >>| fun table -> { table; source = content }
        in
        lookup
        >>| (fun lookup -> String.Table.set lookups ~key:(File.Handle.show handle) ~data:lookup)
        |> ignore;
        lookup


  let get ~state ~configuration file =
    handle ~configuration file
    >>= fun handle -> get_by_handle ~state ~file ~handle


  let evict ~state:{ lookups; _ } ~configuration file =
    handle ~configuration file
    >>| File.Handle.show
    >>| String.Table.remove lookups
    |> ignore


  let log_lookup ~handle ~position ~timer ~name ?(integers = []) ?(normals = []) () =
    let normals =
      let base_normals = [
        "handle", File.Handle.show handle;
        "position", Location.show_position position;
      ]
      in
      base_normals @ normals
    in
    Statistics.performance
      ~section:`Event
      ~category:"perfpipe_pyre_ide_integration"
      ~name
      ~timer
      ~integers
      ~normals
      ()


  let find_annotation ~state ~configuration ~file ~position =
    let find_annotation_by_handle handle =
      let timer = Timer.start () in
      let annotation =
        get_by_handle ~state ~file ~handle
        >>= fun { table; source } ->
        Lookup.get_annotation table ~position ~source
      in
      let normals =
        annotation
        >>| fun (location, annotation) ->
        [
          "resolved location", Location.Instantiated.show location;
          "resolved annotation", Type.show annotation;
        ]
      in
      log_lookup
        ~handle
        ~position
        ~timer
        ~name:"find annotation"
        ?normals
        ();
      annotation
    in
    handle ~configuration file
    >>= find_annotation_by_handle


  let find_all_annotations ~state ~configuration ~file =
    let find_annotation_by_handle handle =
      let timer = Timer.start () in
      let annotations =
        get_by_handle ~state ~file ~handle
        >>| (fun { table; _ } -> Lookup.get_all_annotations table)
        |> Option.value ~default:[]
      in
      let integers = ["annotation list size", List.length annotations] in
      log_lookup
        ~handle
        ~position:Location.any_position
        ~timer
        ~name:"find all annotations"
        ~integers
        ();
      annotations
    in
    handle ~configuration file
    >>| find_annotation_by_handle


  let find_definition ~state ~configuration file position =
    let find_definition_by_handle handle =
      let timer = Timer.start () in
      let definition =
        get_by_handle ~state ~file ~handle
        >>= fun { table; source } ->
        Lookup.get_definition table ~position ~source
      in
      let normals =
        definition
        >>| fun location -> ["resolved location", Location.Instantiated.show location]
      in
      log_lookup
        ~handle
        ~position
        ~timer
        ~name:"find definition"
        ?normals
        ();
      definition
    in
    handle ~configuration file
    >>= find_definition_by_handle
end


let process_client_shutdown_request ~state ~id =
  let open LanguageServer.Protocol in
  let response =
    ShutdownResponse.default id
    |> ShutdownResponse.to_yojson
    |> Yojson.Safe.to_string
  in
  { state; response = Some (LanguageServerProtocolResponse response) }


let process_type_query_request ~state:({ State.environment; _ } as state) ~configuration ~request =
  let (module Handler: Environment.Handler) = environment in
  let process_request () =
    let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
    let resolution = TypeCheck.resolution environment () in
    let parse_and_validate access =
      let annotation =
        Expression.Access.expression access
        |> Resolution.parse_annotation resolution
      in
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
                (Expression.Access.show annotation)))

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
        >>| Annotated.Class.methods ~resolution
        >>| List.map ~f:to_method
        >>| (fun methods -> TypeQuery.Response (TypeQuery.FoundMethods methods))
        |> Option.value
          ~default:(
            TypeQuery.Error
              (Format.sprintf
                 "No class definition found for %s"
                 (Expression.Access.show annotation)))

    | TypeQuery.NormalizeType expression ->
        parse_and_validate expression
        |> (fun annotation -> TypeQuery.Response (TypeQuery.Type annotation))

    | TypeQuery.SaveServerState path ->
        let path = Path.absolute path in
        Log.info "Saving server state into `%s`" path;
        Memory.save_shared_memory ~path;
        TypeQuery.Response (TypeQuery.Success ())

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
                | Type.Callable { Type.Callable.implementation; overloads; _ } ->
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
                         (List.filter_map (implementation :: overloads) ~f:overload_signature))
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
              (Format.sprintf
                 "No class definition found for %s"
                 (Expression.Access.show annotation)))

    | TypeQuery.Type expression ->
        begin
          let state =
            let define =
              Statement.Define.create_toplevel
                ~qualifier:[]
                ~statements:[]
              |> Node.create_with_default_location
            in
            TypeCheck.State.create ~resolution ~define ()
          in
          let { TypeCheck.State.state; resolved = annotation; } =
            TypeCheck.State.forward_expression
              ~state
              ~expression
          in
          match TypeCheck.State.errors state with
          | [] ->
              TypeQuery.Response (TypeQuery.Type annotation)
          | errors ->
              let descriptions =
                List.map errors ~f:(Analysis.Error.description ~detailed:false)
                |> String.concat ~sep:", "
              in
              TypeQuery.Error (Format.sprintf "Expression had errors: %s" descriptions)
        end

    | TypeQuery.TypeAtPosition { file; position; } ->
        let default =
          TypeQuery.Error (
            Format.asprintf
              "Not able to get lookup at %a:%a"
              Path.pp (File.path file)
              Location.pp_position position)
        in
        LookupCache.find_annotation ~state ~configuration ~file ~position
        >>| (fun (location, annotation) ->
            TypeQuery.Response (TypeQuery.TypeAtLocation { TypeQuery.location; annotation }))
        |> Option.value ~default

    | TypeQuery.TypesInFile file ->
        let default =
          TypeQuery.Error (
            Format.asprintf
              "Not able to get lookups in %a"
              Path.pp (File.path file))
        in
        LookupCache.find_all_annotations ~state ~configuration ~file
        >>| List.map ~f:(fun (location, annotation) -> { TypeQuery.location; annotation })
        >>| (fun list -> TypeQuery.Response (TypeQuery.TypesAtLocations list))
        |> Option.value ~default
  in
  let response =
    try
      process_request ()
    with TypeOrder.Untracked untracked ->
      let untracked_response =
        Format.asprintf "Type `%a` was not found in the type order." Type.pp untracked
      in
      TypeQuery.Error untracked_response
  in
  { state; response = Some (TypeQueryResponse response) }


let build_file_to_error_map ?(checked_files = None) ~state:{ State.errors; _ } error_list =
  let initial_files = Option.value ~default:(Hashtbl.keys errors) checked_files in
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


let process_display_type_errors_request
    ~state:({ State.errors; _ } as state)
    ~configuration
    ~files =
  let errors =
    match files with
    | [] ->
        Hashtbl.data errors
        |> List.concat
    | _ ->
        let errors file =
          try
            File.handle ~configuration file
            |> Hashtbl.find errors
            |> Option.value ~default:[]
          with (File.NonexistentHandle _) ->
            []
        in
        List.concat_map ~f:errors files
  in
  { state; response = Some (TypeCheckResponse (build_file_to_error_map ~state errors)) }


let process_type_check_request
    ~state:({ State.environment; errors; scheduler; deferred_requests; _ } as state)
    ~configuration:({ debug; _ } as configuration)
    ~request:{ TypeCheckRequest.update_environment_with; check} =
  Annotated.Class.Attribute.Cache.clear ();
  let update_environment_with, check =
    let keep file =
      match File.handle ~configuration file with
      | exception ((File.NonexistentHandle _) as uncaught_exception) ->
          Statistics.log_exception uncaught_exception ~fatal:false ~origin:"server";
          false
      | handle ->
          begin
            match Ast.SharedMemory.Modules.get ~qualifier:(Source.qualifier ~handle) with
            | Some existing ->
                let existing_handle =
                  Module.handle existing
                  |> Option.value ~default:handle
                in
                File.Handle.equal existing_handle handle
            | _  ->
                true
          end
    in
    List.filter update_environment_with ~f:keep,
    List.filter check ~f:keep
  in
  let (module Handler: Environment.Handler) = environment in
  let scheduler = Scheduler.with_parallel scheduler ~is_parallel:(List.length check > 5) in

  (* Compute requests we do not serve immediately. *)
  let deferred_requests =
    if not (List.is_empty update_environment_with) then
      let files =
        let old_signature_hashes, new_signature_hashes =
          let signature_hashes ~default =
            let table = File.Handle.Table.create () in
            let add_signature_hash file =
              try
                let handle = File.handle file ~configuration in
                let signature_hash =
                  Ast.SharedMemory.Sources.get handle
                  >>| Source.signature_hash
                  |> Option.value ~default
                in
                Hashtbl.set table ~key:handle ~data:signature_hash
              with (File.NonexistentHandle _) ->
                Log.log ~section:`Server "Unable to get handle for %a" File.pp file
            in
            List.iter update_environment_with ~f:add_signature_hash;
            table
          in
          let old_signature_hashes = signature_hashes ~default:0 in

          (* Clear and re-populate ASTs in shared memory. *)
          let handles = List.map update_environment_with ~f:(File.handle ~configuration) in
          (* Update the tracked handles, if necessary. *)
          let newly_introduced_handles =
            List.filter
              handles
              ~f:(fun handle -> Option.is_none (Ast.SharedMemory.Sources.get handle))
          in
          if not (List.is_empty newly_introduced_handles) then
            Ast.SharedMemory.HandleKeys.add ~handles:newly_introduced_handles;
          Ast.SharedMemory.Sources.remove ~handles;
          let targets =
            let find_target file = Path.readlink (File.path file) in
            List.filter_map update_environment_with ~f:find_target
          in
          Ast.SharedMemory.SymlinksToPaths.remove ~targets;
          Service.Parser.parse_sources ~configuration ~scheduler ~files:update_environment_with
          |> ignore;

          let new_signature_hashes = signature_hashes ~default:(-1) in
          old_signature_hashes, new_signature_hashes
        in

        let dependents =
          let handle file = File.handle file ~configuration in
          let update_environment_with = List.map update_environment_with ~f:handle in
          let check = List.map check ~f:handle in
          Log.log
            ~section:`Server
            "Handling type check request for files %a"
            Sexp.pp [%message (update_environment_with: File.Handle.t list)];
          let get_dependencies handle =
            let signature_hash_changed =
              let old_signature_hash = Hashtbl.find_exn old_signature_hashes handle in
              let new_signature_hash = Hashtbl.find_exn new_signature_hashes handle in
              new_signature_hash <> old_signature_hash
            in
            let has_starred_import () =
              let was_starred_import { Node.value; _ } =
                (* Heuristic: if the list of exports for a module we import matches exactly
                   what that module exports, this was a starred import before preprocessing. *)
                let open Statement in
                match value with
                | Import { Import.from = Some from; imports } ->
                    begin
                      match Ast.SharedMemory.Modules.get_exports ~qualifier:from with
                      | Some exports ->
                          let import_names =
                            List.map imports ~f:(fun { Import.name; _ } -> name )
                          in
                          List.equal ~equal:Access.equal import_names exports
                      | _ ->
                          false
                    end
                | _ ->
                    false
              in

              Ast.SharedMemory.Sources.get handle
              >>| Source.statements
              |> Option.value ~default:[]
              |> List.exists ~f:was_starred_import
            in
            if signature_hash_changed or has_starred_import () then
              let qualifier = Ast.Source.qualifier ~handle in
              Handler.dependencies qualifier
            else
              None
          in
          Dependencies.of_list
            ~get_dependencies
            ~handles:update_environment_with
          |> Fn.flip Set.diff (File.Handle.Set.of_list check)
          |> Set.to_list
        in

        Log.log
          ~section:`Server
          "Inferred affected files: %a"
          Sexp.pp [%message (dependents: File.Handle.t list)];
        let to_file handle =
          Ast.SharedMemory.Sources.get handle
          >>= (fun { Ast.Source.handle; _ } -> File.Handle.to_path ~configuration handle)
          >>| File.create
        in
        List.filter_map dependents ~f:to_file
      in

      if List.is_empty files then
        deferred_requests
      else
        (TypeCheckRequest
           (TypeCheckRequest.create
              ~update_environment_with:files
              ~check:files
              ())) ::
        deferred_requests
    else
      deferred_requests
  in

  (* Repopulate the environment. *)
  let repopulate_handles =
    (* Clean up all data related to updated files. *)
    let handle file =
      try
        Some (File.handle ~configuration file)
      with File.NonexistentHandle _ ->
        None
    in
    let handles = List.filter_map update_environment_with ~f:handle in
    Ast.SharedMemory.Sources.remove ~handles;
    let targets =
      let find_target file = Path.readlink (File.path file) in
      List.filter_map update_environment_with ~f:find_target
    in
    Ast.SharedMemory.SymlinksToPaths.remove ~targets;
    Handler.purge ~debug handles;
    update_environment_with
    |> List.iter ~f:(LookupCache.evict ~state ~configuration);

    let stubs, sources =
      let is_stub file =
        file
        |> File.path
        |> Path.absolute
        |> String.is_suffix ~suffix:".pyi"
      in
      List.partition_tf ~f:is_stub update_environment_with
    in
    let { Service.Parser.parsed = stubs; _ } =
      Service.Parser.parse_sources ~configuration ~scheduler ~files:stubs
    in
    let sources =
      let keep file =
        (handle file
         >>= fun handle -> Some (Source.qualifier ~handle)
         >>= Handler.module_definition
         >>= Module.handle
         >>| (fun existing_handle -> File.Handle.equal handle existing_handle))
        |> Option.value ~default:true
      in
      List.filter ~f:keep sources
    in
    let { Service.Parser.parsed = sources; _ } =
      Service.Parser.parse_sources ~configuration ~scheduler ~files:sources
    in
    stubs @ sources
  in
  Log.log
    ~section:`Debug
    "Repopulating the environment with %a"
    Sexp.pp [%message (repopulate_handles: File.Handle.t list)];
  List.filter_map ~f:Ast.SharedMemory.Sources.get repopulate_handles
  |> Service.Environment.populate ~configuration environment;
  let classes_to_infer =
    let get_class_keys handle =
      Handler.DependencyHandler.get_class_keys ~handle
    in
    List.concat_map repopulate_handles ~f:get_class_keys
  in
  let resolution = TypeCheck.resolution environment () in
  Analysis.Environment.infer_protocols ~handler:environment resolution ~classes_to_infer ();
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Service.EnvironmentSharedMemory.heap_size ()]
    ();
  Service.Postprocess.register_ignores ~configuration scheduler repopulate_handles;

  (* Compute new set of errors. *)
  let handle file =
    try
      Some (File.handle ~configuration file)
    with File.NonexistentHandle _ ->
      None
  in
  let new_source_handles = List.filter_map ~f:handle check in

  (* Clear all type resolution info from shared memory for all affected sources. *)
  List.filter_map ~f:Ast.SharedMemory.Sources.get new_source_handles
  |> List.concat_map ~f:(Preprocessing.defines ~extract_into_toplevel:true)
  |> List.map ~f:(fun { Node.value = { Statement.Define.name; _ }; _ } -> name)
  |> ResolutionSharedMemory.remove;

  let new_errors, _ =
    Service.Check.analyze_sources
      ~scheduler
      ~configuration
      ~environment
      ~handles:new_source_handles
  in
  (* Kill all previous errors for new files we just checked *)
  List.iter ~f:(Hashtbl.remove errors) new_source_handles;
  (* Associate the new errors with new files *)
  List.iter
    new_errors
    ~f:(fun error ->
        Hashtbl.add_multi errors ~key:(File.Handle.create (Error.path error)) ~data:error);
  let checked_files =
    List.filter_map
      ~f:(fun file -> File.path file |> Path.relative >>| File.Handle.create)
      check
    |> Option.some
  in
  {
    state = { state with deferred_requests };
    response = Some (TypeCheckResponse (build_file_to_error_map ~checked_files ~state new_errors));
  }


let process_get_definition_request
    ~state
    ~configuration
    ~request:{ DefinitionRequest.id; file; position } =
  let response =
    let open LanguageServer.Protocol in
    let definition = LookupCache.find_definition ~state ~configuration file position in
    TextDocumentDefinitionResponse.create ~configuration ~id ~location:definition
    |> TextDocumentDefinitionResponse.to_yojson
    |> Yojson.Safe.to_string
    |> (fun response -> LanguageServerProtocolResponse response)
    |> Option.some
  in
  { state; response }


let rec process
    ~socket
    ~state:({ State.environment; deferred_requests; errors; lock; connections; _ } as state)
    ~configuration:({
        configuration;
        _;
      } as server_configuration)
    ~request =
  let timer = Timer.start () in
  let (module Handler: Environment.Handler) = environment in
  let log_request_error ~error =
    Statistics.event
      ~section:`Error
      ~name:"request error"
      ~normals:[
        "request", Request.show request;
        "error", error;
      ]
      ~flush:true
      ()
  in
  let result =
    try
      match request with
      | TypeCheckRequest request ->
          SharedMem.collect `aggressive;
          process_type_check_request ~state ~configuration ~request

      | TypeQueryRequest request ->
          process_type_query_request ~state ~configuration ~request

      | DisplayTypeErrors files ->
          process_display_type_errors_request ~state ~configuration ~files

      | FlushTypeErrorsRequest ->
          let state =
            let deferred_requests = Request.flatten deferred_requests in
            let state = { state with deferred_requests = [] } in
            let update_state state request =
              let { state; _ } =
                process
                  ~socket
                  ~state
                  ~configuration:server_configuration
                  ~request
              in
              state
            in
            List.fold ~init:state ~f:update_state deferred_requests
          in
          let errors =
            Hashtbl.data errors
            |> List.concat
          in
          { state; response = Some (TypeCheckResponse (build_file_to_error_map ~state errors)) }

      | StopRequest ->
          Socket.write socket StopResponse;
          Mutex.critical_section
            lock
            ~f:(fun () ->
                Operations.stop
                  ~reason:"explicit request"
                  ~configuration:server_configuration
                  ~socket:!connections.socket);
          { state; response = None }

      | LanguageServerProtocolRequest request ->
          parse_lsp
            ~configuration
            ~request:(Yojson.Safe.from_string request)
          >>| (fun request -> process ~state ~socket ~configuration:server_configuration ~request)
          |> Option.value ~default:{ state; response = None }

      | ClientShutdownRequest id ->
          process_client_shutdown_request ~state ~id

      | ClientExitRequest client ->
          Log.log ~section:`Server "Stopping %s client" (show_client client);
          { state; response = Some (ClientExitResponse client) }

      | RageRequest id ->
          let response =
            let items = Service.Rage.get_logs configuration in
            LanguageServer.Protocol.RageResponse.create ~items ~id
            |> LanguageServer.Protocol.RageResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }

      | GetDefinitionRequest request ->
          process_get_definition_request ~state ~configuration ~request

      | HoverRequest { DefinitionRequest.id; file; position } ->
          let response =
            let open LanguageServer.Protocol in
            let result =
              LookupCache.find_annotation ~state ~configuration ~file ~position
              >>| fun (location, annotation) ->
              {
                HoverResponse.location;
                contents = Type.show annotation;
              }
            in
            HoverResponse.create ~id ~result
            |> HoverResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }

      | OpenDocument file ->
          (* Make sure cache is fresh. We might not have received a close notification. *)
          LookupCache.evict ~state ~configuration file;
          (* Make sure the IDE flushes its state about this file, by sending back all the
             errors for this file. *)
          process_type_check_request
            ~state
            ~configuration
            ~request:{ TypeCheckRequest.update_environment_with = [file]; check = [file]; }

      | CloseDocument file ->
          LookupCache.evict ~state ~configuration file;
          { state; response = None }

      | SaveDocument file ->
          (* On save, evict entries from the lookup cache. The updated
             source will be picked up at the next lookup (if any). *)
          LookupCache.evict ~state ~configuration file;
          let check_on_save =
            Mutex.critical_section
              lock
              ~f:(fun () ->
                  let { file_notifiers; _ } = !connections in
                  List.is_empty file_notifiers)
          in
          if check_on_save then
            process_type_check_request
              ~state
              ~configuration
              ~request:{ TypeCheckRequest.update_environment_with = [file]; check = [file]; }
          else
            begin
              Log.log ~section:`Server "Explicitly ignoring didSave request";
              { state; response = None }
            end

      (* Requests that cannot be fulfilled here. *)
      | ClientConnectionRequest _ ->
          Log.warning  "Explicitly ignoring ClientConnectionRequest request";
          { state; response = None }
    with
    | Unix.Unix_error (kind, name, parameters) ->
        Log.log_unix_error (kind, name, parameters);
        log_request_error
          ~error:(Format.sprintf "Unix error %s: %s(%s)" (Unix.error_message kind) name parameters);
        { state; response = None }
    | Analysis.TypeOrder.Untracked annotation ->
        log_request_error ~error:(Format.sprintf "Untracked %s" (Type.show annotation));
        { state; response = None }
    | uncaught_exception ->
        let should_stop =
          match request with
          | HoverRequest _
          | GetDefinitionRequest _ ->
              false
          | _ ->
              true
        in
        Statistics.log_exception uncaught_exception ~fatal:should_stop ~origin:"server";
        if should_stop then
          Mutex.critical_section
            lock
            ~f:(fun () ->
                Operations.stop
                  ~reason:"uncaught exception"
                  ~configuration:server_configuration
                  ~socket:!connections.socket);
        { state; response = None }

  in
  Statistics.performance
    ~name:"server request"
    ~timer
    ~normals:["request kind", Request.name request]
    ();
  result
