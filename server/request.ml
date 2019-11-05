(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ServerDependencies = Dependencies
module ModuleTracker = Analysis.ModuleTracker
open Ast
open Analysis
open State
open Configuration.Server
open Protocol
open Request
open Pyre

exception IncorrectParameters of Type.t

let to_pyre_position { LanguageServer.Types.Position.line; character } =
  (* The LSP protocol starts a file at line 0, column 0. Pyre starts a file at line 1, column 0. *)
  { Location.line = line + 1; column = character }


let errors_of_path ~configuration ~state:{ State.module_tracker; errors; _ } path =
  ModuleTracker.lookup_path ~configuration module_tracker path
  >>= (fun { SourcePath.qualifier; _ } -> Hashtbl.find errors qualifier)
  |> Option.value ~default:[]


let instantiate_error ~configuration ~state:{ State.environment; _ } error =
  let ast_environment = AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment in
  Error.instantiate
    ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment)
    error


let parse_lsp
    ~configuration:( { Configuration.Analysis.perform_autocompletion; go_to_definition_enabled; _ }
                   as configuration )
    ~state:{ State.symlink_targets_to_sources; _ }
    ~request
  =
  let open LanguageServer.Types in
  let log_method_error method_name =
    Log.error
      "Error for method %s: %s does not have required parameters"
      method_name
      (Yojson.Safe.pretty_to_string request)
  in
  let uri_to_path ~uri =
    let search_paths = Configuration.Analysis.search_path configuration in
    Path.from_uri uri
    >>= fun path ->
    match SearchPath.search_for_path ~search_paths path with
    | Some SearchPath.{ relative_path; _ } -> Some (Path.Relative relative_path)
    | None ->
        Hashtbl.find symlink_targets_to_sources (Path.absolute path)
        >>= fun path ->
        SearchPath.search_for_path ~search_paths path
        >>| fun SearchPath.{ relative_path; _ } -> Path.Relative relative_path
  in
  let string_to_path string_path = Path.create_absolute ~follow_symbolic_links:false string_path in
  let process_request request_method =
    match request_method with
    | "textDocument/definition" -> (
      match TextDocumentDefinitionRequest.of_yojson request with
      | Ok
          {
            TextDocumentDefinitionRequest.parameters =
              Some
                {
                  TextDocumentPositionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  position;
                };
            id;
            _;
          }
        when go_to_definition_enabled ->
          uri_to_path ~uri
          >>| fun path ->
          GetDefinitionRequest { DefinitionRequest.id; path; position = to_pyre_position position }
      | Ok _ -> None
      | Error yojson_error ->
          Log.dump "%s" yojson_error;
          None )
    | "textDocument/didClose" -> (
      match DidCloseTextDocument.of_yojson request with
      | Ok
          {
            DidCloseTextDocument.parameters =
              Some
                {
                  DidCloseTextDocumentParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  _;
                };
            _;
          } ->
          uri_to_path ~uri
          >>| fun path ->
          Log.log ~section:`Server "Closed file %a" Path.pp path;
          CloseDocument path
      | Ok _ ->
          log_method_error request_method;
          None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/didOpen" -> (
      match DidOpenTextDocument.of_yojson request with
      | Ok
          {
            DidOpenTextDocument.parameters =
              Some { DidOpenTextDocumentParameters.textDocument = { TextDocumentItem.uri; _ }; _ };
            _;
          } ->
          uri_to_path ~uri
          >>| fun path ->
          Log.log ~section:`Server "Opened file %a" Path.pp path;
          OpenDocument path
      | Ok _ ->
          log_method_error request_method;
          None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/didChange" -> (
      match DidChangeTextDocument.of_yojson request with
      | Ok
          {
            DidChangeTextDocument.parameters =
              Some
                {
                  DidChangeTextDocumentParameters.textDocument =
                    { VersionedTextDocumentIdentifier.uri; _ };
                  contentChanges = content_changes;
                };
            _;
          } ->
          (* We only care about the last text update since we receive full text. *)
          Option.both
            (uri_to_path ~uri)
            (content_changes |> List.last >>| fun change -> change.text)
          >>| (fun (path, content) -> File.create ~content path)
          >>| fun file -> DocumentChange file
      | Ok _ ->
          log_method_error request_method;
          None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/didSave" -> (
      match DidSaveTextDocument.of_yojson request with
      | Ok
          {
            DidSaveTextDocument.parameters =
              Some
                {
                  DidSaveTextDocumentParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  _;
                };
            _;
          } ->
          uri_to_path ~uri >>| fun path -> SaveDocument path
      | Ok _ ->
          log_method_error request_method;
          None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/completion" -> (
      match CompletionRequest.of_yojson request with
      | Ok
          {
            CompletionRequest.parameters =
              Some { textDocument = { TextDocumentIdentifier.uri; _ }; position; _ };
            id;
            _;
          }
        when perform_autocompletion ->
          uri_to_path ~uri
          >>| fun path ->
          CompletionRequest
            { Protocol.CompletionRequest.id; path; position = to_pyre_position position }
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/hover" -> (
      match HoverRequest.of_yojson request with
      | Ok
          {
            HoverRequest.parameters =
              Some
                {
                  TextDocumentPositionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  position;
                };
            id;
            _;
          } ->
          uri_to_path ~uri
          >>| fun path ->
          HoverRequest { DefinitionRequest.id; path; position = to_pyre_position position }
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/codeAction" -> (
      match CodeActionRequest.of_yojson request with
      | Ok
          {
            CodeActionRequest.parameters =
              Some
                {
                  CodeActionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  context = { diagnostics; _ };
                  _;
                };
            id;
            _;
          } ->
          uri_to_path ~uri >>| fun path -> CodeActionRequest { id; uri; diagnostics; path }
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/typeCoverage" -> (
      match TypeCoverage.of_yojson request with
      | Ok
          {
            TypeCoverage.parameters =
              Some { TypeCoverageParameters.textDocument = { TextDocumentIdentifier.uri; _ }; _ };
            id;
            _;
          } ->
          uri_to_path ~uri >>| fun path -> TypeCoverageRequest { path; id }
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "workspace/executeCommand" -> (
      match ExecuteCommandRequest.of_yojson request with
      | Ok
          {
            ExecuteCommandRequest.parameters = Some { ExecuteCommandParameters.arguments; _ };
            id;
            _;
          } ->
          Some (ExecuteCommandRequest { id; arguments })
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "updateFiles" -> (
      match UpdateFiles.of_yojson request with
      | Ok { UpdateFiles.parameters = Some { files; invalidated = targets; _ }; _ } ->
          let files = List.map files ~f:string_to_path in
          if not (List.is_empty targets) then (
            Log.info "Invalidate %d symlinks" (List.length targets);
            List.iter targets ~f:(Hashtbl.remove symlink_targets_to_sources) );
          Some (TypeCheckRequest files)
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "displayTypeErrors" -> (
      match LanguageServer.Types.DisplayTypeErrors.of_yojson request with
      | Ok { LanguageServer.Types.DisplayTypeErrors.parameters = Some { files }; _ } ->
          let files = List.map files ~f:string_to_path in
          Some (DisplayTypeErrors files)
      | Ok _ -> Some (DisplayTypeErrors [])
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "shutdown" -> (
      match ShutdownRequest.of_yojson request with
      | Ok { ShutdownRequest.id; _ } -> Some (ClientShutdownRequest id)
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "exit" -> Some (ClientExitRequest Persistent)
    | "telemetry/rage" -> (
      match RageRequest.of_yojson request with
      | Ok { RageRequest.id; _ } -> Some (Request.RageRequest id)
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | unmatched_method ->
        Log.log ~section:`Server "Unhandled %s" unmatched_method;
        None
  in
  try
    let request_method = Yojson.Safe.Util.member "method" request in
    process_request (Yojson.Safe.Util.to_string request_method)
  with
  | Yojson.Safe.Util.Type_error _ -> None


type response = {
  state: State.t;
  response: Protocol.response option;
}

module AnnotationEdit = struct
  type t = {
    new_text: string;
    range: LanguageServer.Types.Range.t;
    title: string;
  }

  let range { range; _ } = range

  let new_text { new_text; _ } = new_text

  let title { title; _ } = title

  let is_replacement_edit kind =
    match kind with
    | Error.IncompatibleVariableType _
    | Error.IncompatibleReturnType _ ->
        true
    | _ -> false


  let create_range ~error:{ Error.kind = error_kind; location; _ } ~file =
    let token =
      match error_kind with
      | Error.MissingReturnAnnotation _ -> Some "):"
      | Error.MissingAttributeAnnotation { missing_annotation = { name; _ }; _ }
      | Error.MissingParameterAnnotation { name; _ }
      | Error.MissingGlobalAnnotation { name; _ } ->
          Some (Format.asprintf "%a" Reference.pp_sanitized name)
      | Error.IncompatibleReturnType { mismatch = { expected; _ }; _ } ->
          Some (Format.asprintf " -> %s" (Type.show expected))
      | Error.IncompatibleVariableType { name; mismatch = { expected; _ }; _ } ->
          Some (Format.asprintf "%a: %s" Reference.pp_sanitized name (Type.show expected))
      | _ -> None
    in
    let start_line =
      let line =
        match error_kind with
        | Error.IncompatibleReturnType { define_location; _ } -> Location.line define_location
        | Error.IncompatibleVariableType { declare_location; _ } -> Location.line declare_location
        | _ -> Location.line location
      in
      line - 1
    in
    let get_range lines token =
      List.findi lines ~f:(fun _ line -> Option.is_some (String.substr_index line ~pattern:token))
      >>| (fun (index, line) ->
            let position =
              {
                LanguageServer.Types.Position.line = index + start_line;
                character = String.substr_index_exn line ~pattern:token + 1;
              }
            in
            let end_ =
              match error_kind with
              | Error.IncompatibleVariableType _
              | Error.IncompatibleReturnType _
              | Error.MissingGlobalAnnotation _
              | Error.MissingAttributeAnnotation _ ->
                  let { LanguageServer.Types.Position.character; _ } = position in
                  { position with character = character + String.length token }
              | _ -> position
            in
            Some { LanguageServer.Types.Range.start = position; end_ })
      |> Option.value ~default:None
    in
    let lines = File.lines file in
    match token, lines with
    | Some token, Some lines ->
        let _, lines = List.split_n lines start_line in
        get_range lines token
    | _, _ -> None


  let create ~file ~error =
    error
    >>| (fun ({ Error.kind; _ } as error) ->
          let format_type annotation =
            Type.weaken_literals annotation |> Type.infer_transform |> Type.show
          in
          let new_text =
            match kind with
            | Error.MissingReturnAnnotation { annotation = Some annotation; _ } ->
                Some (" -> " ^ format_type annotation)
            | Error.MissingAttributeAnnotation
                { missing_annotation = { annotation = Some annotation; _ }; _ }
            | Error.MissingParameterAnnotation { annotation = Some annotation; _ }
            | Error.MissingGlobalAnnotation { annotation = Some annotation; _ } ->
                Some (": " ^ format_type annotation)
            | Error.IncompatibleReturnType { mismatch = { actual = annotation; _ }; _ } ->
                Some (Format.asprintf "-> %s:" @@ format_type annotation)
            | Error.IncompatibleVariableType { mismatch = { actual = annotation; _ }; _ } ->
                Some (Format.asprintf ": %s " @@ format_type annotation)
            | _ -> None
          in
          let range = create_range ~error ~file in
          let title =
            if is_replacement_edit kind then
              "Fix annotation"
            else
              "Add annotation"
          in
          match range, new_text with
          | Some range, Some new_text -> Some { new_text; range; title }
          | _, _ -> None)
    |> Option.value ~default:None
end

let process_client_shutdown_request ~state ~id =
  let open LanguageServer.Protocol in
  let response =
    ShutdownResponse.default id |> ShutdownResponse.to_yojson |> Yojson.Safe.to_string
  in
  { state; response = Some (LanguageServerProtocolResponse response) }


let process_type_query_request
    ~state:({ State.module_tracker; environment; _ } as state)
    ~configuration
    ~request
  =
  let process_request () =
    let global_resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
    let order = GlobalResolution.class_hierarchy global_resolution in
    let resolution = TypeCheck.resolution global_resolution () in
    let parse_and_validate ?(unknown_is_top = false) expression =
      let annotation =
        (* Return untracked so we can specifically message the user about them. *)
        GlobalResolution.parse_annotation
          ~allow_untracked:true
          ~allow_invalid_type_parameters:true
          global_resolution
          expression
      in
      let annotation =
        if unknown_is_top then
          let constraints = function
            | Type.Primitive "unknown" -> Some Type.Top
            | _ -> None
          in
          Type.instantiate annotation ~constraints
        else
          annotation
      in
      if ClassHierarchy.is_instantiated order annotation then
        let mismatches, _ =
          GlobalResolution.check_invalid_type_parameters global_resolution annotation
        in
        if List.is_empty mismatches then
          annotation
        else
          raise (IncorrectParameters annotation)
      else
        raise (ClassHierarchy.Untracked annotation)
    in
    let class_metadata_environment =
      AnnotatedGlobalEnvironment.ReadOnly.class_metadata_environment environment
    in
    let class_hierarchy_environment =
      ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment class_metadata_environment
    in
    let alias_environment =
      ClassHierarchyEnvironment.ReadOnly.alias_environment class_hierarchy_environment
    in
    let unannotated_global_environment =
      AliasEnvironment.ReadOnly.unannotated_global_environment alias_environment
    in
    match request with
    | TypeQuery.RunCheck { check_name; paths } ->
        let source_paths =
          List.filter_map
            paths
            ~f:(Analysis.ModuleTracker.lookup_path ~configuration module_tracker)
        in
        let errors =
          IncrementalStaticAnalysis.run_additional_check
            ~configuration
            ~scheduler:state.scheduler
            ~environment
            ~source_paths
            ~check:check_name
        in
        TypeQuery.Response (TypeQuery.Errors errors)
    | TypeQuery.Attributes annotation ->
        let to_attribute { Node.value = { Annotated.Class.Attribute.name; annotation; _ }; _ } =
          { TypeQuery.name; annotation }
        in
        parse_and_validate (Expression.from_reference ~location:Location.Reference.any annotation)
        |> GlobalResolution.class_definition global_resolution
        >>| Annotated.Class.create
        >>| (fun annotated_class ->
              Annotated.Class.attributes ~resolution:global_resolution annotated_class)
        >>| List.map ~f:to_attribute
        >>| (fun attributes -> TypeQuery.Response (TypeQuery.FoundAttributes attributes))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | TypeQuery.Callees caller ->
        TypeQuery.Response
          (TypeQuery.Callees
             ( Dependencies.Callgraph.get ~caller
             |> List.map ~f:(fun { Dependencies.Callgraph.callee; _ } -> callee) ))
    | TypeQuery.CalleesWithLocation caller ->
        let instantiate =
          Location.instantiate
            ~lookup:
              (AstEnvironment.ReadOnly.get_real_path_relative
                 ~configuration
                 (AstEnvironment.read_only state.ast_environment))
        in
        let callees =
          Dependencies.Callgraph.get ~caller
          |> List.map ~f:(fun { Dependencies.Callgraph.callee; locations } ->
                 { TypeQuery.callee; locations = List.map locations ~f:instantiate })
        in
        TypeQuery.Response (TypeQuery.CalleesWithLocation callees)
    | TypeQuery.ComputeHashesToKeys ->
        (* Type order. *)
        let extend_map map ~new_map =
          Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
        in
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        (* Environments *)
        let map =
          List.fold
            [
              ClassMetadataEnvironment.ReadOnly.hash_to_key_map class_metadata_environment;
              ClassHierarchyEnvironment.ReadOnly.hash_to_key_map class_hierarchy_environment;
              AliasEnvironment.ReadOnly.hash_to_key_map alias_environment;
              UnannotatedGlobalEnvironment.ReadOnly.hash_to_key_map unannotated_global_environment;
            ]
            ~f:(fun sofar new_map -> extend_map sofar ~new_map)
            ~init:(AnnotatedGlobalEnvironment.ReadOnly.hash_to_key_map environment)
        in
        (* AST shared memory. *)
        let map =
          map |> extend_map ~new_map:(AstEnvironment.shared_memory_hash_to_key_map qualifiers)
        in
        (* Resolution shared memory. *)
        let map =
          let keys = ResolutionSharedMemory.get_keys ~qualifiers in
          map
          |> extend_map ~new_map:(ResolutionSharedMemory.compute_hashes_to_keys ~keys)
          |> extend_map
               ~new_map:(ResolutionSharedMemory.Keys.compute_hashes_to_keys ~keys:qualifiers)
        in
        (* Coverage. *)
        let map =
          extend_map map ~new_map:(Coverage.SharedMemory.compute_hashes_to_keys ~keys:qualifiers)
        in
        (* TODO (T56904923): Track the CallGraph table consistency *)
        map
        |> Map.to_alist
        |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
        |> List.map ~f:(fun (hash, key) -> { TypeQuery.hash; key })
        |> fun response -> TypeQuery.Response (TypeQuery.FoundKeyMapping response)
    | TypeQuery.CoverageInFile path ->
        let default =
          TypeQuery.Error (Format.asprintf "Not able to get lookups in `%a`" Path.pp path)
        in
        let map_to_coverage (location, annotation) =
          let coverage =
            if Type.is_partially_typed annotation then
              TypeQuery.Partial
            else if Type.is_untyped annotation then
              TypeQuery.Untyped
            else
              TypeQuery.Typed
          in
          { location; TypeQuery.coverage }
        in
        LookupCache.find_all_annotations ~state ~configuration ~path
        >>| List.map ~f:map_to_coverage
        >>| (fun list -> TypeQuery.Response (TypeQuery.CoverageAtLocations list))
        |> Option.value ~default
    | TypeQuery.DecodeOcamlValues values ->
        let decode key value =
          let key, value = Base64.decode key, Base64.decode value in
          match key, value with
          | Ok key, Ok value -> (
            match Memory.decode ~key ~value with
            | Ok decoded -> Some decoded
            | _ -> None )
          | _ -> None
        in
        let serialize_decoded decoded =
          match decoded with
          | Coverage.SharedMemory.Decoded (key, value) ->
              Some (Coverage.CoverageValue.description, Reference.show key, value >>| Coverage.show)
          | Analysis.Dependencies.Callgraph.SharedMemory.Decoded (key, value) ->
              let show { Dependencies.Callgraph.callee; locations } =
                Format.asprintf
                  "%s: [%s]"
                  (Dependencies.Callgraph.show_callee callee)
                  (List.map locations ~f:Location.Reference.show |> String.concat ~sep:", ")
              in
              Some
                ( Dependencies.Callgraph.CalleeValue.description,
                  Reference.show key,
                  value >>| List.map ~f:show >>| String.concat ~sep:"," )
          | ResolutionSharedMemory.Decoded (key, value) ->
              Some
                ( ResolutionSharedMemory.TypeAnnotationsValue.description,
                  Reference.show key,
                  value >>| LocalAnnotationMap.show )
          | ResolutionSharedMemory.Keys.Decoded (key, value) ->
              Some
                ( ResolutionSharedMemory.AnnotationsKeyValue.description,
                  Reference.show key,
                  value >>| List.map ~f:Reference.show >>| String.concat ~sep:"," )
          | _ ->
              List.find_map
                [
                  AnnotatedGlobalEnvironment.ReadOnly.serialize_decoded environment;
                  ClassMetadataEnvironment.ReadOnly.serialize_decoded class_metadata_environment;
                  ClassHierarchyEnvironment.ReadOnly.serialize_decoded class_hierarchy_environment;
                  AliasEnvironment.ReadOnly.serialize_decoded alias_environment;
                  UnannotatedGlobalEnvironment.ReadOnly.serialize_decoded
                    unannotated_global_environment;
                  AstEnvironment.serialize_decoded;
                ]
                ~f:(fun serialize -> serialize decoded)
        in
        let build_response { TypeQuery.decoded; undecodable_keys } = function
          | TypeQuery.SerializedValue { serialized_key; serialized_value } -> (
              let serialized = decode serialized_key serialized_value >>= serialize_decoded in
              match serialized with
              | Some (kind, key, value) ->
                  let decoded_value =
                    TypeQuery.DecodedValue
                      { serialized_key; kind; actual_key = key; actual_value = value }
                  in
                  { TypeQuery.decoded = decoded_value :: decoded; undecodable_keys }
              | None ->
                  { TypeQuery.decoded; undecodable_keys = serialized_key :: undecodable_keys } )
          | TypeQuery.SerializedPair
              { serialized_key; first_serialized_value; second_serialized_value } -> (
              let first_decoded = decode serialized_key first_serialized_value in
              let second_decoded = decode serialized_key second_serialized_value in
              match first_decoded, second_decoded with
              | Some first, Some second -> (
                  let equal =
                    match first, second with
                    | ( Coverage.SharedMemory.Decoded (_, first),
                        Coverage.SharedMemory.Decoded (_, second) ) ->
                        Option.equal Coverage.equal first second
                    | ( ResolutionSharedMemory.Decoded (_, first),
                        ResolutionSharedMemory.Decoded (_, second) ) ->
                        Option.equal LocalAnnotationMap.equal first second
                    | _ ->
                        List.find_map
                          [
                            AnnotatedGlobalEnvironment.ReadOnly.decoded_equal environment;
                            ClassMetadataEnvironment.ReadOnly.decoded_equal
                              class_metadata_environment;
                            ClassHierarchyEnvironment.ReadOnly.decoded_equal
                              class_hierarchy_environment;
                            AliasEnvironment.ReadOnly.decoded_equal alias_environment;
                            UnannotatedGlobalEnvironment.ReadOnly.decoded_equal
                              unannotated_global_environment;
                            AstEnvironment.decoded_equal;
                          ]
                          ~f:(fun decoded_equal -> decoded_equal first second)
                        |> Option.value ~default:false
                  in
                  match serialize_decoded first, serialize_decoded second with
                  | Some (kind, key, first_value), Some (_, _, second_value) ->
                      let value =
                        TypeQuery.DecodedPair
                          {
                            serialized_key;
                            kind;
                            actual_key = key;
                            first_value;
                            second_value;
                            equal;
                          }
                      in
                      { TypeQuery.decoded = value :: decoded; undecodable_keys }
                  | _ ->
                      { TypeQuery.decoded; undecodable_keys = serialized_key :: undecodable_keys }
                  )
              | _ -> { TypeQuery.decoded; undecodable_keys = serialized_key :: undecodable_keys } )
        in
        let decoded =
          List.fold
            values
            ~init:{ TypeQuery.decoded = []; undecodable_keys = [] }
            ~f:build_response
        in
        TypeQuery.Response (TypeQuery.Decoded decoded)
    | TypeQuery.DependentDefines paths ->
        let modules =
          List.filter_map paths ~f:(ModuleTracker.lookup_path ~configuration module_tracker)
          |> List.map ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
        in
        let ast_environment = AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment in
        let legacy_dependency_tracker = Dependencies.create ast_environment in
        let dependency_set = Dependencies.transitive_of_list legacy_dependency_tracker ~modules in
        let dependencies =
          let source_to_define_name source =
            let define_to_name { Statement.Define.signature = { name; _ }; _ } = name in
            define_to_name (Source.top_level_define source)
          in
          Reference.Set.to_list dependency_set
          |> List.filter_map ~f:(AstEnvironment.ReadOnly.get_source ast_environment)
          |> List.map ~f:source_to_define_name
        in
        TypeQuery.Response (TypeQuery.References dependencies)
    | TypeQuery.DumpCallGraph ->
        let get_callgraph module_qualifier =
          let callees
              {
                Node.value =
                  {
                    Statement.Define.signature = { Statement.Define.Signature.name = caller; _ };
                    _;
                  };
                _;
              }
            =
            let instantiate =
              Location.instantiate
                ~lookup:
                  (AstEnvironment.ReadOnly.get_real_path_relative
                     ~configuration
                     (AstEnvironment.read_only state.ast_environment))
            in
            Dependencies.Callgraph.get ~caller
            |> List.map ~f:(fun { Dependencies.Callgraph.callee; locations } ->
                   { TypeQuery.callee; locations = List.map locations ~f:instantiate })
            |> fun callees -> { Protocol.TypeQuery.caller; callees }
          in
          let ast_environment = Analysis.AstEnvironment.read_only state.ast_environment in
          AstEnvironment.ReadOnly.get_source ast_environment module_qualifier
          >>| Preprocessing.defines ~include_toplevels:false ~include_stubs:false
          >>| List.map ~f:callees
          |> Option.value ~default:[]
        in
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        TypeQuery.Response (TypeQuery.Callgraph (List.concat_map qualifiers ~f:get_callgraph))
    | TypeQuery.DumpClassHierarchy ->
        let resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
        let class_hierarchy_json =
          let indices =
            Analysis.AnnotatedGlobalEnvironment.ReadOnly.class_metadata_environment environment
            |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
            |> ClassHierarchyEnvironment.ReadOnly.alias_environment
            |> AliasEnvironment.ReadOnly.unannotated_global_environment
            |> Analysis.UnannotatedGlobalEnvironment.ReadOnly.all_indices
          in
          ClassHierarchy.to_json (GlobalResolution.class_hierarchy resolution) ~indices
        in
        TypeQuery.Response (TypeQuery.ClassHierarchy class_hierarchy_json)
    | TypeQuery.DumpDependencies path ->
        let () =
          match ModuleTracker.lookup_path ~configuration module_tracker path with
          | None -> ()
          | Some { SourcePath.qualifier; _ } ->
              let ast_environment =
                AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment
              in
              let legacy_dependency_tracker = Dependencies.create ast_environment in
              Path.create_relative
                ~root:(Configuration.Analysis.log_directory configuration)
                ~relative:"dependencies.dot"
              |> File.create ~content:(Dependencies.to_dot legacy_dependency_tracker ~qualifier)
              |> File.write
        in
        TypeQuery.Response (TypeQuery.Success "Dependencies dumped.")
    | TypeQuery.DumpMemoryToSqlite path ->
        let path = Path.absolute path in
        let () =
          try Unix.unlink path with
          | Unix.Unix_error _ -> ()
        in
        let timer = Timer.start () in
        (* Normalize the environment for comparison. *)
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        let ast_environment = AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment in
        let legacy_dependency_tracker = Dependencies.create ast_environment in
        Dependencies.normalize legacy_dependency_tracker qualifiers;
        Memory.SharedMemory.save_table_sqlite path |> ignore;
        let { Memory.SharedMemory.used_slots; _ } = Memory.SharedMemory.hash_stats () in
        Log.info
          "Dumped %d slots in %.2f seconds to %s"
          used_slots
          (Timer.stop timer |> Time.Span.to_sec)
          path;
        TypeQuery.Response (TypeQuery.Path (Path.create_absolute path))
    | TypeQuery.IsCompatibleWith (left, right) ->
        (* We need a special version of parse_and_validate to handle the "unknown" type that
           Monkeycheck may send us *)
        let left = parse_and_validate ~unknown_is_top:true left in
        let right = parse_and_validate ~unknown_is_top:true right in
        let right =
          match Type.coroutine_value right with
          | None -> right
          | Some unwrapped -> unwrapped
        in
        GlobalResolution.is_compatible_with global_resolution ~left ~right
        |> fun result ->
        TypeQuery.Response (TypeQuery.Compatibility { actual = left; expected = right; result })
    | TypeQuery.Join (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.join global_resolution left right
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.less_or_equal global_resolution ~left ~right
        |> fun response -> TypeQuery.Response (TypeQuery.Boolean response)
    | TypeQuery.Meet (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.meet global_resolution left right
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.Methods annotation ->
        let to_method = function
          | {
              Node.value =
                {
                  Annotated.Attribute.annotation =
                    Callable
                      {
                        implementation = { annotation; parameters = Defined parameters; _ };
                        kind = Named name;
                        _;
                      };
                  _;
                };
              _;
            } ->
              let parameters =
                parameters
                |> List.filter_map ~f:Type.Callable.Parameter.annotation
                |> fun parameters -> Type.Primitive "self" :: parameters
              in
              let return_annotation = annotation in
              Some { TypeQuery.name = Reference.last name; parameters; return_annotation }
          | _ -> None
        in
        parse_and_validate (Expression.from_reference ~location:Location.Reference.any annotation)
        |> GlobalResolution.class_definition global_resolution
        >>| Annotated.Class.create
        >>| Annotated.Class.attributes ~resolution:global_resolution
        >>| List.filter_map ~f:to_method
        >>| (fun methods -> TypeQuery.Response (TypeQuery.FoundMethods methods))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | TypeQuery.NormalizeType expression ->
        parse_and_validate expression
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.PathOfModule module_name ->
        ModuleTracker.lookup_source_path module_tracker module_name
        >>= (fun source_path ->
              let path = SourcePath.full_path ~configuration source_path |> Path.absolute in
              Some (TypeQuery.Response (TypeQuery.FoundPath path)))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No path found for module `%s`" (Reference.show module_name)))
    | TypeQuery.SaveServerState path ->
        let path = Path.absolute path in
        Log.info "Saving server state into `%s`" path;
        Memory.save_shared_memory ~path;
        TypeQuery.Response (TypeQuery.Success (Format.sprintf "Saved state."))
    | TypeQuery.Signature function_name -> (
        let keep_known_annotation annotation =
          match annotation with
          | Type.Top -> None
          | _ -> Some annotation
        in
        match GlobalResolution.global global_resolution function_name with
        | Some { Node.value; _ } -> (
          match Annotation.annotation value with
          | Type.Callable { Type.Callable.implementation; overloads; _ } ->
              let overload_signature { Type.Callable.annotation; parameters; _ } =
                match parameters with
                | Type.Callable.Defined parameters ->
                    let format parameter =
                      match parameter with
                      | Type.Callable.Parameter.Named { name; annotation; _ } ->
                          let name = Identifier.sanitized name in
                          Some
                            {
                              TypeQuery.parameter_name = name;
                              annotation = keep_known_annotation annotation;
                            }
                      | _ -> None
                    in
                    let parameters = List.filter_map ~f:format parameters in
                    Some { TypeQuery.return_type = keep_known_annotation annotation; parameters }
                | _ -> None
              in
              TypeQuery.Response
                (TypeQuery.FoundSignature
                   (List.filter_map (implementation :: overloads) ~f:overload_signature))
          | _ ->
              TypeQuery.Error
                (Format.sprintf "%s is not a callable" (Reference.show function_name)) )
        | None ->
            TypeQuery.Error
              (Format.sprintf "No signature found for %s" (Reference.show function_name)) )
    | TypeQuery.Superclasses annotation ->
        parse_and_validate annotation
        |> GlobalResolution.class_definition global_resolution
        >>| Annotated.Class.create
        >>| Annotated.Class.superclasses ~resolution:global_resolution
        >>| List.map ~f:Annotated.Class.annotation
        >>| (fun classes -> TypeQuery.Response (TypeQuery.Superclasses classes))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Expression.show annotation)))
    | TypeQuery.Type expression -> (
        let define =
          Statement.Define.create_toplevel ~qualifier:None ~statements:[]
          |> Node.create_with_default_location
        in
        let module State = TypeCheck.State (struct
          let configuration = configuration

          let define = define

          module Builder = Dependencies.Callgraph.DefaultBuilder
        end)
        in
        let state = State.create ~resolution () in
        let { State.state; resolved = annotation; _ } =
          State.forward_expression ~state ~expression
        in
        match State.errors state with
        | [] -> TypeQuery.Response (TypeQuery.Type annotation)
        | errors ->
            let descriptions =
              let lookup reference =
                let ast_environment =
                  AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment
                in
                AstEnvironment.ReadOnly.get_real_path_relative
                  ~configuration
                  ast_environment
                  reference
              in
              errors
              |> List.map ~f:(Analysis.Error.instantiate ~lookup)
              |> List.map ~f:(Analysis.Error.Instantiated.description ~show_error_traces:false)
              |> String.concat ~sep:", "
            in
            TypeQuery.Error (Format.sprintf "Expression had errors: %s" descriptions) )
    | TypeQuery.TypeAtPosition { path; position } ->
        let default =
          TypeQuery.Error
            (Format.asprintf
               "Not able to get lookup at %a:%a"
               Path.pp
               path
               Location.pp_position
               position)
        in
        LookupCache.find_annotation ~state ~configuration ~path ~position
        >>| (fun (location, annotation) ->
              TypeQuery.Response (TypeQuery.TypeAtLocation { TypeQuery.location; annotation }))
        |> Option.value ~default
    | TypeQuery.TypesInFiles paths ->
        let annotations = LookupCache.find_all_annotations_batch ~state ~configuration ~paths in
        let create_result = function
          | { LookupCache.path; types_by_location = Some types } ->
              `Fst { TypeQuery.path; types = List.map ~f:TypeQuery.create_type_at_location types }
          | { LookupCache.path; _ } -> `Snd path
        in
        let results, errors = List.partition_map ~f:create_result annotations in
        if List.is_empty errors then
          TypeQuery.Response (TypeQuery.TypesByFile results)
        else
          let paths =
            List.fold
              ~init:""
              ~f:(fun sofar path -> Format.asprintf "%s\n\t`%a`" sofar PyrePath.pp path)
              errors
          in
          TypeQuery.Error (Format.asprintf "Not able to get lookups in: %s" paths)
    | TypeQuery.ValidateTaintModels path -> (
      try
        let directories =
          match path with
          | Some path -> [path]
          | None -> configuration.Configuration.Analysis.taint_models_directories
        in
        let configuration = Taint.TaintConfiguration.create ~rule_filter:None ~directories in
        let create_models sources =
          let create_model (path, source) =
            Taint.Model.parse
              ~resolution:(TypeCheck.resolution global_resolution ())
              ~path
              ~verify:true
              ~source
              ~configuration
              Interprocedural.Callable.Map.empty
            |> ignore
          in
          List.iter sources ~f:create_model
        in
        Taint.Model.get_model_sources ~directories |> create_models;
        TypeQuery.Response
          (TypeQuery.Success
             (Format.asprintf
                "Models in `%s` are valid."
                (directories |> List.map ~f:Path.show |> String.concat ~sep:", ")))
      with
      | error -> TypeQuery.Error (Exn.to_string error) )
  in
  let response =
    try process_request () with
    | ClassHierarchy.Untracked untracked ->
        let untracked_response =
          Format.asprintf "Type `%a` was not found in the type order." Type.pp untracked
        in
        TypeQuery.Error untracked_response
    | IncorrectParameters untracked ->
        let untracked_response =
          Format.asprintf "Type `%a` has the wrong number of parameters." Type.pp untracked
        in
        TypeQuery.Error untracked_response
  in
  { state; response = Some (TypeQueryResponse response) }


let process_type_check_request ~state ~configuration paths =
  let state, response = IncrementalCheck.recheck ~state ~configuration paths in
  let response = List.map response ~f:(instantiate_error ~configuration ~state) in
  { state; response = Some (TypeCheckResponse response) }


let process_display_type_errors_request ~state ~configuration paths =
  let errors =
    let { errors; _ } = state in
    match paths with
    | [] -> Hashtbl.data errors |> List.concat |> List.sort ~compare:Error.compare
    | _ -> List.concat_map ~f:(errors_of_path ~configuration ~state) paths
  in
  let errors = List.map errors ~f:(instantiate_error ~configuration ~state) in
  { state; response = Some (TypeCheckResponse errors) }


let process_get_definition_request
    ~state:({ State.environment; _ } as state)
    ~configuration
    ~request:{ DefinitionRequest.id; path; position }
  =
  let response =
    let ast_environment = AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment in
    let open LanguageServer.Protocol in
    let response =
      match LookupCache.find_definition ~state ~configuration path position with
      | None -> TextDocumentDefinitionResponse.create_empty ~id
      | Some { Location.start; stop; path } -> (
        match AstEnvironment.ReadOnly.get_source_path ast_environment path with
        | None -> TextDocumentDefinitionResponse.create_empty ~id
        | Some source_path ->
            let path = SourcePath.full_path ~configuration source_path in
            TextDocumentDefinitionResponse.create ~id ~start ~stop ~path )
    in
    TextDocumentDefinitionResponse.to_yojson response
    |> Yojson.Safe.to_string
    |> (fun response -> LanguageServerProtocolResponse response)
    |> Option.some
  in
  { state; response }


let rec process
    ~state:({ State.module_tracker; connections; _ } as state)
    ~configuration:({ configuration; _ } as server_configuration)
    ~request
  =
  let timer = Timer.start () in
  let log_request_error ~error =
    Statistics.event
      ~section:`Error
      ~name:"request error"
      ~normals:["request", Request.show request; "error", error]
      ~flush:true
      ()
  in
  let result =
    try
      match request with
      | TypeCheckRequest paths ->
          SharedMem.collect `aggressive;
          process_type_check_request ~state ~configuration paths
      | StopRequest ->
          Mutex.critical_section connections.lock ~f:(fun () ->
              Operations.stop ~reason:"explicit request" ~configuration:server_configuration)
      | TypeQueryRequest request -> process_type_query_request ~state ~configuration ~request
      | DisplayTypeErrors paths ->
          let configuration = { configuration with include_hints = true } in
          process_display_type_errors_request ~state ~configuration paths
      | LanguageServerProtocolRequest request ->
          parse_lsp ~configuration ~state ~request:(Yojson.Safe.from_string request)
          >>| (fun request -> process ~state ~configuration:server_configuration ~request)
          |> Option.value ~default:{ state; response = None }
      | ClientShutdownRequest id -> process_client_shutdown_request ~state ~id
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
      | CompletionRequest { CompletionRequest.id; path; position = cursor_position; _ } ->
          let completion_items =
            AutoComplete.get_completion_items ~state ~configuration ~path ~cursor_position
          in
          let response =
            LanguageServer.Protocol.CompletionResponse.create ~id ~items:completion_items
            |> LanguageServer.Protocol.CompletionResponse.to_yojson
            |> Yojson.Safe.to_string
            |> fun response -> Some (LanguageServerProtocolResponse response)
          in
          { state; response }
      | HoverRequest { DefinitionRequest.id; path; position } ->
          let response =
            let open LanguageServer.Protocol in
            let result =
              LookupCache.find_annotation ~state ~configuration ~path ~position
              >>| fun (location, annotation) ->
              { HoverResponse.location; contents = Type.show_for_hover annotation }
            in
            HoverResponse.create ~id ~result
            |> HoverResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | CodeActionRequest { id; diagnostics; uri; path } ->
          let is_range_equal_location
              { LanguageServer.Types.Range.start = range_start; end_ }
              { Location.start = location_start; stop; _ }
            =
            let compare_position
                { LanguageServer.Types.Position.line = range_line; character }
                { Location.line = location_line; column }
              =
              Int.equal (range_line + 1) location_line && Int.equal character column
            in
            compare_position range_start location_start && compare_position end_ stop
          in
          let response =
            let open LanguageServer.Protocol in
            let { Configuration.Server.server_uuid; _ } = server_configuration in
            let code_actions =
              diagnostics
              |> List.filter_map
                   ~f:(fun (LanguageServer.Types.Diagnostic.{ range; _ } as diagnostic) ->
                     let error =
                       List.find
                         (errors_of_path ~configuration ~state path)
                         ~f:(fun { location; _ } -> is_range_equal_location range location)
                     in
                     AnnotationEdit.create ~file:(File.create path) ~error
                     >>| (fun edit ->
                           Some
                             {
                               LanguageServer.Types.CodeAction.diagnostics = Some [diagnostic];
                               command =
                                 Some
                                   {
                                     title = "Fix it";
                                     command = "add_pyre_annotation_" ^ server_uuid;
                                     arguments =
                                       [
                                         {
                                           range = AnnotationEdit.range edit;
                                           newText = AnnotationEdit.new_text edit;
                                           uri;
                                         };
                                       ];
                                   };
                               title = AnnotationEdit.title edit;
                               kind = Some "refactor.rewrite";
                             })
                     |> Option.value ~default:None)
            in
            CodeActionResponse.create ~id ~code_actions
            |> CodeActionResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | ExecuteCommandRequest { arguments; id } ->
          let response =
            List.hd arguments
            >>| (fun { uri; newText; range } ->
                  let edit =
                    {
                      LanguageServer.Types.WorkspaceEdit.changes =
                        Some { uri; textEdit = [{ newText; range }] };
                    }
                  in
                  LanguageServer.Protocol.ApplyWorkspaceEdit.create ~id edit
                  |> LanguageServer.Protocol.ApplyWorkspaceEdit.to_yojson
                  |> Yojson.Safe.to_string
                  |> (fun response -> LanguageServerProtocolResponse response)
                  |> Option.some)
            |> Option.value ~default:None
          in
          { state; response }
      | OpenDocument path ->
          (* Make sure cache is fresh. We might not have received a close notification. *)
          LookupCache.evict_path ~state ~configuration path;

          (* Make sure the IDE flushes its state about this file, by sending back all the errors
             for this file. *)
          let { State.open_documents; _ } = state in
          let _ =
            match ModuleTracker.lookup_path ~configuration module_tracker path with
            | Some { SourcePath.qualifier; _ } ->
                Reference.Table.set
                  open_documents
                  ~key:qualifier
                  ~data:(File.create path |> File.content |> Option.value ~default:"")
            | _ -> ()
          in
          (* We do not recheck dependencies because nothing changes when we open a document, and we
             do the type checking here just to make type checking resolution appear in shared
             memory. *)
          process_type_check_request
            ~state
            ~configuration:
              { configuration with Configuration.Analysis.incremental_style = Shallow }
            [path]
      | CloseDocument path ->
          let { State.open_documents; _ } = state in
          let _ =
            match ModuleTracker.lookup_path ~configuration module_tracker path with
            | Some { SourcePath.qualifier; _ } -> Reference.Table.remove open_documents qualifier
            | _ -> ()
          in
          LookupCache.evict_path ~state ~configuration path;
          { state; response = None }
      | DocumentChange file ->
          (* On change, update open document's content but do not trigger recheck. *)
          let { State.open_documents; _ } = state in
          let _ =
            match ModuleTracker.lookup_path ~configuration module_tracker (File.path file) with
            | Some { SourcePath.qualifier; _ } ->
                Reference.Table.set
                  open_documents
                  ~key:qualifier
                  ~data:(File.content file |> Option.value ~default:"")
            | _ -> ()
          in
          { state; response = None }
      | SaveDocument path ->
          if Random.bool () then
            Telemetry.send_telemetry () ~f:(fun _ ->
                let { Configuration.Server.server_uuid; _ } = server_configuration in
                let { Configuration.Analysis.local_root; _ } = configuration in
                let file_path = Path.absolute path in
                let content =
                  In_channel.with_file ~binary:false file_path ~f:In_channel.input_all
                in
                let relative_path =
                  match Path.get_relative_to_root ~root:local_root ~path with
                  | Some relative -> relative
                  | None -> file_path
                in
                {
                  Telemetry.Message.uuid = server_uuid;
                  message = Telemetry.Message.Update { path = relative_path; content };
                });

          (* On save, evict entries from the lookup cache. The updated source will be picked up at
             the next lookup (if any). *)
          LookupCache.evict_path ~state ~configuration path;
          let check_on_save =
            Mutex.critical_section connections.lock ~f:(fun () ->
                let { file_notifiers; _ } = !(connections.connections) in
                List.is_empty file_notifiers)
          in
          if check_on_save then
            let configuration = { configuration with include_hints = true } in
            process_type_check_request ~state ~configuration [path]
          else (
            Log.log ~section:`Server "Explicitly ignoring didSave request";
            { state; response = None } )
      | TypeCoverageRequest { path; id } ->
          let response =
            ModuleTracker.lookup_path ~configuration module_tracker path
            >>= fun { SourcePath.qualifier; _ } ->
            match Coverage.get ~qualifier with
            | Some { Coverage.full; partial; untyped; _ } ->
                let total = Float.of_int (full + partial + untyped) in
                let covered_percent =
                  if total > 0.0 then
                    Int.of_float (Float.of_int full /. total *. 100.0)
                  else
                    0
                in
                LanguageServer.Protocol.TypeCoverageResponse.create ~id ~covered_percent
                |> LanguageServer.Protocol.TypeCoverageResponse.to_yojson
                |> Yojson.Safe.to_string
                |> fun response -> Some (LanguageServerProtocolResponse response)
            | _ -> None
          in
          { state; response }
      | GetServerUuid ->
          let { Configuration.Server.server_uuid; _ } = server_configuration in
          { state; response = Some (ServerUuidResponse server_uuid) }
      (* Requests that cannot be fulfilled here. *)
      | ClientConnectionRequest _ ->
          Log.warning "Explicitly ignoring ClientConnectionRequest request";
          { state; response = None }
    with
    | Unix.Unix_error (kind, name, parameters) ->
        Log.log_unix_error (kind, name, parameters);
        log_request_error
          ~error:(Format.sprintf "Unix error %s: %s(%s)" (Unix.error_message kind) name parameters);
        { state; response = None }
    | Analysis.ClassHierarchy.Untracked annotation ->
        log_request_error ~error:(Format.sprintf "Untracked %s" (Type.show annotation));
        { state; response = None }
    | uncaught_exception ->
        let should_stop =
          match request with
          | HoverRequest _
          | GetDefinitionRequest _ ->
              false
          | _ -> true
        in
        Statistics.log_exception uncaught_exception ~fatal:should_stop ~origin:"server";
        if should_stop then
          Mutex.critical_section connections.lock ~f:(fun () ->
              Operations.stop ~reason:"uncaught exception" ~configuration:server_configuration);
        { state; response = None }
  in
  Statistics.performance
    ~name:"server request"
    ~timer
    ~normals:["request kind", Request.name request]
    ();
  result
