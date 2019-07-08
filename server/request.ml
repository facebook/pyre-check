(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ServerDependencies = Dependencies
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
    match SearchPath.search_for_path ~search_path path with
    | Some SearchPath.{ relative_path; _ } -> Some (Path.Relative relative_path)
    | None ->
        Ast.SharedMemory.SymlinksToPaths.get (Path.absolute path)
        >>= fun path ->
        SearchPath.search_for_path ~search_path path
        >>| fun SearchPath.{ relative_path; _ } -> Path.Relative relative_path
  in
  let string_path_to_file string_path =
    File.create (Path.create_absolute ~follow_symbolic_links:false string_path)
  in
  let process_request request_method =
    match request_method with
    | "textDocument/definition" -> (
      match TextDocumentDefinitionRequest.of_yojson request with
      | Ok
          { TextDocumentDefinitionRequest.parameters =
              Some
                { TextDocumentPositionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  position
                };
            id;
            _
          } ->
          uri_to_path ~uri
          >>| File.create
          >>| fun file ->
          GetDefinitionRequest { DefinitionRequest.id; file; position = to_pyre_position position }
      | Ok _ -> None
      | Error yojson_error ->
          Log.dump "%s" yojson_error;
          None )
    | "textDocument/didClose" -> (
      match DidCloseTextDocument.of_yojson request with
      | Ok
          { DidCloseTextDocument.parameters =
              Some
                { DidCloseTextDocumentParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  _
                };
            _
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
          None )
    | "textDocument/didOpen" -> (
      match DidOpenTextDocument.of_yojson request with
      | Ok
          { DidOpenTextDocument.parameters =
              Some { DidOpenTextDocumentParameters.textDocument = { TextDocumentItem.uri; _ }; _ };
            _
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
          None )
    | "textDocument/didChange" -> (
      match DidChangeTextDocument.of_yojson request with
      | Ok
          { DidChangeTextDocument.parameters =
              Some
                { DidChangeTextDocumentParameters.textDocument =
                    { VersionedTextDocumentIdentifier.uri; _ };
                  contentChanges = content_changes
                };
            _
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
          { DidSaveTextDocument.parameters =
              Some
                { DidSaveTextDocumentParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  text
                };
            _
          } ->
          uri_to_path ~uri >>| File.create ?content:text >>| fun file -> SaveDocument file
      | Ok _ ->
          log_method_error request_method;
          None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/completion" -> (
      match CompletionRequest.of_yojson request with
      | Ok
          { CompletionRequest.parameters =
              Some
                { textDocument = { TextDocumentIdentifier.uri; _ };
                  position;
                  context = Some { triggerKind; triggerCharacter = Some "." };
                  _
                };
            id;
            _
          } ->
          let open CompletionParameters in
          if triggerKindNumber TriggerCharacter <> triggerKind then
            None
          else
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
          { HoverRequest.parameters =
              Some
                { TextDocumentPositionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  position
                };
            id;
            _
          } ->
          uri_to_path ~uri
          >>| File.create
          >>| fun file ->
          HoverRequest { DefinitionRequest.id; file; position = to_pyre_position position }
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/codeAction" -> (
      match CodeActionRequest.of_yojson request with
      | Ok
          { CodeActionRequest.parameters =
              Some
                { CodeActionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                  context = { diagnostics; _ };
                  _
                };
            id;
            _
          } ->
          uri_to_path ~uri
          >>| File.create
          >>| fun file -> CodeActionRequest { id; uri; diagnostics; file }
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "textDocument/typeCoverage" -> (
      match TypeCoverage.of_yojson request with
      | Ok
          { TypeCoverage.parameters =
              Some { TypeCoverageParameters.textDocument = { TextDocumentIdentifier.uri; _ }; _ };
            id;
            _
          } ->
          uri_to_path ~uri >>| File.create >>| fun file -> TypeCoverageRequest { file; id }
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "workspace/executeCommand" -> (
      match ExecuteCommandRequest.of_yojson request with
      | Ok
          { ExecuteCommandRequest.parameters = Some { ExecuteCommandParameters.arguments; _ };
            id;
            _
          } ->
          Some (ExecuteCommandRequest { id; arguments })
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "updateFiles" -> (
      match UpdateFiles.of_yojson request with
      | Ok { UpdateFiles.parameters = Some { files; _ }; _ } ->
          let files = List.map files ~f:string_path_to_file in
          Some (TypeCheckRequest files)
      | Ok _ -> None
      | Error yojson_error ->
          Log.log ~section:`Server "Error: %s" yojson_error;
          None )
    | "displayTypeErrors" -> (
      match LanguageServer.Types.DisplayTypeErrors.of_yojson request with
      | Ok { LanguageServer.Types.DisplayTypeErrors.parameters = Some { files }; _ } ->
          let files = List.map files ~f:string_path_to_file in
          Some (DisplayTypeErrors files)
      | Ok _ -> None
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
  response: Protocol.response option
}

module AnnotationEdit = struct
  type t = {
    new_text: string;
    range: LanguageServer.Types.Range.t;
    title: string
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


  let create_range ~error ~file =
    let error_kind = Error.kind error in
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
        | _ -> Error.location error |> Location.line
      in
      line - 1
    in
    let get_range lines token =
      List.findi lines ~f:(fun _ line -> Option.is_some (String.substr_index line ~pattern:token))
      >>| (fun (index, line) ->
            let position =
              { LanguageServer.Types.Position.line = index + start_line;
                character = String.substr_index_exn line ~pattern:token + 1
              }
            in
            let end_ =
              if is_replacement_edit error_kind then
                let { LanguageServer.Types.Position.character; _ } = position in
                { position with character = character + String.length token }
              else
                position
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
    >>| (fun error ->
          let error_kind = Error.kind error in
          let new_text =
            match error_kind with
            | Error.MissingReturnAnnotation { annotation = Some annotation; _ } ->
                Some (" -> " ^ Type.show (Type.weaken_literals annotation))
            | Error.MissingAttributeAnnotation
                { missing_annotation = { annotation = Some annotation; _ }; _ }
            | Error.MissingParameterAnnotation { annotation = Some annotation; _ }
            | Error.MissingGlobalAnnotation { annotation = Some annotation; _ } ->
                Some (": " ^ Type.show (Type.weaken_literals annotation))
            | Error.IncompatibleReturnType { mismatch = { actual = annotation; _ }; _ } ->
                Some (Format.asprintf "-> %s:" @@ Type.show (Type.weaken_literals annotation))
            | Error.IncompatibleVariableType { mismatch = { actual = annotation; _ }; _ } ->
                Some (Format.asprintf ": %s " @@ Type.show (Type.weaken_literals annotation))
            | _ -> None
          in
          let range = create_range ~error ~file in
          let title =
            if is_replacement_edit error_kind then
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


let process_type_query_request ~state:({ State.environment; _ } as state) ~configuration ~request =
  let (module Handler : Environment.Handler) = environment in
  let process_request () =
    let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
    let resolution = TypeCheck.resolution environment () in
    let parse_and_validate ?(unknown_is_top = false) expression =
      let annotation =
        (* Return untracked so we can specifically message the user about them. *)
        Resolution.parse_annotation
          ~allow_untracked:true
          ~allow_invalid_type_parameters:true
          resolution
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
      if TypeOrder.is_instantiated order annotation then
        let mismatches, _ = Resolution.check_invalid_type_parameters resolution annotation in
        if List.is_empty mismatches then
          annotation
        else
          raise (IncorrectParameters annotation)
      else
        raise (TypeOrder.Untracked annotation)
    in
    match request with
    | TypeQuery.Attributes annotation ->
        let to_attribute { Node.value = { Annotated.Class.Attribute.name; annotation; _ }; _ } =
          let annotation = Annotation.annotation annotation in
          { TypeQuery.name; annotation }
        in
        parse_and_validate (Reference.expression ~location:Location.Reference.any annotation)
        |> Type.primitive_name
        >>= Handler.class_definition
        >>| Annotated.Class.create
        >>| (fun annotated_class -> Annotated.Class.attributes ~resolution annotated_class)
        >>| List.map ~f:to_attribute
        >>| (fun attributes -> TypeQuery.Response (TypeQuery.FoundAttributes attributes))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | TypeQuery.Callees caller ->
        TypeQuery.Response (TypeQuery.Callees (Dependencies.Callgraph.get ~caller))
    | TypeQuery.ComputeHashesToKeys ->
        let open Service.EnvironmentSharedMemory in
        (* Type order. *)
        let extend_map map ~new_map =
          Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
        in
        let map =
          let map =
            Map.set
              String.Map.empty
              ~key:(OrderKeys.hash_of_key SharedMemory.SingletonKey.key)
              ~data:(OrderKeys.serialize_key SharedMemory.SingletonKey.key)
          in
          match OrderKeys.get SharedMemory.SingletonKey.key with
          | Some indices ->
              let annotations = List.filter_map indices ~f:OrderAnnotations.get in
              extend_map
                map
                ~new_map:(Service.TypeOrder.compute_hashes_to_keys ~indices ~annotations)
          | None -> map
        in
        let handles = Ast.SharedMemory.HandleKeys.get () |> File.Handle.Set.Tree.to_list in
        (* AST shared memory. *)
        let map =
          map
          |> extend_map ~new_map:(Ast.SharedMemory.HandleKeys.compute_hashes_to_keys ())
          |> extend_map
               ~new_map:
                 (Ast.SharedMemory.SymlinksToPaths.compute_hashes_to_keys
                    ~keys:(List.map handles ~f:File.Handle.show))
          |> extend_map ~new_map:(Ast.SharedMemory.Sources.compute_hashes_to_keys ~keys:handles)
          |> extend_map
               ~new_map:
                 (Ast.SharedMemory.Modules.compute_hashes_to_keys
                    ~keys:(List.map ~f:(fun handle -> Ast.Source.qualifier ~handle) handles))
          |> extend_map
               ~new_map:
                 (Ast.SharedMemory.Handles.compute_hashes_to_keys
                    ~keys:(List.map ~f:File.Handle.show handles))
        in
        (* Handle-based keys. *)
        let map =
          map
          |> extend_map ~new_map:(FunctionKeys.compute_hashes_to_keys ~keys:handles)
          |> extend_map ~new_map:(ClassKeys.compute_hashes_to_keys ~keys:handles)
          |> extend_map ~new_map:(GlobalKeys.compute_hashes_to_keys ~keys:handles)
          |> extend_map ~new_map:(AliasKeys.compute_hashes_to_keys ~keys:handles)
          |> extend_map ~new_map:(DependentKeys.compute_hashes_to_keys ~keys:handles)
        in
        (* Class definitions. *)
        let map =
          let keys = List.filter_map handles ~f:ClassKeys.get |> List.concat in
          extend_map map ~new_map:(ClassDefinitions.compute_hashes_to_keys ~keys)
          |> extend_map ~new_map:(ClassMetadata.compute_hashes_to_keys ~keys)
        in
        (* Aliases. *)
        let map =
          let keys = List.filter_map handles ~f:AliasKeys.get |> List.concat in
          extend_map map ~new_map:(Aliases.compute_hashes_to_keys ~keys)
        in
        (* Globals and undecorated functions. *)
        let map =
          let keys = List.filter_map handles ~f:GlobalKeys.get |> List.concat in
          extend_map map ~new_map:(Globals.compute_hashes_to_keys ~keys)
          |> extend_map ~new_map:(UndecoratedFunctions.compute_hashes_to_keys ~keys)
        in
        (* Dependents. *)
        let map =
          let keys = List.filter_map handles ~f:DependentKeys.get |> List.concat in
          extend_map map ~new_map:(Dependents.compute_hashes_to_keys ~keys)
        in
        (* Resolution shared memory. *)
        let map =
          let keys = ResolutionSharedMemory.get_keys ~handles in
          map
          |> extend_map ~new_map:(ResolutionSharedMemory.compute_hashes_to_keys ~keys)
          |> extend_map ~new_map:(ResolutionSharedMemory.Keys.compute_hashes_to_keys ~keys:handles)
        in
        (* Coverage. *)
        let map =
          extend_map map ~new_map:(Coverage.SharedMemory.compute_hashes_to_keys ~keys:handles)
        in
        (* Calls *)
        let map =
          let keys =
            let open Statement.Define in
            List.filter_map handles ~f:Ast.SharedMemory.Sources.get
            |> List.concat_map
                 ~f:
                   (Preprocessing.defines
                      ~include_stubs:true
                      ~include_nested:true
                      ~include_toplevels:true)
            |> List.map ~f:(fun { Node.value = { signature = { name; _ }; _ }; _ } -> name)
          in
          extend_map
            map
            ~new_map:(Analysis.Dependencies.Callgraph.SharedMemory.compute_hashes_to_keys ~keys)
        in
        map
        |> Map.to_alist
        |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
        |> List.map ~f:(fun (hash, key) -> { TypeQuery.hash; key })
        |> fun response -> TypeQuery.Response (TypeQuery.FoundKeyMapping response)
    | TypeQuery.CoverageInFile file ->
        let default =
          TypeQuery.Error
            (Format.asprintf "Not able to get lookups in `%a`" Path.pp (File.path file))
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
        LookupCache.find_all_annotations ~state ~configuration ~path:(File.path file)
        >>| List.map ~f:map_to_coverage
        >>| (fun list -> TypeQuery.Response (TypeQuery.CoverageAtLocations list))
        |> Option.value ~default
    | TypeQuery.DecodeOcamlValues values ->
        let open Service.EnvironmentSharedMemory in
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
          let decode index =
            let annotation =
              Handler.TypeOrderHandler.find (Handler.TypeOrderHandler.annotations ()) index
            in
            match annotation with
            | None -> Format.sprintf "Undecodable(%d)" index
            | Some annotation -> annotation
          in
          let decode_target { TypeOrder.Target.target; parameters } =
            Format.sprintf
              "%s[%s]"
              (decode target)
              (List.map parameters ~f:Type.show |> String.concat ~sep:", ")
          in
          match decoded with
          | ClassDefinitions.Decoded (key, value) ->
              let value =
                match value with
                | Some { Node.value = definition; _ } ->
                    `Assoc ["class_definition", `String (Ast.Statement.Class.show definition)]
                    |> Yojson.to_string
                    |> Option.some
                | None -> None
              in
              Some (ClassValue.description, key, value)
          | ClassMetadata.Decoded (key, value) ->
              let value =
                match value with
                | Some { Resolution.successors; is_test; is_final; extends_placeholder_stub_class }
                  ->
                    `Assoc
                      [ "successors", `String (List.to_string ~f:Type.Primitive.show successors);
                        "is_test", `Bool is_test;
                        "is_final", `Bool is_final;
                        "extends_placeholder_stub_class", `Bool extends_placeholder_stub_class ]
                    |> Yojson.to_string
                    |> Option.some
                | None -> None
              in
              Some (ClassMetadataValue.description, key, value)
          | Aliases.Decoded (key, value) ->
              Some (AliasValue.description, key, value >>| Type.show_alias)
          | Globals.Decoded (key, value) ->
              let value = value >>| Node.value >>| Annotation.sexp_of_t >>| Sexp.to_string in
              Some (GlobalValue.description, Reference.show key, value)
          | UndecoratedFunctions.Decoded (key, value) ->
              Some
                ( UndecoratedFunctionValue.description,
                  Reference.show key,
                  value >>| Type.Callable.show_overload Type.pp )
          | Dependents.Decoded (key, value) ->
              Some
                ( DependentValue.description,
                  Reference.show key,
                  value >>| Reference.Set.Tree.to_list >>| List.to_string ~f:Reference.show )
          | FunctionKeys.Decoded (key, value) ->
              Some
                ( FunctionKeyValue.description,
                  File.Handle.show key,
                  value >>| List.to_string ~f:Reference.show )
          | ClassKeys.Decoded (key, value) ->
              Some
                (ClassKeyValue.description, File.Handle.show key, value >>| List.to_string ~f:Fn.id)
          | GlobalKeys.Decoded (key, value) ->
              Some
                ( GlobalKeyValue.description,
                  File.Handle.show key,
                  value >>| List.to_string ~f:Reference.show )
          | AliasKeys.Decoded (key, value) ->
              Some
                (AliasKeyValue.description, File.Handle.show key, value >>| List.to_string ~f:ident)
          | DependentKeys.Decoded (key, value) ->
              Some
                ( DependentKeyValue.description,
                  File.Handle.show key,
                  value >>| List.to_string ~f:Reference.show )
          | OrderIndices.Decoded (key, value) ->
              Some (OrderIndexValue.description, key, value >>| Int.to_string)
          | OrderAnnotations.Decoded (key, value) ->
              Some (OrderAnnotationValue.description, Int.to_string key, value)
          | OrderEdges.Decoded (key, value) ->
              Some (EdgeValue.description, decode key, value >>| List.to_string ~f:decode_target)
          | OrderBackedges.Decoded (key, value) ->
              Some
                ( BackedgeValue.description,
                  decode key,
                  value
                  >>| Analysis.TypeOrder.Target.Set.Tree.to_list
                  >>| List.to_string ~f:decode_target )
          | OrderKeys.Decoded (key, value) ->
              Some
                (OrderKeyValue.description, Int.to_string key, value >>| List.to_string ~f:decode)
          | Ast.SharedMemory.SymlinksToPaths.SymlinksToPaths.Decoded (key, value) ->
              Some
                ( Ast.SharedMemory.SymlinksToPaths.SymlinkSource.description,
                  key,
                  value >>| Path.show )
          | Ast.SharedMemory.Sources.Sources.Decoded (key, value) ->
              Some
                ( Ast.SharedMemory.Sources.SourceValue.description,
                  File.Handle.show key,
                  value >>| Source.show )
          | Ast.SharedMemory.Sources.QualifiersToHandles.Decoded (key, value) ->
              Some
                ( Ast.SharedMemory.Sources.HandleValue.description,
                  Reference.show key,
                  value >>| File.Handle.show )
          | Ast.SharedMemory.HandleKeys.HandleKeys.Decoded (key, value) ->
              Some
                ( Ast.SharedMemory.HandleKeys.HandleKeysValue.description,
                  Int.to_string key,
                  value >>| File.Handle.Set.Tree.sexp_of_t >>| Sexp.to_string )
          | Ast.SharedMemory.Modules.Modules.Decoded (key, value) ->
              Some
                ( Ast.SharedMemory.Modules.ModuleValue.description,
                  Reference.show key,
                  value >>| Module.sexp_of_t >>| Sexp.to_string )
          | Ast.SharedMemory.Handles.Paths.Decoded (key, value) ->
              Some (Ast.SharedMemory.Handles.PathValue.description, Int.to_string key, value)
          | Coverage.SharedMemory.Decoded (key, value) ->
              Some
                (Coverage.CoverageValue.description, File.Handle.show key, value >>| Coverage.show)
          | Analysis.Dependencies.Callgraph.SharedMemory.Decoded (key, value) ->
              Some
                ( Dependencies.Callgraph.CalleeValue.description,
                  Reference.show key,
                  value
                  >>| List.map ~f:Dependencies.Callgraph.show_callee
                  >>| String.concat ~sep:"," )
          | ResolutionSharedMemory.Decoded (key, value) ->
              Some
                ( ResolutionSharedMemory.TypeAnnotationsValue.description,
                  Reference.show key,
                  value >>| ResolutionSharedMemory.show_annotations )
          | _ -> None
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
                    | ClassDefinitions.Decoded (_, first), ClassDefinitions.Decoded (_, second) ->
                        Option.equal (Node.equal Statement.Class.equal) first second
                    | ClassMetadata.Decoded (_, first), ClassMetadata.Decoded (_, second) ->
                        Option.equal Resolution.equal_class_metadata first second
                    | Aliases.Decoded (_, first), Aliases.Decoded (_, second) ->
                        Option.equal Type.equal_alias first second
                    | Globals.Decoded (_, first), Globals.Decoded (_, second) ->
                        Option.equal Annotation.equal (first >>| Node.value) (second >>| Node.value)
                    | ( UndecoratedFunctions.Decoded (_, first),
                        UndecoratedFunctions.Decoded (_, second) ) ->
                        Option.equal (Type.Callable.equal_overload Type.equal) first second
                    | Dependents.Decoded (_, first), Dependents.Decoded (_, second) ->
                        Option.equal Reference.Set.Tree.equal first second
                    | FunctionKeys.Decoded (_, first), FunctionKeys.Decoded (_, second) ->
                        Option.equal (List.equal ~equal:Reference.equal) first second
                    | ClassKeys.Decoded (_, first), ClassKeys.Decoded (_, second) ->
                        Option.equal (List.equal ~equal:Identifier.equal) first second
                    | GlobalKeys.Decoded (_, first), GlobalKeys.Decoded (_, second) ->
                        Option.equal (List.equal ~equal:Reference.equal) first second
                    | AliasKeys.Decoded (_, first), AliasKeys.Decoded (_, second) ->
                        Option.equal (List.equal ~equal:Identifier.equal) first second
                    | DependentKeys.Decoded (_, first), DependentKeys.Decoded (_, second) ->
                        Option.equal (List.equal ~equal:Reference.equal) first second
                    | OrderIndices.Decoded (_, first), OrderIndices.Decoded (_, second) ->
                        Option.equal Int.equal first second
                    | OrderAnnotations.Decoded (_, first), OrderAnnotations.Decoded (_, second) ->
                        Option.equal String.equal first second
                    | OrderEdges.Decoded (_, first), OrderEdges.Decoded (_, second) ->
                        Option.equal (List.equal ~equal:TypeOrder.Target.equal) first second
                    | OrderBackedges.Decoded (_, first), OrderBackedges.Decoded (_, second) ->
                        Option.equal TypeOrder.Target.Set.Tree.equal first second
                    | OrderKeys.Decoded (_, first), OrderKeys.Decoded (_, second) ->
                        Option.equal (List.equal ~equal:Int.equal) first second
                    | ( Ast.SharedMemory.SymlinksToPaths.SymlinksToPaths.Decoded (_, first),
                        Ast.SharedMemory.SymlinksToPaths.SymlinksToPaths.Decoded (_, second) ) ->
                        Option.equal Path.equal first second
                    | ( Ast.SharedMemory.Sources.Sources.Decoded (_, first),
                        Ast.SharedMemory.Sources.Sources.Decoded (_, second) ) ->
                        Option.equal Source.equal first second
                    | ( Ast.SharedMemory.Sources.QualifiersToHandles.Decoded (_, first),
                        Ast.SharedMemory.Sources.QualifiersToHandles.Decoded (_, second) ) ->
                        Option.equal File.Handle.equal first second
                    | ( Ast.SharedMemory.HandleKeys.HandleKeys.Decoded (_, first),
                        Ast.SharedMemory.HandleKeys.HandleKeys.Decoded (_, second) ) ->
                        Option.equal File.Handle.Set.Tree.equal first second
                    | ( Ast.SharedMemory.Modules.Modules.Decoded (_, first),
                        Ast.SharedMemory.Modules.Modules.Decoded (_, second) ) ->
                        Option.equal Module.equal first second
                    | ( Ast.SharedMemory.Handles.Paths.Decoded (_, first),
                        Ast.SharedMemory.Handles.Paths.Decoded (_, second) ) ->
                        Option.equal String.equal first second
                    | ( Coverage.SharedMemory.Decoded (_, first),
                        Coverage.SharedMemory.Decoded (_, second) ) ->
                        Option.equal Coverage.equal first second
                    | ( ResolutionSharedMemory.Decoded (_, first),
                        ResolutionSharedMemory.Decoded (_, second) ) ->
                        Option.equal ResolutionSharedMemory.equal_annotations first second
                    | _ -> false
                  in
                  match serialize_decoded first, serialize_decoded second with
                  | Some (kind, key, first_value), Some (_, _, second_value) ->
                      let value =
                        TypeQuery.DecodedPair
                          { serialized_key;
                            kind;
                            actual_key = key;
                            first_value;
                            second_value;
                            equal
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
    | TypeQuery.DependentDefines files -> (
      try
        let handles = List.map files ~f:(File.handle ~configuration) in
        let modules = List.map handles ~f:(fun handle -> Source.qualifier ~handle) in
        let get_dependencies =
          let (module Handler : Environment.Handler) = environment in
          Handler.dependencies
        in
        let dependency_set = Dependencies.transitive_of_list ~get_dependencies ~modules in
        let dependencies =
          let source_to_define_name source =
            let define_to_name { Statement.Define.signature = { name; _ }; _ } = name in
            define_to_name (Source.top_level_define source)
          in
          Reference.Set.to_list dependency_set
          |> List.filter_map ~f:Ast.SharedMemory.Sources.get_for_qualifier
          |> List.map ~f:source_to_define_name
        in
        TypeQuery.Response (TypeQuery.References dependencies)
      with
      | File.NonexistentHandle message -> TypeQuery.Error message )
    | TypeQuery.DumpDependencies file ->
        let () =
          try
            let qualifier =
              File.handle ~configuration file |> fun handle -> Source.qualifier ~handle
            in
            Path.create_relative
              ~root:(Configuration.Analysis.pyre_root configuration)
              ~relative:"dependencies.dot"
            |> File.create
                 ~content:(Dependencies.to_dot ~get_dependencies:Handler.dependencies ~qualifier)
            |> File.write
          with
          | File.NonexistentHandle _ -> ()
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
        Service.Environment.normalize_shared_memory ();
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
          | Type.Top -> right
          | unwrapped -> unwrapped
        in
        Resolution.is_compatible_with resolution ~left ~right
        |> fun result ->
        TypeQuery.Response (TypeQuery.Compatibility { actual = left; expected = right; result })
    | TypeQuery.Join (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        Resolution.join resolution left right
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        Resolution.less_or_equal resolution ~left ~right
        |> fun response -> TypeQuery.Response (TypeQuery.Boolean response)
    | TypeQuery.Meet (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        Resolution.meet resolution left right
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.Methods annotation ->
        let to_method annotated_method =
          let open Annotated.Class.Method in
          let annotations =
            parameter_annotations ~resolution annotated_method
            |> List.mapi ~f:(fun index (_, annotation) -> index, annotation)
            |> Int.Map.of_alist_exn
          in
          let parameters =
            Map.keys annotations
            |> List.sort ~compare:Int.compare
            |> Fn.flip List.drop 1 (* Drop the self argument *)
            |> List.map ~f:(Map.find_exn annotations)
            |> fun parameters -> Type.Primitive "self" :: parameters
          in
          let return_annotation = return_annotation ~resolution annotated_method in
          { TypeQuery.name = name annotated_method; parameters; return_annotation }
        in
        parse_and_validate (Reference.expression ~location:Location.Reference.any annotation)
        |> Type.primitive_name
        >>= Handler.class_definition
        >>| Annotated.Class.create
        >>| Annotated.Class.methods
        >>| List.map ~f:to_method
        >>| (fun methods -> TypeQuery.Response (TypeQuery.FoundMethods methods))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | TypeQuery.NormalizeType expression ->
        parse_and_validate expression
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.PathOfModule module_name ->
        Handler.module_definition module_name
        >>= Module.handle
        >>= File.Handle.to_path ~configuration
        >>| Path.absolute
        >>| (fun path -> TypeQuery.Response (TypeQuery.FoundPath path))
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
        match Resolution.global resolution function_name with
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
                            { TypeQuery.parameter_name = name;
                              annotation = keep_known_annotation annotation
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
        |> Type.primitive_name
        >>= Handler.class_definition
        >>| Annotated.Class.create
        >>| Annotated.Class.superclasses ~resolution
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

          let calls = Location.Reference.Table.create ()
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
              List.map errors ~f:(Analysis.Error.description ~show_error_traces:false)
              |> String.concat ~sep:", "
            in
            TypeQuery.Error (Format.sprintf "Expression had errors: %s" descriptions) )
    | TypeQuery.TypeAtPosition { file; position } ->
        let default =
          TypeQuery.Error
            (Format.asprintf
               "Not able to get lookup at %a:%a"
               Path.pp
               (File.path file)
               Location.pp_position
               position)
        in
        LookupCache.find_annotation ~state ~configuration ~path:(File.path file) ~position
        >>| (fun (location, annotation) ->
              TypeQuery.Response (TypeQuery.TypeAtLocation { TypeQuery.location; annotation }))
        |> Option.value ~default
    | TypeQuery.TypesInFile file ->
        let default =
          TypeQuery.Error
            (Format.asprintf "Not able to get lookups in `%a`" Path.pp (File.path file))
        in
        LookupCache.find_all_annotations ~state ~configuration ~path:(File.path file)
        >>| List.map ~f:(fun (location, annotation) -> { TypeQuery.location; annotation })
        >>| (fun list -> TypeQuery.Response (TypeQuery.TypesAtLocations list))
        |> Option.value ~default
    | TypeQuery.ValidateTaintModels path -> (
      try
        let directory =
          match path with
          | Some path -> path
          | None ->
              configuration.Configuration.Analysis.taint_models_directory
              |> fun value -> Option.value_exn value
        in
        let configuration = Taint.TaintConfiguration.create ~directory in
        let create_models sources =
          let create_model source =
            Taint.Model.parse
              ~resolution:(TypeCheck.resolution environment ())
              ~source
              ~configuration
              Interprocedural.Callable.Map.empty
            |> ignore
          in
          List.iter sources ~f:create_model
        in
        Path.list ~file_filter:(String.is_suffix ~suffix:".pysa") ~root:directory ()
        |> List.map ~f:File.create
        |> List.filter_map ~f:File.content
        |> create_models;
        TypeQuery.Response
          (TypeQuery.Success (Format.asprintf "Models in `%a` are valid." Path.pp directory))
      with
      | error -> TypeQuery.Error (Exn.to_string error) )
  in
  let response =
    try process_request () with
    | TypeOrder.Untracked untracked ->
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


let process_type_check_request ~state ~configuration ~files =
  let state, response = IncrementalCheck.recheck ~state ~configuration ~files in
  { state; response = Some (TypeCheckResponse response) }


let process_display_type_errors_request ~state ~configuration ~files =
  let errors =
    let { errors; _ } = state in
    match files with
    | [] -> Hashtbl.data errors |> List.concat |> List.sort ~compare:Error.compare
    | _ ->
        let errors file =
          try
            File.handle ~configuration file |> Hashtbl.find errors |> Option.value ~default:[]
          with
          | File.NonexistentHandle _ -> []
        in
        List.concat_map ~f:errors files
  in
  { state; response = Some (TypeCheckResponse errors) }


let process_get_definition_request
    ~state
    ~configuration
    ~request:{ DefinitionRequest.id; file; position }
  =
  let response =
    let open LanguageServer.Protocol in
    let definition = LookupCache.find_definition ~state ~configuration (File.path file) position in
    TextDocumentDefinitionResponse.create ~configuration ~id ~location:definition
    |> TextDocumentDefinitionResponse.to_yojson
    |> Yojson.Safe.to_string
    |> (fun response -> LanguageServerProtocolResponse response)
    |> Option.some
  in
  { state; response }


let rec process
    ~state:({ State.environment; connections; errors; _ } as state)
    ~configuration:({ configuration; _ } as server_configuration)
    ~request
  =
  let timer = Timer.start () in
  let (module Handler : Environment.Handler) = environment in
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
      | TypeCheckRequest files ->
          SharedMem.collect `aggressive;
          process_type_check_request ~state ~configuration ~files
      | StopRequest ->
          Mutex.critical_section connections.lock ~f:(fun () ->
              Operations.stop ~reason:"explicit request" ~configuration:server_configuration)
      | TypeQueryRequest request -> process_type_query_request ~state ~configuration ~request
      | DisplayTypeErrors files ->
          let configuration = { configuration with include_hints = true } in
          process_display_type_errors_request ~state ~configuration ~files
      | LanguageServerProtocolRequest request ->
          parse_lsp ~configuration ~request:(Yojson.Safe.from_string request)
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
      | HoverRequest { DefinitionRequest.id; file; position } ->
          let response =
            let open LanguageServer.Protocol in
            let result =
              LookupCache.find_annotation ~state ~configuration ~path:(File.path file) ~position
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
      | CodeActionRequest { id; diagnostics; uri; file } ->
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
            let errors file =
              try
                File.handle ~configuration file |> Hashtbl.find errors |> Option.value ~default:[]
              with
              | File.NonexistentHandle _ -> []
            in
            let code_actions =
              diagnostics
              |> List.filter_map
                   ~f:(fun (LanguageServer.Types.Diagnostic.{ range; _ } as diagnostic) ->
                     let error =
                       List.find (errors file) ~f:(fun { location; _ } ->
                           is_range_equal_location range location)
                     in
                     AnnotationEdit.create ~file ~error
                     >>| (fun edit ->
                           Some
                             { LanguageServer.Types.CodeAction.diagnostics = Some [diagnostic];
                               command =
                                 Some
                                   { title = "Fix it";
                                     command = "add_annotation";
                                     arguments =
                                       [ { range = AnnotationEdit.range edit;
                                           newText = AnnotationEdit.new_text edit;
                                           uri
                                         } ]
                                   };
                               title = AnnotationEdit.title edit;
                               kind = Some "refactor.rewrite"
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
                    { LanguageServer.Types.WorkspaceEdit.changes =
                        Some { uri; textEdit = [{ newText; range }] }
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
      | OpenDocument file ->
          (* Make sure cache is fresh. We might not have received a close notification. *)
          LookupCache.evict_path ~state ~configuration (File.path file);

          (* Make sure the IDE flushes its state about this file, by sending back all the errors
             for this file. *)
          let { State.open_documents; _ } = state in
          let open_documents =
            Path.Map.set
              open_documents
              ~key:(File.path file)
              ~data:(File.content file |> Option.value ~default:"")
          in
          (* We do not recheck dependencies because nothing changes when we open a document, and we
             do the type checking here just to make type checking resolution appear in shared
             memory. *)
          process_type_check_request
            ~state:{ state with open_documents }
            ~configuration:{ configuration with ignore_dependencies = true }
            ~files:[file]
      | CloseDocument file ->
          let { State.open_documents; _ } = state in
          let path = File.path file in
          let open_documents = Path.Map.remove open_documents path in
          LookupCache.evict_path ~state ~configuration path;
          { state = { state with open_documents }; response = None }
      | DocumentChange file ->
          (* On change, update open document's content but do not trigger recheck. *)
          let { State.open_documents; _ } = state in
          let open_documents =
            Path.Map.set
              open_documents
              ~key:(File.path file)
              ~data:(File.content file |> Option.value ~default:"")
          in
          { state = { state with open_documents }; response = None }
      | SaveDocument file ->
          (* On save, evict entries from the lookup cache. The updated source will be picked up at
             the next lookup (if any). *)
          LookupCache.evict_path ~state ~configuration (File.path file);
          let check_on_save =
            Mutex.critical_section connections.lock ~f:(fun () ->
                let { file_notifiers; _ } = !(connections.connections) in
                List.is_empty file_notifiers)
          in
          if check_on_save then
            let configuration = { configuration with include_hints = true } in
            process_type_check_request ~state ~configuration ~files:[file]
          else (
            Log.log ~section:`Server "Explicitly ignoring didSave request";
            { state; response = None } )
      | TypeCoverageRequest { file; id } ->
          let response =
            File.handle ~configuration file
            |> fun handle ->
            match Coverage.get ~handle with
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
    | Analysis.TypeOrder.Untracked annotation ->
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
