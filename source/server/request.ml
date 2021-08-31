(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open State
open Configuration.Server
open Protocol
open Request
open Pyre

let errors_of_path ~configuration ~state:{ State.environment; errors; _ } path =
  let module_tracker = TypeEnvironment.module_tracker environment in
  match ModuleTracker.lookup_path ~configuration module_tracker path with
  | ModuleTracker.PathLookup.Found { SourcePath.qualifier; _ } ->
      Hashtbl.find errors qualifier |> Option.value ~default:[]
  | _ -> []


let instantiate_error
    ~configuration:({ Configuration.Analysis.show_error_traces; _ } as configuration)
    ~state:{ State.environment; _ }
    error
  =
  let ast_environment = TypeEnvironment.ast_environment environment |> AstEnvironment.read_only in
  AnalysisError.instantiate
    ~show_error_traces
    ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment)
    error


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
    | AnalysisError.IncompatibleVariableType _
    | AnalysisError.IncompatibleReturnType _ ->
        true
    | _ -> false


  let create_range ~error:{ AnalysisError.kind = error_kind; location; _ } ~file =
    let token =
      match error_kind with
      | AnalysisError.MissingReturnAnnotation _ -> Some "):"
      | AnalysisError.MissingAttributeAnnotation { missing_annotation = { name; _ }; _ }
      | AnalysisError.MissingParameterAnnotation { name; _ }
      | AnalysisError.MissingGlobalAnnotation { name; _ } ->
          Some (Format.asprintf "%a" Reference.pp_sanitized name)
      | AnalysisError.IncompatibleReturnType { mismatch = { expected; _ }; _ } ->
          Some (Format.asprintf " -> %s" (Type.show expected))
      | AnalysisError.IncompatibleVariableType
          { incompatible_type = { name; mismatch = { expected; _ }; _ }; _ } ->
          Some (Format.asprintf "%a: %s" Reference.pp_sanitized name (Type.show expected))
      | _ -> None
    in
    let start_line =
      let line =
        match error_kind with
        | AnalysisError.IncompatibleReturnType { define_location; _ } ->
            Location.line define_location
        | AnalysisError.IncompatibleVariableType { declare_location; _ } ->
            Location.WithPath.line declare_location
        | _ -> Location.WithModule.line location
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
              | AnalysisError.IncompatibleVariableType _
              | AnalysisError.IncompatibleReturnType _
              | AnalysisError.MissingGlobalAnnotation _
              | AnalysisError.MissingAttributeAnnotation _ ->
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
    >>| (fun ({ AnalysisError.kind; _ } as error) ->
          let format_type annotation =
            Type.weaken_literals annotation |> Type.infer_transform |> Type.show
          in
          let new_text =
            match kind with
            | AnalysisError.MissingReturnAnnotation { annotation = Some annotation; _ } ->
                Some (" -> " ^ format_type annotation)
            | AnalysisError.MissingAttributeAnnotation
                { missing_annotation = { annotation = Some annotation; _ }; _ }
            | AnalysisError.MissingParameterAnnotation { annotation = Some annotation; _ }
            | AnalysisError.MissingGlobalAnnotation { annotation = Some annotation; _ } ->
                Some (": " ^ format_type annotation)
            | AnalysisError.IncompatibleReturnType { mismatch = { actual = annotation; _ }; _ } ->
                Some (Format.asprintf "-> %s:" @@ format_type annotation)
            | AnalysisError.IncompatibleVariableType
                { incompatible_type = { mismatch = { actual = annotation; _ }; _ }; _ } ->
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


let process_type_check_request ~state:({ errors; _ } as state) ~configuration paths =
  let _ = IncrementalCheck.recheck_with_state ~state ~configuration paths in
  let response =
    Hashtbl.data errors |> List.concat |> List.map ~f:(instantiate_error ~configuration ~state)
  in
  { state; response = Some (TypeCheckResponse response) }


let process_display_type_errors_request ~state ~configuration paths =
  let errors =
    let { errors; _ } = state in
    match paths with
    | [] -> Hashtbl.data errors |> List.concat |> List.sort ~compare:AnalysisError.compare
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
    let open LanguageServer.Protocol in
    let response =
      match LookupProcessor.find_definition ~environment ~configuration path position with
      | None -> TextDocumentDefinitionResponse.create_empty ~id
      | Some { Location.start; stop } -> (
          let module_tracker = TypeEnvironment.module_tracker environment in
          match ModuleTracker.lookup_path ~configuration module_tracker path with
          | ModuleTracker.PathLookup.Found source_path ->
              let path = SourcePath.full_path ~configuration source_path in
              TextDocumentDefinitionResponse.create ~id ~start ~stop ~path
          | _ -> TextDocumentDefinitionResponse.create_empty ~id)
    in
    TextDocumentDefinitionResponse.to_yojson response
    |> Yojson.Safe.to_string
    |> (fun response -> LanguageServerProtocolResponse response)
    |> Option.some
  in
  { state; response }


let rec process
    ~state:({ State.environment; connections; scheduler; _ } as state)
    ~configuration:({ configuration; _ } as server_configuration)
    ~request
  =
  let { Configuration.Features.go_to_definition; click_to_fix; hover } =
    Configuration.Analysis.features configuration
  in
  let { Configuration.Analysis.perform_autocompletion = autocomplete; expected_version; _ } =
    configuration
  in
  let timer = Timer.start () in
  let module_tracker = TypeEnvironment.module_tracker environment in
  let log_request_error ~error =
    Statistics.event
      ~section:`Error
      ~name:"request error"
      ~normals:["request", Request.show request; "error", error]
      ~flush:true
      ()
  in
  let update_open_documents ~state path =
    let { State.open_documents; _ } = state in
    match ModuleTracker.lookup_path ~configuration module_tracker path with
    | ModuleTracker.PathLookup.Found { SourcePath.qualifier; _ } ->
        Reference.Table.set
          open_documents
          ~key:qualifier
          ~data:(File.create path |> File.content |> Option.value ~default:"")
    | ModuleTracker.PathLookup.ShadowedBy _ ->
        Statistics.event
          ~flush:true
          ~name:"ModuleTracker failed lookup"
          ~normals:
            ["reason", "Module shadowed by another path in ModuleTracker"; "path", Path.show path]
          ()
    | ModuleTracker.PathLookup.NotFound ->
        Statistics.event
          ~flush:true
          ~name:"ModuleTracker failed lookup"
          ~normals:["reason", "Unable to find path in ModuleTracker"; "path", Path.show path]
          ()
  in
  let result =
    try
      match request with
      | TypeCheckRequest paths -> process_type_check_request ~state ~configuration paths
      | StopRequest ->
          Error_checking_mutex.critical_section connections.lock ~f:(fun () ->
              Operations.stop
                ~reason:"explicit request"
                ~configuration:server_configuration
                ~scheduler)
      | TypeQueryRequest request ->
          let response = Query.process_request ~environment ~configuration request in
          { state; response = Some (TypeQueryResponse response) }
      | UnparsableQuery { query; reason } ->
          let response =
            Query.Response.Error (Format.sprintf "Unable to parse %s: %s" query reason)
          in
          { state; response = Some (TypeQueryResponse response) }
      | DisplayTypeErrors paths ->
          let configuration = { configuration with include_hints = true } in
          process_display_type_errors_request ~state ~configuration paths
      | LanguageServerProtocolRequest request ->
          RequestParser.parse_and_translate
            ~configuration
            ~state
            ~request:(Yojson.Safe.from_string request)
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
      | GetDefinitionRequest { DefinitionRequest.id; _ } when not go_to_definition ->
          let response =
            LanguageServer.Protocol.TextDocumentDefinitionResponse.create_empty ~id
            |> LanguageServer.Protocol.TextDocumentDefinitionResponse.to_yojson
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
      | HoverRequest { DefinitionRequest.id; _ } when not hover ->
          let response =
            LanguageServer.Protocol.HoverResponse.create_empty ~id
            |> LanguageServer.Protocol.HoverResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | HoverRequest { DefinitionRequest.id; path; position } ->
          let response =
            let open LanguageServer.Protocol in
            let result =
              LookupProcessor.find_annotation ~environment ~configuration ~path ~position
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
      | CodeActionRequest { id; _ } when not click_to_fix ->
          let response =
            LanguageServer.Protocol.CodeActionResponse.create_empty ~id
            |> LanguageServer.Protocol.CodeActionResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | CodeActionRequest { id; diagnostics; uri; path } ->
          let is_range_equal_location
              { LanguageServer.Types.Range.start = range_start; end_ }
              { Location.WithModule.start = location_start; stop; _ }
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
            let { State.server_uuid; _ } = state in
            let command =
              server_uuid
              >>| (fun server_uuid -> "add_pyre_annotation_" ^ server_uuid)
              |> Option.value ~default:"add_pyre_annotation"
            in
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
                                     command;
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
          (* Make sure the IDE flushes its state about this file, by sending back all the errors for
             this file. *)
          update_open_documents ~state path;
          process_display_type_errors_request ~state ~configuration [path]
      | CloseDocument path ->
          let { State.open_documents; _ } = state in
          let relative_path =
            match ModuleTracker.lookup_path ~configuration module_tracker path with
            | ModuleTracker.PathLookup.Found { SourcePath.qualifier; relative; _ } ->
                Reference.Table.remove open_documents qualifier;
                Some relative
            | _ -> None
          in
          let response =
            relative_path
            >>| (fun path ->
                  LanguageServer.Protocol.PublishDiagnostics.clear_diagnostics_for_uri
                    ~uri:(Path.uri (Path.create_absolute ~follow_symbolic_links:true path)))
            >>| LanguageServer.Protocol.PublishDiagnostics.to_yojson
            >>| Yojson.Safe.to_string
            >>| (fun response -> LanguageServerProtocolResponse response)
            >>| Option.some
            |> Option.value ~default:None
          in
          { state; response }
      | DocumentChange file ->
          (* On change, update open document's content but do not trigger recheck. *)
          update_open_documents ~state (File.path file);
          { state; response = None }
      | SaveDocument path ->
          (if Random.bool () then
             let { Configuration.Analysis.local_root; filter_directories; project_root; _ } =
               configuration
             in
             Telemetry.send_telemetry () ~f:(fun _ ->
                 Telemetry.create_update_message ~local_root ~project_root ~filter_directories));

          let configuration = { configuration with include_hints = true } in
          process_type_check_request ~state ~configuration [path]
      | ShowStatusRequest { message; type_; _ } ->
          let update_function =
            let open LanguageServer.Types in
            match ShowMessageParameters.fromMessageTypeNumber type_ with
            | ShowMessageParameters.InfoMessage -> StatusUpdate.information
            | _ -> StatusUpdate.warning
          in
          update_function ~message ~state;
          { state; response = None }
      (* Requests that cannot be fulfilled here. *)
      | ClientConnectionRequest _ ->
          Log.warning "Explicitly ignoring ClientConnectionRequest request";
          { state; response = None }
      | InitializeRequest request_id ->
          let server_uuid = Uuid_unix.create () |> Uuid.to_string in
          let response =
            LanguageServer.Protocol.InitializeResponse.default
              ~server_uuid
              ~features:{ click_to_fix; autocomplete; hover; go_to_definition }
              request_id
            |> LanguageServer.Protocol.InitializeResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          let state = { state with server_uuid = Some server_uuid } in
          { state; response }
      | InitializedRequest ->
          expected_version
          >>| (fun expected_version ->
                Statistics.event
                  ~flush:true
                  ~name:"LSP Initialized"
                  ~normals:["reason", "LSP Initialized"; "server_version", expected_version]
                  ())
          |> ignore;
          { state; response = None }
    with
    | Unix.Unix_error (kind, name, parameters) ->
        Log.log_unix_error (kind, name, parameters);
        log_request_error
          ~error:(Format.sprintf "Unix error %s: %s(%s)" (Unix.error_message kind) name parameters);
        { state; response = None }
    | Analysis.ClassHierarchy.Untracked annotation ->
        log_request_error ~error:(Format.sprintf "Untracked %s" annotation);
        { state; response = None }
    | Worker.Worker_exited_abnormally (pid, status) ->
        Statistics.log_worker_exception ~pid status ~origin:"server";
        Error_checking_mutex.critical_section connections.lock ~f:(fun () ->
            Operations.stop
              ~reason:"Worker exited abnormally"
              ~configuration:server_configuration
              ~scheduler)
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
          Error_checking_mutex.critical_section connections.lock ~f:(fun () ->
              Operations.stop
                ~reason:"uncaught exception"
                ~configuration:server_configuration
                ~scheduler);
        { state; response = None }
  in
  Statistics.performance
    ~name:"server request"
    ~timer
    ~normals:
      [
        "request kind", Request.name request;
        ( "LSP request",
          match request with
          | LanguageServerProtocolRequest request -> request
          | _ -> "" );
      ]
    ();
  result
