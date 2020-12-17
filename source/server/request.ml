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

exception IncorrectParameters of Type.t

exception MissingFunction of Reference.t

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


let rec process_type_query_request
    ~state:({ State.environment; _ } as state)
    ~configuration
    ~request
  =
  let process_request () =
    let module_tracker = TypeEnvironment.module_tracker environment in
    let read_only_environment = TypeEnvironment.read_only environment in
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution read_only_environment in
    let order = GlobalResolution.class_hierarchy global_resolution in
    let resolution =
      TypeCheck.resolution
        global_resolution
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
    in

    let parse_and_validate
        ?(unknown_is_top = false)
        ?(fill_missing_type_parameters_with_any = false)
        expression
      =
      let annotation =
        (* Return untracked so we can specifically message the user about them. *)
        GlobalResolution.parse_annotation ~validation:NoValidation global_resolution expression
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
      let annotation =
        match fill_missing_type_parameters_with_any, annotation with
        | true, Type.Primitive annotation -> (
            let generics = GlobalResolution.variables global_resolution annotation in
            match generics with
            | Some generics
              when (not (List.is_empty generics))
                   && List.for_all generics ~f:(function
                          | Type.Variable.Unary _ -> true
                          | _ -> false) ->
                Type.parametric
                  annotation
                  (List.map generics ~f:(fun _ -> Type.Parameter.Single Type.Any))
            | _ -> Type.Primitive annotation )
        | _ -> annotation
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
    let global_environment = TypeEnvironment.ReadOnly.global_environment read_only_environment in
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment global_resolution
    in
    let get_error_paths errors =
      List.fold
        ~init:""
        ~f:(fun sofar (path, error_reason) ->
          let print_reason = function
            | Some LookupCache.StubShadowing -> " (file shadowed by .pyi stub file)"
            | Some LookupCache.FileNotFound -> " (file not found)"
            | None -> ""
          in
          Format.asprintf
            "%s%s`%a`%s"
            sofar
            (if String.is_empty sofar then "" else ", ")
            PyrePath.pp
            path
            (print_reason error_reason))
        errors
    in
    match request with
    | TypeQuery.RunCheck { check_name; paths } ->
        let source_paths =
          let lookup_path path =
            ModuleTracker.lookup_path ~configuration module_tracker path
            |> function
            | ModuleTracker.PathLookup.Found source_path -> Some source_path
            | _ -> None
          in
          List.filter_map paths ~f:lookup_path
        in
        let errors =
          IncrementalStaticAnalysis.run_additional_check
            ~configuration
            ~environment
            ~source_paths
            ~check:check_name
        in
        TypeQuery.Response (TypeQuery.Errors errors)
    | TypeQuery.Attributes annotation ->
        let to_attribute attribute =
          let name = Annotated.Attribute.name attribute in
          let instantiated_annotation =
            GlobalResolution.instantiate_attribute
              ~resolution:global_resolution
              ~accessed_through_class:false
              attribute
          in
          let annotation =
            instantiated_annotation |> Annotated.Attribute.annotation |> Annotation.annotation
          in
          let property = Annotated.Attribute.property attribute in
          let kind =
            if property then
              TypeQuery.Property
            else
              TypeQuery.Regular
          in
          let final = Annotated.Attribute.is_final instantiated_annotation in
          { TypeQuery.name; annotation; kind; final }
        in
        parse_and_validate (Expression.from_reference ~location:Location.any annotation)
        |> Type.split
        |> fst
        |> Type.primitive_name
        >>= GlobalResolution.attributes ~resolution:global_resolution
        >>| List.map ~f:to_attribute
        >>| (fun attributes -> TypeQuery.Response (TypeQuery.FoundAttributes attributes))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | TypeQuery.Batch requests ->
        TypeQuery.Response
          (TypeQuery.Batch
             (List.map
                ~f:(fun request ->
                  let { response; _ } = process_type_query_request ~state ~configuration ~request in
                  match response with
                  | Some (TypeQueryResponse response) -> response
                  | _ -> TypeQuery.Error "Invalid response for query.")
                requests))
    | TypeQuery.Callees caller ->
        (* We don't yet support a syntax for fetching property setters. *)
        TypeQuery.Response
          (TypeQuery.Callees
             ( Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
             |> List.map ~f:(fun { Callgraph.callee; _ } -> callee) ))
    | TypeQuery.CalleesWithLocation caller ->
        let instantiate =
          Location.WithModule.instantiate
            ~lookup:
              (AstEnvironment.ReadOnly.get_real_path_relative
                 ~configuration
                 (TypeEnvironment.ReadOnly.ast_environment read_only_environment))
        in
        let callees =
          (* We don't yet support a syntax for fetching property setters. *)
          Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
          |> List.map ~f:(fun { Callgraph.callee; locations } ->
                 { TypeQuery.callee; locations = List.map locations ~f:instantiate })
        in
        TypeQuery.Response (TypeQuery.CalleesWithLocation callees)
    | TypeQuery.Defines module_or_class_names ->
        let ast_environment = TypeEnvironment.ReadOnly.ast_environment read_only_environment in
        let defines_of_module module_or_class_name =
          let module_name, filter_define =
            if AstEnvironment.ReadOnly.is_module ast_environment module_or_class_name then
              Some module_or_class_name, fun _ -> false
            else
              let filter
                  { Statement.Define.signature = { Statement.Define.Signature.parent; _ }; _ }
                =
                not (Option.equal Reference.equal parent (Some module_or_class_name))
              in
              let rec find_module_name current_reference =
                if AstEnvironment.ReadOnly.is_module ast_environment current_reference then
                  Some current_reference
                else
                  Reference.prefix current_reference >>= find_module_name
              in
              find_module_name module_or_class_name, filter
          in
          let defines =
            module_name
            >>= AstEnvironment.ReadOnly.get_processed_source ast_environment
            >>| Analysis.FunctionDefinition.collect_defines
            >>| List.map ~f:snd
            >>| List.concat_map ~f:Analysis.FunctionDefinition.all_bodies
            >>| List.filter ~f:(fun { Node.value = define; _ } ->
                    not
                      ( Statement.Define.is_toplevel define
                      || Statement.Define.is_class_toplevel define
                      || Statement.Define.is_overloaded_function define
                      || filter_define define ))
            |> Option.value ~default:[]
          in
          let represent
              {
                Node.value =
                  { Statement.Define.signature = { name; return_annotation; parameters; _ }; _ };
                _;
              }
            =
            let represent_parameter { Node.value = { Expression.Parameter.name; annotation; _ }; _ }
              =
              {
                TypeQuery.parameter_name = Identifier.sanitized name;
                parameter_annotation = annotation;
              }
            in
            {
              TypeQuery.define_name = Node.value name;
              parameters = List.map parameters ~f:represent_parameter;
              return_annotation;
            }
          in
          List.map defines ~f:represent
        in
        List.concat_map module_or_class_names ~f:defines_of_module
        |> fun defines -> TypeQuery.Response (TypeQuery.FoundDefines defines)
    | TypeQuery.DumpCallGraph ->
        let get_callgraph module_qualifier =
          let callees
              {
                Node.value =
                  {
                    Statement.Define.signature =
                      { Statement.Define.Signature.name = { Node.value = caller; _ }; _ };
                    _;
                  };
                _;
              }
            =
            let instantiate =
              Location.WithModule.instantiate
                ~lookup:
                  (AstEnvironment.ReadOnly.get_real_path_relative
                     ~configuration
                     (TypeEnvironment.ReadOnly.ast_environment read_only_environment))
            in
            Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
            |> List.map ~f:(fun { Callgraph.callee; locations } ->
                   { TypeQuery.callee; locations = List.map locations ~f:instantiate })
            |> fun callees -> { Protocol.TypeQuery.caller; callees }
          in
          let ast_environment = TypeEnvironment.ReadOnly.ast_environment read_only_environment in
          AstEnvironment.ReadOnly.get_processed_source ast_environment module_qualifier
          >>| Preprocessing.defines ~include_toplevels:false ~include_stubs:false
          >>| List.map ~f:callees
          |> Option.value ~default:[]
        in
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        TypeQuery.Response (TypeQuery.Callgraph (List.concat_map qualifiers ~f:get_callgraph))
    | TypeQuery.DumpClassHierarchy ->
        let resolution = GlobalResolution.create global_environment in
        let class_hierarchy_json =
          let indices =
            Analysis.UnannotatedGlobalEnvironment.ReadOnly.all_indices
              unannotated_global_environment
          in
          ClassHierarchy.to_json (GlobalResolution.class_hierarchy resolution) ~indices
        in
        TypeQuery.Response (TypeQuery.ClassHierarchy class_hierarchy_json)
    | TypeQuery.Help help_list -> TypeQuery.Response (TypeQuery.Help help_list)
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
        let parsed_annotation =
          parse_and_validate ~fill_missing_type_parameters_with_any:true annotation
        in
        let to_method attribute =
          match
            GlobalResolution.instantiate_attribute
              ~resolution:global_resolution
              ~instantiated:parsed_annotation
              ~accessed_through_class:false
              attribute
            |> Annotated.Attribute.annotation
            |> Annotation.annotation
          with
          | Type.Parametric
              {
                name = "BoundMethod";
                parameters =
                  [
                    Single
                      (Type.Callable
                        {
                          implementation = { annotation; parameters = Defined parameters; _ };
                          kind = Named name;
                          _;
                        });
                    _;
                  ];
              }
          | Type.Callable
              {
                implementation = { annotation; parameters = Defined parameters; _ };
                kind = Named name;
                _;
              } ->
              let parameters =
                parameters |> List.filter_map ~f:Type.Callable.Parameter.annotation
              in
              let return_annotation = annotation in
              Some { TypeQuery.name = Reference.last name; parameters; return_annotation }
          | _ -> None
        in
        parsed_annotation
        |> Type.split
        |> fst
        |> Type.primitive_name
        >>= GlobalResolution.attributes ~resolution:global_resolution
        >>| List.filter_map ~f:to_method
        >>| (fun methods -> TypeQuery.Response (TypeQuery.FoundMethods methods))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Expression.show annotation)))
    | TypeQuery.NamesInFiles paths ->
        let qualified_names = LookupCache.find_all_qualified_names ~state ~configuration ~paths in
        let create_result = function
          | { LookupCache.path; qualified_names_by_location = Some qualified_names; _ } ->
              Either.First
                {
                  TypeQuery.path;
                  qualified_names =
                    List.map ~f:TypeQuery.create_qualified_name_at_location qualified_names;
                }
          | { LookupCache.path; error_reason; _ } -> Either.Second (path, error_reason)
        in
        let results, errors = List.partition_map ~f:create_result qualified_names in
        if List.is_empty errors then
          TypeQuery.Response (TypeQuery.NamesByPath results)
        else
          TypeQuery.Error
            (Format.asprintf "Not able to get lookups in: %s" (get_error_paths errors))
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
        Memory.save_shared_memory ~path ~configuration;
        TypeQuery.Response (TypeQuery.Success (Format.sprintf "Saved state."))
    | TypeQuery.Signature function_names -> (
        let get_signatures function_name =
          let keep_known_annotation annotation =
            match annotation with
            | Type.Top -> None
            | _ -> Some annotation
          in
          match
            UnannotatedGlobalEnvironment.ReadOnly.get_define_body
              unannotated_global_environment
              function_name
          with
          | Some { value = { Statement.Define.signature; _ }; _ } -> (
              let parser = GlobalResolution.annotation_parser global_resolution in
              let variables = GlobalResolution.variables global_resolution in
              let { Type.Callable.annotation; parameters; _ } =
                Analysis.Annotated.Callable.create_overload_without_applying_decorators
                  ~parser
                  ~variables
                  signature
              in
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
                  {
                    TypeQuery.return_type = keep_known_annotation annotation;
                    parameters;
                    function_name = Reference.show function_name;
                  }
              | _ -> raise (MissingFunction function_name) )
          | None -> raise (MissingFunction function_name)
        in
        try
          TypeQuery.Response (TypeQuery.FoundSignature (List.map function_names ~f:get_signatures))
        with
        | MissingFunction function_name ->
            TypeQuery.Error
              (Format.sprintf "No signature found for %s" (Reference.show function_name)) )
    | TypeQuery.Superclasses class_names ->
        let get_superclasses class_name =
          let class_type = parse_and_validate class_name in
          class_type
          |> Type.split
          |> fst
          |> Type.primitive_name
          >>| GlobalResolution.successors ~resolution:global_resolution
          >>| List.map ~f:(fun name -> Type.Primitive name)
          >>| (fun classes ->
                Either.First
                  { TypeQuery.class_name = Type.class_name class_type; superclasses = classes })
          |> Option.value ~default:(Either.Second class_name)
        in
        let results, errors = List.partition_map ~f:get_superclasses class_names in
        if List.is_empty errors then
          TypeQuery.Response (TypeQuery.Superclasses results)
        else
          let bad_annotations =
            List.fold
              ~init:""
              ~f:(fun sofar annotation ->
                Format.asprintf
                  "%s`%a`"
                  (if String.equal sofar "" then "" else sofar ^ ", ")
                  Expression.pp
                  annotation)
              errors
          in
          let plural = if List.length errors > 1 then "s" else "" in
          TypeQuery.Error
            (Format.asprintf "No class definition%s found for %s" plural bad_annotations)
    | TypeQuery.Type expression ->
        let annotation = Resolution.resolve_expression_to_type resolution expression in
        TypeQuery.Response (TypeQuery.Type annotation)
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
          | { LookupCache.path; types_by_location = Some types; _ } ->
              Either.First
                { TypeQuery.path; types = List.map ~f:TypeQuery.create_type_at_location types }
          | { LookupCache.path; error_reason; _ } -> Either.Second (path, error_reason)
        in
        let results, errors = List.partition_map ~f:create_result annotations in
        if List.is_empty errors then
          TypeQuery.Response (TypeQuery.TypesByPath results)
        else
          TypeQuery.Error
            (Format.asprintf "Not able to get lookups in: %s" (get_error_paths errors))
    | TypeQuery.ValidateTaintModels path -> (
        try
          let paths =
            match path with
            | Some path -> [path]
            | None -> configuration.Configuration.Analysis.taint_model_paths
          in
          let configuration =
            Taint.TaintConfiguration.create
              ~rule_filter:None
              ~find_missing_flows:None
              ~dump_model_query_results_path:None
              ~paths
          in
          let get_model_errors sources =
            let model_errors (path, source) =
              Taint.Model.parse
                ~resolution:
                  (TypeCheck.resolution
                     global_resolution
                     (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
                     (module TypeCheck.DummyContext))
                ~path
                ~source
                ~configuration
                Interprocedural.Callable.Map.empty
              |> fun { Taint.Model.errors; _ } -> errors
            in
            List.concat_map sources ~f:model_errors
          in
          let errors = Taint.Model.get_model_sources ~paths |> get_model_errors in
          if List.is_empty errors then
            TypeQuery.Response
              (TypeQuery.Success
                 (Format.asprintf
                    "Models in `%s` are valid."
                    (paths |> List.map ~f:Path.show |> String.concat ~sep:", ")))
          else
            TypeQuery.Response (TypeQuery.ModelVerificationErrors errors)
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
      match LookupCache.find_definition ~state ~configuration path position with
      | None -> TextDocumentDefinitionResponse.create_empty ~id
      | Some { Location.start; stop } -> (
          let module_tracker = TypeEnvironment.module_tracker environment in
          match ModuleTracker.lookup_path ~configuration module_tracker path with
          | ModuleTracker.PathLookup.Found source_path ->
              let path = SourcePath.full_path ~configuration source_path in
              TextDocumentDefinitionResponse.create ~id ~start ~stop ~path
          | _ -> TextDocumentDefinitionResponse.create_empty ~id )
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
      | TypeQueryRequest request -> process_type_query_request ~state ~configuration ~request
      | UnparsableQuery { query; reason } ->
          let response = TypeQuery.Error (Format.sprintf "Unable to parse %s: %s" query reason) in
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
          (* Make sure cache is fresh. We might not have received a close notification. *)
          LookupCache.evict_path ~state ~configuration path;

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
          LookupCache.evict_path ~state ~configuration path;
          let response =
            relative_path
            >>| (fun path ->
                  LanguageServer.Protocol.PublishDiagnostics.clear_diagnostics_for_uri
                    ~uri:(Path.uri (Path.create_absolute path)))
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
          ( if Random.bool () then
              let { Configuration.Analysis.local_root; filter_directories; project_root; _ } =
                configuration
              in
              Telemetry.send_telemetry () ~f:(fun _ ->
                  Telemetry.create_update_message ~local_root ~project_root ~filter_directories) );

          (* On save, evict entries from the lookup cache. The updated source will be picked up at
             the next lookup (if any). *)
          LookupCache.evict_path ~state ~configuration path;
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
        log_request_error ~error:(Format.sprintf "Untracked %s" (Type.show annotation));
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
