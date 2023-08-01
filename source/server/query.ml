(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Analysis
open Pyre

exception InvalidQuery of string

exception IncorrectParameters of Type.t

module Request = struct
  type t =
    | Attributes of Reference.t
    | Batch of t list
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | Defines of Reference.t list
    | DumpCallGraph
    | ExpressionLevelCoverage of string list
    | GlobalLeaks of {
        qualifiers: Reference.t list;
        parse_errors: string list;
      }
    | LessOrEqual of Expression.t * Expression.t
    | ModelQuery of {
        path: PyrePath.t;
        query_name: string;
      }
    | ModulesOfPath of PyrePath.t
    | PathOfModule of Reference.t
    | ReferencesUsedByFile of string
    | SaveServerState of PyrePath.t
    | Superclasses of Reference.t list
    | Type of Expression.t
    | TypesInFiles of string list
    | ValidateTaintModels of {
        path: string option;
        verify_dsl: bool;
      }
  [@@deriving equal, show]
end

module Response = struct
  module Base = struct
    type attribute_kind =
      | Regular
      | Property
    [@@deriving equal, to_yojson]

    type attribute = {
      name: string;
      annotation: Type.t;
      kind: attribute_kind;
      final: bool;
    }
    [@@deriving equal, to_yojson]

    type type_at_location = {
      location: Location.t;
      annotation: Type.t;
    }
    [@@deriving equal, to_yojson]

    type types_at_path = {
      path: string;
      types: type_at_location list;
    }
    [@@deriving equal, to_yojson]

    type coverage_at_path = {
      path: string;
      total_expressions: int;
      coverage_gaps: LocationBasedLookup.coverage_gap_by_location list;
    }
    [@@deriving equal, to_yojson]

    type error_at_path = {
      path: string;
      error: string;
    }
    [@@deriving equal, to_yojson, show]

    type coverage_response_at_path =
      | CoverageAtPath of coverage_at_path
      | ErrorAtPath of error_at_path
    [@@deriving equal, to_yojson]

    type callee_with_instantiated_locations = {
      callee: Analysis.Callgraph.callee;
      locations: Location.WithPath.t list;
    }
    [@@deriving equal]

    type callees = {
      caller: Reference.t;
      callees: callee_with_instantiated_locations list;
    }
    [@@deriving equal]

    type parameter_representation = {
      parameter_name: string;
      parameter_annotation: Expression.t option;
    }
    [@@deriving equal]

    type define = {
      define_name: Reference.t;
      parameters: parameter_representation list;
      return_annotation: Expression.t option;
    }
    [@@deriving equal]

    type superclasses_mapping = {
      class_name: Reference.t;
      superclasses: Reference.t list;
    }
    [@@deriving equal, to_yojson]

    type position = {
      line: int;
      character: int;
    }
    [@@deriving equal, to_yojson]

    type range = {
      start: position;
      end_: position;
    }
    [@@deriving equal]

    let range_to_yojson { start; end_ } =
      `Assoc ["start", position_to_yojson start; "end", position_to_yojson end_]


    type global_leak_errors = {
      global_leaks: Analysis.AnalysisError.Instantiated.t list;
      query_errors: string list;
    }
    [@@deriving equal, to_yojson]

    type taint_model = {
      callable: string;
      model: Yojson.Safe.t;
    }
    [@@deriving equal]

    let taint_model_to_yojson { callable; model } =
      `Assoc ["callable", `String callable; "model", model]


    type t =
      | Boolean of bool
      | Callees of Analysis.Callgraph.callee list
      | CalleesWithLocation of callee_with_instantiated_locations list
      | Callgraph of callees list
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | ExpressionLevelCoverageResponse of coverage_response_at_path list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundModels of taint_model list
      | FoundModules of Reference.t list
      | FoundPath of string
      | GlobalLeakErrors of global_leak_errors
      | ModelVerificationErrors of Taint.ModelVerificationError.t list
      | ReferenceTypesInPath of types_at_path
      | Success of string
      | Superclasses of superclasses_mapping list
      | Type of Type.t
      | TypesByPath of types_at_path list
    [@@deriving equal]

    let to_yojson response =
      let open Analysis in
      match response with
      | Boolean boolean -> `Assoc ["boolean", `Bool boolean]
      | Callees callees ->
          `Assoc ["callees", `List (List.map callees ~f:Callgraph.callee_to_yojson)]
      | CalleesWithLocation callees ->
          let callee_to_yojson { callee; locations } =
            Callgraph.callee_to_yojson ~locations callee
          in
          `Assoc ["callees", `List (List.map callees ~f:callee_to_yojson)]
      | Callgraph callees ->
          let callee_to_yojson { callee; locations } =
            Callgraph.callee_to_yojson ~locations callee
          in
          `Assoc
            (List.map callees ~f:(fun { caller; callees } ->
                 Reference.show caller, `List (List.map callees ~f:callee_to_yojson)))
      | Errors errors ->
          `Assoc
            [
              ( "errors",
                `List (List.map ~f:(fun error -> AnalysisError.Instantiated.to_yojson error) errors)
              );
            ]
      | ExpressionLevelCoverageResponse paths ->
          `List (List.map paths ~f:coverage_response_at_path_to_yojson)
      | GlobalLeakErrors { global_leaks; query_errors } ->
          let string_to_yojson string = `String string in
          `Assoc
            [
              "query_errors", `List (List.map ~f:string_to_yojson query_errors);
              ( "global_leaks",
                `List
                  (List.map
                     ~f:(fun error -> AnalysisError.Instantiated.to_yojson error)
                     global_leaks) );
            ]
      | ModelVerificationErrors errors ->
          `Assoc ["errors", `List (List.map errors ~f:Taint.ModelVerificationError.to_json)]
      | FoundAttributes attributes ->
          let attribute_to_yojson { name; annotation; kind; final } =
            let kind =
              match kind with
              | Regular -> "regular"
              | Property -> "property"
            in

            `Assoc
              [
                "name", `String name;
                "annotation", Type.to_yojson annotation;
                "kind", `String kind;
                "final", `Bool final;
              ]
          in
          `Assoc ["attributes", `List (List.map attributes ~f:attribute_to_yojson)]
      | FoundDefines defines ->
          let define_to_yojson { define_name; parameters; return_annotation } =
            let annotation_to_yojson = function
              | None -> `Null
              | Some annotation ->
                  Ast.Expression.sanitized annotation
                  |> Expression.show
                  |> fun annotation -> `String annotation
            in
            let parameter_representation_to_yojson { parameter_name; parameter_annotation } =
              `Assoc
                [
                  "name", `String parameter_name;
                  "annotation", annotation_to_yojson parameter_annotation;
                ]
            in
            `Assoc
              [
                "name", `String (Reference.show define_name);
                "parameters", `List (List.map parameters ~f:parameter_representation_to_yojson);
                "return_annotation", annotation_to_yojson return_annotation;
              ]
          in
          `List (List.map defines ~f:define_to_yojson)
      | FoundModels models -> `List (List.map ~f:taint_model_to_yojson models)
      | FoundModules references ->
          let reference_to_yojson reference = `String (Reference.show reference) in
          `List (List.map references ~f:reference_to_yojson)
      | FoundPath path -> `Assoc ["path", `String path]
      | ReferenceTypesInPath referenceTypesInPath -> types_at_path_to_yojson referenceTypesInPath
      | Success message -> `Assoc ["message", `String message]
      | Superclasses class_to_superclasses_mapping ->
          let reference_to_yojson reference = `String (Reference.show reference) in
          let mapping_to_yojson { class_name; superclasses } =
            `Assoc [Reference.show class_name, `List (List.map superclasses ~f:reference_to_yojson)]
          in
          `List (List.map class_to_superclasses_mapping ~f:mapping_to_yojson)
      | Type annotation -> `Assoc ["type", Type.to_yojson annotation]
      | TypesByPath paths_to_annotations ->
          `List (List.map paths_to_annotations ~f:types_at_path_to_yojson)
  end

  type t =
    | Single of Base.t
    | Batch of t list
    | Error of string
  [@@deriving equal]

  let rec to_yojson = function
    | Single base_response -> `Assoc ["response", Base.to_yojson base_response]
    | Batch responses -> `Assoc ["response", `List (List.map ~f:to_yojson responses)]
    | Error message -> `Assoc ["error", `String message]


  let create_type_at_location (location, annotation) = { Base.location; annotation }
end

let rec parse_request_exn query =
  let open Expression in
  match PyreParser.Parser.parse [query] with
  | Ok
      [
        {
          Node.value =
            Expression
              {
                Node.value =
                  Call { callee = { Node.value = Name (Name.Identifier name); _ }; arguments };
                _;
              };
          _;
        };
      ] -> (
      let expression { Call.Argument.value; _ } = value in
      let access = function
        | { Call.Argument.value; _ } when has_identifier_base value -> value
        | _ -> raise (InvalidQuery "expected access")
      in
      let reference = function
        | { Call.Argument.value = { Node.value = Name name; _ }; _ } when is_simple_name name ->
            name_to_reference_exn name
        | _ -> raise (InvalidQuery "expected reference")
      in
      let string_of_expression = function
        | {
            Node.value =
              Expression.Constant
                (Constant.String { StringLiteral.value; kind = StringLiteral.String });
            _;
          } ->
            value
        | _ -> raise (InvalidQuery "expected string")
      in
      let string argument = argument |> expression |> string_of_expression in
      let boolean argument =
        let boolean_of_expression = function
          | Expression.Constant Constant.True -> true
          | Expression.Constant Constant.False -> false
          | _ -> raise (InvalidQuery "expected boolean")
        in
        argument |> boolean_of_expression
      in
      let parse_validate_taint_models arguments =
        let path, verify_dsl =
          match arguments with
          | [] -> None, false
          | [
           {
             Call.Argument.name = Some { Node.value = "verify_dsl"; _ };
             value = { Node.value = Expression.Constant (True | False) as verify_dsl; _ };
           };
          ] ->
              None, boolean verify_dsl
          | [path] -> Some (string path), false
          | [
              {
                Call.Argument.name = Some { Node.value = "verify_dsl"; _ };
                value = { Node.value = Expression.Constant (True | False) as verify_dsl; _ };
              };
              path;
            ]
          | [
              path;
              {
                Call.Argument.name = Some { Node.value = "verify_dsl"; _ };
                value = { Node.value = Expression.Constant (True | False) as verify_dsl; _ };
              };
            ] ->
              Some (string path), boolean verify_dsl
          | _ ->
              raise
                (InvalidQuery
                   "validate_taint_models expects optional path and optional `verify_dsl=...`")
        in
        Request.ValidateTaintModels { path; verify_dsl }
      in
      match String.lowercase name, arguments with
      | "attributes", [name] -> Request.Attributes (reference name)
      | "batch", queries ->
          let construct_batch batch_queries query =
            match query with
            | Request.Batch _ -> raise (InvalidQuery "cannot nest batch queries")
            | query -> query :: batch_queries
          in
          List.map ~f:expression queries
          |> List.map ~f:Expression.show
          |> List.map ~f:parse_request_exn
          |> List.fold ~f:construct_batch ~init:[]
          |> List.rev
          |> fun query_list -> Request.Batch query_list
      | "callees", [name] -> Request.Callees (reference name)
      | "callees_with_location", [name] -> Request.CalleesWithLocation (reference name)
      | "defines", names -> Request.Defines (List.map names ~f:reference)
      | "dump_call_graph", [] -> Request.DumpCallGraph
      | "dump_class_hierarchy", [] -> Request.Superclasses []
      | "expression_level_coverage", paths ->
          Request.ExpressionLevelCoverage (List.map ~f:string paths)
      | "global_leaks", arguments ->
          let single_argument_to_reference { Call.Argument.value = qualifier; _ } =
            let construct_invalid_qualifier_string () =
              Ast.Expression.show qualifier
              |> Format.sprintf "Invalid qualifier provided, expected reference but got `%s`"
            in
            match qualifier with
            | { Node.value = Name name; _ } -> (
                match name_to_reference name with
                | None -> Result.Error (construct_invalid_qualifier_string ())
                | Some name -> Result.Ok name)
            | _ -> Result.Error (construct_invalid_qualifier_string ())
          in
          List.map ~f:single_argument_to_reference arguments
          |> List.partition_result
          |> fun (qualifiers, parse_errors) -> Request.GlobalLeaks { qualifiers; parse_errors }
      | "less_or_equal", [left; right] -> Request.LessOrEqual (access left, access right)
      | "model_query", [path; model_query_name] ->
          Request.ModelQuery
            { path = PyrePath.create_absolute (string path); query_name = string model_query_name }
      | "modules_of_path", [path] -> Request.ModulesOfPath (PyrePath.create_absolute (string path))
      | "path_of_module", [module_access] -> Request.PathOfModule (reference module_access)
      | "references_used_by_file", [path] -> Request.ReferencesUsedByFile (string path)
      | "save_server_state", [path] ->
          Request.SaveServerState (PyrePath.create_absolute (string path))
      | "superclasses", names -> Superclasses (List.map ~f:reference names)
      | "type", [argument] -> Type (expression argument)
      | "types", paths -> Request.TypesInFiles (List.map ~f:string paths)
      | "validate_taint_models", arguments -> parse_validate_taint_models arguments
      | _ -> raise (InvalidQuery "unexpected query"))
  | Ok _ -> raise (InvalidQuery "unexpected query")
  | Error _ -> raise (InvalidQuery "failed to parse query")


let parse_request query =
  try Result.Ok (parse_request_exn query) with
  | InvalidQuery reason -> Result.Error reason


let rec process_request ~type_environment ~build_system request =
  let process_request () =
    let configuration =
      TypeEnvironment.ReadOnly.controls type_environment |> EnvironmentControls.configuration
    in
    let module_tracker = TypeEnvironment.ReadOnly.module_tracker type_environment in
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
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
            | _ -> Type.Primitive annotation)
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
        raise (ClassHierarchy.Untracked (Type.show annotation))
    in
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment global_resolution
    in
    let get_error_paths errors =
      List.fold
        ~init:""
        ~f:(fun sofar (path, error_reason) ->
          let print_reason = function
            | LocationBasedLookupProcessor.StubShadowing -> " (file shadowed by .pyi stub file)"
            | LocationBasedLookupProcessor.FileNotFound -> " (file not found)"
          in
          Format.asprintf
            "%s%s`%s`%s"
            sofar
            (if String.is_empty sofar then "" else ", ")
            path
            (print_reason error_reason))
        errors
    in
    let setup_and_execute_model_queries model_queries =
      let scheduler_wrapper scheduler =
        let qualifiers = ModuleTracker.ReadOnly.tracked_explicit_modules module_tracker in
        let initial_callables =
          Interprocedural.FetchCallables.from_qualifiers
            ~scheduler
            ~configuration
            ~environment:type_environment
            ~include_unit_tests:false
            ~qualifiers
        in
        let class_hierarchy_graph =
          Interprocedural.ClassHierarchyGraph.Heap.from_qualifiers
            ~scheduler
            ~environment:type_environment
            ~qualifiers
        in
        Taint.ModelQueryExecution.generate_models_from_queries
          ~resolution:global_resolution
          ~scheduler
          ~class_hierarchy_graph
          ~source_sink_filter:None
          ~verbose:false
          ~definitions_and_stubs:
            (Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true)
          ~stubs:
            (Interprocedural.Target.HashSet.of_list
               (Interprocedural.FetchCallables.get_stubs initial_callables))
          model_queries
      in
      Scheduler.with_scheduler
        ~configuration
        ~should_log_exception:(fun _ -> true)
        ~f:scheduler_wrapper
    in
    let open Response in
    let get_program_call_graph () =
      let get_callgraph callgraph_map module_qualifier =
        let callees
            callgraph_map
            {
              Node.value =
                { Statement.Define.signature = { Statement.Define.Signature.name = caller; _ }; _ };
              _;
            }
          =
          Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
          |> fun callees ->
          Reference.Map.change callgraph_map (Reference.delocalize caller) ~f:(fun old_callees ->
              Option.value ~default:[] old_callees |> fun old_callees -> Some (old_callees @ callees))
        in
        let ast_environment = TypeEnvironment.ReadOnly.ast_environment type_environment in
        AstEnvironment.ReadOnly.get_processed_source ast_environment module_qualifier
        >>| Preprocessing.defines ~include_toplevels:false ~include_stubs:false ~include_nested:true
        >>| List.fold_left ~init:callgraph_map ~f:callees
        |> Option.value ~default:callgraph_map
      in
      let qualifiers = ModuleTracker.ReadOnly.tracked_explicit_modules module_tracker in
      List.fold_left qualifiers ~f:get_callgraph ~init:Reference.Map.empty
    in
    match request with
    | Request.Attributes annotation ->
        let to_attribute attribute =
          let name = AnnotatedAttribute.name attribute in
          let instantiated_annotation =
            GlobalResolution.instantiate_attribute
              ~resolution:global_resolution
              ~accessed_through_class:false
              ~accessed_through_readonly:false
              attribute
          in
          let annotation =
            instantiated_annotation |> AnnotatedAttribute.annotation |> Annotation.annotation
          in
          let property = AnnotatedAttribute.property attribute in
          let kind =
            if property then
              Base.Property
            else
              Base.Regular
          in
          let final = AnnotatedAttribute.is_final instantiated_annotation in
          { Base.name; annotation; kind; final }
        in
        parse_and_validate (Expression.from_reference ~location:Location.any annotation)
        |> Type.split
        |> fst
        |> Type.primitive_name
        >>= GlobalResolution.attributes ~resolution:global_resolution
        >>| List.map ~f:to_attribute
        >>| (fun attributes -> Single (Base.FoundAttributes attributes))
        |> Option.value
             ~default:
               (Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | Batch requests ->
        Batch (List.map ~f:(process_request ~type_environment ~build_system) requests)
    | Callees caller ->
        (* We don't yet support a syntax for fetching property setters. *)
        Single
          (Base.Callees
             (Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
             |> List.map ~f:(fun { Callgraph.callee; _ } -> callee)))
    | CalleesWithLocation caller ->
        let instantiate =
          Location.WithModule.instantiate
            ~lookup:(ModuleTracker.ReadOnly.lookup_relative_path module_tracker)
        in
        let callees =
          (* We don't yet support a syntax for fetching property setters. *)
          Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
          |> List.map ~f:(fun { Callgraph.callee; locations } ->
                 { Base.callee; locations = List.map locations ~f:instantiate })
        in
        Single (Base.CalleesWithLocation callees)
    | Defines module_or_class_names ->
        let defines_of_module module_or_class_name =
          let module_name, filter_define =
            if ModuleTracker.ReadOnly.is_module_tracked module_tracker module_or_class_name then
              Some module_or_class_name, fun _ -> false
            else
              let filter
                  { Statement.Define.signature = { Statement.Define.Signature.parent; _ }; _ }
                =
                not (Option.equal Reference.equal parent (Some module_or_class_name))
              in
              let rec find_module_name current_reference =
                if ModuleTracker.ReadOnly.is_module_tracked module_tracker current_reference then
                  Some current_reference
                else
                  Reference.prefix current_reference >>= find_module_name
              in
              find_module_name module_or_class_name, filter
          in
          let defines =
            let ast_environment = TypeEnvironment.ReadOnly.ast_environment type_environment in
            module_name
            >>= AstEnvironment.ReadOnly.get_processed_source ast_environment
            >>| Analysis.FunctionDefinition.collect_defines
            >>| List.map ~f:snd
            >>| List.concat_map ~f:Analysis.FunctionDefinition.all_bodies
            >>| List.filter ~f:(fun { Node.value = define; _ } ->
                    not
                      (Statement.Define.is_toplevel define
                      || Statement.Define.is_class_toplevel define
                      || Statement.Define.is_overloaded_function define
                      || filter_define define))
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
              { Base.parameter_name = Identifier.sanitized name; parameter_annotation = annotation }
            in
            {
              Base.define_name = name;
              parameters = List.map parameters ~f:represent_parameter;
              return_annotation;
            }
          in
          List.map defines ~f:represent
        in
        List.concat_map module_or_class_names ~f:defines_of_module
        |> fun defines -> Single (Base.FoundDefines defines)
    | DumpCallGraph ->
        let create_response_with_caller ~key:caller ~data:callees response =
          let instantiate =
            Location.WithModule.instantiate
              ~lookup:(PathLookup.instantiate_path_with_build_system ~build_system ~module_tracker)
          in
          List.map
            ~f:(fun { Callgraph.callee; locations } ->
              { Base.callee; locations = List.map locations ~f:instantiate })
            callees
          |> fun callees -> { Base.caller; callees } :: response
        in
        get_program_call_graph ()
        |> Reference.Map.fold ~f:create_response_with_caller ~init:[]
        |> fun result -> Single (Base.Callgraph result)
    | ExpressionLevelCoverage paths ->
        let read_text_file path =
          try In_channel.read_lines path |> List.map ~f:(fun x -> Result.Ok x) with
          | _ -> [Result.Error (path, LocationBasedLookupProcessor.FileNotFound)]
        in
        let get_text_file_path_list path =
          read_text_file path
          |> List.filter ~f:(fun path ->
                 match path with
                 | Result.Ok path -> not (String.equal (String.strip path) "")
                 | Result.Error _ -> true)
        in
        let extract_paths path =
          match String.chop_prefix path ~prefix:"@" with
          | None -> [Result.Ok path]
          | Some arguments_path -> get_text_file_path_list arguments_path
        in
        let find_resolved_types result =
          let has_valid_suffix path =
            let valid_suffixes =
              ".py" :: ".pyi" :: Configuration.Analysis.extension_suffixes configuration
            in
            let extension =
              Filename.split_extension path
              |> snd
              >>| (fun extension -> "." ^ extension)
              |> Option.value ~default:""
            in
            List.exists ~f:(String.equal extension) valid_suffixes
          in
          let format_error error =
            Format.asprintf "Not able to get lookups in: %s" (get_error_paths error)
          in
          match result with
          | Result.Error (path, error_reason) ->
              Base.ErrorAtPath { Base.path; error = format_error [path, error_reason] }
          | Result.Ok path ->
              if has_valid_suffix path then
                match
                  LocationBasedLookupProcessor.find_expression_level_coverage_for_path
                    ~type_environment
                    ~build_system
                    path
                with
                | Result.Ok { total_expressions; coverage_gaps } ->
                    Base.CoverageAtPath { Base.path; total_expressions; coverage_gaps }
                | Result.Error error_reason ->
                    Base.ErrorAtPath { Base.path; error = format_error [path, error_reason] }
              else
                Base.ErrorAtPath
                  {
                    Base.path;
                    error = format_error [path, LocationBasedLookupProcessor.FileNotFound];
                  }
        in

        let results = List.concat_map paths ~f:extract_paths |> List.map ~f:find_resolved_types in
        Single (Base.ExpressionLevelCoverageResponse results)
    | GlobalLeaks { qualifiers; parse_errors } ->
        let lookup =
          let module_tracker = GlobalResolution.module_tracker global_resolution in
          PathLookup.instantiate_path_with_build_system ~build_system ~module_tracker
        in
        let find_leak_errors_for_qualifier qualifier =
          Analysis.GlobalLeakCheck.check_qualifier ~type_environment qualifier
          >>| List.map ~f:(fun error ->
                  AnalysisError.instantiate ~show_error_traces:true ~lookup error)
          |> Result.of_option
               ~error:(Format.sprintf "No qualifier found for `%s`" (Reference.show qualifier))
        in
        let construct_result (global_leaks, errors) =
          Single
            (Base.GlobalLeakErrors
               { global_leaks = List.concat global_leaks; query_errors = parse_errors @ errors })
        in
        List.map ~f:find_leak_errors_for_qualifier qualifiers
        |> List.partition_result
        |> construct_result
    | ModelQuery { path; query_name } -> (
        if not (PyrePath.file_exists path) then
          Error (Format.sprintf "File path `%s` does not exist" (PyrePath.show path))
        else
          let taint_configuration_result = Taint.TaintConfiguration.from_taint_model_paths [path] in
          match taint_configuration_result with
          | Error (error :: _) -> Error (Taint.TaintConfiguration.Error.show error)
          | Error _ -> failwith "Taint.TaintConfiguration.create returned empty errors list"
          | Ok taint_configuration -> (
              let python_version =
                Taint.ModelParser.PythonVersion.from_configuration configuration
              in
              let get_model_queries (path, source) =
                Taint.ModelParser.parse
                  ~resolution:global_resolution
                  ~path
                  ~source
                  ~taint_configuration
                  ~source_sink_filter:None
                  ~definitions:None
                  ~stubs:(Interprocedural.Target.HashSet.create ())
                  ~python_version
                  ()
                |> fun { Taint.ModelParseResult.queries; errors; _ } ->
                if List.is_empty errors then
                  Ok
                    (List.filter queries ~f:(fun rule ->
                         String.equal rule.Taint.ModelParseResult.ModelQuery.name query_name))
                else
                  Error errors
              in
              let rules =
                let sources = Taint.ModelParser.get_model_sources ~paths:[path] in
                if List.is_empty sources then
                  failwith
                    (Format.sprintf
                       "ModelParser.get_model_sources ~paths:[%s] was empty"
                       (PyrePath.show path))
                else
                  let open Result in
                  sources |> List.map ~f:get_model_queries |> Result.combine_errors >>| List.concat
              in
              match rules with
              | Error errors ->
                  Error
                    (List.fold (List.concat errors) ~init:"" ~f:(fun accum error ->
                         accum ^ Taint.ModelVerificationError.display error))
              | Ok rules ->
                  if List.is_empty rules then
                    Error
                      (Format.sprintf
                         "No model query with name `%s` was found in path `%s`"
                         query_name
                         (PyrePath.show path))
                  else
                    let models_and_names, errors = setup_and_execute_model_queries rules in
                    let to_taint_model (callable, model) =
                      {
                        Base.callable = Interprocedural.Target.external_name callable;
                        model =
                          Taint.Model.to_json
                            ~expand_overrides:None
                            ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
                            ~filename_lookup:None
                            ~export_leaf_names:Taint.Domains.ExportLeafNames.Always
                            callable
                            model;
                      }
                    in
                    let models =
                      models_and_names
                      |> Taint.ModelQueryExecution.ModelQueryRegistryMap.get_registry
                           ~model_join:Taint.Model.join_user_models
                      |> Taint.Registry.to_alist
                      |> List.map ~f:to_taint_model
                    in
                    if List.is_empty models then
                      Error
                        (Format.sprintf
                           "No models found for model query `%s` in path `%s`"
                           query_name
                           (PyrePath.show path))
                    else if not (List.is_empty errors) then
                      Error
                        (List.fold errors ~init:"" ~f:(fun accum error ->
                             accum ^ Taint.ModelVerificationError.display error))
                    else
                      Single (Base.FoundModels models)))
    | ModulesOfPath path ->
        Single
          (Base.FoundModules
             (SourcePath.create path
             |> PathLookup.modules_of_source_path_with_build_system ~build_system ~module_tracker))
    | LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.less_or_equal global_resolution ~left ~right
        |> fun response -> Single (Base.Boolean response)
    | PathOfModule module_name ->
        ModuleTracker.ReadOnly.lookup_module_path module_tracker module_name
        >>= (fun source_path ->
              let path =
                ModulePath.full_path ~configuration source_path
                |> ArtifactPath.raw
                |> PyrePath.absolute
              in
              Some (Single (Base.FoundPath path)))
        |> Option.value
             ~default:
               (Error (Format.sprintf "No path found for module `%s`" (Reference.show module_name)))
    | ReferencesUsedByFile path ->
        if
          TypeEnvironment.ReadOnly.controls type_environment
          |> EnvironmentControls.no_validation_on_class_lookup_failure
        then
          let resolved_types =
            LocationBasedLookupProcessor.find_all_resolved_types_for_path
              ~type_environment
              ~build_system
              path
          in
          match resolved_types with
          | Result.Ok types ->
              let result = { Base.path; types = List.map ~f:create_type_at_location types } in
              Single (Base.ReferenceTypesInPath result)
          | Error error_reason ->
              Error
                (Format.asprintf
                   "Not able get lookups in: %s, with error: %s"
                   path
                   (LocationBasedLookupProcessor.show_error_reason error_reason))
        else
          Error
            (Format.asprintf
               "Cannot run query references_used_by_file(path='%s') because flag \
                'no_validation_on_class_lookup_failure' flag is false, and it is expected to be \
                set to true for all 'references_used_by_file queries'. Please set the value of \
                'no_validation_on_class_lookup_failure' to true."
               path)
    | SaveServerState path ->
        let path = PyrePath.absolute path in
        Log.info "Saving server state into `%s`" path;
        Memory.save_shared_memory ~path ~configuration;
        Single (Base.Success (Format.sprintf "Saved state."))
    | Superclasses class_names ->
        let get_superclasses class_name =
          Reference.show class_name
          |> GlobalResolution.successors ~resolution:global_resolution
          |> List.sort ~compare:String.compare
          |> List.map ~f:Reference.create
          |> fun superclasses -> { Base.class_name; superclasses }
        in
        let class_names =
          match class_names with
          | [] ->
              (* Because of lazy parsing, `UnannotatedGlobalEnvironment.ReadOnly.get_all_classes
                 only returns all *loaded* classes, which will not include all classes from external
                 modules if they are not needed for type checking. So, we force them to load. *)
              let load_all_modules scheduler =
                let load_modules qualifiers =
                  let _ =
                    List.map
                      qualifiers
                      ~f:
                        (UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata
                           unannotated_global_environment)
                  in
                  ()
                in
                Scheduler.iter
                  scheduler
                  ~policy:
                    (Scheduler.Policy.fixed_chunk_count
                       ~minimum_chunks_per_worker:1
                       ~minimum_chunk_size:100
                       ~preferred_chunks_per_worker:5
                       ())
                  ~f:load_modules
                  ~inputs:(ModuleTracker.ReadOnly.tracked_explicit_modules module_tracker)
              in
              Scheduler.with_scheduler
                ~configuration
                ~should_log_exception:(fun _ -> true)
                ~f:load_all_modules;
              UnannotatedGlobalEnvironment.ReadOnly.all_classes unannotated_global_environment
              |> List.map ~f:Reference.create
          | _ ->
              List.filter class_names ~f:(fun class_name ->
                  Reference.show class_name |> GlobalResolution.class_exists global_resolution)
        in
        Single (Superclasses (List.map ~f:get_superclasses class_names))
    | Type expression ->
        let annotation = Resolution.resolve_expression_to_type resolution expression in
        Single (Type annotation)
    | TypesInFiles paths ->
        let find_resolved_types path =
          match
            LocationBasedLookupProcessor.find_all_resolved_types_for_path
              ~type_environment
              ~build_system
              path
          with
          | Result.Ok types ->
              Either.First { Base.path; types = List.map ~f:create_type_at_location types }
          | Result.Error error_reason -> Either.Second (path, error_reason)
        in
        let results, errors = List.partition_map ~f:find_resolved_types paths in
        if List.is_empty errors then
          Single (Base.TypesByPath results)
        else
          Error (Format.asprintf "Not able to get lookups in: %s" (get_error_paths errors))
    | ValidateTaintModels { path; verify_dsl } ->
        let paths =
          match path with
          | Some path ->
              if String.is_prefix ~prefix:"/" path then
                [PyrePath.create_absolute ~follow_symbolic_links:true path]
              else
                let { Configuration.Analysis.local_root = root; _ } = configuration in
                [PyrePath.create_relative ~root ~relative:path]
          | None -> configuration.Configuration.Analysis.taint_model_paths
        in
        let taint_configuration =
          Taint.TaintConfiguration.from_taint_model_paths paths
          |> Taint.TaintConfiguration.exception_on_error
        in
        let get_model_errors_and_model_queries sources =
          let python_version = Taint.ModelParser.PythonVersion.from_configuration configuration in
          let get_model_errors_and_model_queries (path, source) =
            Taint.ModelParser.parse
              ~resolution:global_resolution
              ~path
              ~source
              ~taint_configuration
              ~source_sink_filter:None
              ~definitions:None
              ~stubs:(Interprocedural.Target.HashSet.create ())
              ~python_version
              ()
            |> fun { Taint.ModelParseResult.errors; queries; _ } -> errors, queries
          in
          let model_errors_and_model_queries =
            List.map sources ~f:get_model_errors_and_model_queries
          in
          let errors = List.concat (List.map model_errors_and_model_queries ~f:fst) in
          let model_queries = List.concat (List.map model_errors_and_model_queries ~f:snd) in
          errors, model_queries
        in
        let model_parse_errors, model_queries =
          get_model_errors_and_model_queries (Taint.ModelParser.get_model_sources ~paths)
        in
        let model_query_errors =
          if verify_dsl then
            setup_and_execute_model_queries model_queries |> snd
          else
            []
        in
        let errors = List.append model_parse_errors model_query_errors in
        if List.is_empty errors then
          Single
            (Base.Success
               (Format.asprintf
                  "Models in `%s` are valid."
                  (paths |> List.map ~f:PyrePath.show |> String.concat ~sep:", ")))
        else
          Single (Base.ModelVerificationErrors errors)
  in
  try process_request () with
  | ClassHierarchy.Untracked untracked ->
      let untracked_response =
        Format.asprintf "Type `%s` was not found in the type order." untracked
      in
      Error untracked_response
  | IncorrectParameters untracked ->
      let untracked_response =
        Format.asprintf "Type `%a` has the wrong number of parameters." Type.pp untracked
      in
      Error untracked_response
  | ClassHierarchy.Cyclic trace ->
      Error
        (Format.asprintf
           "Cyclic class hierarchy: {%s}"
           (Hash_set.to_list trace |> String.concat ~sep:", "))
  | Taint.TaintConfiguration.TaintConfigurationError errors ->
      errors
      |> List.map ~f:Taint.TaintConfiguration.Error.show
      |> String.concat ~sep:"\n"
      |> Format.sprintf "Found %d taint configuration errors:\n%s" (List.length errors)
      |> fun message -> Response.Error message
  | Taint.ModelVerificationError.ModelVerificationErrors errors ->
      errors
      |> List.map ~f:Taint.ModelVerificationError.show
      |> String.concat ~sep:"\n"
      |> Format.sprintf "Found %d model verification errors:\n%s" (List.length errors)
      |> fun message -> Response.Error message


let parse_and_process_request ~overlaid_environment ~build_system request overlay_id =
  let type_environment =
    match overlay_id with
    | Some overlay_id -> (
        let overlay_environment = OverlaidEnvironment.overlay overlaid_environment overlay_id in
        match overlay_environment with
        | Some env -> ErrorsEnvironment.ReadOnly.type_environment env
        | None ->
            Log.info
              "No valid overlay found for overlay_id: %s. Using the base environment to serve the \
               request instead."
              overlay_id;
            ErrorsEnvironment.ReadOnly.type_environment
              (OverlaidEnvironment.root overlaid_environment))
    | None ->
        OverlaidEnvironment.root overlaid_environment |> ErrorsEnvironment.ReadOnly.type_environment
  in
  match parse_request request with
  | Result.Error reason -> Response.Error reason
  | Result.Ok request -> process_request ~type_environment ~build_system request
