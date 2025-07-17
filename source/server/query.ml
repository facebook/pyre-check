(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module implements the backend of the Pyre query API, which powers
 * various use cases that ask a running server to do things, including:
 * - saved state dumping
 * - various endpoints for pysa tools to get type information
 * - experimental integrations with linters
 * - IDE features like coverage in the `pyre persistent` language server
 *
 * Processing a query (which comes to a running daemon by way of a request from
 * the client) works as follows:
 * - Parse the query (which is a string of pseudo-python) into a Request.t
 * - Pass the request to process_request_exn
 * - Return a json representation of the resulting Response.t
 *)

open Core
open Ast
open Analysis
open Pyre

exception InvalidQuery of string

exception IncorrectParameters of Type.t

module Request = struct
  type define_kind =
    | DefBody
    | ClassToplevel
    | ModuleToplevel
  [@@deriving equal, show]

  type t =
    | Attributes of Reference.t
    | Batch of t list
    | Callees of Reference.t
    | CalleesWithLocation of {
        caller: Reference.t;
        define_kind: define_kind;
      }
    | Defines of Reference.t list
    | DumpCallGraph
    | ExpressionLevelCoverage of string list
    | GlobalLeaks of {
        qualifiers: Reference.t list;
        parse_errors: string list;
      }
    | TypeAtLocation of {
        path: PyrePath.t;
        location: Location.t;
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
    | IsTypechecked of string list
    | TypecheckedPaths
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
      coverage_gaps: LocationBasedLookup.ExpressionLevelCoverage.coverage_gap_by_location list;
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

    type typechecked = {
      path: string;
      is_typechecked: bool;
    }
    [@@deriving equal, to_yojson]

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
      | CalleesWithLocation of callee_with_instantiated_locations list option
      | Callgraph of callees list
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | ExpressionLevelCoverageResponse of coverage_response_at_path list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundModels of taint_model list
      | FoundModules of Reference.t list
      | FoundPath of string
      | GlobalLeakErrors of global_leak_errors
      | TypeAtLocation of Type.t option
      | ModelVerificationErrors of Taint.ModelVerificationError.t list
      | ReferenceTypesInPath of types_at_path
      | Success of string
      | Superclasses of superclasses_mapping list
      | Type of Type.t
      | IsTypechecked of typechecked list
      | TypecheckedPaths of string list
      | TypesByPath of types_at_path list
    [@@deriving equal]

    let to_yojson response =
      let open Analysis in
      match response with
      | Boolean boolean -> `Assoc ["boolean", `Bool boolean]
      | Callees callees ->
          `Assoc ["callees", `List (List.map callees ~f:Callgraph.callee_to_yojson)]
      | CalleesWithLocation maybe_callees ->
          let callee_to_yojson { callee; locations } =
            Callgraph.callee_to_yojson ~locations callee
          in
          let callees_to_yojson = function
            | Some callees -> `List (List.map callees ~f:callee_to_yojson)
            | None -> `Null
          in
          `Assoc ["callees", callees_to_yojson maybe_callees]
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
      | TypecheckedPaths paths -> `List (List.map paths ~f:(fun path -> `String path))
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
      | TypeAtLocation maybe_type -> [%to_yojson: Type.t option] maybe_type
      | ReferenceTypesInPath referenceTypesInPath -> types_at_path_to_yojson referenceTypesInPath
      | Success message -> `Assoc ["message", `String message]
      | Superclasses class_to_superclasses_mapping ->
          let reference_to_yojson reference = `String (Reference.show reference) in
          let mapping_to_yojson { class_name; superclasses } =
            `Assoc [Reference.show class_name, `List (List.map superclasses ~f:reference_to_yojson)]
          in
          `List (List.map class_to_superclasses_mapping ~f:mapping_to_yojson)
      | Type annotation -> `Assoc ["type", Type.to_yojson annotation]
      | IsTypechecked paths -> `List (List.map paths ~f:typechecked_to_yojson)
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

(* An cache for expensive queries. We don't generally cache queries, but we have added caching on an
   ad-hoc basis to support specific static analysis use cases *)
module Cache = struct
  type t = LocationBasedLookupProcessor.types_by_location Reference.Table.t

  let create () = Reference.Table.create ()

  let invalidate cache update_result =
    Analysis.ErrorsEnvironment.UpdateResult.modules_with_invalidated_type_check update_result
    |> Set.iter ~f:(fun module_name -> Hashtbl.remove cache module_name)


  let find cache qualifier = Hashtbl.find cache qualifier

  let save cache qualifier types = Hashtbl.set ~key:qualifier ~data:types cache
end

let rec parse_request_exn query =
  let open Expression in
  match PyreMenhirParser.Parser.parse [query] with
  | Ok
      [
        {
          Node.value =
            Expression
              {
                Node.value =
                  Call
                    {
                      callee = { Node.value = Name (Name.Identifier name); _ };
                      arguments;
                      origin = _;
                    };
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
      let integer argument =
        let integer_of_expression = function
          | { Node.value = Expression.Constant (Constant.Integer value); _ } -> value
          | _ -> raise (InvalidQuery "expected integer")
        in
        argument |> expression |> integer_of_expression
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
      | "callees_with_location", arguments -> (
          match arguments with
          | [caller] ->
              Request.CalleesWithLocation
                { caller = reference caller; define_kind = Request.DefBody }
          | [caller; define_kind_string] ->
              let define_kind =
                match string define_kind_string with
                | "def_body" -> Request.DefBody
                | "module_toplevel" -> Request.ModuleToplevel
                | "class_toplevel" -> Request.ClassToplevel
                | unknown_define_kind ->
                    raise
                      (InvalidQuery
                         ("Unexpected define kind "
                         ^ unknown_define_kind
                         ^ "', expected one of 'def_body', 'module_top_level', 'class_top_level'"))
              in
              Request.CalleesWithLocation { caller = reference caller; define_kind }
          | _ -> raise (InvalidQuery "Expected one or two arguments to `callees_with_location`"))
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
      | "type_at_location", [path; start_line; start_column; stop_line; stop_column] ->
          Request.TypeAtLocation
            {
              path = PyrePath.create_absolute (string path);
              location =
                {
                  Location.start =
                    { Location.line = integer start_line; column = integer start_column };
                  stop = { Location.line = integer stop_line; column = integer stop_column };
                };
            }
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
      | "typechecked_paths", _ -> Request.TypecheckedPaths
      | "types", paths -> Request.TypesInFiles (List.map ~f:string paths)
      | "is_typechecked", paths -> Request.IsTypechecked (List.map ~f:string paths)
      | "validate_taint_models", arguments -> parse_validate_taint_models arguments
      | _ -> raise (InvalidQuery "unexpected query"))
  | Ok _ -> raise (InvalidQuery "unexpected query")
  | Error _ -> raise (InvalidQuery "failed to parse query")


let parse_request query =
  try Result.Ok (parse_request_exn query) with
  | InvalidQuery reason -> Result.Error reason


let rec process_request_exn
    ~type_environment
    ~global_module_paths_api
    ~scheduler
    ~build_system
    ~query_cache
    request
  =
  let process_request_exn () =
    let configuration =
      TypeEnvironment.ReadOnly.controls type_environment |> EnvironmentControls.configuration
    in
    let source_code_api = TypeEnvironment.ReadOnly.get_untracked_source_code_api type_environment in
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
        ?(raise_if_missing_arguments = true)
        expression
      =
      let annotation =
        (* Return untracked so we can specifically message the user about them. *)
        GlobalResolution.parse_annotation ~validation:NoValidation global_resolution expression
      in
      let annotation =
        if unknown_is_top then
          let type_map = function
            | Type.Primitive "unknown" -> Some Type.Top
            | _ -> None
          in
          Type.apply_type_map annotation ~type_map
        else
          annotation
      in
      let annotation =
        match fill_missing_type_parameters_with_any, annotation with
        | true, Type.Primitive annotation -> (
            let generics =
              GlobalResolution.generic_parameters_as_variables global_resolution annotation
            in
            match generics with
            | Some generics
              when (not (List.is_empty generics))
                   && List.for_all generics ~f:(function
                          | Type.Variable.TypeVarVariable _ -> true
                          | _ -> false) ->
                Type.parametric
                  annotation
                  (List.map generics ~f:(fun _ -> Type.Argument.Single Type.Any))
            | _ -> Type.Primitive annotation)
        | _ -> annotation
      in
      if ClassHierarchy.is_instantiated order annotation then
        if raise_if_missing_arguments then
          let mismatches, _ =
            GlobalResolution.validate_and_sanitize_type_arguments global_resolution annotation
          in
          if List.is_empty mismatches then
            annotation
          else
            raise (IncorrectParameters annotation)
        else
          annotation
      else
        raise (ClassHierarchy.Untracked (Type.show annotation))
    in
    let get_error_paths errors =
      List.fold
        ~init:""
        ~f:(fun sofar (path, error_reason) ->
          let print_reason = function
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
    let parse_model_queries
        ~pyre_api
        ~taint_configuration
        ~python_version
        ~filter_query
        ~model_paths
      =
      let step_logger =
        Taint.StepLogger.start
          ~command:"query"
          ~start_message:"Parsing taint models"
          ~end_message:"Parsed taint models"
          ()
      in
      let sources = Taint.ModelParser.get_model_sources ~paths:model_paths in
      let parse_model_source (path, source) =
        Taint.ModelParser.parse
          ~pyre_api
          ~path
          ~source
          ~taint_configuration
          ~source_sink_filter:None
          ~definitions:None
          ~stubs:
            ([]
            |> Interprocedural.Target.HashsetSharedMemory.from_heap
            |> Interprocedural.Target.HashsetSharedMemory.read_only)
          ~python_version
          ()
        |> fun { Taint.ModelParseResult.queries; errors; _ } ->
        {
          Taint.ModelParseResult.queries = List.filter queries ~f:filter_query;
          models = Taint.Registry.empty;
          errors;
        }
      in
      let map sources =
        List.fold
          ~init:Taint.ModelParseResult.empty
          ~f:(fun sofar source -> parse_model_source source |> Taint.ModelParseResult.join sofar)
          sources
      in
      let result =
        Scheduler.map_reduce
          scheduler
          ~policy:
            (Scheduler.Policy.fixed_chunk_count
               ~minimum_chunks_per_worker:1
               ~minimum_chunk_size:1
               ~preferred_chunks_per_worker:1
               ())
          ~initial:Taint.ModelParseResult.empty
          ~map
          ~reduce:Taint.ModelParseResult.join
          ~inputs:sources
          ()
      in
      Taint.StepLogger.finish step_logger;
      result
    in
    let setup_and_execute_model_queries ~pyre_api model_queries =
      let qualifiers = GlobalModulePathsApi.explicit_qualifiers global_module_paths_api in
      let initial_callables =
        let step_logger =
          Taint.StepLogger.start
            ~command:"query"
            ~start_message:"Fetching initial callables to analyze"
            ~end_message:"Fetched initial callables to analyze"
            ()
        in
        let initial_callables =
          Interprocedural.FetchCallables.from_qualifiers
            ~scheduler
            ~scheduler_policy:
              (Scheduler.Policy.fixed_chunk_count
                 ~minimum_chunks_per_worker:1
                 ~minimum_chunk_size:1
                 ~preferred_chunks_per_worker:1
                 ())
            ~configuration
            ~pyre_api
            ~qualifiers
        in
        Taint.StepLogger.finish step_logger;
        initial_callables
      in
      let class_hierarchy_graph =
        let step_logger =
          Taint.StepLogger.start
            ~command:"query"
            ~start_message:"Computing class hierarchy graph"
            ~end_message:"Computed class hierarchy graph"
            ()
        in
        let class_hierarchy_graph =
          Interprocedural.ClassHierarchyGraph.Heap.from_qualifiers
            ~scheduler
            ~scheduler_policies:Configuration.SchedulerPolicies.empty
            ~pyre_api
            ~qualifiers
        in
        Taint.StepLogger.finish step_logger;
        class_hierarchy_graph
      in
      let stubs_shared_memory_handle =
        Interprocedural.Target.HashsetSharedMemory.from_heap
          (Interprocedural.FetchCallables.get_stubs initial_callables)
      in
      let definitions_and_stubs =
        Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
      in
      let callables_to_definitions_map =
        let step_logger =
          Taint.StepLogger.start
            ~command:"query"
            ~start_message:"Building a map from callable names to definitions"
            ~end_message:"Map from callable names to definitions built"
            ()
        in
        let callables_to_definitions_map =
          Interprocedural.Target.CallablesSharedMemory.from_callables
            ~scheduler
            ~scheduler_policy:
              (Scheduler.Policy.fixed_chunk_count
                 ~minimum_chunks_per_worker:1
                 ~minimum_chunk_size:1
                 ~preferred_chunks_per_worker:1
                 ())
            ~pyre_api
            definitions_and_stubs
        in
        Taint.StepLogger.finish step_logger;
        callables_to_definitions_map
      in
      Taint.ModelQueryExecution.generate_models_from_queries
        ~pyre_api
        ~scheduler
        ~scheduler_policies:Configuration.SchedulerPolicies.empty
        ~class_hierarchy_graph
        ~callables_to_definitions_map:
          (Interprocedural.Target.CallablesSharedMemory.read_only callables_to_definitions_map)
        ~source_sink_filter:None
        ~verbose:false
        ~error_on_unexpected_models:true
        ~error_on_empty_result:true
        ~definitions_and_stubs:
          (Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true)
        ~stubs:(Interprocedural.Target.HashsetSharedMemory.read_only stubs_shared_memory_handle)
        model_queries
    in
    let open Response in
    let get_program_call_graph () =
      let get_callgraph callgraph_map module_qualifier =
        let callees callgraph_map { Node.value = caller_define; _ } =
          let qualified_caller =
            FunctionDefinition.qualified_name_of_define ~module_name:module_qualifier caller_define
          in
          TypeEnvironment.ReadOnly.get_callees type_environment qualified_caller
          |> Option.value ~default:[]
          |> fun callees ->
          Map.change callgraph_map qualified_caller ~f:(fun old_callees ->
              Option.value ~default:[] old_callees |> fun old_callees -> Some (old_callees @ callees))
        in

        SourceCodeApi.source_of_qualifier source_code_api module_qualifier
        >>| Preprocessing.defines ~include_toplevels:false ~include_stubs:false ~include_nested:true
        >>| List.fold_left ~init:callgraph_map ~f:callees
        |> Option.value ~default:callgraph_map
      in
      let qualifiers = GlobalModulePathsApi.explicit_qualifiers global_module_paths_api in
      List.fold_left qualifiers ~f:get_callgraph ~init:Reference.Map.empty
    in
    let qualifier_of_path path =
      let relative_path =
        let { Configuration.Analysis.local_root = root; _ } = configuration in
        PyrePath.create_relative ~root ~relative:(PyrePath.absolute path) |> SourcePath.create
      in
      match
        PathLookup.qualifiers_of_source_path_with_build_system
          ~build_system
          ~source_code_api
          relative_path
      with
      | [found_module] -> Some found_module
      | _ -> None
    in
    match request with
    | Request.Attributes annotation ->
        let to_attribute attribute =
          let name = AnnotatedAttribute.name attribute in
          let instantiated_annotation =
            GlobalResolution.instantiate_attribute
              global_resolution
              ~accessed_through_class:false
              ~accessed_through_readonly:false
              attribute
          in
          let annotation =
            instantiated_annotation |> AnnotatedAttribute.annotation |> TypeInfo.Unit.annotation
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
        parse_and_validate
          (Expression.from_reference
             ~location:Location.any
             ~create_origin:(fun _ -> None)
             annotation)
          ~raise_if_missing_arguments:false
        |> Type.split
        |> fst
        |> Type.primitive_name
        >>= GlobalResolution.uninstantiated_attributes global_resolution
        >>| List.map ~f:to_attribute
        >>| (fun attributes -> Single (Base.FoundAttributes attributes))
        |> Option.value
             ~default:
               (Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | Batch requests ->
        Batch
          (List.map
             ~f:
               (process_request_exn
                  ~type_environment
                  ~global_module_paths_api
                  ~scheduler
                  ~build_system
                  ~query_cache)
             requests)
    | Callees caller ->
        let callees =
          TypeEnvironment.ReadOnly.get_callees type_environment caller
          |> Option.value ~default:[]
          |> List.map ~f:(fun { Callgraph.callee; _ } -> callee)
        in
        Single (Base.Callees callees)
    | CalleesWithLocation { caller; define_kind } ->
        let mangled_caller =
          match define_kind with
          | DefBody -> caller
          | ClassToplevel -> Reference.create ~prefix:caller Statement.class_toplevel_define_name
          | ModuleToplevel -> Reference.create ~prefix:caller Statement.toplevel_define_name
        in
        let instantiate =
          Location.WithModule.instantiate
            ~lookup:(SourceCodeApi.relative_path_of_qualifier source_code_api)
        in
        let callees =
          TypeEnvironment.ReadOnly.get_callees type_environment mangled_caller
          >>| List.map ~f:(fun { Callgraph.callee; locations } ->
                  { Base.callee; locations = List.map locations ~f:instantiate })
        in
        Single (Base.CalleesWithLocation callees)
    | Defines module_or_class_names ->
        let defines_of_module module_or_class_name =
          let module_name, filter_define =
            if SourceCodeApi.is_qualifier_tracked source_code_api module_or_class_name then
              Some module_or_class_name, fun _ -> false
            else
              let filter
                  {
                    Statement.Define.signature = { Statement.Define.Signature.legacy_parent; _ };
                    _;
                  }
                =
                not (Option.equal Reference.equal legacy_parent (Some module_or_class_name))
              in
              let rec find_module_name current_reference =
                if SourceCodeApi.is_qualifier_tracked source_code_api current_reference then
                  Some current_reference
                else
                  Reference.prefix current_reference >>= find_module_name
              in
              find_module_name module_or_class_name, filter
          in
          let defines =
            let source_code_api =
              TypeEnvironment.ReadOnly.get_untracked_source_code_api type_environment
            in
            module_name
            >>= SourceCodeApi.source_of_qualifier source_code_api
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
              ~lookup:
                (PathLookup.absolute_source_path_of_qualifier_with_build_system
                   ~build_system
                   ~source_code_api)
          in
          List.map
            ~f:(fun { Callgraph.callee; locations } ->
              { Base.callee; locations = List.map locations ~f:instantiate })
            callees
          |> fun callees -> { Base.caller; callees } :: response
        in
        get_program_call_graph ()
        |> Map.fold ~f:create_response_with_caller ~init:[]
        |> fun result -> Single (Base.Callgraph result)
    | TypecheckedPaths ->
        Single
          (Base.TypecheckedPaths
             (GlobalModulePathsApi.module_paths global_module_paths_api
             |> List.filter ~f:ModulePath.should_type_check
             |> List.map ~f:(fun { ModulePath.qualifier; _ } ->
                    PathLookup.absolute_source_path_of_qualifier_with_build_system
                      ~build_system
                      ~source_code_api
                      qualifier)
             |> List.filter ~f:Option.is_some
             |> List.map ~f:(function
                    | Some x -> x
                    | None -> assert false)))
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
          PathLookup.absolute_source_path_of_qualifier_with_build_system
            ~build_system
            ~source_code_api
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
    | TypeAtLocation { path; location } ->
        qualifier_of_path path
        >>| (fun module_reference ->
              LocationBasedLookup.SingleSymbolQueries.type_at_location
                ~type_environment
                ~module_reference
                location)
        >>| (fun maybe_type -> Single (Base.TypeAtLocation maybe_type))
        |> Option.value
             ~default:(Error (Format.sprintf "No module found for path `%s`" (PyrePath.show path)))
    | ModelQuery { path; query_name } -> (
        let pyre_api =
          Interprocedural.PyrePysaApi.ReadOnly.from_pyre1_environment
            ~type_environment
            ~global_module_paths_api
        in
        if not (PyrePath.file_exists path) then
          Error (Format.sprintf "File path `%s` does not exist" (PyrePath.show path))
        else
          let taint_configuration_result =
            let step_logger =
              Taint.StepLogger.start
                ~command:"query"
                ~start_message:"Initializing and verifying taint configuration"
                ~end_message:"Initialized and verified taint configuration"
                ()
            in
            let taint_configuration_result =
              Taint.TaintConfiguration.from_taint_model_paths [path]
            in
            Taint.StepLogger.finish step_logger;
            taint_configuration_result
          in
          match taint_configuration_result with
          | Error (error :: _) -> Error (Taint.TaintConfiguration.Error.show error)
          | Error _ -> failwith "Taint.TaintConfiguration.create returned empty errors list"
          | Ok taint_configuration -> (
              let python_version =
                Taint.ModelParser.PythonVersion.from_configuration configuration
              in
              let parse_result =
                let filter_query { Taint.ModelParseResult.ModelQuery.name; _ } =
                  String.equal name query_name
                in
                parse_model_queries
                  ~pyre_api
                  ~taint_configuration
                  ~python_version
                  ~filter_query
                  ~model_paths:[path]
              in
              match parse_result with
              | { Taint.ModelParseResult.queries = []; errors; _ } ->
                  let pp_errors formatter = function
                    | [] -> ()
                    | errors ->
                        Format.fprintf
                          formatter
                          " This might be because of the following parse errors:\n%s"
                          (List.map ~f:Taint.ModelVerificationError.display errors
                          |> String.concat ~sep:"\n")
                  in
                  Error
                    (Format.asprintf
                       "No model query with name `%s` was found in path `%s`.%a"
                       query_name
                       (PyrePath.show path)
                       pp_errors
                       errors)
              | { Taint.ModelParseResult.queries = _ :: _ :: _ as queries; _ } ->
                  let show_query_location { Taint.ModelParseResult.ModelQuery.location; path; _ } =
                    Format.sprintf
                      "%s:%d:%d"
                      (path >>| PyrePath.absolute |> Option.value ~default:"?")
                      location.start.line
                      location.start.column
                  in
                  Error
                    (Format.sprintf
                       "Found multiple model queries with name `%s` in path `%s`:\n%s"
                       query_name
                       (PyrePath.show path)
                       (List.map ~f:show_query_location queries |> String.concat ~sep:"\n"))
              | { Taint.ModelParseResult.queries = [query]; _ } ->
                  let model_query_results = setup_and_execute_model_queries ~pyre_api [query] in
                  let models =
                    Taint.ModelQueryExecution.ExecutionResult.get_models model_query_results
                  in
                  let errors =
                    Taint.ModelQueryExecution.ExecutionResult.get_errors model_query_results
                  in
                  let to_taint_model (callable, model) =
                    {
                      Base.callable = Interprocedural.Target.external_name callable;
                      model =
                        Taint.Model.to_json
                          ~expand_overrides:None
                          ~is_valid_callee:(fun ~trace_kind:_ ~port:_ ~path:_ ~callee:_ -> true)
                          ~resolve_module_path:None
                          ~resolve_callable_location:None
                          ~export_leaf_names:Taint.Domains.ExportLeafNames.Always
                          callable
                          model;
                    }
                  in
                  let models = Taint.SharedModels.to_alist models |> List.map ~f:to_taint_model in
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
             |> PathLookup.qualifiers_of_source_path_with_build_system
                  ~build_system
                  ~source_code_api))
    | LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.less_or_equal global_resolution ~left ~right
        |> fun response -> Single (Base.Boolean response)
    | PathOfModule module_name ->
        SourceCodeApi.module_path_of_qualifier source_code_api module_name
        >>= (fun source_path ->
              let path =
                ArtifactPaths.artifact_path_of_module_path ~configuration source_path
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
          |> GlobalResolution.successors global_resolution
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
              let load_modules qualifiers =
                let _ =
                  List.map qualifiers ~f:(GlobalResolution.get_module_metadata global_resolution)
                in
                ()
              in
              Scheduler.iter
                scheduler
                ~policy:
                  (Scheduler.Policy.fixed_chunk_count
                     ~minimum_chunks_per_worker:1
                     ~minimum_chunk_size:1
                     ~preferred_chunks_per_worker:5
                     ())
                ~f:load_modules
                ~inputs:(GlobalModulePathsApi.explicit_qualifiers global_module_paths_api);
              UnannotatedGlobalEnvironment.ReadOnly.GlobalApis.all_classes
                ~scheduler
                ~global_module_paths_api
                (TypeEnvironment.ReadOnly.unannotated_global_environment type_environment)
              |> List.map ~f:Reference.create
          | _ ->
              List.filter class_names ~f:(fun class_name ->
                  Reference.show class_name |> GlobalResolution.class_exists global_resolution)
        in
        Single (Superclasses (List.map ~f:get_superclasses class_names))
    | Type expression ->
        let annotation = Resolution.resolve_expression_to_type resolution expression in
        Single (Type annotation)
    | IsTypechecked paths ->
        let get_is_typechecked path =
          match
            LocationBasedLookupProcessor.get_module_path ~build_system ~type_environment path
          with
          | Result.Ok module_path ->
              { Base.path; is_typechecked = ModulePath.should_type_check module_path }
          | Result.Error _ -> { Base.path; is_typechecked = false }
        in
        Single (Base.IsTypechecked (List.map paths ~f:get_is_typechecked))
    | TypesInFiles paths ->
        let find_resolved_types path =
          let module_path_to_resolved_types module_path =
            let qualifier = ModulePath.qualifier module_path in
            match Cache.find query_cache qualifier with
            | Some types -> types
            | None ->
                let types =
                  LocationBasedLookupProcessor.find_all_resolved_types_for_qualifier
                    ~type_environment
                    qualifier
                in
                Cache.save query_cache qualifier types;
                types
          in
          match
            LocationBasedLookupProcessor.get_module_path ~type_environment ~build_system path
            |> Result.bind ~f:module_path_to_resolved_types
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
        let pyre_api =
          Interprocedural.PyrePysaApi.ReadOnly.from_pyre1_environment
            ~type_environment
            ~global_module_paths_api
        in
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
          let step_logger =
            Taint.StepLogger.start
              ~command:"query"
              ~start_message:"Initializing and verifying taint configuration"
              ~end_message:"Initialized and verified taint configuration"
              ()
          in
          let taint_configuration =
            Taint.TaintConfiguration.from_taint_model_paths paths
            |> Taint.TaintConfiguration.exception_on_error
          in
          Taint.StepLogger.finish step_logger;
          taint_configuration
        in
        let { Taint.ModelParseResult.queries = model_queries; errors = model_parse_errors; _ } =
          let python_version = Taint.ModelParser.PythonVersion.from_configuration configuration in
          parse_model_queries
            ~pyre_api
            ~taint_configuration
            ~python_version
            ~filter_query:(fun _ -> true)
            ~model_paths:paths
        in
        let model_query_errors =
          if verify_dsl then
            setup_and_execute_model_queries ~pyre_api model_queries
            |> Taint.ModelQueryExecution.ExecutionResult.get_errors
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
  try process_request_exn () with
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


let process_request
    ~type_environment
    ~global_module_paths_api
    ~scheduler
    ~build_system
    ~query_cache
    request
  =
  match
    process_request_exn
      ~type_environment
      ~global_module_paths_api
      ~scheduler
      ~build_system
      ~query_cache
      request
  with
  | exception e ->
      Log.error "Fatal exception in no-daemon query: %s" (Exn.to_string e);
      Response.Error (Exn.to_string e)
  | result -> result


let parse_and_process_request
    ~overlaid_environment
    ~scheduler
    ~build_system
    ~query_cache
    request
    overlay_id
  =
  let global_module_paths_api =
    OverlaidEnvironment.AssumeGlobalModuleListing.global_module_paths_api overlaid_environment
  in
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
  | Result.Ok request ->
      process_request
        ~type_environment
        ~global_module_paths_api
        ~scheduler
        ~build_system
        ~query_cache
        request
