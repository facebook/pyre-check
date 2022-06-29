(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
    | Help of string
    | HoverInfoForPosition of {
        path: PyrePath.t;
        position: Location.position;
      }
    | InlineDecorators of {
        function_reference: Reference.t;
        decorators_to_skip: Reference.t list;
      }
    | IsCompatibleWith of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | LocationOfDefinition of {
        path: PyrePath.t;
        position: Location.position;
      }
    | ModelQuery of {
        path: PyrePath.t;
        query_name: string;
      }
    | ModulesOfPath of PyrePath.t
    | PathOfModule of Reference.t
    | FindReferences of {
        path: PyrePath.t;
        position: Location.position;
      }
    | SaveServerState of PyrePath.t
    | Superclasses of Reference.t list
    | Type of Expression.t
    | TypesInFiles of string list
    | ValidateTaintModels of string option
  [@@deriving sexp, compare]

  let inline_decorators ?(decorators_to_skip = []) function_reference =
    InlineDecorators { function_reference; decorators_to_skip }
end

module Response = struct
  module Base = struct
    type attribute_kind =
      | Regular
      | Property
    [@@deriving sexp, compare, to_yojson]

    type attribute = {
      name: string;
      annotation: Type.t;
      kind: attribute_kind;
      final: bool;
    }
    [@@deriving sexp, compare, to_yojson]

    type type_at_location = {
      location: Location.t;
      annotation: Type.t;
    }
    [@@deriving sexp, compare, to_yojson]

    type types_at_path = {
      path: string;
      types: type_at_location list;
    }
    [@@deriving sexp, compare, to_yojson]

    type coverage_at_path = {
      path: string;
      total_expressions: int;
      coverage_gaps: LocationBasedLookup.coverage_gap_by_location list;
    }
    [@@deriving sexp, compare, to_yojson]

    type error_at_path = {
      path: string;
      error: string;
    }
    [@@deriving sexp, compare, to_yojson, show]

    type coverage_response_at_path =
      | CoverageAtPath of coverage_at_path
      | ErrorAtPath of error_at_path
    [@@deriving sexp, compare, to_yojson]

    type compatibility = {
      actual: Type.t;
      expected: Type.t;
      result: bool;
    }
    [@@deriving sexp, compare]

    type callee_with_instantiated_locations = {
      callee: Analysis.Callgraph.callee;
      locations: Location.WithPath.t list;
    }
    [@@deriving sexp, compare]

    type callees = {
      caller: Reference.t;
      callees: callee_with_instantiated_locations list;
    }
    [@@deriving sexp, compare]

    type parameter_representation = {
      parameter_name: string;
      parameter_annotation: Expression.t option;
    }
    [@@deriving sexp, compare]

    type define = {
      define_name: Reference.t;
      parameters: parameter_representation list;
      return_annotation: Expression.t option;
    }
    [@@deriving sexp, compare]

    type superclasses_mapping = {
      class_name: Reference.t;
      superclasses: Reference.t list;
    }
    [@@deriving sexp, compare, to_yojson]

    type position = {
      line: int;
      character: int;
    }
    [@@deriving sexp, compare, to_yojson]

    type range = {
      start: position;
      end_: position;
    }
    [@@deriving sexp, compare]

    let range_to_yojson { start; end_ } =
      `Assoc ["start", position_to_yojson start; "end", position_to_yojson end_]


    type code_location = {
      path: string;
      range: range;
    }
    [@@deriving sexp, compare, to_yojson]

    type t =
      | Boolean of bool
      | Callees of Analysis.Callgraph.callee list
      | CalleesWithLocation of callee_with_instantiated_locations list
      | Callgraph of callees list
      | Compatibility of compatibility
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | ExpressionLevelCoverageResponse of coverage_response_at_path list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundLocationsOfDefinitions of code_location list
      | FoundModels of string
      | FoundModules of Reference.t list
      | FoundPath of string
      | FoundReferences of code_location list
      | FunctionDefinition of Statement.Define.t
      | Help of string
      | HoverInfoForPosition of string
      | ModelVerificationErrors of Taint.ModelVerificationError.t list
      | Success of string
      | Superclasses of superclasses_mapping list
      | Type of Type.t
      | TypesByPath of types_at_path list
    [@@deriving sexp, compare]

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
      | Compatibility { actual; expected; result } ->
          `Assoc
            [
              "actual", Type.to_yojson actual;
              "expected", Type.to_yojson expected;
              "boolean", `Bool result;
            ]
      | Errors errors ->
          `Assoc
            [
              ( "errors",
                `List (List.map ~f:(fun error -> AnalysisError.Instantiated.to_yojson error) errors)
              );
            ]
      | ExpressionLevelCoverageResponse paths ->
          `List (List.map paths ~f:coverage_response_at_path_to_yojson)
      | Help string -> `Assoc ["help", `String string]
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
      | FoundLocationsOfDefinitions locations ->
          `List (List.map locations ~f:code_location_to_yojson)
      | FoundModels models -> `String models
      | FoundModules references ->
          let reference_to_yojson reference = `String (Reference.show reference) in
          `List (List.map references ~f:reference_to_yojson)
      | FoundPath path -> `Assoc ["path", `String path]
      | FoundReferences locations -> `List (List.map locations ~f:code_location_to_yojson)
      | FunctionDefinition define ->
          `Assoc
            [
              ( "definition",
                `String
                  (Statement.show
                     (Statement.Statement.Define define |> Node.create_with_default_location)) );
            ]
      | HoverInfoForPosition message -> `Assoc ["message", `String message]
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
  [@@deriving sexp, compare]

  let rec to_yojson = function
    | Single base_response -> `Assoc ["response", Base.to_yojson base_response]
    | Batch responses -> `Assoc ["response", `List (List.map ~f:to_yojson responses)]
    | Error message -> `Assoc ["error", `String message]


  let create_type_at_location (location, annotation) = { Base.location; annotation }
end

let help () =
  let open Request in
  let open Expression in
  let help = function
    | Batch _ ->
        Some
          "batch(query1(arg), query2(arg)): Runs a batch of queries and returns a map of \
           responses. List of given queries may include any combination of other valid queries \
           except for `batch` itself."
    | Attributes _ ->
        Some
          "attributes(class_name): Returns a list of attributes, including functions, for a class."
    | Callees _ -> Some "callees(function): calls from a given function."
    | CalleesWithLocation _ ->
        Some
          "callees_with_location(function): calls from a given function, including the locations \
           at which they are called."
    | Defines _ ->
        Some
          "defines(module_or_class_name): Returns a JSON with the signature of all defines for \
           given module or class."
    | DumpCallGraph ->
        Some "dump_call_graph(): Returns a comprehensive JSON of caller -> list of callees."
    | ExpressionLevelCoverage _ ->
        Some
          "expression_level_coverage(path='path') or expression_level_coverage('path1', 'path2', \
           ...): Return JSON output containing the number of covered and uncovered expressions \
           from above, along with a list of known coverage gaps."
    | HoverInfoForPosition _ ->
        Some
          "hover_info_for_position(path='<absolute path>', line=<line>, character=<character>): \
           Return JSON output containing the type of the symbol at the given position."
    | InlineDecorators _ ->
        Some
          "inline_decorators(qualified_function_name, optional decorators_to_skip=[decorator1, \
           decorator2]): Shows the function definition after decorators have been inlined."
    | IsCompatibleWith _ -> None
    | LessOrEqual _ -> Some "less_or_equal(T1, T2): Returns whether T1 is a subtype of T2."
    | LocationOfDefinition _ ->
        Some
          "location_of_definition(path='<absolute path>', line=<line>, character=<character>): \
           Returns the location of the definition for the symbol at the given line and character."
    | ModelQuery _ ->
        Some
          "model_query(path='<absolute path>', query_name=<model_query_name>): Returns in JSON a \
           list of all models generated from the query with the name `query_name` in the directory \
           `path`."
    | ModulesOfPath _ ->
        Some "modules_of_path(path): Returns the modules of a file pointed to by path."
    | PathOfModule _ -> Some "path_of_module(module): Gives an absolute path for `module`."
    | FindReferences _ ->
        Some
          "find_references(path='<absolute path>', line=<line>, character=<character>): Returns \
           the locations of all references to the symbol at the given line and character."
    | SaveServerState _ ->
        Some "save_server_state('path'): Saves Pyre's serialized state into `path`."
    | Superclasses _ ->
        Some
          "superclasses(class_name1, class_name2, ...): Returns a mapping of class_name to the \
           list of superclasses for `class_name`."
    | Type _ -> Some "type(expression): Evaluates the type of `expression`."
    | TypesInFiles _ ->
        Some
          "types(path='path') or types('path1', 'path2', ...): Returns a map from each given path \
           to a list of all types for that path."
    | ValidateTaintModels _ ->
        Some
          "validate_taint_models('optional path'): Validates models and returns errors. Defaults \
           to model path in configuration if no parameter is passed in."
    | Help _ -> None
  in
  let path = PyrePath.current_working_directory () in
  let empty = Expression.Name (Name.Identifier "") |> Node.create_with_default_location in
  List.filter_map
    ~f:help
    [
      Batch [];
      Attributes (Reference.create "");
      Callees (Reference.create "");
      CalleesWithLocation (Reference.create "");
      Defines [Reference.create ""];
      DumpCallGraph;
      ExpressionLevelCoverage [""];
      HoverInfoForPosition { path; position = Location.any_position };
      IsCompatibleWith (empty, empty);
      LessOrEqual (empty, empty);
      ModelQuery { path; query_name = "" };
      ModulesOfPath path;
      PathOfModule (Reference.create "");
      SaveServerState path;
      Superclasses [Reference.empty];
      Type (Node.create_with_default_location (Expression.Constant Constant.True));
      TypesInFiles [""];
      ValidateTaintModels None;
      Request.inline_decorators (Reference.create "");
    ]
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:"\n  "
  |> Format.sprintf "Possible queries:\n  %s"


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
      let parse_inline_decorators arguments =
        match arguments with
        | [name] -> Request.inline_decorators (reference name)
        | [
         name;
         {
           Call.Argument.name = Some { Node.value = "decorators_to_skip"; _ };
           value = { Node.value = Expression.List decorators; _ };
         };
        ] -> (
            let decorator_to_reference = function
              | { Node.value = Expression.Name name; _ } as decorator ->
                  name_to_reference name |> Result.of_option ~error:decorator
              | decorator -> Result.Error decorator
            in
            let valid_decorators, invalid_decorators =
              List.map decorators ~f:decorator_to_reference |> List.partition_result
            in
            match valid_decorators, invalid_decorators with
            | decorators_to_skip, [] ->
                InlineDecorators { function_reference = reference name; decorators_to_skip }
            | _, invalid_decorators ->
                InvalidQuery
                  (Format.asprintf
                     "inline_decorators: invalid decorators `(%s)`"
                     (List.map invalid_decorators ~f:Expression.show |> String.concat ~sep:", "))
                |> raise)
        | _ ->
            raise
              (InvalidQuery
                 "inline_decorators expects qualified name and optional `decorators_to_skip=[...]`")
      in
      let string argument = argument |> expression |> string_of_expression in
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
      | "callees_with_location", [name] -> Request.CalleesWithLocation (reference name)
      | "defines", names -> Request.Defines (List.map names ~f:reference)
      | "dump_call_graph", [] -> Request.DumpCallGraph
      | "dump_class_hierarchy", [] -> Request.Superclasses []
      | "expression_level_coverage", paths ->
          Request.ExpressionLevelCoverage (List.map ~f:string paths)
      | "help", _ -> Request.Help (help ())
      | "hover_info_for_position", [path; line; column] ->
          Request.HoverInfoForPosition
            {
              path = PyrePath.create_absolute (string path);
              position = { Location.line = integer line; column = integer column };
            }
      | "inline_decorators", arguments -> parse_inline_decorators arguments
      | "is_compatible_with", [left; right] -> Request.IsCompatibleWith (access left, access right)
      | "less_or_equal", [left; right] -> Request.LessOrEqual (access left, access right)
      | "location_of_definition", [path; line; column] ->
          Request.LocationOfDefinition
            {
              path = PyrePath.create_absolute (string path);
              position = { line = integer line; column = integer column };
            }
      | "model_query", [path; model_query_name] ->
          Request.ModelQuery
            { path = PyrePath.create_absolute (string path); query_name = string model_query_name }
      | "modules_of_path", [path] -> Request.ModulesOfPath (PyrePath.create_absolute (string path))
      | "path_of_module", [module_access] -> Request.PathOfModule (reference module_access)
      | "find_references", [path; line; column] ->
          Request.FindReferences
            {
              path = PyrePath.create_absolute (string path);
              position = { line = integer line; column = integer column };
            }
      | "save_server_state", [path] ->
          Request.SaveServerState (PyrePath.create_absolute (string path))
      | "superclasses", names -> Superclasses (List.map ~f:reference names)
      | "type", [argument] -> Type (expression argument)
      | "types", paths -> Request.TypesInFiles (List.map ~f:string paths)
      | "validate_taint_models", [] -> ValidateTaintModels None
      | "validate_taint_models", [argument] -> Request.ValidateTaintModels (Some (string argument))
      | _ -> raise (InvalidQuery "unexpected query"))
  | Ok _ when String.equal query "help" -> Help (help ())
  | Ok _ -> raise (InvalidQuery "unexpected query")
  | Error _ -> raise (InvalidQuery "failed to parse query")


let parse_request query =
  try Result.Ok (parse_request_exn query) with
  | InvalidQuery reason -> Result.Error reason


module InlineDecorators = struct
  let inline_decorators ~environment ~decorators_to_skip function_reference =
    let define =
      GlobalResolution.define
        (TypeEnvironment.ReadOnly.global_resolution environment)
        function_reference
    in
    match define with
    | Some define -> (
        let get_source =
          AstEnvironment.ReadOnly.get_processed_source
            (TypeEnvironment.ReadOnly.ast_environment environment)
        in
        let define_with_inlining =
          InlineDecorator.inline_decorators_for_define
            ~get_decorator_body:
              (InlineDecorator.decorator_body
                 ~should_skip_decorator:(Set.mem decorators_to_skip)
                 ~get_source)
            ~location:Location.any
            define
        in
        match Statement.Statement.Define define_with_inlining |> Transform.sanitize_statement with
        | Statement.Statement.Define define -> Response.Single (FunctionDefinition define)
        | _ -> failwith "Expected define")
    | None ->
        Response.Error
          (Format.asprintf "Could not find function `%s`" (Reference.show function_reference))
end

let rec process_request ~environment ~build_system request =
  let process_request () =
    let configuration =
      TypeEnvironment.ReadOnly.controls environment |> EnvironmentControls.configuration
    in
    let module_tracker = TypeEnvironment.ReadOnly.module_tracker environment in
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
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
    let modules_of_path path =
      let module_of_path path =
        match ModuleTracker.ReadOnly.lookup_path module_tracker path with
        | ModuleTracker.PathLookup.Found { ModulePath.qualifier; _ } -> Some qualifier
        | ShadowedBy _
        | NotFound ->
            None
      in
      let artifact_path = BuildSystem.lookup_artifact build_system path in
      List.filter_map ~f:module_of_path artifact_path
    in
    let instantiate_range
        Location.WithModule.
          {
            start = { line = start_line; column = start_column };
            stop = { line = stop_line; column = stop_column };
            module_reference;
          }
      =
      PathLookup.instantiate_path
        ~build_system
        ~ast_environment:(TypeEnvironment.ReadOnly.ast_environment environment)
        module_reference
      >>| fun path ->
      {
        Response.Base.path;
        range =
          {
            start = { line = start_line; character = start_column };
            end_ = { line = stop_line; character = stop_column };
          };
      }
    in
    let module_of_path path =
      let relative_path =
        let { Configuration.Analysis.local_root = root; _ } = configuration in
        PyrePath.create_relative ~root ~relative:(PyrePath.absolute path) |> SourcePath.create
      in
      match modules_of_path relative_path with
      | [found_module] -> Some found_module
      | _ -> None
    in
    let open Response in
    match request with
    | Request.Attributes annotation ->
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
              Base.Property
            else
              Base.Regular
          in
          let final = Annotated.Attribute.is_final instantiated_annotation in
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
    | Batch requests -> Batch (List.map ~f:(process_request ~environment ~build_system) requests)
    | Callees caller ->
        (* We don't yet support a syntax for fetching property setters. *)
        Single
          (Base.Callees
             (Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
             |> List.map ~f:(fun { Callgraph.callee; _ } -> callee)))
    | CalleesWithLocation caller ->
        let instantiate =
          Location.WithModule.instantiate
            ~lookup:
              (AstEnvironment.ReadOnly.get_real_path_relative
                 (TypeEnvironment.ReadOnly.ast_environment environment))
        in
        let callees =
          (* We don't yet support a syntax for fetching property setters. *)
          Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
          |> List.map ~f:(fun { Callgraph.callee; locations } ->
                 { Base.callee; locations = List.map locations ~f:instantiate })
        in
        Single (Base.CalleesWithLocation callees)
    | Defines module_or_class_names ->
        let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
        let defines_of_module module_or_class_name =
          let module_name, filter_define =
            if AstEnvironment.ReadOnly.is_module_tracked ast_environment module_or_class_name then
              Some module_or_class_name, fun _ -> false
            else
              let filter
                  { Statement.Define.signature = { Statement.Define.Signature.parent; _ }; _ }
                =
                not (Option.equal Reference.equal parent (Some module_or_class_name))
              in
              let rec find_module_name current_reference =
                if AstEnvironment.ReadOnly.is_module_tracked ast_environment current_reference then
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
              Location.WithModule.instantiate
                ~lookup:
                  (AstEnvironment.ReadOnly.get_real_path_relative
                     (TypeEnvironment.ReadOnly.ast_environment environment))
            in
            Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
            |> List.map ~f:(fun { Callgraph.callee; locations } ->
                   { Base.callee; locations = List.map locations ~f:instantiate })
            |> fun callees -> { Base.caller; callees }
          in
          let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
          AstEnvironment.ReadOnly.get_processed_source ast_environment module_qualifier
          >>| Preprocessing.defines ~include_toplevels:false ~include_stubs:false
          >>| List.map ~f:callees
          |> Option.value ~default:[]
        in
        let qualifiers = ModuleTracker.ReadOnly.tracked_explicit_modules module_tracker in
        Single (Base.Callgraph (List.concat_map qualifiers ~f:get_callgraph))
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
                    ~environment
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
    | Help help_list -> Single (Base.Help help_list)
    | HoverInfoForPosition { path; position } ->
        module_of_path path
        >>| (fun module_reference ->
              LocationBasedLookup.hover_info_for_position
                ~type_environment:environment
                ~module_reference
                position)
        >>| (fun hover_info -> Single (Base.HoverInfoForPosition hover_info))
        |> Option.value
             ~default:(Error (Format.sprintf "No module found for path `%s`" (PyrePath.show path)))
    | InlineDecorators { function_reference; decorators_to_skip } ->
        InlineDecorators.inline_decorators
          ~environment
          ~decorators_to_skip:(Reference.Set.of_list decorators_to_skip)
          function_reference
    | IsCompatibleWith (left, right) ->
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
        |> fun result -> Single (Base.Compatibility { actual = left; expected = right; result })
    | ModelQuery { path; query_name } -> (
        if not (PyrePath.file_exists path) then
          Error (Format.sprintf "File path `%s` does not exist" (PyrePath.show path))
        else
          let taint_configuration_result =
            Taint.TaintConfiguration.create
              ~rule_filter:None
              ~find_missing_flows:None
              ~dump_model_query_results_path:None
              ~maximum_trace_length:None
              ~maximum_tito_depth:None
              ~taint_model_paths:[path]
          in
          match taint_configuration_result with
          | Error (error :: _) -> Error (Taint.TaintConfiguration.Error.show error)
          | Error _ -> failwith "Taint.TaintConfiguration.create returned empty errors list"
          | Ok taint_configuration ->
              let static_analysis_configuration =
                Configuration.StaticAnalysis.create
                  (Configuration.Analysis.create ~source_paths:[SearchPath.Root path] ())
                  ()
              in
              let get_model_queries (path, source) =
                Taint.ModelParser.parse
                  ~resolution:
                    (TypeCheck.resolution
                       global_resolution
                       (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
                       (module TypeCheck.DummyContext))
                  ~path
                  ~source
                  ~configuration:taint_configuration
                  ~callables:None
                  ~stubs:(Interprocedural.Target.HashSet.create ())
                  ()
                |> fun { Taint.ModelParser.queries; _ } ->
                queries
                |> List.filter ~f:(fun rule ->
                       String.equal rule.Taint.ModelParser.Internal.ModelQuery.name query_name)
              in
              let rules =
                let sources = Taint.ModelParser.get_model_sources ~paths:[path] in
                if List.is_empty sources then
                  failwith
                    (Format.sprintf
                       "ModelParser.get_model_sources ~paths:[%s] was empty"
                       (PyrePath.show path))
                else
                  List.concat (List.map sources ~f:get_model_queries)
              in
              if List.is_empty rules then
                Error
                  (Format.sprintf
                     "No model query with name `%s` was found in path `%s`"
                     query_name
                     (PyrePath.show path))
              else
                let get_models_for_query scheduler =
                  let cache = Taint.Cache.load ~scheduler ~configuration ~enabled:false in
                  let initial_callables =
                    Taint.Cache.initial_callables cache (fun () ->
                        let timer = Timer.start () in
                        let qualifiers =
                          ModuleTracker.ReadOnly.tracked_explicit_modules module_tracker
                        in
                        let initial_callables =
                          Interprocedural.FetchCallables.from_qualifiers
                            ~scheduler
                            ~configuration
                            ~environment
                            ~include_unit_tests:false
                            ~qualifiers
                        in
                        Statistics.performance
                          ~name:"Fetched initial callables to analyze"
                          ~phase_name:"Fetching initial callables to analyze"
                          ~timer
                          ();
                        initial_callables)
                  in
                  TaintModelQuery.ModelQuery.generate_models_from_queries
                    ~static_analysis_configuration
                    ~scheduler
                    ~environment
                    ~callables:(Interprocedural.FetchCallables.get_callables initial_callables)
                    ~stubs:
                      (Interprocedural.Target.HashSet.of_list
                         (Interprocedural.FetchCallables.get_stubs initial_callables))
                    ~taint_configuration
                    rules
                in
                let models_and_names, _ =
                  Scheduler.with_scheduler ~configuration ~f:get_models_for_query
                in
                let to_json (callable, model) =
                  `Assoc
                    [
                      "callable", `String (Interprocedural.Target.external_name callable);
                      ( "model",
                        Taint.Model.to_json
                          ~expand_overrides:None
                          ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
                          ~filename_lookup:None
                          callable
                          model );
                    ]
                in
                let models =
                  models_and_names
                  |> TaintModelQuery.ModelQuery.ModelQueryRegistryMap.get_registry
                       ~model_join:Taint.Model.join_user_models
                  |> Taint.Registry.to_alist
                in
                if List.length models == 0 then
                  Error
                    (Format.sprintf
                       "No models found for model query `%s` in path `%s`"
                       query_name
                       (PyrePath.show path))
                else
                  let models_string = `List (List.map models ~f:to_json) |> Yojson.Safe.to_string in
                  Single (Base.FoundModels models_string))
    | ModulesOfPath path -> Single (Base.FoundModules (SourcePath.create path |> modules_of_path))
    | LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.less_or_equal global_resolution ~left ~right
        |> fun response -> Single (Base.Boolean response)
    | LocationOfDefinition { path; position } ->
        module_of_path path
        >>= (fun module_reference ->
              LocationBasedLookup.location_of_definition
                ~type_environment:environment
                ~module_reference
                position)
        >>= instantiate_range
        |> Option.to_list
        |> fun definitions -> Single (Base.FoundLocationsOfDefinitions definitions)
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
    | FindReferences { path; position } -> (
        let find_references_local ~reference ~define_name =
          let is_match identifier =
            let requested_name =
              Reference.delocalize reference |> Reference.last |> Identifier.sanitized
            in
            let name = Identifier.sanitized identifier in
            String.equal requested_name name
          in
          match GlobalResolution.function_definition global_resolution define_name with
          | Some { FunctionDefinition.body = Some define; qualifier; _ } ->
              let location_to_result location =
                Location.with_module ~module_reference:qualifier location |> instantiate_range
              in
              let all_local_bindings =
                Scope.Scope.of_define_exn define.value
                |> UninitializedLocalCheck.local_bindings
                |> Identifier.Map.filter_keys ~f:is_match
                |> Identifier.Map.data
                |> List.map ~f:(fun { Scope.Binding.location; _ } -> location)
                |> Location.Set.of_list
              in
              let all_access_reads =
                let { Statement.Define.body; _ } = Node.value define in
                List.map ~f:UninitializedLocalCheck.extract_reads_in_statement body
                |> List.concat
                |> List.filter ~f:(fun { Node.value; _ } -> is_match value)
                |> List.map ~f:Node.location
                |> Location.Set.of_list
              in
              let all_local_references =
                Set.union all_local_bindings all_access_reads
                |> Location.Set.to_list
                |> List.filter_map ~f:location_to_result
              in
              Single (Base.FoundReferences all_local_references)
          | _ -> Single (Base.FoundReferences [])
        in
        let find_references_global ~reference =
          (* TODO(T114362295): Support find all references. *)
          let _ = reference in
          Single (Base.FoundReferences [])
        in
        let symbol =
          let module_of_path path =
            let relative_path =
              let { Configuration.Analysis.local_root = root; _ } = configuration in
              PyrePath.create_relative ~root ~relative:(PyrePath.absolute path) |> SourcePath.create
            in
            match modules_of_path relative_path with
            | [found_module] -> Some found_module
            | _ -> None
          in
          module_of_path path
          >>= fun module_reference ->
          LocationBasedLookup.find_narrowest_spanning_symbol
            ~type_environment:environment
            ~module_reference
            position
        in
        match symbol with
        | Some
            {
              symbol_with_definition = Expression { Node.value = Name name; _ };
              cfg_data = { define_name; _ };
              _;
            }
        | Some
            {
              symbol_with_definition = TypeAnnotation { Node.value = Name name; _ };
              cfg_data = { define_name; _ };
              _;
            }
          when Expression.is_simple_name name ->
            let reference = Expression.name_to_reference_exn name in
            if Reference.is_local reference || Reference.is_parameter reference then
              find_references_local ~reference ~define_name
            else
              find_references_global ~reference
        | _ ->
            (* Find-all-references is not supported for syntax, keywords, or literal values. *)
            Single (Base.FoundReferences []))
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
              Scheduler.with_scheduler ~configuration ~f:load_all_modules;
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
              ~environment
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
    | ValidateTaintModels path -> (
        try
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
          let configuration =
            Taint.TaintConfiguration.create
              ~rule_filter:None
              ~find_missing_flows:None
              ~dump_model_query_results_path:None
              ~maximum_trace_length:None
              ~maximum_tito_depth:None
              ~taint_model_paths:paths
            |> Taint.TaintConfiguration.exception_on_error
          in
          let get_model_errors sources =
            let model_errors (path, source) =
              Taint.ModelParser.parse
                ~resolution:
                  (TypeCheck.resolution
                     global_resolution
                     (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
                     (module TypeCheck.DummyContext))
                ~path
                ~source
                ~configuration
                ~callables:None
                ~stubs:(Interprocedural.Target.HashSet.create ())
                ()
              |> fun { Taint.ModelParser.errors; _ } -> errors
            in
            List.concat_map sources ~f:model_errors
          in
          let errors = Taint.ModelParser.get_model_sources ~paths |> get_model_errors in
          if List.is_empty errors then
            Single
              (Base.Success
                 (Format.asprintf
                    "Models in `%s` are valid."
                    (paths |> List.map ~f:PyrePath.show |> String.concat ~sep:", ")))
          else
            Single (Base.ModelVerificationErrors errors)
        with
        | error -> Error (Exn.to_string error))
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


let parse_and_process_request ~environment ~build_system request =
  match parse_request request with
  | Result.Error reason -> Response.Error reason
  | Result.Ok request -> process_request ~environment ~build_system request
