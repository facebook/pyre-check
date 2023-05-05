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
    | ReferencesUsedByFile of string
    | SaveServerState of PyrePath.t
    | Superclasses of Reference.t list
    | Type of Expression.t
    | TypesInFiles of string list
    | ValidateTaintModels of {
        path: string option;
        verify_dsl: bool;
      }
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

    type hover_info = {
      value: string option;
      docstring: string option;
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

    type global_leak_errors = {
      global_leaks: Analysis.AnalysisError.Instantiated.t list;
      query_errors: string list;
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
      | GlobalLeakErrors of global_leak_errors
      | Help of string
      | HoverInfoForPosition of hover_info
      | ModelVerificationErrors of Taint.ModelVerificationError.t list
      | ReferenceTypesInPath of types_at_path
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
      | HoverInfoForPosition hover_info -> hover_info_to_yojson hover_info
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
    | GlobalLeaks _ ->
        Some
          "global_leaks(function1, ...): analyzes the given function(s) and emits errors when \
           global variables are mutated."
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
    | ReferencesUsedByFile _ ->
        Some
          "references_used_by_file(path='<absolute path>'): Similar to the `types` query, this \
           query will return all the types of every symbol (for a given path). Unlike the `types` \
           query response, types that are defined outside this project will be treated as valid \
           types & also be included in the query response."
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
          "validate_taint_models('optional path', verify_dsl=<bool>): Validates models and returns \
           errors. Defaults to model path in configuration if no parameter is passed in, and \
           verify_dsl=False. Pass in verify_dsl=True to validate ModelQueries as well."
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
      ValidateTaintModels { path = None; verify_dsl = false };
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
      | "references_used_by_file", [path] -> Request.ReferencesUsedByFile (string path)
      | "save_server_state", [path] ->
          Request.SaveServerState (PyrePath.create_absolute (string path))
      | "superclasses", names -> Superclasses (List.map ~f:reference names)
      | "type", [argument] -> Type (expression argument)
      | "types", paths -> Request.TypesInFiles (List.map ~f:string paths)
      | "validate_taint_models", arguments -> parse_validate_taint_models arguments
      | _ -> raise (InvalidQuery "unexpected query"))
  | Ok _ when String.equal query "help" -> Help (help ())
  | Ok _ -> raise (InvalidQuery "unexpected query")
  | Error _ -> raise (InvalidQuery "failed to parse query")


let parse_request query =
  try Result.Ok (parse_request_exn query) with
  | InvalidQuery reason -> Result.Error reason


module InlineDecorators = struct
  let inline_decorators ~type_environment ~decorators_to_skip function_reference =
    let define =
      GlobalResolution.define
        (TypeEnvironment.ReadOnly.global_resolution type_environment)
        function_reference
    in
    match define with
    | Some define -> (
        let get_source =
          AstEnvironment.ReadOnly.get_processed_source
            (TypeEnvironment.ReadOnly.ast_environment type_environment)
        in
        let define_with_inlining =
          DecoratorPreprocessing.inline_decorators_for_define
            ~get_source
            ~get_decorator_action:(fun reference ->
              if Set.mem decorators_to_skip reference then
                Some DecoratorPreprocessing.Action.DoNotInline
              else
                None)
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
    let instantiate_range
        Location.WithModule.
          {
            start = { line = start_line; column = start_column };
            stop = { line = stop_line; column = stop_column };
            module_reference;
          }
      =
      PathLookup.instantiate_path_with_build_system ~build_system ~module_tracker module_reference
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
    let setup_and_execute_model_queries model_queries =
      let scheduler_wrapper scheduler =
        let cache =
          Taint.Cache.try_load
            ~scheduler
            ~configuration
            ~decorator_configuration:
              Analysis.DecoratorPreprocessing.Configuration.disable_preprocessing
            ~enabled:false
        in
        let initial_callables =
          Taint.Cache.initial_callables cache (fun () ->
              let timer = Timer.start () in
              let qualifiers = ModuleTracker.ReadOnly.tracked_explicit_modules module_tracker in
              let initial_callables =
                Interprocedural.FetchCallables.from_qualifiers
                  ~scheduler
                  ~configuration
                  ~environment:type_environment
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
        let qualifiers =
          Analysis.TypeEnvironment.ReadOnly.module_tracker type_environment
          |> Analysis.ModuleTracker.ReadOnly.tracked_explicit_modules
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
          ~callables_and_stubs:
            (Interprocedural.FetchCallables.get_callables_and_stubs initial_callables)
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
    let module_of_path path =
      let relative_path =
        let { Configuration.Analysis.local_root = root; _ } = configuration in
        PyrePath.create_relative ~root ~relative:(PyrePath.absolute path) |> SourcePath.create
      in
      match
        PathLookup.modules_of_source_path_with_build_system
          ~build_system
          ~module_tracker
          relative_path
      with
      | [found_module] -> Some found_module
      | _ -> None
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
          let name = Annotated.Attribute.name attribute in
          let instantiated_annotation =
            GlobalResolution.instantiate_attribute
              ~resolution:global_resolution
              ~accessed_through_class:false
              ~accessed_through_readonly:false
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
    | Help help_list -> Single (Base.Help help_list)
    | HoverInfoForPosition { path; position } ->
        module_of_path path
        >>| (fun module_reference ->
              LocationBasedLookup.hover_info_for_position
                ~type_environment
                ~module_reference
                position)
        >>| (fun { value; docstring } -> Single (Base.HoverInfoForPosition { value; docstring }))
        |> Option.value
             ~default:(Error (Format.sprintf "No module found for path `%s`" (PyrePath.show path)))
    | InlineDecorators { function_reference; decorators_to_skip } ->
        InlineDecorators.inline_decorators
          ~type_environment
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
                  ~callables:None
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
                    let to_json (callable, model) =
                      `Assoc
                        [
                          "callable", `String (Interprocedural.Target.external_name callable);
                          ( "model",
                            Taint.Model.to_json
                              ~expand_overrides:None
                              ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
                              ~filename_lookup:None
                              ~export_leaf_names:Taint.Domains.ExportLeafNames.Always
                              callable
                              model );
                        ]
                    in
                    let models =
                      models_and_names
                      |> Taint.ModelQueryExecution.ModelQueryRegistryMap.get_registry
                           ~model_join:Taint.Model.join_user_models
                      |> Taint.Registry.to_alist
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
                      let models_string =
                        `List (List.map models ~f:to_json) |> Yojson.Safe.to_string
                      in
                      Single (Base.FoundModels models_string)))
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
    | LocationOfDefinition { path; position } -> (
        let module_reference = module_of_path path in
        match module_reference with
        | Some module_reference -> (
            (* Performing check on whether source contains parse error for telemetry purposes -
               understanding whether the file was parsed correct to gauge the usefulness of an error
               recoverable parser.

               This can be removed when we either no longer need telemetry data or if we can
               separate symbol resolution from location finding logic. *)
            let raw_source =
              AstEnvironment.ReadOnly.get_raw_source
                (TypeEnvironment.ReadOnly.ast_environment type_environment)
                module_reference
            in
            match raw_source with
            | Some (Result.Error error) ->
                Error
                  (Format.sprintf
                     "Parse error in location request. Location: %s, message: %s"
                     (Location.show error.location)
                     error.message)
            | _ ->
                LocationBasedLookup.location_of_definition
                  ~type_environment
                  ~module_reference
                  position
                >>= instantiate_range
                |> Option.to_list
                |> fun definitions -> Single (Base.FoundLocationsOfDefinitions definitions))
        | None -> Single (Base.FoundLocationsOfDefinitions []))
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
          module_of_path path
          >>= fun module_reference ->
          LocationBasedLookup.find_narrowest_spanning_symbol
            ~type_environment
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
              ~callables:None
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
