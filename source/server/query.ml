(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
    | Help of string
    | IsCompatibleWith of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | PathOfModule of Reference.t
    | SaveServerState of PyrePath.t
    | Superclasses of Reference.t list
    | Type of Expression.t
    | TypesInFiles of string list
    | ValidateTaintModels of string option
    | InlineDecorators of {
        function_reference: Reference.t;
        decorators_to_skip: Reference.t list;
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

    type t =
      | Boolean of bool
      | Callees of Analysis.Callgraph.callee list
      | CalleesWithLocation of callee_with_instantiated_locations list
      | Callgraph of callees list
      | Compatibility of compatibility
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundPath of string
      | FunctionDefinition of Statement.Define.t
      | Help of string
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
      | FoundPath path -> `Assoc ["path", `String path]
      | FunctionDefinition define ->
          `Assoc
            [
              ( "definition",
                `String
                  (Statement.show
                     (Statement.Statement.Define define |> Node.create_with_default_location)) );
            ]
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
    | IsCompatibleWith _ -> None
    | LessOrEqual _ -> Some "less_or_equal(T1, T2): Returns whether T1 is a subtype of T2."
    | PathOfModule _ -> Some "path_of_module(module): Gives an absolute path for `module`."
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
    | InlineDecorators _ ->
        Some
          "inline_decorators(qualified_function_name, optional decorators_to_skip=[decorator1, \
           decorator2]): Shows the function definition after decorators have been inlined."
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
      IsCompatibleWith (empty, empty);
      LessOrEqual (empty, empty);
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
      | "help", _ -> Request.Help (help ())
      | "is_compatible_with", [left; right] -> Request.IsCompatibleWith (access left, access right)
      | "less_or_equal", [left; right] -> Request.LessOrEqual (access left, access right)
      | "path_of_module", [module_access] -> Request.PathOfModule (reference module_access)
      | "save_server_state", [path] ->
          Request.SaveServerState (PyrePath.create_absolute (string path))
      | "superclasses", names -> Superclasses (List.map ~f:reference names)
      | "type", [argument] -> Type (expression argument)
      | "types", paths -> Request.TypesInFiles (List.map ~f:string paths)
      | "validate_taint_models", [] -> ValidateTaintModels None
      | "validate_taint_models", [argument] -> Request.ValidateTaintModels (Some (string argument))
      | "inline_decorators", arguments -> parse_inline_decorators arguments
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

let rec process_request ~environment ~build_system ~configuration request =
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
            | LookupProcessor.StubShadowing -> " (file shadowed by .pyi stub file)"
            | LookupProcessor.FileNotFound -> " (file not found)"
          in
          Format.asprintf
            "%s%s`%s`%s"
            sofar
            (if String.is_empty sofar then "" else ", ")
            path
            (print_reason error_reason))
        errors
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
    | Batch requests ->
        Batch (List.map ~f:(process_request ~environment ~build_system ~configuration) requests)
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
                 ~configuration
                 (TypeEnvironment.ReadOnly.ast_environment read_only_environment))
        in
        let callees =
          (* We don't yet support a syntax for fetching property setters. *)
          Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
          |> List.map ~f:(fun { Callgraph.callee; locations } ->
                 { Base.callee; locations = List.map locations ~f:instantiate })
        in
        Single (Base.CalleesWithLocation callees)
    | Defines module_or_class_names ->
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
                     ~configuration
                     (TypeEnvironment.ReadOnly.ast_environment read_only_environment))
            in
            Callgraph.get ~caller:(Callgraph.FunctionCaller caller)
            |> List.map ~f:(fun { Callgraph.callee; locations } ->
                   { Base.callee; locations = List.map locations ~f:instantiate })
            |> fun callees -> { Base.caller; callees }
          in
          let ast_environment = TypeEnvironment.ReadOnly.ast_environment read_only_environment in
          AstEnvironment.ReadOnly.get_processed_source ast_environment module_qualifier
          >>| Preprocessing.defines ~include_toplevels:false ~include_stubs:false
          >>| List.map ~f:callees
          |> Option.value ~default:[]
        in
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        Single (Base.Callgraph (List.concat_map qualifiers ~f:get_callgraph))
    | Help help_list -> Single (Base.Help help_list)
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
    | LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.less_or_equal global_resolution ~left ~right
        |> fun response -> Single (Base.Boolean response)
    | PathOfModule module_name ->
        ModuleTracker.lookup_source_path module_tracker module_name
        >>= (fun source_path ->
              let path = SourcePath.full_path ~configuration source_path |> PyrePath.absolute in
              Some (Single (Base.FoundPath path)))
        |> Option.value
             ~default:
               (Error (Format.sprintf "No path found for module `%s`" (Reference.show module_name)))
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
        let annotations =
          LookupProcessor.find_all_annotations_batch ~environment ~build_system ~configuration paths
        in
        let create_result { LookupProcessor.path; types_by_location } =
          match types_by_location with
          | Result.Ok types ->
              Either.First { Base.path; types = List.map ~f:create_type_at_location types }
          | Result.Error error_reason -> Either.Second (path, error_reason)
        in
        let results, errors = List.partition_map ~f:create_result annotations in
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
                Interprocedural.Target.Map.empty
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
    | InlineDecorators { function_reference; decorators_to_skip } ->
        InlineDecorators.inline_decorators
          ~environment:(TypeEnvironment.read_only environment)
          ~decorators_to_skip:(Reference.Set.of_list decorators_to_skip)
          function_reference
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


let parse_and_process_request ~environment ~build_system ~configuration request =
  match parse_request request with
  | Result.Error reason -> Response.Error reason
  | Result.Ok request -> process_request ~environment ~build_system ~configuration request
