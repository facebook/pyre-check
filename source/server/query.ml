(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Expression
open Pyre

exception InvalidQuery of string

module Request = struct
  type t =
    | Attributes of Reference.t
    | Batch of t list
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | Defines of Reference.t list
    | DumpCallGraph
    | DumpClassHierarchy
    | Help of string
    | IsCompatibleWith of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | Methods of Expression.t
    | PathOfModule of Reference.t
    | SaveServerState of Path.t
    | Superclasses of Expression.t list
    | Type of Expression.t
    | TypeAtPosition of {
        path: Path.t;
        position: Location.position;
      }
    | TypesInFiles of Path.t list
    | ValidateTaintModels of Path.t option
  [@@deriving eq, show]
end

module Response = struct
  module Base = struct
    type attribute_kind =
      | Regular
      | Property
    [@@deriving eq, show, to_yojson]

    type attribute = {
      name: string;
      annotation: Type.t;
      kind: attribute_kind;
      final: bool;
    }
    [@@deriving eq, show, to_yojson]

    type method_representation = {
      name: string;
      parameters: Type.t list;
      return_annotation: Type.t;
    }
    [@@deriving eq, show, to_yojson]

    type type_at_location = {
      location: Location.t;
      annotation: Type.t;
    }
    [@@deriving eq, show, to_yojson]

    type types_at_path = {
      path: PyrePath.t;
      types: type_at_location list;
    }
    [@@deriving eq, show, to_yojson]

    type compatibility = {
      actual: Type.t;
      expected: Type.t;
      result: bool;
    }
    [@@deriving eq, show]

    type callee_with_instantiated_locations = {
      callee: Analysis.Callgraph.callee;
      locations: Location.WithPath.t list;
    }
    [@@deriving eq, show]

    type callees = {
      caller: Reference.t;
      callees: callee_with_instantiated_locations list;
    }
    [@@deriving eq, show]

    type parameter_representation = {
      parameter_name: string;
      parameter_annotation: Expression.t option;
    }
    [@@deriving eq, show]

    type define = {
      define_name: Reference.t;
      parameters: parameter_representation list;
      return_annotation: Expression.t option;
    }
    [@@deriving eq, show]

    type superclasses_mapping = {
      class_name: Reference.t;
      superclasses: Type.t list;
    }
    [@@deriving eq, show, to_yojson]

    let _ = show_compatibility (* unused, but pp is *)

    type t =
      | Boolean of bool
      | Callees of Analysis.Callgraph.callee list
      | CalleesWithLocation of callee_with_instantiated_locations list
      | Callgraph of callees list
      | ClassHierarchy of Yojson.Safe.t
      | Compatibility of compatibility
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundMethods of method_representation list
      | FoundPath of string
      | Help of string
      | ModelVerificationErrors of Taint.Model.ModelVerificationError.t list
      | Success of string
      | Superclasses of superclasses_mapping list
      | Type of Type.t
      | TypeAtLocation of type_at_location
      | TypesByPath of types_at_path list
    [@@deriving eq, show]

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
      | ClassHierarchy hierarchy -> hierarchy
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
          `Assoc ["errors", `List (List.map errors ~f:Taint.Model.verification_error_to_json)]
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
      | FoundMethods methods ->
          `Assoc ["methods", `List (List.map methods ~f:method_representation_to_yojson)]
      | FoundPath path -> `Assoc ["path", `String path]
      | Success message -> `Assoc ["message", `String message]
      | Superclasses class_to_superclasses_mapping -> (
          match class_to_superclasses_mapping with
          | [{ superclasses; _ }] ->
              `Assoc ["superclasses", `List (List.map superclasses ~f:Type.to_yojson)]
          | _ ->
              let superclasses_to_json { class_name; superclasses } =
                `Assoc
                  [
                    "class_name", `String (Reference.show class_name);
                    "superclasses", `List (List.map superclasses ~f:Type.to_yojson);
                  ]
              in
              `List (List.map class_to_superclasses_mapping ~f:superclasses_to_json) )
      | Type annotation -> `Assoc ["type", Type.to_yojson annotation]
      | TypeAtLocation annotation -> type_at_location_to_yojson annotation
      | TypesByPath paths_to_annotations ->
          `List (List.map paths_to_annotations ~f:types_at_path_to_yojson)
  end

  type t =
    | Single of Base.t
    | Batch of t list
    | Error of string
  [@@deriving eq, show]

  let rec to_yojson = function
    | Single base_response -> `Assoc ["response", Base.to_yojson base_response]
    | Batch responses -> `Assoc ["response", `List (List.map ~f:to_yojson responses)]
    | Error message -> `Assoc ["error", `String message]


  let create_type_at_location (location, annotation) = { Base.location; annotation }
end

let help () =
  let open Request in
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
    | DumpClassHierarchy ->
        Some
          "dump_class_hierarchy(): Prints out the entire class hierarchy as Pyre understands it, \
           elides type variables."
    | IsCompatibleWith _ -> None
    | LessOrEqual _ -> Some "less_or_equal(T1, T2): Returns whether T1 is a subtype of T2."
    | Methods _ -> Some "methods(class_name): Evaluates to the list of methods for `class_name`."
    | PathOfModule _ -> Some "path_of_module(module): Gives an absolute path for `module`."
    | SaveServerState _ ->
        Some "save_server_state('path'): Saves Pyre's serialized state into `path`."
    | Superclasses _ ->
        Some
          "superclasses(class_name1, class_name2, ...): Returns a mapping of class_name to the \
           list of superclasses for `class_name`."
    | Type _ -> Some "type(expression): Evaluates the type of `expression`."
    | TypeAtPosition _ ->
        Some "type_at_position('path', line, column): Returns the type for the given cursor."
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
  let path = Path.current_working_directory () in
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
      DumpClassHierarchy;
      IsCompatibleWith (empty, empty);
      LessOrEqual (empty, empty);
      Methods empty;
      PathOfModule (Reference.create "");
      SaveServerState path;
      Superclasses [empty];
      Type (Node.create_with_default_location Expression.True);
      TypeAtPosition { path; position = Location.any_position };
      TypesInFiles [path];
      ValidateTaintModels None;
    ]
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:"\n  "
  |> Format.sprintf "Possible queries:\n  %s"


let rec parse_query
    ~configuration:({ Configuration.Analysis.local_root = root; _ } as configuration)
    query
  =
  match PyreParser.Parser.parse [query] with
  | [
   {
     Node.value =
       Expression
         {
           Node.value = Call { callee = { Node.value = Name (Name.Identifier name); _ }; arguments };
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
        | { Node.value = Expression.String { StringLiteral.value; kind = StringLiteral.String }; _ }
          ->
            value
        | _ -> raise (InvalidQuery "expected string")
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
          |> List.map ~f:(parse_query ~configuration)
          |> List.fold ~f:construct_batch ~init:[]
          |> List.rev
          |> fun query_list -> Request.Batch query_list
      | "callees", [name] -> Request.Callees (reference name)
      | "callees_with_location", [name] -> Request.CalleesWithLocation (reference name)
      | "defines", names -> Request.Defines (List.map names ~f:reference)
      | "dump_call_graph", [] -> Request.DumpCallGraph
      | "dump_class_hierarchy", [] -> Request.DumpClassHierarchy
      | "help", _ -> Request.Help (help ())
      | "is_compatible_with", [left; right] -> Request.IsCompatibleWith (access left, access right)
      | "less_or_equal", [left; right] -> Request.LessOrEqual (access left, access right)
      | "methods", [name] -> Request.Methods (expression name)
      | "path_of_module", [module_access] -> Request.PathOfModule (reference module_access)
      | "save_server_state", [path] ->
          Request.SaveServerState (Path.create_absolute ~follow_symbolic_links:false (string path))
      | "superclasses", names -> Superclasses (List.map ~f:access names)
      | "type", [argument] -> Type (expression argument)
      | ( "type_at_position",
          [
            path;
            { Call.Argument.value = { Node.value = Integer line; _ }; _ };
            { Call.Argument.value = { Node.value = Integer column; _ }; _ };
          ] ) ->
          let path = Path.create_relative ~root ~relative:(string path) in
          let position = { Location.line; column } in
          Request.TypeAtPosition { path; position }
      | "types", paths ->
          let paths =
            List.map ~f:(fun path -> Path.create_relative ~root ~relative:(string path)) paths
          in
          Request.TypesInFiles paths
      | "validate_taint_models", [] -> ValidateTaintModels None
      | "validate_taint_models", [argument] ->
          let path =
            let path = string argument in
            if String.is_prefix ~prefix:"/" path then
              Path.create_absolute path
            else
              Path.create_relative ~root ~relative:(string argument)
          in
          Request.ValidateTaintModels (Some path)
      | _ -> raise (InvalidQuery "unexpected query") )
  | _ when String.equal query "help" -> Help (help ())
  | _ -> raise (InvalidQuery "unexpected query")
  | exception PyreParser.Parser.Error _ -> raise (InvalidQuery "failed to parse query")
