(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Expression
open Protocol
open TypeQuery
open Pyre

exception InvalidQuery of string

let help () =
  let help = function
    | RunCheck _ ->
        Some
          "run_check('check_name', 'path1.py', 'path2.py'): Runs the `check_name` static analysis \
           on the provided list of paths."
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
      RunCheck { check_name = ""; paths = [] };
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
      | "attributes", [name] -> Attributes (reference name)
      | "batch", queries ->
          let construct_batch batch_queries (query : request) =
            match query with
            | Batch _ -> raise (InvalidQuery "cannot nest batch queries")
            | query -> query :: batch_queries
          in
          List.map ~f:expression queries
          |> List.map ~f:Expression.show
          |> List.map ~f:(parse_query ~configuration)
          |> List.fold ~f:construct_batch ~init:[]
          |> List.rev
          |> fun query_list -> (Batch query_list : request)
      | "callees", [name] -> Callees (reference name)
      | "callees_with_location", [name] -> CalleesWithLocation (reference name)
      | "defines", names -> Defines (List.map names ~f:reference)
      | "dump_call_graph", [] -> DumpCallGraph
      | "dump_class_hierarchy", [] -> DumpClassHierarchy
      | "help", _ -> Help (help ())
      | "is_compatible_with", [left; right] -> IsCompatibleWith (access left, access right)
      | "less_or_equal", [left; right] -> LessOrEqual (access left, access right)
      | "methods", [name] -> Methods (expression name)
      | "path_of_module", [module_access] -> PathOfModule (reference module_access)
      | "run_check", check_name :: paths ->
          let check_name = string check_name in
          let paths =
            List.map paths ~f:(fun path ->
                Path.create_absolute ~follow_symbolic_links:false (string path))
          in
          RunCheck { check_name; paths }
      | "save_server_state", [path] ->
          SaveServerState (Path.create_absolute ~follow_symbolic_links:false (string path))
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
          TypeAtPosition { path; position }
      | "types", paths ->
          let paths =
            List.map ~f:(fun path -> Path.create_relative ~root ~relative:(string path)) paths
          in
          TypesInFiles paths
      | "type_check", arguments ->
          let paths =
            arguments
            |> List.map ~f:string
            |> List.map ~f:(fun relative -> Path.create_relative ~root ~relative)
          in
          RunCheck { check_name = "typeCheck"; paths }
      | "validate_taint_models", [] -> ValidateTaintModels None
      | "validate_taint_models", [argument] ->
          let path =
            let path = string argument in
            if String.is_prefix ~prefix:"/" path then
              Path.create_absolute path
            else
              Path.create_relative ~root ~relative:(string argument)
          in
          ValidateTaintModels (Some path)
      | _ -> raise (InvalidQuery "unexpected query") )
  | _ when String.equal query "help" -> Help (help ())
  | _ -> raise (InvalidQuery "unexpected query")
  | exception PyreParser.Parser.Error _ -> raise (InvalidQuery "failed to parse query")
