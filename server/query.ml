(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
    | ComputeHashesToKeys -> None
    | DecodeOcamlValues _ -> None
    | DumpClassHierarchy ->
        Some
          "dump_class_hierarchy(): Prints out the entire class hierarchy as Pyre understands it, \
           elides type variables."
    | DumpMemoryToSqlite _ -> None
    | IsCompatibleWith _ -> None
    | Join _ -> Some "join(T1, T2): Returns the least common supertype of T1 and T2."
    | LessOrEqual _ -> Some "less_or_equal(T1, T2): Returns whether T1 is a subtype of T2."
    | Meet _ -> Some "meet(T1, T2): Returns the greatest common subtype of T1 and T2."
    | Methods _ -> Some "methods(class_name): Evaluates to the list of methods for `class_name`."
    | NamesInFiles _ ->
        Some
          "qualified_names(path='path') or qualified_names('path1', 'path2', ...): Returns a map \
           from each given path to a list of all qualified names for that path."
    | NormalizeType _ -> Some "normalize_type(T): Resolves all type aliases for `T`."
    | PathOfModule _ -> Some "path_of_module(module): Gives an absolute path for `module`."
    | SaveServerState _ ->
        Some "save_server_state('path'): Saves Pyre's serialized state into `path`."
    | Signature _ ->
        Some "signature(a, b, ...): Gives a human-readable signature for the given function names."
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
      ComputeHashesToKeys;
      DecodeOcamlValues [];
      Defines [Reference.create ""];
      DumpCallGraph;
      DumpClassHierarchy;
      DumpMemoryToSqlite path;
      IsCompatibleWith (empty, empty);
      Join (empty, empty);
      LessOrEqual (empty, empty);
      Meet (empty, empty);
      Methods empty;
      NamesInFiles [path];
      NormalizeType empty;
      PathOfModule (Reference.create "");
      SaveServerState path;
      Signature [Reference.create ""];
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
      | "attributes", [name] -> Request.TypeQueryRequest (Attributes (reference name))
      | "batch", queries ->
          let construct_batch batch_queries query =
            match query with
            | Request.TypeQueryRequest (Batch _) -> raise (InvalidQuery "cannot nest batch queries")
            | Request.TypeQueryRequest query -> query :: batch_queries
            | _ -> raise (InvalidQuery "unexpected query")
          in
          List.map ~f:expression queries
          |> List.map ~f:Expression.show
          |> List.map ~f:(parse_query ~configuration)
          |> List.fold ~f:construct_batch ~init:[]
          |> List.rev
          |> fun query_list -> Request.TypeQueryRequest (Batch query_list)
      | "callees", [name] -> Request.TypeQueryRequest (Callees (reference name))
      | "callees_with_location", [name] ->
          Request.TypeQueryRequest (CalleesWithLocation (reference name))
      | "defines", names -> Request.TypeQueryRequest (Defines (List.map names ~f:reference))
      | "dump_call_graph", [] -> Request.TypeQueryRequest DumpCallGraph
      | "compute_hashes_to_keys", [] -> Request.TypeQueryRequest ComputeHashesToKeys
      | "decode_ocaml_values", values ->
          let parse_values_to_decode = function
            | {
                Call.Argument.value = { Node.value = Tuple [serialized_key; serialized_value]; _ };
                _;
              } ->
                SerializedValue
                  {
                    serialized_key = string_of_expression serialized_key;
                    serialized_value = string_of_expression serialized_value;
                  }
            | {
                Call.Argument.value =
                  { Node.value = Tuple [serialized_key; first_value; second_value]; _ };
                _;
              } ->
                SerializedPair
                  {
                    serialized_key = string_of_expression serialized_key;
                    first_serialized_value = string_of_expression first_value;
                    second_serialized_value = string_of_expression second_value;
                  }
            | { Call.Argument.value; _ } ->
                raise
                  (InvalidQuery
                     (Format.sprintf "expected pair of strings, got `%s`" (Expression.show value)))
          in
          Request.TypeQueryRequest (DecodeOcamlValues (List.map values ~f:parse_values_to_decode))
      | "decode_ocaml_values_from_file", [path] -> (
          let lines =
            let format line =
              line
              |> String.split ~on:','
              |> function
              | [serialized_key; serialized_value] ->
                  Some (SerializedValue { serialized_key; serialized_value })
              | [serialized_key; first_serialized_value; second_serialized_value] ->
                  Some
                    (SerializedPair
                       { serialized_key; first_serialized_value; second_serialized_value })
              | _ -> None
            in
            string path
            |> Path.create_absolute
            |> File.create
            |> File.lines
            >>| List.filter_map ~f:format
          in
          match lines with
          | Some pairs -> Request.TypeQueryRequest (DecodeOcamlValues pairs)
          | None -> raise (InvalidQuery (Format.sprintf "malformatted file at `%s`" (string path)))
          )
      | "dump_class_hierarchy", [] -> Request.TypeQueryRequest DumpClassHierarchy
      | "dump_memory_to_sqlite", arguments ->
          let path =
            match arguments with
            | [argument] ->
                let path = string argument in
                if Filename.is_relative path then
                  Path.create_relative ~root:(Path.current_working_directory ()) ~relative:path
                else
                  Path.create_absolute ~follow_symbolic_links:false path
            | [] ->
                Path.create_relative
                  ~root:(Configuration.Analysis.log_directory configuration)
                  ~relative:"memory.sqlite"
            | _ -> raise (InvalidQuery "too many arguments to `dump_memory_to_sqlite`")
          in
          Request.TypeQueryRequest (DumpMemoryToSqlite path)
      | "help", _ -> Request.TypeQueryRequest (Help (help ()))
      | "is_compatible_with", [left; right] ->
          Request.TypeQueryRequest (IsCompatibleWith (access left, access right))
      | "join", [left; right] -> Request.TypeQueryRequest (Join (access left, access right))
      | "less_or_equal", [left; right] ->
          Request.TypeQueryRequest (LessOrEqual (access left, access right))
      | "meet", [left; right] -> Request.TypeQueryRequest (Meet (access left, access right))
      | "methods", [name] -> Request.TypeQueryRequest (Methods (expression name))
      | "normalize_type", [name] -> Request.TypeQueryRequest (NormalizeType (access name))
      | "path_of_module", [module_access] ->
          Request.TypeQueryRequest (PathOfModule (reference module_access))
      | "qualified_names", paths ->
          let paths =
            List.map ~f:(fun path -> Path.create_relative ~root ~relative:(string path)) paths
          in
          Request.TypeQueryRequest (NamesInFiles paths)
      | "run_check", check_name :: paths ->
          let check_name = string check_name in
          let paths =
            List.map paths ~f:(fun path ->
                Path.create_absolute ~follow_symbolic_links:false (string path))
          in
          Request.TypeQueryRequest (RunCheck { check_name; paths })
      | "save_server_state", [path] ->
          Request.TypeQueryRequest
            (SaveServerState (Path.create_absolute ~follow_symbolic_links:false (string path)))
      | "signature", names -> Request.TypeQueryRequest (Signature (List.map names ~f:reference))
      | "superclasses", names -> Request.TypeQueryRequest (Superclasses (List.map ~f:access names))
      | "type", [argument] -> Request.TypeQueryRequest (Type (expression argument))
      | ( "type_at_position",
          [
            path;
            { Call.Argument.value = { Node.value = Integer line; _ }; _ };
            { Call.Argument.value = { Node.value = Integer column; _ }; _ };
          ] ) ->
          let path = Path.create_relative ~root ~relative:(string path) in
          let position = { Location.line; column } in
          Request.TypeQueryRequest (TypeAtPosition { path; position })
      | "types", paths ->
          let paths =
            List.map ~f:(fun path -> Path.create_relative ~root ~relative:(string path)) paths
          in
          Request.TypeQueryRequest (TypesInFiles paths)
      | "type_check", arguments ->
          let paths =
            arguments
            |> List.map ~f:string
            |> List.map ~f:(fun relative -> Path.create_relative ~root ~relative)
          in
          Request.TypeQueryRequest (RunCheck { check_name = "typeCheck"; paths })
      | "validate_taint_models", [] -> Request.TypeQueryRequest (ValidateTaintModels None)
      | "validate_taint_models", [argument] ->
          let path =
            let path = string argument in
            if String.is_prefix ~prefix:"/" path then
              Path.create_absolute path
            else
              Path.create_relative ~root ~relative:(string argument)
          in
          Request.TypeQueryRequest (ValidateTaintModels (Some path))
      | _ -> raise (InvalidQuery "unexpected query") )
  | _ when String.equal query "help" -> Request.TypeQueryRequest (Help (help ()))
  | _ -> raise (InvalidQuery "unexpected query")
  | exception PyreParser.Parser.Error _ -> raise (InvalidQuery "failed to parse query")
