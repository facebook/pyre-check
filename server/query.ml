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
    | Attributes _ ->
        Some
          "attributes(class_name): Returns a list of attributes, including functions, for a class."
    | Callees _ -> Some "callees(function): calls from a given function."
    | ComputeHashesToKeys -> None
    | CoverageInFile _ ->
        Some "coverage_in_file('path'): Gives detailed coverage information for the given path."
    | DecodeOcamlValues _ -> None
    | DependentDefines _ -> None
    | DumpDependencies _ ->
        Some
          (Format.sprintf
             "%s: %s"
             "dump_dependencies('path')"
             "Writes the dependencies of 'path' to `.pyre/dependencies.dot`.")
    | DumpMemoryToSqlite _ -> None
    | IsCompatibleWith _ ->
        Some "is_compatible_with(T1, T2): Returns whether T2 can be used in place of T1."
    | Join _ -> Some "join(T1, T2): Returns the least common supertype of T1 and T2."
    | LessOrEqual _ -> Some "less_or_equal(T1, T2): Returns whether T1 is a subtype of T2."
    | Meet _ -> Some "meet(T1, T2): Returns the greatest common subtype of T1 and T2."
    | Methods _ -> Some "methods(class_name): Evaluates to the list of methods for `class_name`."
    | NormalizeType _ -> Some "normalize_type(T): Resolves all type aliases for `T`."
    | PathOfModule _ -> Some "path_of_module(module): Gives an absolute path for `module`."
    | SaveServerState _ ->
        Some "save_server_state('path'): Saves Pyre's serialized state into `path`."
    | Signature _ ->
        Some "signature(function_name): Gives a human-readable signature for `function_name`."
    | Superclasses _ ->
        Some "superclasses(class_name): Returns the list of superclasses for `class_name`."
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
  in
  let path = Path.current_working_directory () in
  let empty = Name (Name.Identifier "") |> Node.create_with_default_location in
  List.filter_map
    ~f:help
    [ RunCheck { check_name = ""; paths = [] };
      Attributes (Reference.create "");
      Callees (Reference.create "");
      ComputeHashesToKeys;
      CoverageInFile path;
      DecodeOcamlValues [];
      DumpDependencies path;
      DumpMemoryToSqlite path;
      IsCompatibleWith (empty, empty);
      Join (empty, empty);
      LessOrEqual (empty, empty);
      Meet (empty, empty);
      Methods (Reference.create "");
      NormalizeType empty;
      PathOfModule (Reference.create "");
      SaveServerState path;
      Signature (Reference.create "");
      Superclasses empty;
      Type (Node.create_with_default_location Expression.True);
      TypeAtPosition { path; position = Location.any_position };
      TypesInFiles [path];
      ValidateTaintModels None ]
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:"\n  "
  |> Format.sprintf "Possible queries:\n  %s"


let parse_query
    ~configuration:({ Configuration.Analysis.local_root = root; _ } as configuration)
    query
  =
  match PyreParser.Parser.parse [query] with
  | [ {
        Node.value =
          Statement.Expression
            {
              Node.value =
                Call { callee = { Node.value = Name (Name.Identifier name); _ }; arguments };
              _;
            };
        _;
      } ] -> (
      let expression { Call.Argument.value; _ } = value in
      let access = function
        | { Call.Argument.value; _ } when Expression.has_identifier_base value -> value
        | _ -> raise (InvalidQuery "expected access")
      in
      let reference = function
        | { Call.Argument.value = { Node.value = Name name; _ }; _ }
          when Expression.is_simple_name name ->
            Expression.name_to_reference_exn name
        | _ -> raise (InvalidQuery "expected reference")
      in
      let string_of_expression = function
        | { Node.value = String { StringLiteral.value; kind = StringLiteral.String }; _ } -> value
        | _ -> raise (InvalidQuery "expected string")
      in
      let string argument = argument |> expression |> string_of_expression in
      match String.lowercase name, arguments with
      | "attributes", [name] -> Request.TypeQueryRequest (Attributes (reference name))
      | "callees", [name] -> Request.TypeQueryRequest (Callees (reference name))
      | "compute_hashes_to_keys", [] -> Request.TypeQueryRequest ComputeHashesToKeys
      | "coverage_in_file", [path] ->
          let path = Path.create_relative ~root ~relative:(string path) in
          Request.TypeQueryRequest (CoverageInFile path)
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
          | None -> raise (InvalidQuery (Format.sprintf "Malformatted file at `%s`" (string path)))
          )
      | "dependent_defines", paths ->
          let create_path path = Path.create_relative ~root ~relative:(string path) in
          let paths = List.map paths ~f:create_path in
          Request.TypeQueryRequest (DependentDefines paths)
      | "dump_dependencies", [path] ->
          let path = Path.create_relative ~root ~relative:(string path) in
          Request.TypeQueryRequest (DumpDependencies path)
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
                  ~root:(Configuration.Analysis.pyre_root configuration)
                  ~relative:"memory.sqlite"
            | _ -> raise (InvalidQuery "Too many arguments to `dump_memory_to_sqlite`")
          in
          Request.TypeQueryRequest (DumpMemoryToSqlite path)
      | "is_compatible_with", [left; right] ->
          Request.TypeQueryRequest (IsCompatibleWith (access left, access right))
      | "join", [left; right] -> Request.TypeQueryRequest (Join (access left, access right))
      | "less_or_equal", [left; right] ->
          Request.TypeQueryRequest (LessOrEqual (access left, access right))
      | "meet", [left; right] -> Request.TypeQueryRequest (Meet (access left, access right))
      | "methods", [name] -> Request.TypeQueryRequest (Methods (reference name))
      | "normalize_type", [name] -> Request.TypeQueryRequest (NormalizeType (access name))
      | "path_of_module", [module_access] ->
          Request.TypeQueryRequest (PathOfModule (reference module_access))
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
      | "signature", [name] -> Request.TypeQueryRequest (Signature (reference name))
      | "superclasses", [name] -> Request.TypeQueryRequest (Superclasses (access name))
      | "type", [argument] -> Request.TypeQueryRequest (Type (expression argument))
      | ( "type_at_position",
          [ path;
            { Call.Argument.value = { Node.value = Integer line; _ }; _ };
            { Call.Argument.value = { Node.value = Integer column; _ }; _ } ] ) ->
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
      | _ -> raise (InvalidQuery "unexpected query call") )
  | _ -> raise (InvalidQuery "unexpected query")
  | exception PyreParser.Parser.Error message ->
      raise (InvalidQuery ("failed to parse query: " ^ message))
