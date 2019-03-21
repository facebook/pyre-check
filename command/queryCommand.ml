(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Network
open Pyre
open PyreParser
open Server
open Protocol
open TypeQuery


exception InvalidQuery of string


let parse_query
    ~configuration:({ Configuration.Analysis.local_root = root; _ } as configuration)
    query =
  match (Parser.parse [query]) with
  | [{
      Node.value = Statement.Expression {
          Node.value =
            Access
              (SimpleAccess [
                  Access.Identifier name;
                  Access.Call { Node.value = arguments; _ };
                ]);
          _;
        };
      _;
    }] ->
      let expression { Argument.value; _ } = value in
      let access = function
        | { Argument.value = { Node.value = Access (SimpleAccess access); _ }; _ } -> access
        | _ -> raise (InvalidQuery "expected access")
      in
      let string_of_expression = function
        | {Node.value = String { StringLiteral.value; kind = StringLiteral.String }; _ } ->
            value
        | _ ->
            raise (InvalidQuery "expected string")
      in
      let string argument =
        argument
        |> expression
        |> string_of_expression
      in
      begin
        match String.lowercase name, arguments with
        | "attributes", [name] ->
            Request.TypeQueryRequest (Attributes (access name))
        | "compute_hashes_to_keys", [] ->
            Request.TypeQueryRequest ComputeHashesToKeys
        | "decode_ocaml_values", pairs ->
            let pair_of_strings = function
              | {
                Argument.value = { Node.value = Tuple [serialized_key; serialized_value]; _ };
                _;
              } ->
                  {
                    serialized_key = string_of_expression serialized_key;
                    serialized_value = string_of_expression serialized_value;
                  }
              | { Argument.value; _ } ->
                  raise
                    (InvalidQuery
                       (Format.sprintf
                          "expected pair of strings, got `%s`"
                          (Expression.show value)))
            in
            Request.TypeQueryRequest (DecodeOcamlValues (List.map pairs ~f:pair_of_strings))
        | "decode_ocaml_values_from_file", [path] ->
            let lines =
              let format line =
                line
                |> String.split ~on:','
                |> function
                | [serialized_key; serialized_value] ->
                    Some { serialized_key; serialized_value }
                | _ -> None
              in
              string path
              |> Path.create_absolute
              |> File.create
              |> File.lines
              >>| List.filter_map ~f:format
            in
            begin
              match lines with
              | Some pairs ->
                  Request.TypeQueryRequest (DecodeOcamlValues pairs)
              | None ->
                  raise (InvalidQuery (Format.sprintf "Malformatted file at `%s`" (string path)))
            end
        | "dump_dependencies", [path] ->
            let file =
              Path.create_relative ~root ~relative:(string path)
              |> File.create
            in
            Request.TypeQueryRequest (DumpDependencies file)
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
              | _ ->
                  raise (InvalidQuery "Too many arguments to `dump_memory_to_sqlite`")
            in
            Request.TypeQueryRequest (DumpMemoryToSqlite path)
        | "is_compatible_with", [left; right] ->
            Request.TypeQueryRequest (IsCompatibleWith (access left, access right))
        | "join", [left; right] ->
            Request.TypeQueryRequest (Join (access left, access right))
        | "less_or_equal", [left; right] ->
            Request.TypeQueryRequest (LessOrEqual (access left, access right))
        | "meet", [left; right] ->
            Request.TypeQueryRequest (Meet (access left, access right))
        | "methods", [name] ->
            Request.TypeQueryRequest (Methods (access name))
        | "normalize_type", [name] ->
            Request.TypeQueryRequest (NormalizeType (access name))
        | "path_of_module", [module_access] ->
            Request.TypeQueryRequest (PathOfModule (access module_access))
        | "save_server_state", [path] ->
            Request.TypeQueryRequest
              (SaveServerState
                 (Path.create_absolute
                    ~follow_symbolic_links:false
                    (string path)))
        | "signature", [name] ->
            Request.TypeQueryRequest (Signature (access name))
        | "superclasses", [name] ->
            Request.TypeQueryRequest (Superclasses (access name))
        | "type", [argument] ->
            Request.TypeQueryRequest (Type (expression argument))
        | "type_at_position",
          [
            path;
            { Argument.value = { Node.value = Integer line; _ }; _ };
            { Argument.value = { Node.value = Integer column; _ }; _ };
          ] ->
            let file =
              Path.create_relative ~root ~relative:(string path)
              |> File.create
            in
            let position = { Location.line; column } in
            Request.TypeQueryRequest (TypeAtPosition { file; position })
        | "types_in_file", [path] ->
            let file =
              Path.create_relative ~root ~relative:(string path)
              |> File.create
            in
            Request.TypeQueryRequest (TypesInFile file)
        | "type_check", arguments ->
            let files =
              arguments
              |> List.map ~f:string
              |> List.map ~f:(fun relative -> Path.create_relative ~root ~relative)
              |> List.map ~f:File.create
            in
            Request.TypeCheckRequest (TypeCheckRequest.create ~check:files ())
        | _ ->
            raise (InvalidQuery "unexpected query call")
      end
  | _ ->
      raise (InvalidQuery "unexpected query")
  | exception Parser.Error message ->
      raise (InvalidQuery ("failed to parse query: " ^ message))


let run_query serialized local_root () =
  let local_root = Path.create_absolute local_root in
  let configuration = Configuration.Analysis.create ~local_root () in
  (fun () ->
     let response =
       try
         let query = parse_query ~configuration serialized in
         let socket = Server.Operations.connect ~retries:3 ~configuration in
         Socket.write socket query;
         match Socket.read socket with
         | TypeQueryResponse response ->
             response_to_yojson response
         | TypeCheckResponse errors ->
             errors
             |> List.concat_map ~f:snd
             |> (fun errors ->
                 `Assoc [
                   "response",
                   `List (List.map ~f:(Analysis.Error.to_json ~show_error_traces:false) errors);
                 ])
         | response ->
             `Assoc [
               "response",
               `String
                 (Format.sprintf
                    "Unexpected response %s from server"
                    (Server.Protocol.show_response response))
             ]
       with
       | InvalidQuery reason ->
           `Assoc [
             "error",
             `String (Format.sprintf "Unable to parse query \"%s\": %s." serialized reason);
           ]
       | Parser.Error error ->
           let error =
             String.split ~on:'\n' error
             |> (fun lines -> List.drop lines 1)
             |> String.concat ~sep:"\n"
           in
           `Assoc [
             "error",
             `String (Format.sprintf "Unable to parse query \"%s\": %s." serialized error);
           ]
     in
     Yojson.Safe.to_string response
     |> Log.print "%s")
  |> Scheduler.run_process ~configuration


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Queries the server for type order information."
    (empty
     +> anon ("query" %: string)
     +> anon (maybe_with_default "." ("source-root" %: string)))
    run_query
