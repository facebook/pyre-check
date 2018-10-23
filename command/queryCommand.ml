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


let parse_query ~root query =
  match (Parser.parse [query]) with
  | [{
      Node.value = Statement.Expression {
          Node.value = Access [
              Access.Identifier name;
              Access.Call { Node.value = arguments; _ };
            ];
          _;
        };
      _;
    }] ->
      let expression { Argument.value; _ } = value in
      let access = function
        | { Argument.value = { Node.value = Access access; _ }; _ } -> access
        | _ -> raise (InvalidQuery "expected access")
      in
      let string = function
        | {
          Argument.value = {
            Node.value = String { StringLiteral.value; kind = StringLiteral.String };
            _;
          };
          _;
        } ->
            value
        | _ ->
            raise (InvalidQuery "expected string")
      in
      begin
        match String.lowercase (Identifier.show name), arguments with
        | "attributes", [name] ->
            Request.TypeQueryRequest (Attributes (access name))
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
         let query = parse_query ~root:local_root serialized in
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
                   `List (List.map ~f:(Analysis.Error.to_json ~detailed:false) errors);
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
