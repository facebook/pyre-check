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


exception InvalidQuery


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
        | _ -> raise InvalidQuery
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
        | "signature", [name] ->
            Request.TypeQueryRequest (Signature (access name))
        | "superclasses", [name] ->
            Request.TypeQueryRequest (Superclasses (access name))
        | "type", [argument] ->
            Request.TypeQueryRequest (Type (expression argument))
        | "type_at_location",
          [
            path;
            { Argument.value = { Node.value = Integer line; _ }; _ };
            { Argument.value = { Node.value = Integer column; _ }; _ };
          ] ->
            let location =
              let path = Access.show (access path) in
              let position = { Location.line; column } in
              { Location.path; start = position; stop = position }
            in
            Request.TypeQueryRequest (TypeAtLocation location)
        | "type_check_path", arguments ->
            let files =
              arguments
              |> List.map ~f:expression
              |> List.map ~f:Expression.show
              |> List.map ~f:(fun relative -> Path.create_relative ~root ~relative)
              |> List.map ~f:File.create
            in
            Request.TypeCheckRequest (TypeCheckRequest.create ~check:files ())
        | _ ->
            raise InvalidQuery
      end
  | _ ->
      raise InvalidQuery


let run_query serialized local_root () =
  let local_root = Path.create_absolute local_root in
  let configuration = Configuration.create ~local_root () in
  (fun () ->
     try
       let query = parse_query ~root:local_root serialized in
       let socket = Server.Operations.connect ~retries:3 ~configuration in
       Socket.write socket query;
       match Socket.read socket with
       | TypeQueryResponse response ->
           Log.print "%s\n" (Yojson.Safe.pretty_to_string (response_to_yojson response))
       | (TypeCheckResponse _) as response ->
           Log.print "%s\n" (Server.Protocol.show_response response)
       | response ->
           Log.error "Unexpected response %s from server\n" (Server.Protocol.show_response response)
     with
     | InvalidQuery ->
         Log.error "Unable to parse query \"%s\"" serialized;
         exit 1
     | Parser.Error error ->
         let error =
           String.split ~on:'\n' error
           |> (fun lines -> List.drop lines 1)
           |> String.concat ~sep:"\n"
         in
         Log.error "Unable to parse query:\n%s" error;
         exit 1)
  |> Scheduler.run_process ~configuration


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Queries the server for type order information."
    (empty
     +> anon ("query" %: string)
     +> anon (maybe_with_default "." ("source-root" %: string)))
    run_query
