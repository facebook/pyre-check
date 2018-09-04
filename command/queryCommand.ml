(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
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
          Node.value = Expression.Access [
              Expression.Access.Identifier name;
              Expression.Access.Call { Node.value = arguments; _ };
            ];
          _;
        };
      _;
    }] ->
      let arguments = List.map ~f:(fun { Expression.Argument.value; _ } -> value) arguments in
      begin
        match String.lowercase (Identifier.show name), arguments with
        | "attributes", [class_name] ->
            Request.TypeQueryRequest (Attributes class_name)
        | "join", [left; right] ->
            Request.TypeQueryRequest (Join (left, right))
        | "less_or_equal", [left; right] ->
            Request.TypeQueryRequest (LessOrEqual (left, right))
        | "meet", [left; right] ->
            Request.TypeQueryRequest (Meet (left, right))
        | "methods", [class_name] ->
            Request.TypeQueryRequest (Methods class_name)
        | "normalizetype", [argument] ->
            Request.TypeQueryRequest (NormalizeType argument)
        | "signature", [{ Node.value = Expression.Access function_name; _ }] ->
            Request.TypeQueryRequest (Signature function_name)
        | "superclasses", [class_name] ->
            Request.TypeQueryRequest (Superclasses class_name)
        | "type", [argument] ->
            Request.TypeQueryRequest (Type argument)
        | "type_at_location",
          [
            { Node.value = Expression.Access path; _ };
            { Node.value = Expression.Integer line; _ };
            { Node.value = Expression.Integer column; _ };
          ] ->
            let location =
              let path = Expression.Access.show path in
              let position = { Location.line; column } in
              { Location.path; start = position; stop = position }
            in
            Request.TypeQueryRequest (TypeAtLocation location)
        | "typecheckpath", arguments ->
            let files =
              arguments
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
     let query = parse_query ~root:local_root serialized in
     let socket = Server.Operations.connect ~retries:3 ~configuration in
     Socket.write socket query;
     match Socket.read socket with
     | TypeQueryResponse response ->
         Log.print "%s\n" (Yojson.Safe.pretty_to_string (response_to_yojson response))
     | (TypeCheckResponse _) as response ->
         Log.print "%s\n" (Server.Protocol.show_response response)
     | response ->
         Log.error "Unexpected response %s from server\n" (Server.Protocol.show_response response))
  |> Scheduler.run_process ~configuration


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Queries the server for type order information."
    (empty
     +> anon ("query" %: string)
     +> anon (maybe_with_default "." ("source-root" %: string)))
    run_query
