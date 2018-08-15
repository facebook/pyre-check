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

module Scheduler = Service.Scheduler


let parse_query ~root query =
  let query =
    try
      Parser.parse [query]
    with _ ->
      []
  in
  match query with
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
            Some (Request.TypeQueryRequest (Server.Protocol.Attributes class_name))
        | "less_or_equal", [left; right] ->
            Some (Request.TypeQueryRequest (Server.Protocol.LessOrEqual (left, right)))
        | "meet", [left; right] ->
            Some (Request.TypeQueryRequest (Server.Protocol.Meet (left, right)))
        | "join", [left; right] ->
            Some (Request.TypeQueryRequest (Server.Protocol.Join (left, right)))
        | "typecheckpath", arguments ->
            let files =
              arguments
              |> List.map ~f:Expression.show
              |> List.map ~f:(fun relative -> Path.create_relative ~root ~relative)
              |> List.map ~f:File.create
            in
            Some (Request.TypeCheckRequest (TypeCheckRequest.create ~check:files ()))
        | "normalizetype", [argument] ->
            Some (Request.TypeQueryRequest (Server.Protocol.NormalizeType argument))
        | "superclasses", [class_name] ->
            Some (Request.TypeQueryRequest (Server.Protocol.Superclasses class_name))
        | "methods", [class_name] ->
            Some (Request.TypeQueryRequest (Server.Protocol.Methods class_name))
        | "type_at_location",
          [
            { Node.value = Expression.Access path; _ };
            { Node.value = Expression.Integer line; _ };
            { Node.value = Expression.Integer column; _ };
          ] ->
            let path = Expression.Access.show path in
            let position = { Location.line; column } in
            let location = { Location.path; start = position; stop = position } in
            Some (Request.TypeQueryRequest (Server.Protocol.TypeAtLocation location))
        | _ -> None
      end
  | _ -> None


let run_query serialized local_root () =
  let local_root = Path.create_absolute local_root in
  let configuration = Configuration.create ~local_root () in
  Scheduler.initialize_process ~configuration;

  let query = parse_query ~root:local_root serialized in
  begin
    match query with
    | Some _ ->
        ()
    | None ->
        Log.error "Could not parse query %s; exiting.\n" serialized;
        exit 1
  end;
  let query = Option.value_exn query in
  let socket = Server.Operations.connect ~retries:3 ~configuration in
  Socket.write socket query;
  match Socket.read socket with
  | Server.Protocol.TypeQueryResponse serialized ->
      Log.print "%s" serialized
  | (TypeCheckResponse _) as response ->
      Log.print "%s" (show_response response)
  | response ->
      Log.error "Unexpected response %s from server" (Server.Protocol.show_response response)


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Queries the server for type order information."
    (empty
     +> anon ("query" %: string)
     +> anon (maybe_with_default "." ("source-root" %: string)))
    run_query
