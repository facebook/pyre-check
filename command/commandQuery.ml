(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre
open PyreParser
open ServerProtocol


module Socket = CommandSocket
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
        | "less_or_equal", [left; right] ->
            Some (Request.TypeQueryRequest (ServerProtocol.LessOrEqual (left, right)))
        | "meet", [left; right] ->
            Some (Request.TypeQueryRequest (ServerProtocol.Meet (left, right)))
        | "join", [left; right] ->
            Some (Request.TypeQueryRequest (ServerProtocol.Join (left, right)))
        | "typecheckpath", arguments ->
            let files =
              arguments
              |> List.map ~f:Expression.show
              |> List.map ~f:(fun relative -> Path.create_relative ~root ~relative)
              |> List.map ~f:File.create
            in
            Some (Request.TypeCheckRequest (TypeCheckRequest.create ~check:files ()))
        | "normalizetype", [argument] ->
            Some (Request.TypeQueryRequest (ServerProtocol.NormalizeType argument))
        | "superclasses", [class_name] ->
            Some (Request.TypeQueryRequest (ServerProtocol.Superclasses class_name))
        | _ -> None
      end
  | _ -> None


let run_query serialized source_root () =
  let source_root = Path.create_absolute source_root in
  let configuration = Configuration.create ~source_root () in
  Scheduler.initialize_process ~configuration;

  let query = parse_query ~root:source_root serialized in
  begin
    match query with
    | Some _ ->
        ()
    | None ->
        Log.error "Could not parse query %s; exiting.\n" serialized;
        exit 1
  end;
  let query = Option.value_exn query in
  let socket = ServerOperations.connect ~retries:3 ~configuration in
  Socket.write socket query;
  match Socket.read socket with
  | ServerProtocol.TypeQueryResponse serialized ->
      Log.print "%s" serialized
  | (TypeCheckResponse _) as response ->
      Log.print "%s" (show_response response)
  | response ->
      Log.error "Unexpected response %s from server" (ServerProtocol.show_response response)


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Queries the server for type order information."
    (empty
     +> anon ("query" %: string)
     +> anon (maybe_with_default "." ("source-root" %: string)))
    run_query
