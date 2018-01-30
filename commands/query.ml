(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

module Socket = PyreSocket


let run_query query_kind left right source_root () =
  Log.initialize ~verbose:false ~sections:[];
  let parse_type serialized =
    match PythonParse.parse [serialized] with
    | [{ Ast.Node.value = Ast.Statement.Expression expression; _ }] ->
        Analysis.Type.create ~aliases:(fun _ -> None) expression
    | _ ->
        Log.error "Could not parse type %s; exiting." serialized;
        exit 1
  in
  let left = parse_type left in
  let right = parse_type right in
  let query =
    match query_kind with
    | "less_or_equal" -> Protocol.LessOrEqual (left, right)
    | "join" -> Protocol.Join (left, right)
    | "meet" -> Protocol.Meet (left, right)
    | _ ->
        Log.error "%s"
          ("Could not parse query %s; exiting.\n" ^
           "Query must be one of less_or_equal, join and meet.");
        exit 1
  in
  let configuration = Configuration.create ~source_root:(Path.create_absolute source_root) () in
  let socket = Server.connect ~retries:3 ~configuration in
  Socket.write socket (Protocol.Request.TypeQueryRequest query);
  match Socket.read socket with
  | Protocol.TypeQueryResponse serialized ->
      Log.print "%s" serialized
  | response ->
      Log.error "Unexpected response %s from server" (Protocol.show_response response)


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Queries the server for type order information."
    (empty
     +> anon ("command" %: string)
     +> anon ("left" %: string)
     +> anon ("right" %: string)
     +> anon (maybe_with_default "." ("source-root" %: string)))
    run_query
