(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Network
open Pyre
open Server
open Protocol
open TypeQuery

let run_query serialized local_root () =
  let local_root = Path.create_absolute local_root in
  let configuration = Configuration.Analysis.create ~local_root () in
  (fun () ->
    if serialized = "help" then
      Log.print "%s" (Query.help ())
    else
      let response =
        try
          let query = Query.parse_query ~configuration serialized in
          let socket = Server.Operations.connect ~retries:3 ~configuration in
          Socket.write socket query;
          match Socket.read socket with
          | TypeQueryResponse response -> response_to_yojson response
          | TypeCheckResponse errors ->
              `Assoc
                [ ( "response",
                    `List
                      (List.map
                         ~f:(Analysis.Error.Instantiated.to_json ~show_error_traces:false)
                         errors) ) ]
          | response ->
              `Assoc
                [ ( "response",
                    `String
                      (Format.sprintf
                         "Unexpected response %s from server"
                         (Server.Protocol.show_response response)) ) ]
        with
        | Query.InvalidQuery reason ->
            Log.info "%s" (Query.help ());
            `Assoc
              [ ( "error",
                  `String (Format.sprintf "Unable to parse query \"%s\": %s." serialized reason) )
              ]
        | PyreParser.Parser.Error error ->
            Log.info "%s" (Query.help ());
            let error =
              String.split ~on:'\n' error
              |> (fun lines -> List.drop lines 1)
              |> String.concat ~sep:"\n"
            in
            `Assoc
              [ ( "error",
                  `String (Format.sprintf "Unable to parse query \"%s\": %s." serialized error) )
              ]
      in
      Yojson.Safe.to_string response |> Log.print "%s")
  |> Scheduler.run_process ~configuration


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Queries the server for type order information."
    (empty +> anon ("query" %: string) +> anon (maybe_with_default "." ("source-root" %: string)))
    run_query
