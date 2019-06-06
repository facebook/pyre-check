(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Network

let socket = ref None

let initialize new_socket = socket := Some new_socket

let write ~content ~message_type =
  match !socket with
  | Some socket -> (
    try
      Socket.write
        socket
        ( LanguageServer.Protocol.ShowStatus.create
            ~content
            ~message_type
            ~progress:None
            ~short_message:None
        |> LanguageServer.Protocol.ShowStatus.to_yojson
        |> Yojson.Safe.to_string
        |> fun response -> Protocol.LanguageServerProtocolResponse response )
    with
    | Unix.Unix_error (Unix.EPIPE, _, _) ->
        Log.warning "Got an EPIPE while broadcasting to a persistent client"
    | Unix.Unix_error _ -> Log.warning "Unix error" )
  | _ -> Log.warning "Unable to update status - no socket set."


let information ~message =
  write ~content:message ~message_type:LanguageServer.Types.ShowMessageParameters.InfoMessage


let warning ~message =
  write ~content:message ~message_type:LanguageServer.Types.ShowMessageParameters.WarningMessage


let error ~message =
  write ~content:message ~message_type:LanguageServer.Types.ShowMessageParameters.ErrorMessage
