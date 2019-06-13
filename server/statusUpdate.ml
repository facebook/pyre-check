(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Connections = Connections.Unix

let write ~state:{ State.connections; _ } ~content ~message_type ~short_message =
  LanguageServer.Protocol.ShowStatus.create ~content ~message_type ~progress:None ~short_message
  |> LanguageServer.Protocol.ShowStatus.to_yojson
  |> Yojson.Safe.to_string
  |> (fun response -> Protocol.LanguageServerProtocolResponse response)
  |> fun response -> Connections.broadcast_response ~connections ~response


let information ~message ~short_message ~state =
  write
    ~short_message
    ~content:message
    ~message_type:LanguageServer.Types.ShowMessageParameters.InfoMessage
    ~state


let warning ~message ~short_message ~state =
  write
    ~short_message
    ~content:message
    ~message_type:LanguageServer.Types.ShowMessageParameters.WarningMessage
    ~state


let error ~message ~short_message ~state =
  write
    ~short_message
    ~content:message
    ~message_type:LanguageServer.Types.ShowMessageParameters.ErrorMessage
    ~state
