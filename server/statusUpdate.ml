(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

let broadcast_response = ref None

let initialize broadcast = broadcast_response := Some broadcast

let write ~state ~content ~message_type =
  match !broadcast_response with
  | Some broadcast ->
      LanguageServer.Protocol.ShowStatus.create
        ~content
        ~message_type
        ~progress:None
        ~short_message:None
      |> LanguageServer.Protocol.ShowStatus.to_yojson
      |> Yojson.Safe.to_string
      |> (fun response -> Protocol.LanguageServerProtocolResponse response)
      |> fun response -> broadcast state response
  | None -> Log.warning "Unable to update status - no broadcasting method set."


let information ~message =
  write ~content:message ~message_type:LanguageServer.Types.ShowMessageParameters.InfoMessage


let warning ~message =
  write ~content:message ~message_type:LanguageServer.Types.ShowMessageParameters.WarningMessage


let error ~message =
  write ~content:message ~message_type:LanguageServer.Types.ShowMessageParameters.ErrorMessage
