(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

let handshake_message version =
  let open LanguageServer.Types in
  {
    HandshakeServer.jsonrpc = "2.0";
    method_ = "handshake/server";
    parameters = Some { HandshakeServerParameters.version };
  }


module Response = struct
  module TypeErrors = struct
    let to_json errors =
      let errors =
        `Assoc
          [
            ( "errors",
              `List
                (List.map errors ~f:(Analysis.Error.Instantiated.to_json ~show_error_traces:false))
            );
          ]
      in
      `Assoc ["jsonrpc", `String "2.0"; "error", `Null; "result", errors]
  end
end

module Request = struct
  let request_method request =
    Yojson.Safe.Util.member "method" request |> Yojson.Safe.Util.to_string


  let parameters request = request |> Yojson.Safe.Util.member "params"

  let query_parameter request =
    parameters request
    |> Yojson.Safe.Util.member "query"
    |> Yojson.Safe.to_string
    |> String.strip ~drop:(fun character -> Char.equal character '\"')


  let origin ~socket request =
    match request_method request with
    | "displayTypeErrors"
    | "typeQuery" ->
        Some (Protocol.Request.JSONSocket socket)
    | "updateFiles" -> Some Protocol.Request.FileNotifier
    | _ -> None


  let format_request ~configuration request =
    match request_method request with
    | "typeQuery" -> query_parameter request |> Query.parse_query ~configuration
    | _ ->
        request
        |> Yojson.Safe.to_string
        |> fun request -> Protocol.Request.LanguageServerProtocolRequest request
end
