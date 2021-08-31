(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let handshake_message version =
  let open LanguageServer.Types in
  {
    HandshakeServer.jsonrpc = "2.0";
    method_ = "handshake/server";
    parameters = Some { HandshakeServerParameters.version };
  }


let socket_added_message =
  `Assoc ["jsonrpc", `String "2.0"; "method", `String "handshake/socket_added"; "parameters", `Null]


module Response = struct
  module TypeErrors = struct
    let to_json errors =
      let errors =
        `Assoc ["errors", `List (List.map errors ~f:Analysis.AnalysisError.Instantiated.to_yojson)]
      in
      `Assoc ["jsonrpc", `String "2.0"; "error", `Null; "result", errors]
  end

  module Stop = struct
    let to_json () =
      `Assoc
        [
          "jsonrpc", `String "2.0";
          "error", `Null;
          "result", `String "Server stopped, polling for deletion of socket.";
        ]
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
    | "updateFiles" -> Some Protocol.Request.FileNotifier
    | _ -> Some (Protocol.Request.JSONSocket socket)


  let format_request request =
    match request_method request with
    | "typeQuery" -> (
        let query = query_parameter request in
        match Query.parse_request query with
        | Result.Ok request -> Protocol.Request.TypeQueryRequest request
        | Result.Error reason -> Protocol.Request.UnparsableQuery { query; reason })
    | "stop" -> Protocol.Request.StopRequest
    | _ ->
        request
        |> Yojson.Safe.to_string
        |> fun request -> Protocol.Request.LanguageServerProtocolRequest request
end
