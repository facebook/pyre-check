(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t =
  | Demo
  | GetAttr
  | IdentityCreation
  | LocalReturn  (* Special marker to infer function in-out behavior *)
  | Logging
  | RemoteCodeExecution
  | RequestSend
  | SQL
  | Test
  | Thrift
  | XMLParser
  | XSS
  | FileSystem
[@@deriving compare, eq, sexp, show, hash]


let show = function
  | Demo -> "Demo"
  | GetAttr -> "GetAttr"
  | IdentityCreation -> "IdentityCreation"
  | LocalReturn -> "LocalReturn"
  | Logging -> "Logging"
  | RemoteCodeExecution -> "RemoteCodeExecution"
  | RequestSend -> "RequestSend"
  | SQL -> "SQL"
  | Thrift -> "Thrift"
  | Test -> "Test"
  | XMLParser -> "XMLParser"
  | XSS -> "XSS"
  | FileSystem -> "FileSystem"


let create = function
  | "Demo" -> Demo
  | "GetAttr" -> GetAttr
  | "IdentityCreation" -> IdentityCreation
  | "LocalReturn" -> LocalReturn
  | "Logging" -> Logging
  | "RemoteCodeExecution" -> RemoteCodeExecution
  | "RequestSend" -> RequestSend
  | "SQL" -> SQL
  | "Test" -> Test
  | "Thrift" -> Thrift
  | "XMLParser" -> XMLParser
  | "XSS" -> XSS
  | "FileSystem" -> FileSystem
  | name -> failwith (Format.sprintf "Unsupported taint sink %s" name)
