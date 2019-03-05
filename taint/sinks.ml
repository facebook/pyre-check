(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t =
  | Demo
  | FileSystem
  | GetAttr
  | IdentityCreation
  | LocalReturn  (* Special marker to infer function in-out behavior *)
  | Logging
  | ODS
  | RemoteCodeExecution
  | RequestSend
  | SQL
  | Test
  | Thrift
  | XMLParser
  | XSS
[@@deriving compare, eq, sexp, show, hash]


let show = function
  | Demo -> "Demo"
  | FileSystem -> "FileSystem"
  | GetAttr -> "GetAttr"
  | IdentityCreation -> "IdentityCreation"
  | LocalReturn -> "LocalReturn"
  | Logging -> "Logging"
  | ODS -> "ODS"
  | RemoteCodeExecution -> "RemoteCodeExecution"
  | RequestSend -> "RequestSend"
  | SQL -> "SQL"
  | Thrift -> "Thrift"
  | Test -> "Test"
  | XMLParser -> "XMLParser"
  | XSS -> "XSS"


let create = function
  | "Demo" -> Demo
  | "FileSystem" -> FileSystem
  | "GetAttr" -> GetAttr
  | "IdentityCreation" -> IdentityCreation
  | "LocalReturn" -> LocalReturn
  | "Logging" -> Logging
  | "ODS" -> ODS
  | "RemoteCodeExecution" -> RemoteCodeExecution
  | "RequestSend" -> RequestSend
  | "SQL" -> SQL
  | "Test" -> Test
  | "Thrift" -> Thrift
  | "XMLParser" -> XMLParser
  | "XSS" -> XSS
  | name -> failwith (Format.sprintf "Unsupported taint sink %s" name)
