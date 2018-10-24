(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t =
  | Demo
  | GetAttr
  | LocalReturn  (* Special marker to infer function in-out behavior *)
  | Logging
  | RemoteCodeExecution
  | SQL
  | Test
  | Thrift
  | XMLParser
  | XSS
[@@deriving compare, eq, sexp, show, hash]


let show = function
  | Demo -> "Demo"
  | GetAttr -> "GetAttr"
  | LocalReturn -> "LocalReturn"
  | Logging -> "Logging"
  | RemoteCodeExecution -> "RemoteCodeExecution"
  | SQL -> "SQL"
  | Thrift -> "Thrift"
  | Test -> "Test"
  | XMLParser -> "XMLParser"
  | XSS -> "XSS"


let create = function
  | "Demo" -> Demo
  | "GetAttr" -> GetAttr
  | "LocalReturn" -> LocalReturn
  | "Logging" -> Logging
  | "RemoteCodeExecution" -> RemoteCodeExecution
  | "SQL" -> SQL
  | "Test" -> Test
  | "Thrift" -> Thrift
  | "XMLParser" -> XMLParser
  | "XSS" -> XSS
  | name -> failwith (Format.sprintf "Unsupported taint sink %s" name)
