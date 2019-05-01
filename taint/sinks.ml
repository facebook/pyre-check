(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type t =
  | Demo
  | FileSystem
  | GetAttr
  | IdentityCreation
  | LocalReturn  (* Special marker to describe function in-out behavior *)
  | Logging
  | NamedSink of string
  | ODS
  | ParameterUpdate of int  (* Special marker to describe side effect in-out behavior *)
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
  | NamedSink name -> name
  | ODS -> "ODS"
  | ParameterUpdate index -> Format.sprintf "ParameterUpdate%d" index
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
  | update when String.is_prefix update ~prefix:"ParameterUpdate" ->
      let index = String.chop_prefix_exn update ~prefix:"ParameterUpdate" in
      ParameterUpdate (Int.of_string index)
  | name ->
      failwith (Format.sprintf "Unsupported taint sink `%s`" name)


let parse ~allowed name =
  if List.mem allowed name ~equal:String.equal then
    NamedSink name
  else
    create name
