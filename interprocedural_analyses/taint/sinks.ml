(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module T = struct
  type t =
    | Demo
    | FileSystem
    | GetAttr
    | Attach
    | LocalReturn (* Special marker to describe function in-out behavior *)
    | Logging
    | NamedSink of string
    | ParameterUpdate of int (* Special marker to describe side effect in-out behavior *)
    | RemoteCodeExecution
    | SQL
    | AddFeatureToArgument
    (* Special marker to designate modifying the state the parameter passed in. *)
    | Test
    | XMLParser
    | XSS
  [@@deriving compare, eq, sexp, show, hash]
end

include T

let _ = show (* unused but derived *)

let show = function
  | Demo -> "Demo"
  | FileSystem -> "FileSystem"
  | GetAttr -> "GetAttr"
  | Attach -> "Attach"
  | LocalReturn -> "LocalReturn"
  | Logging -> "Logging"
  | NamedSink name -> name
  | ParameterUpdate index -> Format.sprintf "ParameterUpdate%d" index
  | RemoteCodeExecution -> "RemoteCodeExecution"
  | SQL -> "SQL"
  | AddFeatureToArgument -> "AddFeatureToArgument"
  | Test -> "Test"
  | XMLParser -> "XMLParser"
  | XSS -> "XSS"


let create = function
  | "Demo" -> Demo
  | "FileSystem" -> FileSystem
  | "GetAttr" -> GetAttr
  | "LocalReturn" -> LocalReturn
  | "Logging" -> Logging
  | "RemoteCodeExecution" -> RemoteCodeExecution
  | "SQL" -> SQL
  | "AddFeatureToArgument" -> AddFeatureToArgument
  | "Test" -> Test
  | "XMLParser" -> XMLParser
  | "XSS" -> XSS
  | update when String.is_prefix update ~prefix:"ParameterUpdate" ->
      let index = String.chop_prefix_exn update ~prefix:"ParameterUpdate" in
      ParameterUpdate (Int.of_string index)
  | name -> failwith (Format.sprintf "Unsupported taint sink `%s`" name)


let parse ~allowed name =
  if List.mem allowed name ~equal:String.equal then
    NamedSink name
  else
    create name


let ignore_leaf_at_call = function
  | Attach -> true
  | _ -> false


module Set = Set.Make (struct
  include T
end)
