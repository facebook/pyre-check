(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type t =
  | Demo
  | FileSystem
  | GetAttr
  | Attach
  | LocalReturn (* Special marker to describe function in-out behavior *)
  | Logging
  | NamedSink of string
  | ParameterUpdate of int (* Special marker to describe function in-out behavior *)
  | RemoteCodeExecution
  | SQL
  | Test
  | XMLParser
  | XSS
[@@deriving compare, eq, sexp, show, hash]

val parse : allowed:string list -> string -> t

val ignore_leaf_at_call : t -> bool
