(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type partial_sink = {
  kind: string;
  label: string;
}
[@@deriving compare, eq, sexp, show, hash]

type t =
  | Attach
  | PartialSink of partial_sink
  | TriggeredPartialSink of partial_sink
  | LocalReturn (* Special marker to describe function in-out behavior *)
  | NamedSink of string
  | ParameterUpdate of int (* Special marker to describe function in-out behavior *)
  | AddFeatureToArgument
[@@deriving compare, eq, sexp, show, hash]

val name : string

val parse : allowed:string list -> string -> t

val ignore_leaf_at_call : t -> bool

module Set : Set.S with type Elt.t = t
