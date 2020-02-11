(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre

type rule = {
  sources: Sources.t list;
  sinks: Sinks.t list;
  code: int;
  name: string;
  message_format: string; (* format *)
}

type implicit_sinks = { conditional_test: Sinks.t list }

type t = {
  sources: string list;
  sinks: string list;
  features: string list;
  rules: rule list;
  implicit_sinks: implicit_sinks;
}

val empty : t

val get : unit -> t

exception
  MalformedConfiguration of {
    path: string;
    parse_error: string;
  }

val parse : string -> t

val register : t -> unit

val default : t

val create : rule_filter:int list option -> paths:Path.t list -> t

val conditional_test_sinks : unit -> Sinks.t list
