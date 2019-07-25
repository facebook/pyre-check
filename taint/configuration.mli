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

type t = {
  sources: string list;
  sinks: string list;
  features: string list;
  rules: rule list;
}

val get : unit -> t

val parse : string -> t

val register : t -> unit

val default : t

val create : directories:Path.t list -> t
