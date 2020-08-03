(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module Rule : sig
  type t = {
    sources: Sources.t list;
    sinks: Sinks.t list;
    code: int;
    name: string;
    message_format: string; (* format *)
  }
  [@@deriving eq, show]
end

type implicit_sinks = { conditional_test: Sinks.t list }

type analysis_model_constraints = {
  maximum_model_width: int;
  maximum_complex_access_path_length: int;
}

type partial_sink_converter = (Sources.t * Sinks.t) list String.Map.Tree.t

val analysis_model_constraints : analysis_model_constraints

type t = {
  sources: string list;
  sinks: string list;
  features: string list;
  rules: Rule.t list;
  implicit_sinks: implicit_sinks;
  partial_sink_converter: partial_sink_converter;
  acceptable_sink_labels: string list Core.String.Map.Tree.t;
  find_obscure_flows: bool;
}

val empty : t

val get : unit -> t

exception
  MalformedConfiguration of {
    path: string;
    parse_error: string;
  }

val parse : Yojson.Safe.t list -> t

val register : t -> unit

val default : t

val create : rule_filter:int list option -> find_obscure_flows:bool -> paths:Path.t list -> t

val validate : t -> unit

val conditional_test_sinks : unit -> Sinks.t list

val get_triggered_sink : partial_sink:Sinks.partial_sink -> source:Sources.t -> Sinks.t option
