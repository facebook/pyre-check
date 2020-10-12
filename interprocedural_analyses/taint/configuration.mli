(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

type literal_string_source = {
  pattern: Re2.t;
  kind: Sources.t;
}

type implicit_sources = { literal_strings: literal_string_source list }

type analysis_model_constraints = {
  maximum_model_width: int;
  maximum_complex_access_path_length: int;
  maximum_overrides_to_analyze: int option;
}

type partial_sink_converter = (Sources.t list * Sinks.t) list String.Map.Tree.t

type missing_flows_kind =
  | Obscure
  | Type
[@@deriving eq, show]

val missing_flows_kind_from_string : string -> missing_flows_kind option

type t = {
  sources: string list;
  sinks: string list;
  features: string list;
  rules: Rule.t list;
  implicit_sinks: implicit_sinks;
  implicit_sources: implicit_sources;
  partial_sink_converter: partial_sink_converter;
  partial_sink_labels: string list Core.String.Map.Tree.t;
  find_missing_flows: missing_flows_kind option;
  dump_model_query_results: bool;
  analysis_model_constraints: analysis_model_constraints;
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

val create
  :  rule_filter:int list option ->
  find_missing_flows:missing_flows_kind option ->
  dump_model_query_results:bool ->
  paths:Path.t list ->
  t

val validate : t -> unit

val conditional_test_sinks : unit -> Sinks.t list

val get_triggered_sink : partial_sink:Sinks.partial_sink -> source:Sources.t -> Sinks.t option

val is_missing_flow_analysis : missing_flows_kind -> bool

val get_maximum_model_width : unit -> int

val maximum_return_access_path_width : int

val maximum_return_access_path_depth : int
