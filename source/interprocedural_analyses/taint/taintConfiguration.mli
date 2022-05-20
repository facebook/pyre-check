(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module Rule : sig
  type t = {
    sources: Sources.t list;
    sinks: Sinks.t list;
    transforms: TaintTransform.t list;
    code: int;
    name: string;
    message_format: string; (* format *)
  }
  [@@deriving compare, show]
end

type literal_string_sink = {
  pattern: Re2.t;
  sink_kind: Sinks.t;
}

type implicit_sinks = {
  conditional_test: Sinks.t list;
  literal_string_sinks: literal_string_sink list;
}

type literal_string_source = {
  pattern: Re2.t;
  source_kind: Sources.t;
}

type implicit_sources = { literal_strings: literal_string_source list }

type analysis_model_constraints = {
  maximum_model_width: int;
  maximum_return_access_path_length: int;
  maximum_overrides_to_analyze: int option;
  maximum_trace_length: int option;
  maximum_tito_depth: int option;
}

type partial_sink_converter = (Sources.t list * Sinks.t) list String.Map.Tree.t

type missing_flows_kind =
  | Obscure
  | Type
[@@deriving compare, show]

val missing_flows_kind_from_string : string -> missing_flows_kind option

val missing_flows_kind_to_string : missing_flows_kind -> string

type t = {
  sources: AnnotationParser.source_or_sink list;
  sinks: AnnotationParser.source_or_sink list;
  transforms: TaintTransform.t list;
  features: string list;
  rules: Rule.t list;
  implicit_sinks: implicit_sinks;
  implicit_sources: implicit_sources;
  partial_sink_converter: partial_sink_converter;
  partial_sink_labels: string list Core.String.Map.Tree.t;
  matching_sources: Sources.Set.t Sinks.Map.t;
  matching_sinks: Sinks.Set.t Sources.Map.t;
  possible_tito_transforms: TaintTransforms.Set.t;
  find_missing_flows: missing_flows_kind option;
  dump_model_query_results_path: PyrePath.t option;
  analysis_model_constraints: analysis_model_constraints;
  lineage_analysis: bool;
}

val empty : t

val get : unit -> t

module Error : sig
  type kind =
    | FileNotFound
    | FileRead
    | InvalidJson of string
    | NoConfigurationFound
    | UnexpectedJsonType of {
        json: Yojson.Safe.t;
        message: string;
        section: string option;
      }
    | MissingKey of {
        key: string;
        section: string;
      }
    | UnknownKey of {
        key: string;
        section: string;
      }
    | UnsupportedSource of string
    | UnsupportedSink of string
    | UnsupportedTransform of string
    | UnexpectedCombinedSourceRule of Yojson.Safe.t
    | PartialSinkDuplicate of string
    | InvalidLabelMultiSink of {
        label: string;
        sink: string;
        labels: string list;
      }
    | InvalidMultiSink of string
    | RuleCodeDuplicate of int
    | OptionDuplicate of string
    | SourceDuplicate of string
    | SinkDuplicate of string
    | TransformDuplicate of string
    | FeatureDuplicate of string
  [@@deriving equal, show]

  type t = {
    kind: kind;
    path: PyrePath.t option;
  }
  [@@deriving equal, show]

  val create : path:PyrePath.t -> kind:kind -> t

  val to_json : t -> Yojson.Safe.t
end

val parse : (PyrePath.t * Yojson.Safe.t) list -> (t, Error.t list) Result.t

val register : t -> unit

val default : t

val create
  :  rule_filter:int list option ->
  find_missing_flows:missing_flows_kind option ->
  dump_model_query_results_path:PyrePath.t option ->
  maximum_trace_length:int option ->
  maximum_tito_depth:int option ->
  taint_model_paths:PyrePath.t list ->
  (t, Error.t list) Result.t

val validate : t -> (t, Error.t list) Result.t

exception TaintConfigurationError of Error.t list

val exception_on_error : (t, Error.t list) Result.t -> t

val apply_missing_flows : t -> missing_flows_kind -> t

val conditional_test_sinks : unit -> Sinks.t list

val literal_string_sinks : unit -> literal_string_sink list

val literal_string_sources : unit -> literal_string_source list

val get_triggered_sink : partial_sink:Sinks.partial_sink -> source:Sources.t -> Sinks.t option

val is_missing_flow_analysis : missing_flows_kind -> bool

val get_maximum_model_width : unit -> int

val get_maximum_overrides_to_analyze : unit -> int option

val maximum_return_access_path_width : int

val maximum_return_access_path_depth : int

val maximum_tito_positions : int

val maximum_tree_depth_after_widening : int

val maximum_tito_leaves : int

val transform_splits : 'a list -> ('a list * 'a list) list
