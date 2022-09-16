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
  maximum_overrides_to_analyze: int option;
  maximum_trace_length: int option;
  maximum_tito_depth: int option;
}

type partial_sink_converter = (Sources.t list * Sinks.t) list String.Map.Tree.t

module IntSet : Stdlib.Set.S with type elt = int

module SourceSinkFilter : sig
  type t

  val create
    :  rules:Rule.t list ->
    filtered_rule_codes:IntSet.t option ->
    filtered_sources:Sources.Set.t option ->
    filtered_sinks:Sinks.Set.t option ->
    filtered_transforms:TaintTransform.t list option ->
    t

  val should_keep_source : t -> Sources.t -> bool

  val should_keep_sink : t -> Sinks.t -> bool

  (* Exposed for testing purpose *)
  val matching_sources : t -> Sources.Set.t Sinks.Map.t

  (* Exposed for testing purpose *)
  val matching_sinks : t -> Sinks.Set.t Sources.Map.t

  (* Exposed for testing purpose *)
  val possible_tito_transforms : t -> TaintTransforms.Set.t
end

type t = {
  sources: AnnotationParser.source_or_sink list;
  sinks: AnnotationParser.source_or_sink list;
  transforms: TaintTransform.t list;
  filtered_sources: Sources.Set.t option;
  filtered_sinks: Sinks.Set.t option;
  filtered_transforms: TaintTransform.t list option;
  features: string list;
  rules: Rule.t list;
  filtered_rule_codes: IntSet.t option;
  implicit_sinks: implicit_sinks;
  implicit_sources: implicit_sources;
  partial_sink_converter: partial_sink_converter;
  partial_sink_labels: string list Core.String.Map.Tree.t;
  find_missing_flows: Configuration.MissingFlowKind.t option;
  dump_model_query_results_path: PyrePath.t option;
  analysis_model_constraints: analysis_model_constraints;
  lineage_analysis: bool;
  source_sink_filter: SourceSinkFilter.t option;
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

(** Parse json files to create a taint configuration. *)
val from_json_list : (PyrePath.t * Yojson.Safe.t) list -> (t, Error.t list) Result.t

val register : t -> unit

val default : t

(** Create a taint configuration by finding `.config` files in the given directories. *)
val from_taint_model_paths : PyrePath.t list -> (t, Error.t list) Result.t

(** Update a taint configuration with the given command line options. *)
val with_command_line_options
  :  t ->
  rule_filter:int list option ->
  source_filter:string list option ->
  sink_filter:string list option ->
  transform_filter:string list option ->
  find_missing_flows:Configuration.MissingFlowKind.t option ->
  dump_model_query_results_path:PyrePath.t option ->
  maximum_trace_length:int option ->
  maximum_tito_depth:int option ->
  (t, Error.t list) Result.t

(** Perform additional checks on the taint configuration. *)
val validate : t -> (t, Error.t list) Result.t

exception TaintConfigurationError of Error.t list

val exception_on_error : (t, Error.t list) Result.t -> t

val apply_missing_flows : t -> Configuration.MissingFlowKind.t -> t

val source_can_match_rule : t -> Sources.t -> bool

val sink_can_match_rule : t -> Sinks.t -> bool

val code_metadata : unit -> Yojson.Safe.t

val conditional_test_sinks : unit -> Sinks.t list

val literal_string_sinks : unit -> literal_string_sink list

val literal_string_sources : unit -> literal_string_source list

val get_triggered_sink : partial_sink:Sinks.partial_sink -> source:Sources.t -> Sinks.t option

val is_missing_flow_analysis : Configuration.MissingFlowKind.t -> bool

val transform_splits : 'a list -> ('a list * 'a list) list

val get_maximum_overrides_to_analyze : unit -> int option

val runtime_check_invariants : unit -> bool

val maximum_model_width : int

val maximum_return_access_path_width : int

val maximum_return_access_path_depth : int

val maximum_return_access_path_length : int

val maximum_tito_positions : int

val maximum_tree_depth_after_widening : int

val maximum_tito_leaves : int
