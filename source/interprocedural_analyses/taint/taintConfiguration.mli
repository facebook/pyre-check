(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

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

module ModelConstraints : sig
  type t = {
    maximum_model_source_tree_width: int;
    maximum_model_sink_tree_width: int;
    maximum_model_tito_tree_width: int;
    maximum_tree_depth_after_widening: int;
    maximum_return_access_path_width: int;
    maximum_return_access_path_depth_after_widening: int;
    maximum_tito_collapse_depth: int;
    maximum_tito_positions: int;
    maximum_overrides_to_analyze: int option;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
  }

  val default : t
end

(* A map from partial sink kinds to the related labels *)
module PartialSinkLabelsMap : sig
  type labels = {
    main: string;
    secondary: string;
  }

  type t

  val find_opt : string -> t -> labels option

  val of_alist_exn : (string * labels) list -> t

  val to_alist : t -> (string * labels) list
end

module PartialSinkConverter : sig
  type t
end

module StringOperationPartialSinks : sig
  type t

  val equal : t -> t -> bool

  val singleton : string -> t

  val get_partial_sinks : t -> Sinks.t list
end

(** Taint configuration, stored in the ocaml heap. *)
module Heap : sig
  type t = {
    sources: AnnotationParser.source_or_sink list;
    sinks: AnnotationParser.source_or_sink list;
    transforms: TaintTransform.t list;
    filtered_sources: Sources.Set.t option;
    filtered_sinks: Sinks.Set.t option;
    filtered_transforms: TaintTransform.t list option;
    features: string list;
    rules: Rule.t list;
    filtered_rule_codes: Rule.CodeSet.t option;
    implicit_sinks: implicit_sinks;
    implicit_sources: implicit_sources;
    partial_sink_converter: PartialSinkConverter.t;
    partial_sink_labels: PartialSinkLabelsMap.t;
    string_combine_partial_sinks: StringOperationPartialSinks.t;
    find_missing_flows: Configuration.MissingFlowKind.t option;
    dump_model_query_results_path: PyrePath.t option;
    analysis_model_constraints: ModelConstraints.t;
    lineage_analysis: bool;
    source_sink_filter: SourceSinkFilter.t;
  }

  val empty : t

  val default : t
end

(** Taint configuration, stored in shared memory. *)
module SharedMemory : sig
  type t

  val from_heap : Heap.t -> t

  val get : t -> Heap.t

  (* Get the current registered taint configuration.
   * Prefer to use `get` whenever possible. *)
  val get_global : unit -> Heap.t option
end

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
val from_json_list : (PyrePath.t * Yojson.Safe.t) list -> (Heap.t, Error.t list) Result.t

(** Create a taint configuration by finding `.config` files in the given directories. *)
val from_taint_model_paths : PyrePath.t list -> (Heap.t, Error.t list) Result.t

(** Update a taint configuration with the given command line options. *)
val with_command_line_options
  :  Heap.t ->
  rule_filter:int list option ->
  source_filter:string list option ->
  sink_filter:string list option ->
  transform_filter:string list option ->
  find_missing_flows:Configuration.MissingFlowKind.t option ->
  dump_model_query_results_path:PyrePath.t option ->
  maximum_model_source_tree_width:int option ->
  maximum_model_sink_tree_width:int option ->
  maximum_model_tito_tree_width:int option ->
  maximum_tree_depth_after_widening:int option ->
  maximum_return_access_path_width:int option ->
  maximum_return_access_path_depth_after_widening:int option ->
  maximum_tito_collapse_depth:int option ->
  maximum_tito_positions:int option ->
  maximum_overrides_to_analyze:int option ->
  maximum_trace_length:int option ->
  maximum_tito_depth:int option ->
  (Heap.t, Error.t list) Result.t

(** Perform additional checks on the taint configuration. *)
val validate : Heap.t -> (Heap.t, Error.t list) Result.t

exception TaintConfigurationError of Error.t list

val exception_on_error : (Heap.t, Error.t list) Result.t -> Heap.t

val apply_missing_flows : Heap.t -> Configuration.MissingFlowKind.t -> Heap.t

val code_metadata : Heap.t -> Yojson.Safe.t

val conditional_test_sinks : Heap.t -> Sinks.t list

val literal_string_sinks : Heap.t -> literal_string_sink list

val literal_string_sources : Heap.t -> literal_string_source list

val get_triggered_sink
  :  Heap.t ->
  partial_sink:Sinks.partial_sink ->
  source:Sources.t ->
  Sinks.t option

val is_missing_flow_analysis : Heap.t -> Configuration.MissingFlowKind.t -> bool

val maximum_model_source_tree_width : Heap.t -> int

val maximum_model_sink_tree_width : Heap.t -> int

val maximum_model_tito_tree_width : Heap.t -> int

val maximum_tree_depth_after_widening : Heap.t -> int

val maximum_return_access_path_width : Heap.t -> int

val maximum_return_access_path_depth_after_widening : Heap.t -> int

val maximum_tito_positions : Heap.t -> int

val maximum_overrides_to_analyze : Heap.t -> int option

val maximum_trace_length : Heap.t -> int option

val maximum_tito_depth : Heap.t -> int option

val runtime_check_invariants : unit -> bool
