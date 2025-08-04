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

(* A map from partial sink kinds to other partial sinks that "match" them. Two partial sinks match
   only if both appear in a multi-source rule. *)
module RegisteredPartialSinks : sig
  type t [@@deriving compare, show, equal]

  val empty : t

  type registration_result =
    | Yes
    | No of Sinks.PartialSink.Set.t (* The set of registered labels *)

  val is_registered : partial_sink:Sinks.PartialSink.t -> t -> registration_result

  val find_matches : Sinks.PartialSink.t -> t -> Sinks.PartialSink.Set.t option

  (* For test purpose only. *)
  val of_alist_exn : (string * string list) list -> t
end

(* A map from partial sinks, to the matching sources and the corresponding triggered sinks for each
   match. *)
module PartialSinkConverter : sig
  type t

  val empty : t

  val get_triggered_sinks_if_matched
    :  partial_sink:Sinks.PartialSink.t ->
    source:Sources.t ->
    t ->
    Sinks.PartialSink.Triggered.Set.t

  (* For test purpose only. *)
  val add
    :  first_sources:Sources.t list ->
    first_sink:Sinks.PartialSink.t ->
    second_sources:Sources.t list ->
    second_sink:Sinks.PartialSink.t ->
    t ->
    t

  val merge : t -> t -> t
end

module StringOperationPartialSinks : sig
  type t [@@deriving show]

  val equal : t -> t -> bool

  val singleton : Sinks.PartialSink.t -> t

  val of_list : Sinks.PartialSink.t list -> t

  val get_partial_sinks : t -> Sinks.t list
end

module PartialFlow : sig
  type t = {
    full_issue_code: int;
    partial_issue_code: int;
    full_issue_transform: TaintTransform.t;
    is_prefix_flow: bool;
    feature: string;
  }
end

(** Taint configuration, stored in the ocaml heap. *)
module Heap : sig
  type t = {
    sources: AnnotationParser.KindDefinition.t list;
    sinks: AnnotationParser.KindDefinition.t list;
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
    registered_partial_sinks: RegisteredPartialSinks.t;
    string_combine_partial_sinks: StringOperationPartialSinks.t;
    find_missing_flows: Configuration.MissingFlowKind.t option;
    dump_model_query_results_path: PyrePath.t option;
    infer_self_tito: bool;
    infer_argument_tito: bool;
    analysis_model_constraints: ModelConstraints.t;
    source_sink_filter: SourceSinkFilter.t;
    partial_flows: PartialFlow.t list;
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
        json: JsonParsing.JsonAst.Json.t;
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
    | UnexpectedCombinedSourceRule of JsonParsing.JsonAst.Json.t
    | UnexpectedStringCombineRule of JsonParsing.JsonAst.Json.t
    | InvalidMultiSink of {
        sink: string;
        registered: Sinks.PartialSink.Set.t;
      }
    | RuleCodeDuplicate of {
        code: int;
        previous_location: JsonParsing.JsonAst.LocationWithPath.t option;
      }
    | OptionDuplicate of string
    | SourceDuplicate of {
        name: string;
        previous_location: JsonParsing.JsonAst.LocationWithPath.t option;
      }
    | SinkDuplicate of {
        name: string;
        previous_location: JsonParsing.JsonAst.LocationWithPath.t option;
      }
    | TransformDuplicate of string
    | FeatureDuplicate of string
    | InvalidRegex of {
        regex: string;
        reason: string;
      }
  [@@deriving equal, show]

  type t = {
    kind: kind;
    path: PyrePath.t option;
    location: JsonParsing.JsonAst.Location.t option;
  }
  [@@deriving equal, show]

  val create : path:PyrePath.t -> kind:kind -> t

  val create_with_location
    :  path:PyrePath.t ->
    kind:kind ->
    location:JsonParsing.JsonAst.Location.t ->
    t

  val to_json : t -> Yojson.Safe.t
end

(** Parse json files to create a taint configuration. *)
val from_json_list
  :  (PyrePath.t * JsonParsing.JsonAst.Json.t) list ->
  (Heap.t, Error.t list) Result.t

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
  infer_self_tito:bool ->
  infer_argument_tito:bool ->
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
