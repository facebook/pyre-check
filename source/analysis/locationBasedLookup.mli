(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type resolved_type_lookup

val create_of_module : TypeEnvironment.ReadOnly.t -> Reference.t -> resolved_type_lookup

val get_resolved_type
  :  resolved_type_lookup ->
  position:Location.position ->
  (Location.t * Type.t) option

val get_all_resolved_types : resolved_type_lookup -> (Location.t * Type.t) list

type symbol_with_definition =
  | Expression of Expression.t
  | TypeAnnotation of Expression.t
[@@deriving compare, show]

type cfg_data = {
  define_name: Reference.t;
  node_id: int;
  statement_index: int;
}
[@@deriving compare, show]

type symbol_and_cfg_data = {
  symbol_with_definition: symbol_with_definition;
  cfg_data: cfg_data;
  use_postcondition_info: bool;
}
[@@deriving compare, show]

val location_insensitive_compare_symbol_and_cfg_data
  :  symbol_and_cfg_data ->
  symbol_and_cfg_data ->
  int

val narrowest_match : symbol_and_cfg_data list -> symbol_and_cfg_data option

val find_narrowest_spanning_symbol
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  module_reference:Reference.t ->
  Location.position ->
  symbol_and_cfg_data option

val resolve_definition_for_symbol
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  module_reference:Reference.t ->
  symbol_and_cfg_data ->
  Location.WithModule.t option

val location_of_definition
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  module_reference:Reference.t ->
  Location.position ->
  Location.WithModule.t option

type reason =
  | TypeIsAny
  | ContainerParameterIsAny
  | CallableParameterIsUnknownOrAny
[@@deriving compare, sexp, show, hash]

type coverage_data = {
  expression: Expression.t option;
  type_: Type.t;
}
[@@deriving compare, sexp, show, hash]

type coverage_gap = {
  coverage_data: coverage_data;
  reason: reason;
}
[@@deriving compare, sexp, show, hash]

val classify_coverage_data : coverage_data -> coverage_gap option
