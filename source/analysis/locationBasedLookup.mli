(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type t

val create_of_module : TypeEnvironment.ReadOnly.t -> Reference.t -> t

val get_resolved_type : t -> position:Location.position -> (Location.t * Type.t) option

val get_all_resolved_types : t -> (Location.t * Type.t) list

val get_definition : t -> position:Location.position -> Location.WithModule.t option

val get_all_definitions : t -> (Location.t * Location.WithModule.t) list

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

val find_narrowest_spanning_symbol
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  module_reference:Reference.t ->
  Location.position ->
  symbol_and_cfg_data option
