(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

val add_overriding_types : member:Reference.t -> subtypes:Reference.t list -> unit

val get_overriding_types : member:Reference.t -> Reference.t list option

val remove_overriding_types : Reference.t list -> unit

val overrides_exist : Reference.t -> bool

type cap_overrides_result = {
  overrides: DependencyGraph.overrides;
  skipped_overrides: Reference.t list;
}

val cap_overrides
  :  ?maximum_overrides_to_analyze:int ->
  DependencyGraph.overrides ->
  cap_overrides_result

val record_overrides : DependencyGraph.overrides -> unit
