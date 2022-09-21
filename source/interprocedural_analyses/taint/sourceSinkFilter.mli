(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create
  :  rules:Rule.t list ->
  filtered_rule_codes:Rule.CodeSet.t option ->
  filtered_sources:Sources.Set.t option ->
  filtered_sinks:Sinks.Set.t option ->
  filtered_transforms:TaintTransform.t list option ->
  t

val filter_rules
  :  filtered_rule_codes:Rule.CodeSet.t option ->
  filtered_sources:Sources.Set.t option ->
  filtered_sinks:Sinks.Set.t option ->
  filtered_transforms:TaintTransform.t list option ->
  Rule.t list ->
  Rule.t list

val should_keep_source : t -> Sources.t -> bool

val should_keep_sink : t -> Sinks.t -> bool

(* Exposed for testing purpose *)
val matching_sources : t -> Sources.Set.t Sinks.Map.t

(* Exposed for testing purpose *)
val matching_sinks : t -> Sinks.Set.t Sources.Map.t

(* Exposed for testing purpose *)
val possible_tito_transforms : t -> TaintTransforms.Set.t
