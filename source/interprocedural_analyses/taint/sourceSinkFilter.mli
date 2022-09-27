(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

(* Filters everything. *)
val all : t

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

val possible_tito_transforms : t -> TaintTransforms.Set.t

module MatchingSanitizeTransforms : sig
  type t = {
    transforms: SanitizeTransformSet.t;
    (* False if the set of matching sources or sinks cannot be sanitized,
     * for instance if it contains a transform, or a (triggered) partial sink. *)
    sanitizable: bool;
  }
end

val matching_source_sanitize_transforms
  :  t ->
  named_transforms:TaintTransforms.t ->
  base:Sinks.t ->
  MatchingSanitizeTransforms.t option

val matching_sink_sanitize_transforms
  :  t ->
  named_transforms:TaintTransforms.t ->
  base:Sources.t ->
  MatchingSanitizeTransforms.t option
