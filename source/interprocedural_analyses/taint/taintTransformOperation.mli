(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module InsertLocation : sig
  type t =
    | Front
    | Back
  [@@deriving show]
end

module Source : sig
  val apply_sanitize_transforms
    :  taint_configuration:TaintConfiguration.Heap.t ->
    SanitizeTransformSet.t ->
    InsertLocation.t ->
    Sources.t ->
    Sources.t option

  val apply_transforms
    :  taint_configuration:TaintConfiguration.Heap.t ->
    TaintTransforms.t ->
    InsertLocation.t ->
    TaintTransforms.Order.t ->
    Sources.t ->
    Sources.t option
end

module Sink : sig
  val apply_sanitize_transforms
    :  taint_configuration:TaintConfiguration.Heap.t ->
    SanitizeTransformSet.t ->
    InsertLocation.t ->
    Sinks.t ->
    Sinks.t option

  val apply_transforms
    :  taint_configuration:TaintConfiguration.Heap.t ->
    TaintTransforms.t ->
    InsertLocation.t ->
    TaintTransforms.Order.t ->
    Sinks.t ->
    Sinks.t option
end
