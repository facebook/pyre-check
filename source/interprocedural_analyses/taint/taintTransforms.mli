(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving compare, eq, hash, sexp]

module Order : sig
  type t =
    (* A:B:C represents the transforms for x in `x = A(B(C(taint)))` *)
    | Forward
    (* A:B:C represents the transforms for x in `taint = C(B(A(x)))` *)
    | Backward
  [@@deriving show]
end

module Set : Stdlib.Set.S with type elt = t

val add_sanitize_transforms
  :  preserve_sanitize_sources:bool ->
  preserve_sanitize_sinks:bool ->
  base:SanitizeTransform.t option ->
  local:t ->
  global:t ->
  SanitizeTransformSet.t ->
  t option

val add_transforms
  :  preserve_sanitize_sources:bool ->
  preserve_sanitize_sinks:bool ->
  base:SanitizeTransform.t option ->
  local:t ->
  global:t ->
  order:Order.t ->
  to_add:t ->
  to_add_order:Order.t ->
  t option

val empty : t

val get_named_transforms : t -> TaintTransform.t list

(* This only returns sanitizers that are still valid (i.e, before a named transform. *)
val get_sanitize_transforms : t -> SanitizeTransformSet.t

(* This discards all sanitizers, regardless of whether they are still valid or not. *)
val discard_sanitize_transforms : t -> t

val discard_sanitize_source_transforms : t -> t

val discard_sanitize_sink_transforms : t -> t

val is_empty : t -> bool

val merge : local:t -> global:t -> t

val of_named_transforms : TaintTransform.t list -> t

val of_sanitize_transforms
  :  preserve_sanitize_sources:bool ->
  preserve_sanitize_sinks:bool ->
  base:SanitizeTransform.t option ->
  SanitizeTransformSet.t ->
  t option

val pp_kind
  :  formatter:Format.formatter ->
  pp_base:(Format.formatter -> 'a -> unit) ->
  local:t ->
  global:t ->
  base:'a ->
  unit

val show_transforms : t -> string
