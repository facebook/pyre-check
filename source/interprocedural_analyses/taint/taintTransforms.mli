(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = TaintTransform.t list [@@deriving compare, eq, hash, sexp]

module Order : sig
  type t =
    (* A:B:C represents the transforms for x in `x = A(B(C(taint)))` *)
    | Forward
    (* A:B:C represents the transforms for x in `taint = C(B(A(x)))` *)
    | Backward
  [@@deriving show]
end

val empty : t

val is_empty : t -> bool

val merge : local:t -> global:t -> t

val of_named_transforms : TaintTransform.t list -> t

val get_named_transforms : t -> TaintTransform.t list

(* Split a list of transforms into sanitizers present at the beginning and the rest. *)
val split_sanitizers : t -> SanitizeTransformSet.t * t

(* Return sanitizers that are still valid (i.e, before a named transform. *)
val get_sanitize_transforms : t -> SanitizeTransformSet.t

(* Discard all sanitizers, regardless of whether they are still valid or not. *)
val discard_sanitize_transforms : t -> t

val discard_sanitize_source_transforms : t -> t

val discard_sanitize_sink_transforms : t -> t

val pp_kind
  :  formatter:Format.formatter ->
  pp_base:(Format.formatter -> 'a -> unit) ->
  local:t ->
  global:t ->
  base:'a ->
  unit

val show_transforms : t -> string

(* See transform operations in `taintTransformOperation.mli`. *)

module Set : Stdlib.Set.S with type elt = t
