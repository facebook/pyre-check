(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type partial_sink = {
  kind: string;
  label: string;
}
[@@deriving compare, eq, sexp, show, hash]

type t =
  | Attach
  | PartialSink of partial_sink
  | TriggeredPartialSink of partial_sink
  | LocalReturn (* Special marker to describe function in-out behavior *)
  | NamedSink of string
  | ParametricSink of {
      sink_name: string;
      subkind: string;
    }
  | ParameterUpdate of int (* Special marker to describe function in-out behavior *)
  | AddFeatureToArgument
  | Transform of {
      (* Invariant: concatenation of local @ global is non-empty. *)
      (* Invariant: local @ global is the temporal order in which transforms
       * are applied in the code. *)
      local: TaintTransform.t list;
      global: TaintTransform.t list;
      (* Invariant: not a transform. *)
      base: t;
    }
[@@deriving compare, eq, sexp, show, hash]

val name : string

val ignore_kind_at_call : t -> bool

val apply_call : t -> t

module Set : sig
  include Stdlib.Set.S with type elt = t

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val to_sanitize_taint_transforms_exn : t -> TaintTransform.t list
end

val discard_subkind : t -> t

val discard_transforms : t -> t

val extract_sanitized_sinks_from_transforms : TaintTransform.t list -> Set.t

val extract_transforms : t -> TaintTransform.t list

val extract_partial_sink : t -> partial_sink option

val apply_taint_transform : TaintTransform.t -> t -> t

(* Transforms must be provided in the temporal order in which they are applied. *)
val apply_taint_transforms : TaintTransform.t list -> t -> t

(* Apply taint transforms only to the special `LocalReturn` sink. *)
val apply_sanitize_sink_transform : TaintTransform.t -> t -> t

(* Apply taint transforms only to the special `LocalReturn` sink. *)
val apply_sanitize_sink_transforms : TaintTransform.t list -> t -> t
