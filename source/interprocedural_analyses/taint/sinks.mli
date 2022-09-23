(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type partial_sink = {
  kind: string;
  label: string;
}
[@@deriving compare, show]

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
      local: TaintTransforms.t;
      global: TaintTransforms.t;
      (* Invariant: not a transform. *)
      base: t;
    }
[@@deriving compare, hash, sexp, show]

val equal : t -> t -> bool

val name : string

val make_transform : local:TaintTransforms.t -> global:TaintTransforms.t -> base:t -> t

val ignore_kind_at_call : t -> bool

val apply_call : t -> t

module Set : sig
  include Stdlib.Set.S with type elt = t

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val to_sanitize_transform_set_exn : t -> SanitizeTransformSet.t

  val as_singleton : t -> elt option
end

module Map : sig
  include Stdlib.Map.S with type key = t

  val of_alist_exn : (key * 'a) list -> 'a t

  val to_alist : 'a t -> (key * 'a) list
end

val discard_subkind : t -> t

val discard_transforms : t -> t

val discard_sanitize_transforms : t -> t

val extract_sanitized_sinks_from_transforms : SanitizeTransform.SinkSet.t -> Set.t

val to_sanitized_sink_exn : t -> SanitizeTransform.Sink.t

val from_sanitized_sink : SanitizeTransform.Sink.t -> t

val extract_sanitize_transforms : t -> SanitizeTransformSet.t

val extract_partial_sink : t -> partial_sink option

val get_named_transforms : t -> TaintTransform.t list

val contains_sanitize_transforms : t -> SanitizeTransformSet.t -> bool
