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
      sanitize_local: SanitizeTransform.Set.t;
      sanitize_global: SanitizeTransform.Set.t;
      (* Invariant: not a transform. *)
      base: t;
    }
[@@deriving compare, show]

val equal : t -> t -> bool

val name : string

val ignore_kind_at_call : t -> bool

val apply_call : t -> t

module Set : sig
  include Stdlib.Set.S with type elt = t

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val to_sanitize_transforms_exn : t -> SanitizeTransform.Set.t
end

module Map : sig
  include Stdlib.Map.S with type key = t

  val of_alist_exn : (key * 'a) list -> 'a t

  val to_alist : 'a t -> (key * 'a) list
end

val discard_subkind : t -> t

val discard_transforms : t -> t

val discard_sanitize_transforms : t -> t

val extract_sanitized_sinks_from_transforms : SanitizeTransform.Set.t -> Set.t

val extract_sanitize_transforms : t -> SanitizeTransform.Set.t

val extract_partial_sink : t -> partial_sink option

val apply_sanitize_transforms : SanitizeTransform.Set.t -> t -> t

(* Apply sanitize transforms only to the special `LocalReturn` sink. *)
val apply_sanitize_sink_transforms : SanitizeTransform.Set.t -> t -> t
