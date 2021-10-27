(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Attach
  | NamedSource of string
  | ParametricSource of {
      source_name: string;
      subkind: string;
    }
  | Transform of {
      (* Invariant: concatenation of local @ global is non-empty. *)
      sanitize_local: SanitizeTransform.Set.t;
      sanitize_global: SanitizeTransform.Set.t;
      (* Invariant: not a transform. *)
      base: t;
    }
[@@deriving compare, eq, show]

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

val extract_sanitized_sources_from_transforms : SanitizeTransform.Set.t -> Set.t

val extract_sanitize_transforms : t -> SanitizeTransform.Set.t

val apply_sanitize_transforms : SanitizeTransform.Set.t -> t -> t

val apply_sanitize_sink_transforms : SanitizeTransform.Set.t -> t -> t
