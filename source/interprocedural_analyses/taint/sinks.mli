(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module AccessPath = Analysis.TaintAccessPath

module PartialSink : sig
  type t = string [@@deriving compare, hash, sexp, equal, show]

  module Set : Data_structures.SerializableSet.S with type elt = t

  module Map : Data_structures.SerializableMap.S with type key = t

  module Triggered : sig
    type nonrec t = {
      partial_sink: t;
      triggering_source: string;
          (* The source kind that has flowed into the other partial sink, which results in creating
             this triggered sink. *)
    }
    [@@deriving compare, show, sexp, equal]

    module Set : Data_structures.SerializableSet.S with type elt = t
  end
end

type t =
  | Attach
  | PartialSink of PartialSink.t
  | LocalReturn (* Special marker to describe function in-out behavior *)
  | NamedSink of string
  | ParametricSink of {
      sink_name: string;
      subkind: string;
    }
  | ParameterUpdate of AccessPath.Root.t (* Special marker to describe function in-out behavior *)
  | AddFeatureToArgument
  | ExtraTraceSink (* Special marker to show traces that end with this sink *)
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
  include Data_structures.SerializableSet.S with type elt = t

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

val create_triggered_sink : triggering_source:string -> PartialSink.t -> t

val extract_partial_sink : t -> PartialSink.t option

val get_named_transforms : t -> TaintTransform.t list

val get_non_sanitize_transforms : t -> TaintTransform.t list

val contains_sanitize_transforms : t -> SanitizeTransformSet.t -> bool
