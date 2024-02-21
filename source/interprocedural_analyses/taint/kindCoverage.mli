(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The kinds that are defined by a user, which are composed of sources, sinks, and transforms. They
   are used for computing the rule coverage. *)
module Sources : sig
  type t = Sources.t

  val from_source : t -> t option

  module Set : Stdlib.Set.S with type elt = t
end

module Sinks : sig
  type t = Sinks.t

  val from_sink : t -> t option

  module Set : Stdlib.Set.S with type elt = t
end

module Transforms : sig
  val from_transform : TaintTransform.t -> TaintTransform.t option

  module Set : Data_structures.SerializableSet.S with type elt = TaintTransform.t
end

type t = {
  sources: Sources.Set.t;
  sinks: Sinks.Set.t;
  transforms: Transforms.Set.t;
}
[@@deriving eq, show, compare, sexp, hash]

val empty : t

val from_model : Model.t -> t

val from_rule : Rule.t -> t

val intersect : t -> t -> t

val union : t -> t -> t

val to_json : t -> Yojson.Safe.t
