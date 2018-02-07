(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Type = AnalysisType

type scope = | Local | Global

and immutable = {
  scope: scope;
  original: Type.t;
}

and mutability =
  | Mutable
  | Immutable of immutable

and t = {
  annotation: Type.t;
  mutability: mutability;
}
[@@deriving eq, show, hash]

val create: ?mutability: mutability -> Type.t -> t
val create_immutable: global: bool -> ?original: Type.t option -> Type.t -> t

val annotation: t -> Type.t
val original: t -> Type.t
val mutability: t -> mutability

val is_immutable: t -> bool

val instantiate: t -> constraints: Type.t Type.Map.t -> t
