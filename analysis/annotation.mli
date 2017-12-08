(** Copyright 2016-present Facebook. All rights reserved. **)


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
[@@deriving eq, show]

val create: ?mutability: mutability -> Type.t -> t
val create_immutable: global: bool -> ?original: Type.t option -> Type.t -> t

val annotation: t -> Type.t
val original: t -> Type.t
val mutability: t -> mutability

val is_immutable: t -> bool
