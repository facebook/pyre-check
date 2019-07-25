(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast

type scope =
  | Local
  | Global

and immutable = {
  scope: scope;
  original: Type.t;
  final: bool;
}

and mutability =
  | Mutable
  | Immutable of immutable

and t = {
  annotation: Type.t;
  mutability: mutability;
}
[@@deriving compare, eq, show, hash, sexp]

val create : ?mutability:mutability -> Type.t -> t

val create_immutable : global:bool -> ?original:Type.t option -> ?final:bool -> Type.t -> t

val annotation : t -> Type.t

val original : t -> Type.t

val mutability : t -> mutability

val scope : t -> scope option

val is_global : t -> bool

val is_immutable : t -> bool

val is_final : t -> bool

val instantiate : t -> constraints:(Type.t -> Type.t option) -> t

val dequalify : Reference.t Reference.Map.t -> t -> t
