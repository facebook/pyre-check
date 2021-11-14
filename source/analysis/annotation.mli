(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type immutable = {
  original: Type.t;
  final: bool;
}
[@@deriving compare, eq, hash, sexp]

type mutability =
  | Mutable
  | Immutable of immutable
[@@deriving compare, eq, hash, sexp]

type t = {
  annotation: Type.t;
  mutability: mutability;
}
[@@deriving compare, eq, show, hash, sexp]

val create_mutable : Type.t -> t

val create_immutable : ?original:Type.t option -> ?final:bool -> Type.t -> t

val annotation : t -> Type.t

val original : t -> Type.t

val mutability : t -> mutability

val is_immutable : t -> bool

val is_final : t -> bool

val instantiate : t -> constraints:(Type.t -> Type.t option) -> t

val dequalify : Reference.t Reference.Map.t -> t -> t
