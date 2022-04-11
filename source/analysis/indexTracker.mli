(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t [@@deriving compare, sexp, show]

module IndexKey : Memory.KeyType with type t = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

val index : string -> t

val indices : Type.Primitive.Set.t -> Set.t

val annotation : t -> string
