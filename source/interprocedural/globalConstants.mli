(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

module Heap : sig
  type t

  val empty : t

  val from_source : Source.t -> t
end

module SharedMemory : sig
  type t

  val from_heap : Heap.t -> t

  val get : t -> Reference.t -> StringLiteral.t option

  val from_qualifiers
    :  environment:Analysis.TypeEnvironment.ReadOnly.t ->
    qualifiers:Reference.t list ->
    t
end
