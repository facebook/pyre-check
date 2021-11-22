(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module Unit : sig
  type t [@@deriving eq, show]

  val empty : t

  val top : t

  val create : Annotation.t -> t

  val create_mutable : Type.t -> t

  val base : t -> Annotation.t option

  val set_base : t -> base:Annotation.t -> t

  val add_attribute_refinement : t -> reference:Reference.t -> annotation:Annotation.t -> t

  val annotation : t -> reference:Reference.t -> Annotation.t option

  val less_or_equal : global_resolution:GlobalResolution.t -> left:t -> right:t -> bool

  val join : global_resolution:GlobalResolution.t -> t -> t -> t

  val meet : global_resolution:GlobalResolution.t -> t -> t -> t

  val widen
    :  global_resolution:GlobalResolution.t ->
    widening_threshold:int ->
    previous:t ->
    next:t ->
    iteration:int ->
    t
end

module Store : sig
  type t = {
    annotations: Unit.t Reference.Map.t;
    temporary_annotations: Unit.t Reference.Map.t;
  }
  [@@deriving eq, show]

  val empty : t

  val less_or_equal : global_resolution:GlobalResolution.t -> left:t -> right:t -> bool

  val less_or_equal_monotone : left:t -> right:t -> bool
end
