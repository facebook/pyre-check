(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type t [@@deriving eq, show]

val top : t

val base : t -> Annotation.t option

val create : ?base:Annotation.t -> ?attribute_refinements:t Identifier.Map.Tree.t -> unit -> t

val set_base : t -> base:Annotation.t -> t

val add_attribute_refinement : t -> reference:Reference.t -> base:Annotation.t -> t

val annotation : t -> reference:Reference.t -> Annotation.t option

val less_or_equal : global_resolution:GlobalResolution.t -> t -> t -> bool

val join : global_resolution:GlobalResolution.t -> t -> t -> t

val meet : global_resolution:GlobalResolution.t -> t -> t -> t

val widen
  :  global_resolution:GlobalResolution.t ->
  widening_threshold:int ->
  previous:t ->
  next:t ->
  iteration:int ->
  t
