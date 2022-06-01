(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Ast

type t [@@deriving eq]

type class_name = string

module ClassNameSet : Caml.Set.S with type elt = class_name

module ClassNameMap : sig
  include Caml.Map.S with type key = class_name

  val show : pp_value:(Format.formatter -> 'a -> unit) -> 'a t -> string
end

val empty : t

(* Return the immediate children *)
val children : t -> class_name -> ClassNameSet.t

(* Add an edge in the graph *)
val add : t -> parent:class_name -> child:class_name -> t

val show : t -> string

val from_source : environment:TypeEnvironment.ReadOnly.t -> source:Source.t -> t

val create : roots:class_name list -> edges:(class_name * class_name list) list -> t

val roots : t -> ClassNameSet.t

val join : t -> t -> t

val from_qualifiers
  :  scheduler:Scheduler.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  t
