(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

exception Untracked of string

module MethodResolutionOrderError : sig
  type t =
    | Cyclic of Type.Primitive.t
    | Inconsistent of Type.Primitive.t
  [@@deriving sexp, compare]
end

module CheckIntegrityError : sig
  type t =
    | Cyclic of Type.Primitive.t
    | Incomplete of Type.Primitive.t
  [@@deriving sexp, compare]
end

module Target : sig
  type t = {
    target: Ast.Identifier.t;
    arguments: Type.Argument.t list;
  }
  [@@deriving compare, sexp, show]

  module type ListOrSet = sig
    type record

    val filter : record -> f:(t -> bool) -> record

    val is_empty : record -> bool

    val exists : record -> f:(t -> bool) -> bool

    val iter : record -> f:(t -> unit) -> unit

    val equal : record -> record -> bool

    val mem : record -> t -> bool

    val to_string : f:(t -> string) -> record -> string

    val fold : record -> init:'accum -> f:('accum -> t -> 'accum) -> 'accum

    val empty : record

    val add : record -> t -> record
  end

  module Set : sig
    include Set.S with type Elt.t = t

    include ListOrSet with type record = t
  end

  module List : ListOrSet with type record = t list
end

module GenericMetadata : sig
  type t =
    | NotGeneric
    | GenericBase of Type.GenericParameter.t list
    | InvalidGenericBase
  [@@deriving sexp, show, compare]
end

module Edges : sig
  type t = {
    parents: Target.t list;
    generic_metadata: GenericMetadata.t;
  }
  [@@deriving sexp, compare]
end

(** The handler module for interfacing with ClassHierarchy lookups. See [Environment_handler] for
    more. *)
module type Handler = sig
  val edges : Ast.Identifier.t -> Edges.t option

  val contains : Type.Primitive.t -> bool
end

val parents_of : (module Handler) -> Ast.Identifier.t -> Target.t list option

(* Returns true if the annotation can be deconstructed into primitive types that exist in the class
   hierarchy. If typing.List and int are in the class hierarchy, both `contains hierarchy
   typing.List` and `is_instantiated class hierarchy typing.List[int]` will evaluate to true, but
   `is_instantiated hierarchy typing.List[str]` will evaluate to false. *)
val is_instantiated : (module Handler) -> Type.t -> bool

val method_resolution_order_linearize
  :  get_parents:(Ast.Identifier.t -> Target.t list option) ->
  Type.Primitive.t ->
  (Type.Primitive.t list, MethodResolutionOrderError.t) result

val immediate_parents : (module Handler) -> Type.Primitive.t -> Type.Primitive.t list

val generic_parameters
  :  ?empty_for_nongeneric:bool ->
  (module Handler) ->
  Type.Primitive.t ->
  Type.GenericParameter.t list option

val generic_parameters_as_variables
  :  ?empty_for_nongeneric:bool ->
  (module Handler) ->
  Type.Primitive.t ->
  Type.Variable.t list option

val check_integrity
  :  class_names:Ast.Identifier.t list ->
  (module Handler) ->
  (unit, CheckIntegrityError.t) result

val to_dot : (module Handler) -> class_names:Ast.Identifier.t list -> string

val instantiate_successors_parameters
  :  (module Handler) ->
  source:Type.t ->
  target:Type.Primitive.t ->
  Type.Argument.t list option
