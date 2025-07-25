(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module Mutability : sig
  type t
end

module Unit : sig
  type t = {
    annotation: Type.t;
    mutability: Mutability.t;
  }
  [@@deriving compare, equal, show, hash, sexp]

  val display_as_revealed_type : t -> string

  val create_mutable : Type.t -> t

  val create_immutable : ?original:Type.t option -> ?final:bool -> Type.t -> t

  val annotation : t -> Type.t

  val original : t -> Type.t

  val is_immutable : t -> bool

  val is_final : t -> bool

  val transform_types : f:(Type.t -> Type.t) -> t -> t

  val dequalify : Reference.t Reference.Map.t -> t -> t

  val less_or_equal
    :  type_less_or_equal:(left:Type.t -> right:Type.t -> bool) ->
    left:t ->
    right:t ->
    bool

  val join : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val meet : type_meet:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val refine
    :  type_less_or_equal:(left:Type.t -> right:Type.t -> bool) ->
    solve_less_or_equal:(left:Type.t -> right:Type.t -> Type.t option) ->
    refined_type:Type.t ->
    t ->
    t

  val join_forcing_union : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t
end

module LocalOrGlobal : sig
  type t [@@deriving equal, show]

  val empty : t

  val top : t

  val create : Unit.t -> t

  val create_mutable : Type.t -> t

  val base : t -> Unit.t option

  val set_base : t -> base:Unit.t -> t

  val set_type_info : ?wipe_subtree:bool -> attribute_path:Reference.t -> type_info:Unit.t -> t -> t

  val get_type_info : attribute_path:Reference.t -> t -> Unit.t option

  val less_or_equal
    :  type_less_or_equal:(left:Type.t -> right:Type.t -> bool) ->
    left:t ->
    right:t ->
    bool

  val join : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val meet : type_meet:(Type.t -> Type.t -> Type.t) -> t -> t -> t
end

module Store : sig
  type t = {
    type_info: LocalOrGlobal.t Reference.Map.Tree.t;
    temporary_type_info: LocalOrGlobal.t Reference.Map.Tree.t;
  }
  [@@deriving equal, show]

  val print_as_json : Format.formatter -> t -> unit

  val empty : t

  val has_nontemporary_type_info : name:Reference.t -> t -> bool

  val get_base : name:Reference.t -> t -> Unit.t option

  val get_type_info : name:Reference.t -> attribute_path:Reference.t -> t -> Unit.t option

  val set_base : ?temporary:bool -> name:Reference.t -> base:Unit.t -> t -> t

  val new_as_base : ?temporary:bool -> name:Reference.t -> base:Unit.t -> t -> t

  val set_type_info
    :  ?temporary:bool ->
    ?wipe_subtree:bool ->
    name:Reference.t ->
    attribute_path:Reference.t ->
    base_type_info:Unit.t option ->
    type_info:Unit.t ->
    t ->
    t

  val less_or_equal
    :  type_less_or_equal:(left:Type.t -> right:Type.t -> bool) ->
    left:t ->
    right:t ->
    bool

  val less_or_equal_monotone : left:t -> right:t -> bool

  val meet : type_meet:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val outer_join : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val outer_widen
    :  type_join:(Type.t -> Type.t -> Type.t) ->
    iteration:int ->
    widening_threshold:int ->
    t ->
    t ->
    t

  val update_existing : old_store:t -> new_store:t -> t

  val update_with_filter : old_store:t -> new_store:t -> filter:(Reference.t -> Unit.t -> bool) -> t
end

module ForFunctionBody : sig
  type t

  val empty : unit -> t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val set : ?precondition:Store.t -> ?postcondition:Store.t -> statement_key:int -> t -> unit

  module ReadOnly : sig
    type t [@@deriving equal]

    val get_precondition : t -> statement_key:int -> Store.t option

    val get_postcondition : t -> statement_key:int -> Store.t option
  end

  val read_only : t -> ReadOnly.t
end
