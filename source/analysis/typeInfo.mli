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
  [@@deriving compare, eq, show, hash, sexp]

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
end

module LocalOrGlobal : sig
  type t [@@deriving eq, show]

  val empty : t

  val top : t

  val create : Unit.t -> t

  val create_mutable : Type.t -> t

  val base : t -> Unit.t option

  val set_base : t -> base:Unit.t -> t

  val set_annotation
    :  ?wipe_subtree:bool ->
    attribute_path:Reference.t ->
    annotation:Unit.t ->
    t ->
    t

  val get_annotation : attribute_path:Reference.t -> t -> Unit.t option

  val less_or_equal
    :  type_less_or_equal:(left:Type.t -> right:Type.t -> bool) ->
    left:t ->
    right:t ->
    bool

  val join : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val meet : type_meet:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val join_annotations : type_join:(Type.t -> Type.t -> Type.t) -> Unit.t -> Unit.t -> Unit.t
end

module Store : sig
  type t = {
    annotations: LocalOrGlobal.t Reference.Map.Tree.t;
    temporary_annotations: LocalOrGlobal.t Reference.Map.Tree.t;
  }
  [@@deriving eq, show]

  val empty : t

  val has_nontemporary_annotation : name:Reference.t -> t -> bool

  val get_base : name:Reference.t -> t -> Unit.t option

  val get_annotation : name:Reference.t -> attribute_path:Reference.t -> t -> Unit.t option

  val set_base : ?temporary:bool -> name:Reference.t -> base:Unit.t -> t -> t

  val new_as_base : ?temporary:bool -> name:Reference.t -> base:Unit.t -> t -> t

  val set_annotation
    :  ?temporary:bool ->
    ?wipe_subtree:bool ->
    name:Reference.t ->
    attribute_path:Reference.t ->
    base_annotation:Unit.t option ->
    annotation:Unit.t ->
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
