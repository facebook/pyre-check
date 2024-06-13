(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module LocalOrGlobal : sig
  type t [@@deriving eq, show]

  val empty : t

  val top : t

  val create : Annotation.t -> t

  val create_mutable : Type.t -> t

  val base : t -> Annotation.t option

  val set_base : t -> base:Annotation.t -> t

  val set_annotation
    :  ?wipe_subtree:bool ->
    attribute_path:Reference.t ->
    annotation:Annotation.t ->
    t ->
    t

  val get_annotation : attribute_path:Reference.t -> t -> Annotation.t option

  val less_or_equal
    :  type_less_or_equal:(left:Type.t -> right:Type.t -> bool) ->
    left:t ->
    right:t ->
    bool

  val join : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val meet : type_meet:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val join_annotations
    :  type_join:(Type.t -> Type.t -> Type.t) ->
    Annotation.t ->
    Annotation.t ->
    Annotation.t
end

module Store : sig
  type t = {
    annotations: LocalOrGlobal.t Reference.Map.Tree.t;
    temporary_annotations: LocalOrGlobal.t Reference.Map.Tree.t;
  }
  [@@deriving eq, show]

  val empty : t

  val has_nontemporary_annotation : name:Reference.t -> t -> bool

  val get_base : name:Reference.t -> t -> Annotation.t option

  val get_annotation : name:Reference.t -> attribute_path:Reference.t -> t -> Annotation.t option

  val set_base : ?temporary:bool -> name:Reference.t -> base:Annotation.t -> t -> t

  val new_as_base : ?temporary:bool -> name:Reference.t -> base:Annotation.t -> t -> t

  val set_annotation
    :  ?temporary:bool ->
    ?wipe_subtree:bool ->
    name:Reference.t ->
    attribute_path:Reference.t ->
    base_annotation:Annotation.t option ->
    annotation:Annotation.t ->
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

  val update_with_filter
    :  old_store:t ->
    new_store:t ->
    filter:(Reference.t -> Annotation.t -> bool) ->
    t
end
