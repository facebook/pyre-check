(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractTreeDomain

module Make (_ : CONFIG) (Root : AbstractDomainCore.S) (Element : ELEMENT) () : sig
  include AbstractDomainCore.S

  type _ AbstractDomainCore.part +=
    | Root : Root.t AbstractDomainCore.part
    | (* The abstract value at the tip of each path, not including ancestors (only non-bottom points
         are visitied *)
        Path :
        (Label.path * Element.t) AbstractDomainCore.part

  val create_leaf : Root.t -> Element.t -> t

  (* Creates a new tree that has the given tree as a subtree at the given path. *)
  val prepend : Label.path -> t -> t

  (* Assign the given subtree at the path in the tree. If weak is true, join the subtree with the
     existing tree at that point. *)
  val assign : ?weak:bool -> tree:t -> Label.path -> subtree:t -> t

  val read : ?transform_non_leaves:(Label.path -> Element.t -> Element.t) -> Label.path -> t -> t

  (* Read the subtree at the given path. Returns the pair ancestors, tree_at_tip.
   * ~use_precise_labels overrides the default handling of [*] matching all fields. *)
  val read_raw
    :  ?transform_non_leaves:(Label.path -> Element.t -> Element.t) ->
    ?use_precise_labels:bool ->
    Label.path ->
    t ->
    Element.t * t

  (* Compute minimum/maximum path length to non-bottom element. *)
  val min_depth : t -> int

  val max_depth : t -> int

  val collapse : ?transform:(Element.t -> Element.t) -> t -> Element.t

  (* Collapse subtrees at depth *)
  val collapse_to : ?transform:(Element.t -> Element.t) -> depth:int -> t -> t

  (* Collapses the given tree to a depth that keeps at most `width` leaves. *)
  val limit_to : ?transform:(Element.t -> Element.t) -> width:int -> t -> t

  (* shape tree ~mold performs a join of tree and mold such that the resulting tree only has
     branches that are already in mold. *)
  val shape : ?transform:(Element.t -> Element.t) -> t -> mold:t -> t

  val cut_tree_after : depth:int -> t -> t

  val get_root : t -> Root.t * Element.t

  val set_root : t -> Root.t -> t

  (* Returns the set of labels rooted in the tree that lead to non-trivial subtrees *)
  val labels : t -> Label.t list
end
