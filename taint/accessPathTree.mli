(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

(** Access Path Trees (APT)

    Abstract domain for mapping access paths to abstract elements with the following semantics:

    - forall path p and path extension p.q. apt(p) <= apt(p.q)

    - for e's that are indexes [f] [*] behaves as follows:
      Given a tree t, t[*] covers all fields f, for which there isn't an explicit
      t[f] present.
      Thus, lookup of t[f] reads f if present, otherwise it reads t[*].
      Update of t[f] adds f, leaves * alone (strong update).
      Update of t[*] updates * and all f weakly (join update).

    - trees are minimal, meaning for all p, e the delta element stored at apt(p.e) is
      either
      a) bottom and this is not a leaf
      b) or apt(p) not<= apt(p.e).

    - widen prev next produces a tree whose max depth for any root r is deeper in
      the result than in prev. (Roots not present in prev are considered infinite
      depth, as we cannot grow in width (new roots) indefinitely). *)


open Ast
open Analysis
open Core


module Checks : sig
  module type S = sig
    type witness
    val create_witness: bool -> false_witness:string -> witness
    val option_construct: message:(unit -> string) -> witness -> witness
    val false_witness: message:(unit -> string) -> witness
    (* Captures true, as a witness, i.e. without extra info. *)
    val true_witness: witness
    val is_true: witness -> bool
    val get_witness: witness -> string option
    val and_witness: witness -> witness -> witness
    (* Calls the argument function if checking is on, otherwise ignores it. *)
    val check: (unit -> unit) -> unit
  end
end


(** Performs invariant checking if used to instantiate functor below. *)
module WithChecks: Checks.S


(** No invariant checking. *)
module WithoutChecks: Checks.S


module Label: sig
  type t =
    | Field of Identifier.t
    | Any
  [@@deriving eq, show, compare, sexp, hash]

  type path = t list
  [@@deriving compare, eq, show, sexp]

  val create_name_field: string -> t
  val create_int_field: int -> t
end


module Root: sig
  module type S = sig
    include Map.Key
    val show: t -> string
  end
end


module Make (Checks: Checks.S) (Root: Root.S) (Element: AbstractDomain.S): sig
  type t
  [@@deriving show, sexp]

  type access_path_tree
  [@@deriving sexp]

  val empty: t
  val exists: f: (Element.t -> bool) -> t -> bool
  val is_empty: t -> bool
  val less_or_equal: left: t -> right: t -> bool
  val less_or_equal_witness: left: t -> right: t -> Checks.witness
  val show_just_access_path: t -> string
  val read_access_path:
    ?transform_non_leaves:(Label.path -> Element.t -> Element.t)
    -> root: Root.t
    -> path: Label.path
    -> t
    -> access_path_tree

  (** Return the path elements joined without combining them with the element at the path tip. *)
  val read_access_path_raw:
    ?transform_non_leaves:(Label.path -> Element.t -> Element.t)
    -> root: Root.t
    -> path: Label.path
    -> use_precise_fields: bool
    -> t
    -> Element.t * access_path_tree

  val read: Root.t -> t -> access_path_tree

  val assign: root: Root.t -> path: Label.path -> access_path_tree -> t -> t
  val assign_weak: root: Root.t -> path: Label.path -> access_path_tree -> t -> t

  val filter_map: f: (Root.t -> access_path_tree -> access_path_tree) -> t -> t
  val filter_map_key: f: (Root.t -> Root.t option) -> t -> t
  val filter_map_value: f: (Element.t -> Element.t) -> t -> t
  val keys: t -> Root.t list

  val fold
    : f: (Root.t -> access_path_tree -> 'accumulator -> 'accumulator)
    -> t
    -> init: 'accumulator
    -> 'accumulator

  val fold_paths
    : f:
        (root: Root.t
         -> path: Label.path
         -> path_element: Element.t
         -> element: Element.t
         -> 'accumulator
         -> 'accumulator)
    -> init: 'accumulator
    -> t
    -> 'accumulator

  val join: t -> t -> t
  val of_list: ((Root.t * Label.path) * Element.t) list -> t
  val remove: root: Root.t -> path: Label.path -> t -> t
  val singleton: root: Root.t -> path: Label.path -> access_path_tree -> t
  val widen: iteration: int -> previous: t -> next: t -> t

  val iteri: f:(Root.t -> access_path_tree -> unit) -> t -> unit
  val iterate_paths: f: (root: Root.t -> path: Label.path -> element: Element.t -> unit) -> t -> unit

  val empty_tree: access_path_tree
  val get_root_taint: access_path_tree -> Element.t
  val exists_in_tree: f: (Element.t -> bool) -> access_path_tree -> bool
  val is_empty_tree: access_path_tree -> bool
  val create_tree: Label.path -> access_path_tree -> access_path_tree
  val read_tree:
    ?transform_non_leaves:(Label.path -> Element.t -> Element.t)
    -> Label.path
    -> access_path_tree
    -> access_path_tree
  val tree_has_children: access_path_tree -> bool
  val tree_to_string: access_path_tree -> string
  val tree_to_string_just_access_path: access_path_tree -> string

  val assign_tree_path
    : tree: access_path_tree
    -> Label.path
    -> subtree: access_path_tree
    -> access_path_tree

  val collapse: access_path_tree -> Element.t
  val collapse_to: depth: int -> access_path_tree -> access_path_tree
  val collapse_tree_indices: access_path_tree -> access_path_tree
  val cut_tree_after: depth: int -> access_path_tree -> access_path_tree

  val filter_map_tree: f: (Element.t -> Element.t) -> access_path_tree -> access_path_tree
  val filter_map_tree_paths
    : f:(path: Label.path -> path_element: Element.t -> element: Element.t -> Element.t)
    -> access_path_tree
    -> access_path_tree

  val fold_tree_children
    : access_path_tree
    -> init: 'accumulator
    -> f: (Label.t -> access_path_tree -> 'accumulator -> 'accumulator)
    -> 'accumulator
  val fold_tree_paths:
    init:'accumulator ->
    f:
      (path: Label.path
       -> path_element: Element.t
       -> element: Element.t
       -> 'accumulator
       -> 'accumulator)
    -> access_path_tree
    -> 'accumulator

  val join_tree_path:
    tree: access_path_tree
    -> Label.path
    -> subtree: access_path_tree
    -> access_path_tree
  val join_trees: access_path_tree -> access_path_tree -> access_path_tree
  val join_root_element: access_path_tree -> Element.t -> access_path_tree
  val create_leaf: Element.t -> access_path_tree
  val replace_root_taint: access_path_tree -> Element.t -> access_path_tree
  val iterate_tree_paths
    : f: (path:Label.path -> element: Element.t -> unit)
    -> access_path_tree
    -> unit
end
