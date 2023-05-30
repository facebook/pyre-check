(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open AbstractTreeDomain

module Make (Config : CONFIG) (Root : AbstractDomainCore.S) (Element : ELEMENT) () = struct
  module Tree = AbstractTreeDomain.Make (Config) (Element) ()

  module ProductConfig = struct
    type 'a slot =
      | Tree : Tree.t slot
      | Root : Root.t slot

    let slots = 2

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Tree -> "tree"
      | Root -> "root"


    let slot_domain (type a) (slot : a slot) : a AbstractDomainCore.abstract_domain =
      match slot with
      | Tree -> (module Tree : AbstractDomainCore.S with type t = Tree.t)
      | Root -> (module Root : AbstractDomainCore.S with type t = Root.t)


    let strict (type a) (slot : a slot) =
      match slot with
      | Tree -> true
      | Root -> false
  end

  module Product = AbstractProductDomain.Make (ProductConfig)

  type _ AbstractDomainCore.part +=
    | Root = Root.Self
    | Path = Tree.Path
    | RefinedPath = Tree.RefinedPath

  include Product

  let create_leaf root element =
    create AbstractDomainCore.[Part (Root.Self, root); Part (Tree.Self, Tree.create_leaf element)]


  let prepend path product =
    update ProductConfig.Tree (Tree.prepend path (get ProductConfig.Tree product)) product


  let assign ?weak ~tree path ~subtree =
    create
      AbstractDomainCore.
        [
          Part (Root.Self, Root.join (get ProductConfig.Root tree) (get ProductConfig.Root subtree));
          Part
            ( Tree.Self,
              Tree.assign
                ?weak
                ~tree:(get ProductConfig.Tree tree)
                path
                ~subtree:(get ProductConfig.Tree subtree) );
        ]


  let read ?transform_non_leaves path product =
    let tree = Tree.read ?transform_non_leaves path (get ProductConfig.Tree product) in
    update ProductConfig.Tree tree product


  let read_refined ?transform_non_leaves path product =
    let tree = Tree.read_refined ?transform_non_leaves path (get ProductConfig.Tree product) in
    update ProductConfig.Tree tree product


  let read_raw ?transform_non_leaves ?use_precise_labels path product =
    let element, tree =
      Tree.read_raw ?transform_non_leaves ?use_precise_labels path (get ProductConfig.Tree product)
    in
    element, update ProductConfig.Tree tree product


  let read_raw_refined ?transform_non_leaves ?use_precise_labels path product =
    let element, tree =
      Tree.read_raw_refined
        ?transform_non_leaves
        ?use_precise_labels
        path
        (get ProductConfig.Tree product)
    in
    element, update ProductConfig.Tree tree product


  let min_depth product = Tree.min_depth (get ProductConfig.Tree product)

  let max_depth product = Tree.max_depth (get ProductConfig.Tree product)

  let collapse ?transform product = Tree.collapse ?transform (get ProductConfig.Tree product)

  let collapse_to ?transform ~depth product =
    update
      ProductConfig.Tree
      (Tree.collapse_to ?transform ~depth (get ProductConfig.Tree product))
      product


  let limit_to ?transform ~width product =
    update
      ProductConfig.Tree
      (Tree.limit_to ?transform ~width (get ProductConfig.Tree product))
      product


  let shape ?transform product ~mold =
    let mold = get ProductConfig.Tree mold in
    update ProductConfig.Tree (Tree.shape ?transform ~mold (get ProductConfig.Tree product)) product


  let cut_tree_after ~depth product =
    update ProductConfig.Tree (Tree.cut_tree_after ~depth (get ProductConfig.Tree product)) product


  let get_root product =
    get ProductConfig.Root product, Tree.get_root (get ProductConfig.Tree product)


  let set_root product root = update ProductConfig.Root root product

  let labels product = Tree.labels (get ProductConfig.Tree product)
end
