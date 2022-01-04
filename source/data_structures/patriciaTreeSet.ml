(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type ELEMENT = sig
  type t [@@deriving show]

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val is_zero : t -> bool

  (* This should wrap on overflow. *)
  val add_one : t -> t

  (* This should wrap on overflow. *)
  val subtract_one : t -> t

  val bitwise_and : t -> t -> t

  val bitwise_xor : t -> t -> t

  val bitwise_not : t -> t
end

module type SET = sig
  type element

  type t

  val empty : t

  val is_empty : t -> bool

  val add : element -> t -> t

  val mem : element -> t -> bool

  val singleton : element -> t

  val remove : element -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (element -> unit) -> t -> unit

  val map : (element -> element) -> t -> t

  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (element -> bool) -> t -> bool

  val exists : (element -> bool) -> t -> bool

  val filter : (element -> bool) -> t -> t

  val cardinal : t -> int

  val elements : t -> element list

  val of_list : element list -> t
end

(*
 * This implementation of sets of integers using Patricia trees is based on the
 * following paper:
 *
 *   C. Okasaki, A. Gill. Fast Mergeable Integer Maps. In Workshop on ML (1998).
 *
 * Patricia trees are a highly efficient representation of compressed binary
 * tries. They are well suited for the situation where one has to manipulate
 * many large sets that are identical or nearly identical. In the paper,
 * Patricia trees are entirely reconstructed for each operation. We have
 * modified the original algorithms, so that subtrees that are not affected by
 * an operation remain unchanged. Since this is a functional data structure,
 * identical subtrees are therefore shared among all Patricia tries manipulated
 * by the program. This effectively achieves a form of incremental hash-consing.
 * Note that it's not perfect, since identical trees that are independently
 * constructed are not equated, but it's a lot more efficient than regular
 * hash-consing. This data structure doesn't just reduce the memory footprint of
 * sets, it also significantly speeds up certain operations. Whenever two sets
 * represented as Patricia trees share some structure, their union and
 * intersection can often be computed in sublinear time.
 *
 * Patricia trees can only handle integer-like elements.
 *)
module Tree (Element : ELEMENT) = struct
  type element = Element.t

  type t =
    | Leaf of element
    | Node of {
        prefix: element;
        branching_bit: element;
        left: t;
        right: t;
      }

  let leaf element = Leaf element

  let is_zero_bit k m = Element.is_zero (Element.bitwise_and k m)

  let lowest_bit x = Element.bitwise_and x (Element.add_one (Element.bitwise_not x))

  let branching_bit a b = lowest_bit (Element.bitwise_xor a b)

  let mask k m = Element.bitwise_and k (Element.subtract_one m)

  let match_prefix k p m = Element.equal (mask k m) p

  let join_disjoint left_prefix left_tree right_prefix right_tree =
    let m = branching_bit left_prefix right_prefix in
    if is_zero_bit left_prefix m then
      Node { prefix = mask left_prefix m; branching_bit = m; left = left_tree; right = right_tree }
    else
      Node { prefix = mask left_prefix m; branching_bit = m; left = right_tree; right = left_tree }


  let rec add to_insert tree =
    match tree with
    | Leaf leaf ->
        if Element.equal to_insert leaf then
          tree
        else
          join_disjoint to_insert (Leaf to_insert) leaf tree
    | Node { prefix; branching_bit; left; right } ->
        if match_prefix to_insert prefix branching_bit then
          if is_zero_bit to_insert branching_bit then
            let new_left = add to_insert left in
            if phys_equal new_left left then
              tree
            else
              Node { prefix; branching_bit; left = new_left; right }
          else
            let new_right = add to_insert right in
            if phys_equal new_right right then
              tree
            else
              Node { prefix; branching_bit; left; right = new_right }
        else
          join_disjoint to_insert (Leaf to_insert) prefix tree


  let rec mem element = function
    | Leaf leaf -> Element.equal element leaf
    | Node { branching_bit; left; right; _ } ->
        if is_zero_bit element branching_bit then
          mem element left
        else
          mem element right


  let rec remove element tree =
    match tree with
    | Leaf leaf ->
        if Element.equal element leaf then
          None
        else
          Some tree
    | Node { prefix; branching_bit; left; right } ->
        if match_prefix element prefix branching_bit then
          if is_zero_bit element branching_bit then
            match remove element left with
            | Some new_left ->
                if phys_equal new_left left then
                  Some tree
                else
                  Some (Node { prefix; branching_bit; left = new_left; right })
            | None -> Some right
          else
            match remove element right with
            | Some new_right ->
                if phys_equal new_right right then
                  Some tree
                else
                  Some (Node { prefix; branching_bit; left; right = new_right })
            | None -> Some left
        else
          Some tree


  let rec insert_leaf leaf tree =
    match leaf with
    | Node _ -> failwith "not a leaf"
    | Leaf leaf_element -> (
        match tree with
        | Leaf tree_element ->
            if Element.equal leaf_element tree_element then
              tree
            else
              join_disjoint leaf_element leaf tree_element tree
        | Node { prefix; branching_bit; left; right } ->
            if match_prefix leaf_element prefix branching_bit then
              if is_zero_bit leaf_element branching_bit then
                let new_left = insert_leaf leaf left in
                if phys_equal new_left left then
                  tree
                else
                  Node { prefix; branching_bit; left = new_left; right }
              else
                let new_right = insert_leaf leaf right in
                if phys_equal new_right right then
                  tree
                else
                  Node { prefix; branching_bit; left; right = new_right }
            else
              join_disjoint leaf_element leaf prefix tree)


  let rec union left right =
    if phys_equal left right then
      left
    else
      match left, right with
      | Leaf _, _ -> insert_leaf left right
      | _, Leaf _ -> insert_leaf right left
      | ( Node
            {
              prefix = left_prefix;
              branching_bit = left_branching_bit;
              left = left_left;
              right = left_right;
            },
          Node
            {
              prefix = right_prefix;
              branching_bit = right_branching_bit;
              left = right_left;
              right = right_right;
            } ) ->
          if
            Element.equal left_prefix right_prefix
            && Element.equal left_branching_bit right_branching_bit
          then (* The two trees have the same prefix, merge subtrees. *)
            let new_left = union left_left right_left in
            let new_right = union left_right right_right in
            if phys_equal new_left left_left && phys_equal new_right left_right then
              left
            else if phys_equal new_left right_left && phys_equal new_right right_right then
              right
            else
              Node
                {
                  prefix = left_prefix;
                  branching_bit = left_branching_bit;
                  left = new_left;
                  right = new_right;
                }
          else if
            Element.compare left_branching_bit right_branching_bit < 0
            && match_prefix right_prefix left_prefix left_branching_bit
          then (* The right tree can be merged with a subtree of the left tree. *)
            if is_zero_bit right_prefix left_branching_bit then
              let new_left = union left_left right in
              if phys_equal left_left new_left then
                left
              else
                Node
                  {
                    prefix = left_prefix;
                    branching_bit = left_branching_bit;
                    left = new_left;
                    right = left_right;
                  }
            else
              let new_right = union left_right right in
              if phys_equal left_right new_right then
                left
              else
                Node
                  {
                    prefix = left_prefix;
                    branching_bit = left_branching_bit;
                    left = left_left;
                    right = new_right;
                  }
          else if
            Element.compare left_branching_bit right_branching_bit > 0
            && match_prefix left_prefix right_prefix right_branching_bit
          then (* The left tree can be merged with a subtree of the right tree. *)
            if is_zero_bit left_prefix right_branching_bit then
              let new_left = union left right_left in
              if phys_equal right_left new_left then
                right
              else
                Node
                  {
                    prefix = right_prefix;
                    branching_bit = right_branching_bit;
                    left = new_left;
                    right = right_right;
                  }
            else
              let new_right = union left right_right in
              if phys_equal right_right new_right then
                right
              else
                Node
                  {
                    prefix = right_prefix;
                    branching_bit = right_branching_bit;
                    left = right_left;
                    right = new_right;
                  }
          else (* The trees are disjoint. *)
            join_disjoint left_prefix left right_prefix right


  let rec inter left right =
    if phys_equal left right then
      Some left
    else
      match left, right with
      | Leaf leaf_element, _ ->
          if mem leaf_element right then
            Some left
          else
            None
      | _, Leaf leaf_element ->
          if mem leaf_element left then
            Some right
          else
            None
      | ( Node
            {
              prefix = left_prefix;
              branching_bit = left_branching_bit;
              left = left_left;
              right = left_right;
            },
          Node
            {
              prefix = right_prefix;
              branching_bit = right_branching_bit;
              left = right_left;
              right = right_right;
            } ) ->
          if
            Element.equal left_prefix right_prefix
            && Element.equal left_branching_bit right_branching_bit
          then (* The two trees have the same prefix, intersect subtrees. *)
            match inter left_left right_left, inter left_right right_right with
            | Some new_left, Some new_right -> Some (union new_left new_right)
            | new_left, None -> new_left
            | None, new_right -> new_right
          else if
            Element.compare left_branching_bit right_branching_bit < 0
            && match_prefix right_prefix left_prefix left_branching_bit
          then (* Intersect the right tree with a subtree of the left tree. *)
            if is_zero_bit right_prefix left_branching_bit then
              inter left_left right
            else
              inter left_right right
          else if
            Element.compare left_branching_bit right_branching_bit > 0
            && match_prefix left_prefix right_prefix right_branching_bit
          then (* Intersect the left tree with a subtree of the right tree. *)
            if is_zero_bit left_prefix right_branching_bit then
              inter left right_left
            else
              inter left right_right
          else (* The trees are disjoint. *)
            None


  let rec diff left right =
    if phys_equal left right then
      None
    else
      match left, right with
      | Leaf leaf_element, _ ->
          if mem leaf_element right then
            None
          else
            Some left
      | _, Leaf leaf_element -> remove leaf_element left
      | ( Node
            {
              prefix = left_prefix;
              branching_bit = left_branching_bit;
              left = left_left;
              right = left_right;
            },
          Node
            {
              prefix = right_prefix;
              branching_bit = right_branching_bit;
              left = right_left;
              right = right_right;
            } ) ->
          if
            Element.equal left_prefix right_prefix
            && Element.equal left_branching_bit right_branching_bit
          then (* The two trees have the same prefix, diff subtrees. *)
            match diff left_left right_left, diff left_right right_right with
            | Some new_left, Some new_right -> Some (union new_left new_right)
            | new_left, None -> new_left
            | None, new_right -> new_right
          else if
            Element.compare left_branching_bit right_branching_bit < 0
            && match_prefix right_prefix left_prefix left_branching_bit
          then (* Diff the right tree with a subtree of the left tree. *)
            if is_zero_bit right_prefix left_branching_bit then
              match diff left_left right with
              | Some new_left -> Some (union new_left left_right)
              | None -> Some left_right
            else
              match diff left_right right with
              | Some new_right -> Some (union left_left new_right)
              | None -> Some left_left
          else if
            Element.compare left_branching_bit right_branching_bit > 0
            && match_prefix left_prefix right_prefix right_branching_bit
          then (* Diff the left tree with a subtree of the right tree. *)
            if is_zero_bit left_prefix right_branching_bit then
              diff left right_left
            else
              diff left right_right
          else (* The trees are disjoint. *)
            Some left


  let rec equal left right =
    if phys_equal left right then
      true
    else
      match left, right with
      | Leaf left_element, Leaf right_element -> Element.equal left_element right_element
      | ( Node
            {
              prefix = left_prefix;
              branching_bit = left_branching_bit;
              left = left_left;
              right = left_right;
            },
          Node
            {
              prefix = right_prefix;
              branching_bit = right_branching_bit;
              left = right_left;
              right = right_right;
            } ) ->
          Element.equal left_prefix right_prefix
          && Element.equal left_branching_bit right_branching_bit
          && equal left_left right_left
          && equal left_right right_right
      | _ -> false


  let rec subset left right =
    if phys_equal left right then
      true
    else
      match left, right with
      | Leaf leaf_element, _ -> mem leaf_element right
      | _, Leaf _ -> false
      | ( Node
            {
              prefix = left_prefix;
              branching_bit = left_branching_bit;
              left = left_left;
              right = left_right;
            },
          Node
            {
              prefix = right_prefix;
              branching_bit = right_branching_bit;
              left = right_left;
              right = right_right;
            } ) ->
          if
            Element.equal left_prefix right_prefix
            && Element.equal left_branching_bit right_branching_bit
          then
            subset left_left right_left && subset left_right right_right
          else if
            Element.compare left_branching_bit right_branching_bit > 0
            && match_prefix left_prefix right_prefix right_branching_bit
          then
            if is_zero_bit left_prefix right_branching_bit then
              subset left_left right_left && subset left_right right_left
            else
              subset left_left right_right && subset left_right right_right
          else
            false


  let rec iter f = function
    | Leaf element -> f element
    | Node { left; right; _ } ->
        iter f left;
        iter f right


  let rec fold f tree init =
    match tree with
    | Leaf element -> f element init
    | Node { left; right; _ } -> fold f right (fold f left init)


  let map f tree =
    (* This cannot be done efficiently,
     * since `f` might change the structure of the tree. *)
    let add_apply element = function
      | None -> Some (Leaf (f element))
      | Some tree -> Some (add (f element) tree)
    in
    fold add_apply tree None


  let rec for_all predicate = function
    | Leaf element -> predicate element
    | Node { left; right; _ } -> for_all predicate left && for_all predicate right


  let rec exists predicate = function
    | Leaf element -> predicate element
    | Node { left; right; _ } -> exists predicate left || exists predicate right


  let rec filter predicate tree =
    match tree with
    | Leaf element -> if predicate element then Some tree else None
    | Node { prefix; branching_bit; left; right } -> (
        match filter predicate left, filter predicate right with
        | Some new_left, Some new_right ->
            if phys_equal new_left left && phys_equal new_right right then
              Some tree
            else
              Some (Node { prefix; branching_bit; left = new_left; right = new_right })
        | new_left, None -> new_left
        | None, new_right -> new_right)


  let rec cardinal = function
    | Leaf _ -> 1
    | Node { left; right; _ } -> cardinal left + cardinal right
end

module Make (Element : ELEMENT) = struct
  module Tree = Tree (Element)

  type element = Element.t

  type t = Tree.t option

  let empty = None

  let is_empty = Option.is_none

  let add element = function
    | Some tree -> Some (Tree.add element tree)
    | None -> Some (Tree.leaf element)


  let mem element = function
    | Some tree -> Tree.mem element tree
    | None -> false


  let singleton element = Some (Tree.leaf element)

  let remove element = function
    | Some tree -> Tree.remove element tree
    | None -> None


  let union left right =
    match left, right with
    | Some left_tree, Some right_tree -> Some (Tree.union left_tree right_tree)
    | _, None -> left
    | None, _ -> right


  let inter left right =
    match left, right with
    | Some left_tree, Some right_tree -> Tree.inter left_tree right_tree
    | _ -> None


  let diff set to_remove =
    match set, to_remove with
    | Some set_tree, Some to_remove_tree -> Tree.diff set_tree to_remove_tree
    | _ -> set


  let equal left right =
    match left, right with
    | Some left_tree, Some right_tree -> Tree.equal left_tree right_tree
    | None, None -> true
    | _ -> false


  let subset left right =
    match left, right with
    | Some left_tree, Some right_tree -> Tree.subset left_tree right_tree
    | Some _, None -> false
    | None, _ -> true


  let iter f = function
    | Some tree -> Tree.iter f tree
    | None -> ()


  let map f = function
    | Some tree -> Tree.map f tree
    | None -> None


  let fold f set init =
    match set with
    | Some tree -> Tree.fold f tree init
    | None -> init


  let for_all predicate = function
    | Some tree -> Tree.for_all predicate tree
    | None -> true


  let exists predicate = function
    | Some tree -> Tree.exists predicate tree
    | None -> false


  let filter predicate = function
    | Some tree -> Tree.filter predicate tree
    | None -> None


  let cardinal = function
    | Some tree -> Tree.cardinal tree
    | None -> 0


  let elements set = fold (fun element result -> element :: result) set []

  let of_list = List.fold ~f:(fun result element -> add element result) ~init:empty
end

module PatriciaTreeIntSet = Make (struct
  type t = int [@@deriving show]

  let equal = Int.equal

  let compare = Int.compare

  let is_zero x = Int.equal 0 x

  let add_one x = x + 1

  let subtract_one x = x - 1

  let bitwise_and = ( land )

  let bitwise_xor = ( lxor )

  let bitwise_not = lnot
end)
