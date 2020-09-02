(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore

module type ELEMENT = sig
  include AbstractSetDomain.ELEMENT
end

(** A set starting out as the Universe of objects and joins cause it to shrink by intersection **)
module Make (Element : ELEMENT) = struct
  module Set = Set.Make (Element)

  type t =
    | Universe
    | InvertedSet of Set.t

  let bottom = Universe

  let is_bottom s = s = Universe

  let join left right =
    (* logically intersection *)
    match left, right with
    | Universe, _ -> right
    | _, Universe -> left
    | InvertedSet left, InvertedSet right -> InvertedSet (Set.inter left right)


  let widen ~iteration:_ ~prev ~next = join prev next

  let less_or_equal ~left ~right =
    match left, right with
    | Universe, _ -> true
    | _, Universe -> false
    | InvertedSet left, InvertedSet right -> Set.subset right left


  let subtract _to_remove ~from = from

  let show = function
    | Universe -> "bottom"
    | InvertedSet set ->
        Set.elements set
        |> ListLabels.map ~f:Element.show
        |> String.concat ", "
        |> Format.sprintf "[%s]"


  let pp formatter set = Format.fprintf formatter "%s" (show set)

  type elements = {
    is_universe: bool;
    elements: Element.t list;
  }

  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join
  end

  module C = Common (CommonArg)

  type _ part += Self = C.Self | Element : Element.t part | Set : elements part

  let singleton element =
    let singleton = Set.singleton element in
    InvertedSet singleton


  let add element set =
    match set with
    | Universe -> singleton element
    | InvertedSet set -> InvertedSet (Set.add element set)


  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (set : t) : b =
    match part with
    | Element -> (
        match set with
        | Universe -> init
        | InvertedSet set -> Set.fold f set init )
    | Set -> (
        match set with
        | Universe -> f { is_universe = true; elements = [] } init
        | InvertedSet set -> f { is_universe = false; elements = Set.elements set } init )
    | _ -> C.fold part ~f ~init set


  let elements = function
    | Universe -> { is_universe = true; elements = [] }
    | InvertedSet set -> { is_universe = false; elements = Set.elements set }


  let of_elements set =
    if set.is_universe then
      Universe
    else
      InvertedSet (Set.of_list set.elements)


  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t set ->
    match part, t with
    | Element, Map f -> (
        match set with
        | Universe -> Universe
        | InvertedSet set ->
            Set.elements set
            |> ListLabels.fold_left ~f:(fun result element -> add (f element) result) ~init:bottom )
    | Element, Add a -> add a set
    | Element, Filter f -> (
        match set with
        | Universe -> Universe
        | InvertedSet set -> InvertedSet (Set.filter f set) )
    | Set, Map f -> elements set |> f |> of_elements
    | Set, Add { is_universe; elements = new_elements } -> (
        if is_universe then
          Universe
        else
          match set with
          | Universe -> Universe
          | InvertedSet set -> InvertedSet (Set.union set (Set.of_list new_elements)) )
    | Set, Filter f ->
        if f (elements set) then
          set
        else
          bottom
    | _ -> C.transform transformer part t set


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let partition (type a b) (part : a part) ~(f : a -> b option) (set : t) =
    let update element = function
      | None -> singleton element
      | Some set -> add element set
    in
    match part with
    | Element -> (
        match set with
        | Universe -> Core_kernel.Map.Poly.empty
        | InvertedSet set ->
            let f element result =
              match f element with
              | None -> result
              | Some key -> Core_kernel.Map.Poly.update result key ~f:(update element)
            in
            Set.fold f set Core_kernel.Map.Poly.empty )
    | Set -> (
        match f (elements set) with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key set )
    | _ -> C.partition part ~f set


  let create parts =
    let create_part so_far (Part (part, value)) =
      match part with
      | Set -> join so_far (of_elements value)
      | Element -> add value so_far
      | _ -> C.create part value so_far
    in
    ListLabels.fold_left parts ~f:create_part ~init:bottom


  let introspect (type a) (op : a introspect) : a =
    match op with
    | GetParts f ->
        f#report C.Self;
        f#report Element;
        f#report Set
    | Structure -> [Format.sprintf "InvertedSet(%s)" Element.name]
    | Name part -> (
        match part with
        | Element -> Format.sprintf "InvertedSet(%s).Element" Element.name
        | Set -> Format.sprintf "InvertedSet(%s).Set" Element.name
        | Self -> Format.sprintf "InvertedSet(%s).Self" Element.name
        | _ -> C.introspect op )
end
