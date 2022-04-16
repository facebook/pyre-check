(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

  type inverted =
    | Universe
    | InvertedSet of Set.t

  type elements = {
    is_universe: bool;
    elements: Element.t list;
  }

  type _ part += Element : Element.t part

  let singleton element =
    let singleton = Set.singleton element in
    InvertedSet singleton


  let add element set =
    match set with
    | Universe -> singleton element
    | InvertedSet set -> InvertedSet (Set.add element set)


  let elements = function
    | Universe -> { is_universe = true; elements = [] }
    | InvertedSet set -> { is_universe = false; elements = Set.elements set }


  let of_elements set =
    if set.is_universe then
      Universe
    else
      InvertedSet (Set.of_list set.elements)


  module rec Base : (BASE with type t := inverted) = MakeBase (Domain)

  and Domain : (S with type t = inverted) = struct
    type t = inverted

    type _ part += Self : t part

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

    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f set ->
      match part, op with
      | Element, Map -> (
          match set with
          | Universe -> Universe
          | InvertedSet set ->
              Set.elements set
              |> ListLabels.fold_left ~f:(fun result element -> add (f element) result) ~init:bottom
          )
      | Element, Add -> add f set
      | Element, Filter -> (
          match set with
          | Universe -> Universe
          | InvertedSet set -> InvertedSet (Set.filter f set))
      | Element, FilterMap -> (
          match set with
          | Universe -> Universe
          | InvertedSet set ->
              Set.elements set
              |> ListLabels.fold_left
                   ~f:(fun result element ->
                     match f element with
                     | Some element -> add element result
                     | None -> result)
                   ~init:bottom)
      | _ -> Base.transform part op ~f set


    let reduce
        : type a f b. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init set ->
      match part, op with
      | Element, Acc -> (
          match set with
          | Universe -> init
          | InvertedSet set -> Set.fold f set init)
      | Element, Exists -> (
          match set with
          | Universe -> init
          | InvertedSet set -> init || Set.exists f set)
      | _ -> Base.reduce part ~using:op ~f ~init set


    let partition
        : type a f b.
          a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) Core_kernel.Map.Poly.t
      =
     fun part op ~f set ->
      let update element = function
        | None -> singleton element
        | Some set -> add element set
      in
      match part, op with
      | Element, By -> (
          match set with
          | Universe -> Core_kernel.Map.Poly.empty
          | InvertedSet set ->
              let f element result =
                let key = f element in
                Core_kernel.Map.Poly.update result key ~f:(update element)
              in
              Set.fold f set Core_kernel.Map.Poly.empty)
      | Element, ByFilter -> (
          match set with
          | Universe -> Core_kernel.Map.Poly.empty
          | InvertedSet set ->
              let f element result =
                match f element with
                | None -> result
                | Some key -> Core_kernel.Map.Poly.update result key ~f:(update element)
              in
              Set.fold f set Core_kernel.Map.Poly.empty)
      | _ -> Base.partition part op ~f set


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
          f#report Element
      | Structure -> [Format.sprintf "InvertedSet(%s)" Element.name]
      | Name part -> (
          match part with
          | Element -> Format.sprintf "InvertedSet(%s).Element" Element.name
          | Self -> Format.sprintf "InvertedSet(%s).Self" Element.name
          | _ -> Base.introspect op)


    let create parts =
      let create_part so_far (Part (part, value)) =
        match part with
        | Element -> add value so_far
        | _ -> Base.create part value so_far
      in
      ListLabels.fold_left parts ~f:create_part ~init:bottom


    let fold = Base.fold

    let meet = Base.meet

    let apply = Base.apply
  end

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
