(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Implements an abstract set that computes both an over- and an under-approximation. The
   under-approximation comes into play at joins, where the under-approximations are intersected. *)

open AbstractDomainCore

type 'a approximation = {
  element: 'a;
  in_under: bool;
}

module type S = sig
  include AbstractSetDomain.S

  type _ AbstractDomainCore.part += ElementAndUnder : element approximation AbstractDomainCore.part

  val empty : t

  val is_empty : t -> bool

  val inject : element -> element approximation

  val to_approximation : t -> element approximation list

  val of_approximation : element approximation list -> t

  val add_set : t -> to_add:t -> t

  val sequence_join : t -> t -> t

  val over_to_under : t -> t
end

module MakeWithSet (Set : AbstractSetDomain.SET) = struct
  type biset =
    | Bottom
    | BiSet of {
        over: Set.t;
        under: Set.t;
      }

  module rec Base : (BASE with type t := biset) = MakeBase (Domain)

  and Domain : (S with type t = biset and type element = Set.element) = struct
    (* Implicitly under <= over at all times. Note that Bottom is different from ({}, {}) since ({},
       {}) is not less or equal to e.g. ({a}, {a}) *)
    type t = biset

    type element = Set.element

    type _ part +=
      | Self : t part
      | Element : Set.element part
      | ElementAndUnder : Set.element approximation part

    let bottom = Bottom

    let empty = BiSet { over = Set.empty; under = Set.empty }

    let is_bottom biset = biset = Bottom

    let inject element = { element; in_under = true }

    let element_of ~under element = { element; in_under = Set.mem element under }

    let elements = function
      | Bottom -> []
      | BiSet { over; _ } -> Set.elements over


    let to_approximation = function
      | Bottom -> []
      | BiSet { over; under } ->
          let element_of = element_of ~under in
          Set.elements over |> ListLabels.map ~f:element_of


    let show_approximation { element; in_under } =
      let element_value = Set.show_element element in
      if in_under then
        Format.sprintf "%s" element_value
      else
        Format.sprintf "%s(-)" element_value


    let show set =
      match set with
      | Bottom -> "<bottom>"
      | _ ->
          to_approximation set
          |> ListLabels.map ~f:show_approximation
          |> String.concat ", "
          |> Format.sprintf "[%s]"


    let pp formatter set = Format.fprintf formatter "%s" (show set)

    let join left right =
      if left == right then
        left
      else
        match left, right with
        | Bottom, _ -> right
        | _, Bottom -> left
        | ( BiSet { over = left_over; under = left_under },
            BiSet { over = right_over; under = right_under } ) ->
            BiSet
              { over = Set.union left_over right_over; under = Set.inter left_under right_under }


    let make ~old ~over ~under =
      match old with
      | BiSet { over = old_over; under = old_under } ->
          if old_over == over && old_under == under then
            old
          else
            BiSet { over; under }
      | Bottom -> BiSet { over; under }


    (* Add every element of ~to_add to the existing set. The element is present in the under
       approximation if it is present in either. *)
    let add_set set ~to_add =
      match set, to_add with
      | Bottom, _ -> to_add
      | _, Bottom -> set
      | BiSet { over; under }, BiSet { over = to_add_over; under = to_add_under } ->
          let over = Set.union over to_add_over in
          let under = Set.union under to_add_under in
          make ~old:set ~over ~under


    (* Logically, this is a union with point-wise meet of whether the element is in the
       under-approximation *)
    let add_element set { element; in_under } =
      match set with
      | Bottom ->
          let singleton = Set.singleton element in
          let under =
            if in_under then
              singleton
            else
              Set.empty
          in
          BiSet { over = singleton; under }
      | BiSet { over; under } ->
          let over = Set.add element over in
          let under =
            if in_under then
              Set.add element under
            else
              under
          in
          make ~old:set ~over ~under


    let add set element = add_element set { element; in_under = true }

    let of_list elements = ListLabels.fold_left ~f:add elements ~init:bottom

    let of_approximation elements = ListLabels.fold_left ~f:add_element ~init:empty elements

    let over_to_under set =
      match set with
      | Bottom -> Bottom
      | BiSet { over; _ } -> make ~old:set ~over ~under:over


    let widen ~iteration:_ ~prev ~next = join prev next

    let less_or_equal ~left ~right =
      if left == right then
        true
      else
        match left, right with
        | Bottom, _ -> true
        | _, Bottom -> false
        | ( BiSet { over = left_over; under = left_under },
            BiSet { over = right_over; under = right_under } ) ->
            if left_over == right_over && left_under == right_under then
              true
            else
              Set.subset left_over right_over && Set.subset right_under left_under


    let subtract to_remove ~from =
      if to_remove == from then
        bottom
      else
        match from, to_remove with
        | Bottom, _ -> Bottom
        | _, Bottom -> from
        | BiSet { over; under }, BiSet { over = remove_over; under = remove_under } ->
            let over = Set.diff over remove_over in
            if Set.is_empty over && Set.equal under remove_under then
              Bottom
            else
              make ~old:from ~over:(Set.union over under) ~under


    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f set ->
      match part, op, set with
      | Element, Map, Bottom -> set
      | Element, Map, BiSet _ ->
          to_approximation set
          |> ListLabels.map ~f:(fun e -> { e with element = f e.element })
          |> of_approximation
      | Element, Expand, Bottom -> set
      | Element, Expand, BiSet _ ->
          to_approximation set
          |> ListLabels.map ~f:(fun e ->
                 ListLabels.map (f e.element) ~f:(fun v -> { e with element = v }))
          |> ListLabels.flatten
          |> of_approximation
      | Element, Add, _ -> add set f
      | Element, Filter, Bottom -> set
      | Element, Filter, BiSet { over; under } ->
          let over = Set.filter f over in
          let under = Set.filter f under in
          make ~old:set ~over ~under
      | Element, FilterMap, Bottom -> set
      | Element, FilterMap, BiSet _ ->
          to_approximation set
          |> ListLabels.filter_map ~f:(fun e ->
                 match f e.element with
                 | Some element -> Some { e with element }
                 | None -> None)
          |> of_approximation
      | ElementAndUnder, Map, Bottom -> set
      | ElementAndUnder, Map, BiSet _ ->
          to_approximation set |> ListLabels.map ~f |> of_approximation
      | ElementAndUnder, Expand, Bottom -> set
      | ElementAndUnder, Expand, BiSet _ ->
          to_approximation set |> ListLabels.map ~f |> ListLabels.flatten |> of_approximation
      | ElementAndUnder, Add, _ -> add_element set f
      | ElementAndUnder, Filter, Bottom -> Bottom
      | ElementAndUnder, Filter, BiSet _ ->
          to_approximation set |> ListLabels.filter ~f |> of_approximation
      | ElementAndUnder, FilterMap, Bottom -> set
      | ElementAndUnder, FilterMap, BiSet _ ->
          to_approximation set |> ListLabels.filter_map ~f |> of_approximation
      | Self, Add, _ ->
          (* Special handling of Add here, as we don't want to use the join of the common
             implementation. *)
          add_set set ~to_add:f
      | _ -> Base.transform part op ~f set


    let reduce
        : type a f b. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init set ->
      match part, op, set with
      | Element, Acc, Bottom -> init
      | Element, Acc, BiSet { over; _ } -> Set.fold f over init
      | Element, Exists, Bottom -> init
      | Element, Exists, BiSet { over; _ } -> init || Set.exists f over
      | ElementAndUnder, Acc, Bottom -> init
      | ElementAndUnder, Acc, BiSet { over; under } ->
          let element_of = element_of ~under in
          let f element accumulator = f (element_of element) accumulator in
          Set.fold f over init
      | ElementAndUnder, Exists, Bottom -> init
      | ElementAndUnder, Exists, BiSet { over; under } ->
          init
          ||
          let element_of = element_of ~under in
          let f element = f (element_of element) in
          Set.exists f over
      | _ -> Base.reduce part ~using:op ~f ~init set


    let partition
        : type a f b.
          a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) Core_kernel.Map.Poly.t
      =
     fun part op ~f set ->
      let update_element element = function
        | None -> add_element bottom element
        | Some set -> add_element set element
      in
      match part, op, set with
      | (Element | ElementAndUnder), By, Bottom -> Core_kernel.Map.Poly.empty
      | (Element | ElementAndUnder), ByFilter, Bottom -> Core_kernel.Map.Poly.empty
      | Element, By, _ ->
          let f result element =
            let key = f element.element in
            Core_kernel.Map.Poly.update result key ~f:(update_element element)
          in
          to_approximation set |> ListLabels.fold_left ~f ~init:Core_kernel.Map.Poly.empty
      | ElementAndUnder, By, BiSet { over; under } ->
          let element_of = element_of ~under in
          let f element result =
            let element = element_of element in
            let key = f element in
            Core_kernel.Map.Poly.update result key ~f:(update_element element)
          in
          Set.fold f over Core_kernel.Map.Poly.empty
      | Element, ByFilter, _ ->
          let f result element =
            match f element.element with
            | Some key -> Core_kernel.Map.Poly.update result key ~f:(update_element element)
            | None -> result
          in
          to_approximation set |> ListLabels.fold_left ~f ~init:Core_kernel.Map.Poly.empty
      | ElementAndUnder, ByFilter, BiSet { over; under } ->
          let element_of = element_of ~under in
          let f element result =
            let element = element_of element in
            match f element with
            | Some key -> Core_kernel.Map.Poly.update result key ~f:(update_element element)
            | None -> result
          in
          Set.fold f over Core_kernel.Map.Poly.empty
      | _ -> Base.partition part op ~f set


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
          f#report Element;
          f#report ElementAndUnder
      | Structure -> [Format.sprintf "OverAndUnderSet(%s)" Set.element_name]
      | Name part -> (
          match part with
          | Element -> Format.sprintf "OverAndUnderSet(%s).Element" Set.element_name
          | ElementAndUnder -> Format.sprintf "OverAndUnderSet(%s).ElementAndUnder" Set.element_name
          | Self -> Format.sprintf "OverAndUnderSet(%s).Self" Set.element_name
          | _ -> Base.introspect op)


    let create parts : t =
      let create_part so_far (Part (part, value)) =
        match part with
        | ElementAndUnder -> add_element so_far value
        | Element -> add so_far value
        | Self -> add_set so_far ~to_add:(value : t)
        | _ -> Base.create part value so_far
      in
      ListLabels.fold_left parts ~f:create_part ~init:bottom


    let sequence_join left right =
      if left == right then
        left
      else
        match left, right with
        | Bottom, _ -> right
        | _, Bottom -> left
        | ( BiSet { over = left_over; under = left_under },
            BiSet { over = right_over; under = right_under } ) ->
            let over = Set.union left_over right_over in
            let under = Set.union left_under right_under in
            make ~old:left ~over ~under


    let is_empty = function
      | Bottom -> true
      | BiSet { over; _ } -> Set.is_empty over


    let singleton element =
      let singleton = Set.singleton element in
      BiSet { over = singleton; under = singleton }


    let remove element = function
      | Bottom -> Bottom
      | BiSet { over; under } ->
          BiSet { over = Set.remove element over; under = Set.remove element under }


    let add element set = add set element

    let contains element = function
      | Bottom -> false
      | BiSet { over; _ } -> Set.mem element over


    let meet = Base.meet

    let fold = Base.fold

    let apply = Base.apply
  end

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end

module Make (Element : AbstractSetDomain.ELEMENT) = struct
  module Set = struct
    include Set.Make (Element)

    type element = Element.t

    let show_element = Element.show

    let element_name = Element.name
  end

  include MakeWithSet (Set)
end
