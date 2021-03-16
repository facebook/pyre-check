(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

  type _ AbstractDomainCore.part +=
    | ElementAndUnder : element approximation AbstractDomainCore.part
    | SetAndUnder : element approximation list AbstractDomainCore.part

  val empty : t

  val inject : element -> element approximation

  val to_approximation : t -> element approximation list

  val of_approximation : element approximation list -> t

  val add_set : t -> to_add:t -> t

  val sequence_join : t -> t -> t
end

module Make (Element : AbstractSetDomain.ELEMENT) = struct
  module Set = Set.Make (Element)

  type element = Element.t

  (* Implicitly under <= over at all times. Note that Bottom is different from ({}, {}) since ({},
     {}) is not less or equal to e.g. ({a}, {a}) *)
  type t =
    | Bottom
    | BiSet of {
        over: Set.t;
        under: Set.t;
      }

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
    let element_value = Element.show element in
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
          BiSet { over = Set.union left_over right_over; under = Set.inter left_under right_under }


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


  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join

    let less_or_equal = less_or_equal
  end

  module C = Common (CommonArg)

  type _ part +=
    | Self = C.Self
    | Element : Element.t part
    | ElementAndUnder : Element.t approximation part
    | Set : Element.t list part
    | SetAndUnder : Element.t approximation list part

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) set : b =
    match part, set with
    | Element, Bottom -> init
    | Element, BiSet { over; _ } -> Set.fold f over init
    | Set, _ -> f (elements set) init
    | ElementAndUnder, Bottom -> init
    | ElementAndUnder, BiSet { over; under } ->
        let element_of = element_of ~under in
        let f element accumulator = f (element_of element) accumulator in
        Set.fold f over init
    | SetAndUnder, set -> f (to_approximation set) init
    | _ -> C.fold part ~f ~init set


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

  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t set ->
    match part, t, set with
    | Element, Map _f, Bottom -> set
    | Element, Map f, BiSet _ ->
        to_approximation set
        |> ListLabels.map ~f:(fun e -> { e with element = f e.element })
        |> of_approximation
    | Element, Add e, _ -> add set e
    | Element, Filter _f, Bottom -> set
    | Element, Filter f, BiSet { over; under } ->
        let over = Set.filter f over in
        let under = Set.filter f under in
        make ~old:set ~over ~under
    | ElementAndUnder, Map _f, Bottom -> set
    | ElementAndUnder, Map f, BiSet _ ->
        to_approximation set |> ListLabels.map ~f |> of_approximation
    | ElementAndUnder, Add e, _ -> add_element set e
    | ElementAndUnder, Filter _f, Bottom -> Bottom
    | ElementAndUnder, Filter f, BiSet _ ->
        to_approximation set |> ListLabels.filter ~f |> of_approximation
    | Set, Map f, _ -> of_list (f (elements set))
    | Set, Add l, _ -> ListLabels.fold_left ~f:add l ~init:set
    | Set, Filter f, _ ->
        if f (elements set) then
          set
        else
          bottom
    | SetAndUnder, Map f, set -> of_approximation (f (to_approximation set))
    | SetAndUnder, Add l, _ ->
        let to_add = of_approximation l in
        add_set set ~to_add
    | SetAndUnder, Filter _f, Bottom -> set
    | SetAndUnder, Filter f, _ ->
        if f (to_approximation set) then
          set
        else
          bottom
    | Self, Add to_add, _ ->
        (* Special handling of Add here, as we don't want to use the join of the common
           implementation. *)
        add_set set ~to_add
    | _ -> C.transform transformer part t set


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let partition (type a b) (part : a part) ~(f : a -> b option) (set : t) =
    let update_element element = function
      | None -> add_element bottom element
      | Some set -> add_element set element
    in
    match part, set with
    | Element, Bottom -> Core_kernel.Map.Poly.empty
    | ElementAndUnder, Bottom -> Core_kernel.Map.Poly.empty
    | Element, _ ->
        let f result element =
          match f element.element with
          | Some key -> Core_kernel.Map.Poly.update result key ~f:(update_element element)
          | None -> result
        in
        to_approximation set |> ListLabels.fold_left ~f ~init:Core_kernel.Map.Poly.empty
    | ElementAndUnder, BiSet { over; under } ->
        let element_of = element_of ~under in
        let f element result =
          let element = element_of element in
          match f element with
          | Some key -> Core_kernel.Map.Poly.update result key ~f:(update_element element)
          | None -> result
        in
        Set.fold f over Core_kernel.Map.Poly.empty
    | Set, _ -> (
        match f (elements set) with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key set )
    | SetAndUnder, _ -> (
        match f (to_approximation set) with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key set )
    | _ -> C.partition part ~f set


  let singleton element =
    let singleton = Set.singleton element in
    BiSet { over = singleton; under = singleton }


  let create parts : t =
    let create_part so_far (Part (part, value)) =
      match part with
      | ElementAndUnder -> add_element so_far value
      | Element -> add so_far value
      | Set -> ListLabels.fold_left ~f:add value ~init:so_far
      | SetAndUnder -> ListLabels.fold_left ~f:add_element value ~init:so_far
      | Self -> add_set so_far ~to_add:(value : t)
      | _ -> C.create part value so_far
    in
    ListLabels.fold_left parts ~f:create_part ~init:bottom


  let remove element = function
    | Bottom -> Bottom
    | BiSet { over; under } ->
        BiSet { over = Set.remove element over; under = Set.remove element under }


  let introspect (type a) (op : a introspect) : a =
    match op with
    | GetParts f ->
        f#report C.Self;
        f#report Element;
        f#report ElementAndUnder;
        f#report Set;
        f#report SetAndUnder
    | Structure -> [Format.sprintf "OverAndUnderSet(%s)" Element.name]
    | Name part -> (
        match part with
        | Element -> Format.sprintf "OverAndUnderSet(%s).Element" Element.name
        | Set -> Format.sprintf "OverAndUnderSet(%s).Set" Element.name
        | ElementAndUnder -> Format.sprintf "OverAndUnderSet(%s).ElementAndUnder" Element.name
        | SetAndUnder -> Format.sprintf "OverAndUnderSet(%s).SetAndUnder" Element.name
        | Self -> Format.sprintf "OverAndUnderSet(%s).Self" Element.name
        | _ -> C.introspect op )


  let add element set = add set element

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


  let meet = C.meet
end
