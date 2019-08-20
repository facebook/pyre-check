(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Implements an abstract set that computes both an over- and an under-approximation. The
   under-approximation comes into place during joins are by explicit transforms. *)

open Core
open AbstractDomain

module Make (Element : Set.Elt) = struct
  module Set = struct
    module ElementSet = Set.Make (Element)
    include ElementSet.Tree

    let show set = Sexp.to_string [%message (set : t)]

    let pp formatter set = Format.fprintf formatter "%s" (show set)
  end

  (* Implicitly under <= over at all times. Note that Bottom is different from ({}, {}) since ({},
     {}) is not less or equal to e.g. ({a}, {a}) *)
  type t =
    | Bottom
    | BiSet of {
        over: Set.t;
        under: Set.t;
      }
  [@@deriving show, sexp]

  let bottom = Bottom

  let empty = BiSet { over = Set.empty; under = Set.empty }

  let is_bottom biset = biset = Bottom

  let join left right =
    match left, right with
    | Bottom, _ -> right
    | _, Bottom -> left
    | ( BiSet { over = left_over; under = left_under },
        BiSet { over = right_over; under = right_under } ) ->
        BiSet { over = Set.union left_over right_over; under = Set.inter left_under right_under }


  let widen ~iteration:_ ~previous ~next = join previous next

  let less_or_equal ~left ~right =
    match left, right with
    | Bottom, _ -> true
    | _, Bottom -> false
    | ( BiSet { over = left_over; under = left_under },
        BiSet { over = right_over; under = right_under } ) ->
        Set.is_subset left_over ~of_:right_over && Set.is_subset right_under ~of_:left_under


  let subtract to_remove ~from =
    match from, to_remove with
    | Bottom, _ -> Bottom
    | _, Bottom -> from
    | BiSet { over; under }, BiSet { over = remove_over; under = _remove_under } ->
        let over = Set.diff over remove_over in
        BiSet { over; under }


  type element = {
    element: Element.t;
    in_under: bool;
  }
  [@@deriving compare]

  type _ part +=
    | Element : Element.t part
    | Set : Element.t list part
    | ElementAndUnder : element part
    | SetAndUnder : element list part

  let element element = { element; in_under = true }

  let element_of under element = { element; in_under = Set.mem under element }

  let fold (type a b) (part : a part) ~(f : b -> a -> b) ~(init : b) set : b =
    match set with
    | Bottom -> init
    | BiSet { over; under } -> (
        let element_of = element_of under in
        match part with
        | ElementAndUnder ->
            let f accumulator element = f accumulator (element_of element) in
            Set.fold ~f ~init over
        | SetAndUnder ->
            let elements = Set.elements over |> List.map ~f:element_of in
            f init elements
        | Element -> Set.fold ~f ~init over
        | Set ->
            let elements = Set.elements over in
            f init elements
        | _ ->
            Obj.extension_constructor part
            |> Obj.extension_name
            |> Format.sprintf "Unknown part %s in fold"
            |> failwith )


  let add set element =
    match set with
    | Bottom ->
        let singleton = Set.singleton element in
        BiSet { over = singleton; under = singleton }
    | BiSet { over; under } -> BiSet { over = Set.add over element; under = Set.add under element }


  let add_element set { element; in_under } =
    match set with
    | Bottom ->
        let singleton = Set.singleton element in
        let under = if in_under then singleton else Set.empty in
        BiSet { over = singleton; under }
    | BiSet { over; under } ->
        let over = Set.add over element in
        let under = if in_under then Set.add under element else under in
        BiSet { over; under }


  let transform (type a) (part : a part) ~(f : a -> a) set =
    match set with
    | Bottom -> (
      match part with
      | SetAndUnder -> f [] |> List.fold ~f:add_element ~init:bottom
      | Set -> f [] |> List.fold ~f:add ~init:bottom
      | Element
      | ElementAndUnder ->
          Bottom
      | _ ->
          Obj.extension_constructor part
          |> Obj.extension_name
          |> Format.sprintf "Unknown part %s in transform"
          |> failwith )
    | BiSet { over; under } -> (
        let element_of = element_of under in
        match part with
        | ElementAndUnder ->
            let f set element = add_element set (f (element_of element)) in
            Set.fold ~f ~init:bottom over
        | SetAndUnder ->
            let elements = Set.elements over |> List.map ~f:element_of in
            f elements |> List.fold ~f:add_element ~init:bottom
        | Element ->
            let f set element = add set (f element) in
            Set.fold ~f ~init:bottom over
        | Set ->
            let elements = Set.elements over in
            f elements |> List.fold ~f:add ~init:bottom
        | _ ->
            Obj.extension_constructor part
            |> Obj.extension_name
            |> Format.sprintf "Unknown part %s in transform"
            |> failwith )


  let elements = function
    | Bottom -> []
    | BiSet { over; under } ->
        let element_of = element_of under in
        Set.elements over |> List.map ~f:element_of


  let partition (type a b) (part : a part) ~(f : a -> b) set =
    match set with
    | Bottom -> Map.Poly.empty
    | BiSet { over; under } -> (
        let element_of = element_of under in
        let update_element element = function
          | None -> add_element bottom element
          | Some set -> add_element set element
        in
        let update element = function
          | None -> add bottom element
          | Some set -> add set element
        in
        match part with
        | ElementAndUnder ->
            let f result element =
              let element = element_of element in
              let key = f element in
              Map.Poly.update result key ~f:(update_element element)
            in
            Set.fold over ~f ~init:Map.Poly.empty
        | SetAndUnder ->
            let elements = Set.elements over |> List.map ~f:element_of in
            Map.Poly.singleton (f elements) set
        | Element ->
            let f result element =
              let key = f element in
              Map.Poly.update result key ~f:(update element)
            in
            Set.fold over ~f ~init:Map.Poly.empty
        | Set ->
            let elements = Set.elements over in
            Map.Poly.singleton (f elements) set
        | _ ->
            Obj.extension_constructor part
            |> Obj.extension_name
            |> Format.sprintf "Unknown part %s in partition"
            |> failwith )


  let of_list elements = List.fold ~f:add_element ~init:empty elements

  let singleton element =
    let singleton = Set.singleton element in
    BiSet { over = singleton; under = singleton }


  let create parts =
    let create_part so_far (Part (part, value)) =
      match part with
      | SetAndUnder -> join so_far (of_list value)
      | ElementAndUnder -> add_element so_far value
      | Set ->
          let elements = Set.of_list value in
          join so_far (BiSet { over = elements; under = elements })
      | Element -> add so_far value
      | _ ->
          Obj.extension_constructor part
          |> Obj.extension_name
          |> Format.sprintf "Unknown part %s in transform"
          |> failwith
    in
    List.fold parts ~f:create_part ~init:bottom
end
