(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open AbstractDomain


module type ELEMENT_DOMAIN = sig
  type t
  [@@deriving show, compare, sexp]

  val less_or_equal: left: t -> right: t -> bool

  val widen: t list -> t list
end


(* A set of abstract elements where elements can be related *)
module Make(Element : ELEMENT_DOMAIN) = struct
  module Set = struct
    module ElementSet = Set.Make(Element)
    include ElementSet.Tree
  end

  include Set

  let bottom =
    Set.empty

  let is_bottom =
    Set.is_empty

  let is_subsumed left set =
    Set.exists ~f:(fun right -> Element.less_or_equal ~left ~right) set

  let add set to_add =
    if is_subsumed to_add set then
      set
    else
      let try_remove set existing =
        if Element.less_or_equal ~left:existing ~right:to_add then
          Set.remove set existing
        else
          set
      in
      Set.add
        (Set.fold ~init:set set ~f:try_remove)
        to_add

  let join left right =
    let join1 = Set.fold ~f:add ~init:left right in
    let join2 = Set.fold ~f:add ~init:right left in
    Set.inter join1 join2

  let widen ~iteration:_ ~previous ~next =
    join previous next
    |> Set.to_list
    |> Element.widen
    |> of_list

  let less_or_equal ~left ~right =
    Set.for_all left ~f:(fun element -> is_subsumed element right)

  let subtract to_remove ~from =
    let keep_non_subsumed element =
      not (is_subsumed element to_remove)
    in
    Set.filter ~f:keep_non_subsumed from

  let of_list elements =
    List.fold elements ~f:add ~init:bottom

  let show set =
    Set.elements set
    |> List.map ~f:Element.show
    |> String.concat ~sep:", "
    |> Format.sprintf "[%s]"

  let pp formatter map =
    Format.fprintf
      formatter
      "%s"
      (show map)

  type _ AbstractDomain.part +=
    | Element: Element.t AbstractDomain.part
    | Set: Element.t list part

  let fold (type a b) (part: a part) ~(f: b -> a -> b) ~(init: b) set : b =
    match part with
    | Element ->
        fold ~f ~init set
    | Set ->
        f init (Set.elements set)
    | _ ->
        Obj.extension_constructor part
        |> Obj.extension_name
        |> Format.sprintf "Unknown part %s in fold"
        |> failwith

  let transform (type a) (part: a part) ~(f: a -> a) set =
    match part with
    | Element ->
        elements set
        |> List.fold ~f:(fun result element -> add result (f element)) ~init:empty
    | Set ->
        f (Set.elements set) |> of_list
    | _ ->
        Obj.extension_constructor part
        |> Obj.extension_name
        |> Format.sprintf "Unknown part %s in transform"
        |> failwith

  let partition (type a b) (part: a part) ~(f: a -> b) set =
    let update element = function
      | None -> singleton element
      | Some set -> add set element
    in
    match part with
    | Element ->
        let f result element =
          let key = f element in
          Map.Poly.update result key ~f:(update element)
        in
        Set.fold set ~f ~init:Map.Poly.empty
    | Set ->
        Map.Poly.singleton (f (Set.elements set)) set
    | _ ->
        Obj.extension_constructor part
        |> Obj.extension_name
        |> Format.sprintf "Unknown part %s in partition"
        |> failwith

  let create parts =
    let create_part so_far (Part (part, value)) =
      match part with
      | Set ->
          join so_far (of_list value)
      | Element ->
          add so_far value
      | _ ->
        Obj.extension_constructor part
        |> Obj.extension_name
        |> Format.sprintf "Unknown part %s in transform"
        |> failwith
    in
    List.fold parts ~f:create_part ~init:bottom
end
