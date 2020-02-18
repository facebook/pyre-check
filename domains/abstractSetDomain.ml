(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open AbstractDomainCore

module type ELEMENT = sig
  type t [@@deriving show]

  val name : string

  val compare : t -> t -> int
end

module type S = sig
  include AbstractDomainCore.S

  type element

  type _ AbstractDomainCore.part +=
    | Element : element AbstractDomainCore.part | Set : element list AbstractDomainCore.part

  val add : element -> t -> t

  val remove : element -> t -> t

  val singleton : element -> t

  val elements : t -> element list

  val of_list : element list -> t
end

module Make (Element : ELEMENT) = struct
  module Set = Set.Make (Element)
  include Set

  type element = Element.t

  let bottom = Set.empty

  let is_bottom = Set.is_empty

  let join left right =
    if left == right then
      left
    else
      Set.union left right


  let widen ~iteration:_ ~prev ~next = join prev next

  let less_or_equal ~left ~right =
    if left == right || is_bottom left then
      true
    else if is_bottom right then
      false
    else
      Set.subset left right


  let subtract to_remove ~from =
    if to_remove == from || is_bottom from then
      bottom
    else if is_bottom to_remove then
      from
    else
      Set.diff from to_remove


  let show set =
    Set.elements set
    |> ListLabels.map ~f:Element.show
    |> String.concat ", "
    |> Format.sprintf "[%s]"


  let pp formatter set = Format.fprintf formatter "%s" (show set)

  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join
  end

  module C = Common (CommonArg)

  type _ part += Self = C.Self | Element : Element.t part | Set : Element.t list part

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (set : t) : b =
    match part with
    | Element -> fold f set init
    | Set -> f (Set.elements set) init
    | _ -> C.fold part ~f ~init set


  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t set ->
    match part, t with
    | Element, Map f -> Set.map f set
    | Element, Add a -> add a set
    | Element, Filter f -> Set.filter f set
    | Set, Map f -> f (Set.elements set) |> Set.of_list
    | Set, Add a -> Set.elements set |> List.rev_append a |> Set.of_list
    | Set, Filter f ->
        if f (Set.elements set) then
          set
        else
          bottom
    | _ -> C.transform transformer part t set


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let partition (type a b) (part : a part) ~(f : a -> b option) (set : t) =
    let update element = function
      | None -> singleton element
      | Some set -> Set.add element set
    in
    match part with
    | Element ->
        let f element result =
          match f element with
          | None -> result
          | Some key -> Core_kernel.Map.Poly.update result key ~f:(update element)
        in
        Set.fold f set Core_kernel.Map.Poly.empty
    | Set -> (
        match f (Set.elements set) with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key set )
    | _ -> C.partition part ~f set


  let create parts =
    let create_part so_far (Part (part, value)) =
      match part with
      | Set -> join so_far (of_list value)
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
    | Structure -> [Format.sprintf "Set(%s)" Element.name]
    | Name part -> (
        match part with
        | Element -> Format.sprintf "Set(%s).Element" Element.name
        | Set -> Format.sprintf "Set(%s).Set" Element.name
        | Self -> Format.sprintf "Set(%s).Self" Element.name
        | _ -> C.introspect op )
end
