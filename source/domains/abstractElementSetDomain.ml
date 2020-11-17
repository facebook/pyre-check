(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore

module type ELEMENT = sig
  include AbstractSetDomain.ELEMENT

  val less_or_equal : left:t -> right:t -> bool

  val widen : t list -> t list
end

(* A set of abstract elements where elements can be related *)
module Make (Element : ELEMENT) = struct
  module Set = Set.Make (Element)
  include Set

  let bottom = Set.empty

  let is_bottom = Set.is_empty

  let is_subsumed left set = Set.exists (fun right -> Element.less_or_equal ~left ~right) set

  let add to_add set =
    if is_subsumed to_add set then
      set
    else
      let try_remove existing set =
        if Element.less_or_equal ~left:existing ~right:to_add then
          Set.remove existing set
        else
          set
      in
      Set.add to_add (Set.fold try_remove set set)


  let join left right =
    if left == right then
      left
    else
      let join1 = Set.fold add right left in
      let join2 = Set.fold add left right in
      Set.inter join1 join2


  let widen ~iteration:_ ~prev ~next = join prev next |> Set.elements |> Element.widen |> of_list

  let less_or_equal ~left ~right = Set.for_all (fun element -> is_subsumed element right) left

  let subtract to_remove ~from =
    let keep_non_subsumed element = not (is_subsumed element to_remove) in
    Set.filter keep_non_subsumed from


  let of_list elements = ListLabels.fold_left ~f:(fun set elt -> add elt set) elements ~init:bottom

  let show set =
    Set.elements set
    |> ListLabels.map ~f:Element.show
    |> String.concat ", "
    |> Format.sprintf "[%s]"


  let pp formatter map = Format.fprintf formatter "%s" (show map)

  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join

    let less_or_equal = less_or_equal
  end

  module C = Common (CommonArg)

  type _ part +=
    | Self = C.Self | Element : Element.t AbstractDomainCore.part | Set : Element.t list part

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (set : t) : b =
    match part with
    | Element -> fold f set init
    | Set -> f (Set.elements set) init
    | _ -> C.fold part ~f ~init set


  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t set ->
    match part, t with
    | Element, Map f ->
        elements set
        |> ListLabels.fold_left ~f:(fun result element -> add (f element) result) ~init:empty
    | Element, Add e -> add e set
    | Element, Filter f -> Set.filter f set
    | Set, Map f -> f (Set.elements set) |> of_list
    | Set, Add l -> ListLabels.fold_left l ~f:(fun result element -> add element result) ~init:set
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
      | Some set -> add element set
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
    ListLabels.fold_left ~f:create_part parts ~init:bottom


  let introspect (type a) (op : a introspect) : a =
    match op with
    | GetParts f ->
        f#report C.Self;
        f#report Element;
        f#report Set
    | Structure -> [Format.sprintf "AbstractElementSet(%s)" Element.name]
    | Name part -> (
        match part with
        | Element -> Format.sprintf "AbstractElementSet(%s).Element" Element.name
        | Set -> Format.sprintf "AbstractElementSet(%s).Set" Element.name
        | Self -> Format.sprintf "AbstractElementSet(%s).Self" Element.name
        | _ -> C.introspect op )


  let meet = C.meet
end
