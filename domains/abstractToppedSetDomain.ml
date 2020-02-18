(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open AbstractDomainCore

module type ELEMENT = sig
  include AbstractSetDomain.ELEMENT

  val max_count : unit -> int
end

module Make (Element : ELEMENT) = struct
  module Set = Set.Make (Element)

  type 'a with_top =
    | Top
    | ASet of 'a

  type t = Set.t with_top

  let bottom = ASet Set.empty

  let top = Top

  let is_bottom = function
    | ASet set -> Set.is_empty set
    | Top -> false


  let make set =
    if Set.cardinal set <= Element.max_count () then
      ASet set
    else
      Top


  let make_transformed ~old ~old_topped set =
    if set == old then
      old_topped
    else
      make set


  let join left right =
    match left, right with
    | ASet left_set, ASet right_set ->
        if left_set == right_set then
          left
        else
          make (Set.union left_set right_set)
    | _ -> Top


  let widen ~iteration:_ ~prev ~next = join prev next

  let less_or_equal ~left ~right =
    match left, right with
    | _, Top -> true
    | Top, _ -> false
    | ASet left, ASet right ->
        if left == right then
          true
        else
          Set.subset left right


  let subtract to_remove ~from =
    match to_remove, from with
    | Top, _ -> bottom
    | _, Top -> Top
    | ASet to_remove, ASet from ->
        if to_remove == from || Set.is_empty from then
          bottom
        else
          ASet (Set.diff from to_remove)


  let show = function
    | Top -> "{top}"
    | ASet set ->
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

  type _ part += Self = C.Self | Element : Element.t part | Set : Element.t list with_top part

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (set : t) : b =
    match set, part with
    | Top, Element -> init
    | Top, Set -> f Top init
    | ASet set, Element -> Set.fold f set init
    | ASet set, Set -> f (ASet (Set.elements set)) init
    | _ -> C.fold part ~f ~init set


  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t topped_set ->
    match topped_set, part, t with
    | Top, Element, _ -> Top
    | ASet set, Element, Map f -> Set.map f set |> make_transformed ~old:set ~old_topped:topped_set
    | ASet set, Element, Add a -> Set.add a set |> make_transformed ~old:set ~old_topped:topped_set
    | ASet set, Element, Filter f ->
        Set.filter f set |> make_transformed ~old:set ~old_topped:topped_set
    | Top, Set, _ -> Top
    | ASet set, Set, Map f -> (
        let value = ASet (Set.elements set) in
        match f value with
        | Top -> Top
        | ASet l -> make (Set.of_list l) )
    | ASet set, Set, Add a -> (
        match a with
        | Top -> Top
        | ASet l -> make (Set.union set (Set.of_list l)) )
    | ASet set_elements, Set, Filter f ->
        if f (ASet (Set.elements set_elements)) then
          topped_set
        else
          bottom
    | _ -> C.transform transformer part t topped_set


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let singleton element = make (Set.singleton element)

  let add element = function
    | Top -> Top
    | ASet set -> make (Set.add element set)


  let elements = function
    | Top -> []
    | ASet set -> Set.elements set


  let partition (type a b) (part : a part) ~(f : a -> b option) (set : t) =
    let update element = function
      | None -> singleton element
      | Some set -> add element set
    in
    match set, part with
    | Top, Element -> failwith "Topped set cannot be partitioned by Element"
    | ASet set, Element ->
        let f element result =
          match f element with
          | None -> result
          | Some partition_key ->
              Core_kernel.Map.Poly.update result partition_key ~f:(update element)
        in
        Set.fold f set Core_kernel.Map.Poly.empty
    | _, Set -> (
        let value =
          match set with
          | Top -> Top
          | ASet set -> ASet (Set.elements set)
        in
        match f value with
        | None -> Core_kernel.Map.Poly.empty
        | Some partition_key -> Core_kernel.Map.Poly.singleton partition_key set )
    | _ -> C.partition part ~f set


  let of_list l = make (Set.of_list l)

  let create parts =
    let create_part so_far (Part (part, value)) =
      match part with
      | Set -> (
          match value with
          | Top -> Top
          | ASet l -> join so_far (of_list l) )
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
    | Structure -> [Format.sprintf "ToppedSet(%s)" Element.name]
    | Name part -> (
        match part with
        | Element -> Format.sprintf "ToppedSet(%s).Element" Element.name
        | Set -> Format.sprintf "ToppedSet(%s).Set" Element.name
        | Self -> Format.sprintf "ToppedSet(%s).Self" Element.name
        | _ -> C.introspect op )
end
