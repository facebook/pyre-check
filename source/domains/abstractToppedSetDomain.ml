(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

  let top = Top

  let make set =
    if Set.cardinal set <= Element.max_count () then
      ASet set
    else
      Top


  let singleton element = make (Set.singleton element)

  let add element = function
    | Top -> Top
    | ASet set -> make (Set.add element set)


  let elements = function
    | Top -> []
    | ASet set -> Set.elements set


  let of_list l = make (Set.of_list l)

  type _ part += Element : Element.t part | Set : Element.t list with_top part

  module rec Base : (BASE with type t := Set.t with_top) = MakeBase (Domain)

  and Domain : (S with type t = Set.t with_top) = struct
    type t = Set.t with_top

    type _ part += Self : t part

    let bottom = ASet Set.empty

    let is_bottom = function
      | ASet set -> Set.is_empty set
      | Top -> false


    let make_transformed ~old ~old_topped set =
      if set == old then
        old_topped
      else
        make set


    let join left right =
      match left, right with
      | ASet left_set, ASet right_set ->
          if left_set == right_set || Set.is_empty right_set then
            left
          else
            make (Set.union left_set right_set)
      | _ -> Top


    let meet left right =
      match left, right with
      | ASet left_set, ASet right_set ->
          if left_set == right_set then
            left
          else
            make (Set.inter left_set right_set)
      | Top, _ -> right
      | _, Top -> left


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

    let transform_new : type a f. a part -> (transform2, a, f, t, t) operation -> f:f -> t -> t =
     fun part op ~f topped_set ->
      match topped_set, part, op with
      | ASet set, Element, OpMap ->
          Set.map f set |> make_transformed ~old:set ~old_topped:topped_set
      | ASet set, Element, OpAdd ->
          Set.add f set |> make_transformed ~old:set ~old_topped:topped_set
      | ASet set, Element, OpFilter ->
          Set.filter f set |> make_transformed ~old:set ~old_topped:topped_set
      | ASet set, Set, OpMap -> (
          let value = ASet (Set.elements set) in
          match f value with
          | Top -> Top
          | ASet l -> make (Set.of_list l) )
      | ASet set, Set, OpAdd -> (
          match f with
          | Top -> Top
          | ASet l -> make (Set.union set (Set.of_list l)) )
      | ASet set_elements, Set, OpFilter ->
          if f (ASet (Set.elements set_elements)) then
            topped_set
          else
            bottom
      | Top, (Element | Set), _ -> Top
      | _ -> Base.transform part op ~f topped_set


    let reduce
        : type a f b. a part -> using:(reduce, a, f, t, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init set ->
      match set, part, op with
      | Top, Element, OpAcc -> init
      | Top, Set, OpAcc -> f Top init
      | ASet set, Element, OpAcc -> Set.fold f set init
      | ASet set, Set, OpAcc -> f (ASet (Set.elements set)) init
      | _ -> Base.reduce part ~using:op ~f ~init set


    let partition_new
        : type a f b.
          a part -> (partition, a, f, t, b) operation -> f:f -> t -> (b, t) Core_kernel.Map.Poly.t
      =
     fun part op ~f set ->
      let update element = function
        | None -> singleton element
        | Some set -> add element set
      in
      match set, part, op with
      | ASet set, Element, OpByFilter ->
          let f element result =
            match f element with
            | None -> result
            | Some partition_key ->
                Core_kernel.Map.Poly.update result partition_key ~f:(update element)
          in
          Set.fold f set Core_kernel.Map.Poly.empty
      | Top, Element, _ -> failwith "Topped set cannot be partitioned by Element"
      | _, Set, OpByFilter -> (
          let value =
            match set with
            | Top -> Top
            | ASet set -> ASet (Set.elements set)
          in
          match f value with
          | None -> Core_kernel.Map.Poly.empty
          | Some partition_key -> Core_kernel.Map.Poly.singleton partition_key set )
      | _ -> Base.partition part op ~f set


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
          f#report Element;
          f#report Set
      | Structure -> [Format.sprintf "ToppedSet(%s)" Element.name]
      | Name part -> (
          match part with
          | Element -> Format.sprintf "ToppedSet(%s).Element" Element.name
          | Set -> Format.sprintf "ToppedSet(%s).Set" Element.name
          | Self -> Format.sprintf "ToppedSet(%s).Self" Element.name
          | _ -> Base.introspect op )


    let create parts =
      let create_part so_far (Part (part, value)) =
        match part with
        | Set -> (
            match value with
            | Top -> Top
            | ASet l -> join so_far (of_list l) )
        | Element -> add value so_far
        | _ -> Base.create part value so_far
      in
      ListLabels.fold_left parts ~f:create_part ~init:bottom


    let transform = Base.legacy_transform

    let fold = Base.fold

    let partition = Base.legacy_partition
  end

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
