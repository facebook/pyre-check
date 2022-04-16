(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore

module type ELEMENT = sig
  type t [@@deriving show]

  val name : string

  val compare : t -> t -> int
end

module type SET = sig
  type t

  type element

  val empty : t

  val is_empty : t -> bool

  val singleton : element -> t

  val add : element -> t -> t

  val remove : element -> t -> t

  val mem : element -> t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t

  val subset : t -> t -> bool

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val map : (element -> element) -> t -> t

  val filter : (element -> bool) -> t -> t

  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a

  val exists : (element -> bool) -> t -> bool

  val of_list : element list -> t

  val elements : t -> element list

  val show_element : element -> string

  val element_name : string
end

module type S = sig
  include AbstractDomainCore.S

  type element

  type _ AbstractDomainCore.part += Element : element AbstractDomainCore.part

  val add : element -> t -> t

  val remove : element -> t -> t

  val contains : element -> t -> bool

  val singleton : element -> t

  val elements : t -> element list

  val of_list : element list -> t
end

module MakeWithSet (Set : SET) = struct
  module rec Base : (BASE with type t := Set.t) = MakeBase (Domain)

  and Domain : (S with type t = Set.t and type element = Set.element) = struct
    include Set

    type element = Set.element

    type _ part += Self : t part | Element : Set.element part

    let bottom = Set.empty

    let is_bottom = Set.is_empty

    let contains = Set.mem

    let join left right =
      if left == right then
        left
      else
        Set.union left right


    let meet left right =
      if left == right then
        left
      else
        Set.inter left right


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
      |> ListLabels.map ~f:Set.show_element
      |> String.concat ", "
      |> Format.sprintf "[%s]"


    let pp formatter set = Format.fprintf formatter "%s" (show set)

    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f set ->
      match part, op with
      | Element, Map -> Set.map f set
      | Element, Add -> Set.add f set
      | Element, Filter -> Set.filter f set
      | Element, FilterMap -> Set.elements set |> List.filter_map f |> Set.of_list
      | _ -> Base.transform part op ~f set


    let reduce
        : type a f b. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init set ->
      match part, op with
      | Element, Acc -> Set.fold f set init
      | Element, Exists -> init || Set.exists f set
      | _ -> Base.reduce part ~using:op ~f ~init set


    let partition
        : type a f b.
          a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) Core_kernel.Map.Poly.t
      =
     fun part op ~f set ->
      let update element = function
        | None -> Set.singleton element
        | Some set -> Set.add element set
      in
      match part, op with
      | Element, By ->
          let f element result =
            Core_kernel.Map.Poly.update result (f element) ~f:(update element)
          in
          Set.fold f set Core_kernel.Map.Poly.empty
      | Element, ByFilter ->
          let f element result =
            match f element with
            | None -> result
            | Some key -> Core_kernel.Map.Poly.update result key ~f:(update element)
          in
          Set.fold f set Core_kernel.Map.Poly.empty
      | _ -> Base.partition part op ~f set


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
          f#report Element
      | Structure -> [Format.sprintf "Set(%s)" Set.element_name]
      | Name part -> (
          match part with
          | Element -> Format.sprintf "Set(%s).Element" Set.element_name
          | Self -> Format.sprintf "Set(%s).Self" Set.element_name
          | _ -> Base.introspect op)


    let create parts =
      let create_part so_far (Part (part, value)) =
        match part with
        | Element -> Set.add value so_far
        | _ -> Base.create part value so_far
      in
      ListLabels.fold_left parts ~f:create_part ~init:bottom


    let fold = Base.fold

    let apply = Base.apply
  end

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end

module Make (Element : ELEMENT) = struct
  module Set = struct
    include Set.Make (Element)

    type element = Element.t

    let show_element = Element.show

    let element_name = Element.name
  end

  include MakeWithSet (Set)
end
