(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

  type _ part += Element : Element.t part

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


  let elements = Set.elements

  let singleton = Set.singleton

  let of_list elements =
    ListLabels.fold_left ~f:(fun set elt -> add elt set) elements ~init:Set.empty


  module rec Base : (BASE with type t := Set.t) = MakeBase (Domain)

  and Domain : (S with type t = Set.t) = struct
    type t = Set.t

    type _ part += Self : t part

    let bottom = Set.empty

    let is_bottom = Set.is_empty

    let join left right =
      if left == right || is_bottom right then
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


    let show set =
      Set.elements set
      |> ListLabels.map ~f:Element.show
      |> String.concat ", "
      |> Format.sprintf "[%s]"


    let pp formatter map = Format.fprintf formatter "%s" (show map)

    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f set ->
      match part, op with
      | Element, Map ->
          Set.elements set
          |> ListLabels.fold_left ~f:(fun result element -> add (f element) result) ~init:Set.empty
      | Element, Add -> add f set
      | Element, Filter -> Set.filter f set
      | Element, FilterMap ->
          Set.elements set
          |> ListLabels.fold_left
               ~f:(fun result element ->
                 match f element with
                 | Some element -> add element result
                 | None -> result)
               ~init:Set.empty
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
        | None -> singleton element
        | Some set -> add element set
      in
      match part, op with
      | Element, By ->
          let f element result =
            let key = f element in
            Core_kernel.Map.Poly.update result key ~f:(update element)
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
      | Structure -> [Format.sprintf "AbstractElementSet(%s)" Element.name]
      | Name part -> (
          match part with
          | Element -> Format.sprintf "AbstractElementSet(%s).Element" Element.name
          | Self -> Format.sprintf "AbstractElementSet(%s).Self" Element.name
          | _ -> Base.introspect op)


    let create parts =
      let create_part so_far (Part (part, value)) =
        match part with
        | Element -> add value so_far
        | _ -> Base.create part value so_far
      in
      ListLabels.fold_left ~f:create_part parts ~init:bottom


    let meet = Base.meet

    let fold = Base.fold

    let apply = Base.apply
  end

  let _ = Base.fold (* unused module warning work-around *)

  include Domain

  let count = Set.cardinal
end
