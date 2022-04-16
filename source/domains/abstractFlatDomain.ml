(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore

module type ELEMENT = sig
  type t

  val name : string

  val show : t -> string
end

module Make (Element : ELEMENT) = struct
  type concrete =
    | Bottom
    | Concrete of Element.t
    | Top

  let top = Top

  let is_top v = v = Top

  let is_equal t v =
    match t with
    | Concrete e -> e = v
    | _ -> false


  type _ part += Element : Element.t part

  module rec Base : (BASE with type t := concrete) = MakeBase (Domain)

  and Domain : (S with type t = concrete) = struct
    type t = concrete

    type _ part += Self : t part

    let bottom = Bottom

    let is_bottom v = v = Bottom

    let show value =
      match value with
      | Concrete e -> Format.sprintf "%s" (Element.show e)
      | Top -> "Top"
      | Bottom -> "Bottom"


    let pp formatter value = Format.fprintf formatter "%s" (show value)

    let less_or_equal ~left ~right =
      match left, right with
      | Bottom, _ -> true
      | _, Top -> true
      | Concrete e1, Concrete e2 -> e1 = e2
      | _ -> false


    let join a b =
      if a = b then
        a
      else
        match a, b with
        | Bottom, _ -> b
        | _, Bottom -> a
        | _ -> Top


    let meet a b =
      if a = b then
        a
      else
        match a, b with
        | Top, _ -> b
        | _, Top -> a
        | _ -> Bottom


    let widen ~iteration:_ ~prev ~next = join prev next

    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f p ->
      match part with
      | Element -> (
          match p, op with
          | Bottom, Add -> Concrete f
          | Bottom, _ -> p
          | Top, _ -> p
          | Concrete e, Add -> if e = f then p else Top
          | Concrete e, Map -> Concrete (f e)
          | Concrete e, Filter -> if f e then p else Bottom
          | Concrete e, FilterMap -> (
              match f e with
              | Some e -> Concrete e
              | None -> Bottom)
          | _ -> Base.transform part op ~f p)
      | _ -> Base.transform part op ~f p


    let reduce
        : type a f b. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init p ->
      match part, op, p with
      | Element, Acc, Concrete e -> f e init
      | Element, Acc, (Top | Bottom) -> init
      | Element, Exists, Concrete e -> init || f e
      | Element, Exists, (Top | Bottom) -> init
      | _ -> Base.reduce part ~using:op ~f ~init p


    let partition
        : type a f b.
          a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) Core_kernel.Map.Poly.t
      =
     fun part op ~f flat ->
      match part, op with
      | Element, By -> (
          match flat with
          | Top
          | Bottom ->
              Core_kernel.Map.Poly.empty
          | Concrete e -> Core_kernel.Map.Poly.singleton (f e) flat)
      | Element, ByFilter -> (
          match flat with
          | Top
          | Bottom ->
              Core_kernel.Map.Poly.empty
          | Concrete e -> (
              match f e with
              | None -> Core_kernel.Map.Poly.empty
              | Some key -> Core_kernel.Map.Poly.singleton key flat))
      | _ -> Base.partition part op ~f flat


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
          f#report Element
      | Structure -> [Format.sprintf "Flat(%s)" Element.name]
      | Name part -> (
          match part with
          | Element -> Format.sprintf "Flat(%s).Element" Element.name
          | Self -> Format.sprintf "Flat(%s).Self" Element.name
          | _ -> Base.introspect op)


    let create parts =
      ListLabels.fold_left
        ~f:(fun result (Part (part, value)) ->
          match part with
          | Self -> join (value : t) result
          | Element -> join (Concrete value) result
          | _ -> Base.create part value result)
        parts
        ~init:bottom


    let subtract to_remove ~from =
      if to_remove == from then
        bottom
      else
        from


    let fold = Base.fold

    let apply = Base.apply
  end

  let make e = Concrete e

  let get v =
    match v with
    | Concrete e -> Some e
    | _ -> None


  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
