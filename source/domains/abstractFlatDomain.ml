(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  type t =
    | Bottom
    | Concrete of Element.t
    | Top

  let bottom = Bottom

  let top = Top

  let is_bottom v = v = Bottom

  let is_top v = v = Top

  let is_equal t v =
    match t with
    | Concrete e -> e = v
    | _ -> false


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

  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join

    let less_or_equal = less_or_equal
  end

  module C = Common (CommonArg)

  type _ part += Self = C.Self | Element : Element.t part

  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t p ->
    match part with
    | Element -> (
        match p, t with
        | Bottom, _ -> p
        | Top, _ -> p
        | Concrete e1, Add e2 -> if e1 = e2 then p else Top
        | Concrete e, Map f -> Concrete (f e)
        | Concrete e, Filter f -> if f e then p else Bottom
        | _ -> C.transform transformer part t p )
    | _ -> C.transform transformer part t p


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (p : t) =
    match part, p with
    | Element, Concrete e -> f e init
    | Element, (Top | Bottom) -> init
    | _ -> C.fold part ~f ~init p


  let create parts =
    ListLabels.fold_left
      ~f:(fun result (Part (part, value)) ->
        match part with
        | Self -> join (value : t) result
        | Element -> join (Concrete value) result
        | _ -> C.create part value result)
      parts
      ~init:bottom


  let subtract to_remove ~from =
    if to_remove == from then
      bottom
    else
      from


  let partition (type a b) (part : a part) ~(f : a -> b option) (flat : t)
      : (b, t) Core_kernel.Map.Poly.t
    =
    match part with
    | Element -> (
        match flat with
        | Top
        | Bottom ->
            Core_kernel.Map.Poly.empty
        | Concrete e -> (
            match f e with
            | None -> Core_kernel.Map.Poly.empty
            | Some key -> Core_kernel.Map.Poly.singleton key flat ) )
    | _ -> C.partition part ~f flat


  let introspect (type a) (op : a introspect) : a =
    match op with
    | GetParts f ->
        f#report C.Self;
        f#report Element
    | Structure -> [Format.sprintf "Flat(%s)" Element.name]
    | Name part -> (
        match part with
        | Element -> Format.sprintf "Flat(%s).Element" Element.name
        | Self -> Format.sprintf "Flat(%s).Self" Element.name
        | _ -> C.introspect op )


  let make e = Concrete e

  let get v =
    match v with
    | Concrete e -> Some e
    | _ -> None
end
