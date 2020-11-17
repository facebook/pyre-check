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

  val bottom : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool

  val show : t -> string
end

module Make (Element : ELEMENT) = struct
  include Element

  let pp formatter value = Format.fprintf formatter "%s" (Element.show value)

  let is_bottom v = v = Element.bottom

  let widen ~iteration:_ ~prev ~next = join prev next

  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join

    let less_or_equal = less_or_equal
  end

  module C = Common (CommonArg)

  type _ part += Self = C.Self

  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t p -> C.transform transformer part t p


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (p : t) =
    C.fold part ~f ~init p


  let create parts =
    ListLabels.fold_left
      ~f:(fun result (Part (part, value)) ->
        match part with
        | Self -> join value result
        | _ -> C.create part value result)
      parts
      ~init:bottom


  let subtract to_remove ~from =
    if to_remove == from then
      bottom
    else
      from


  let partition (type a b) (part : a part) ~(f : a -> b option) (product : t)
      : (b, t) Core_kernel.Map.Poly.t
    =
    C.partition part ~f product


  let introspect (type a) (op : a introspect) : a =
    match op with
    | GetParts f -> f#report C.Self
    | Structure -> [Format.sprintf "Simple(%s)" Element.name]
    | Name part -> (
        match part with
        | Self -> Format.sprintf "Simple(%s).Self" Element.name
        | _ -> C.introspect op )
end
