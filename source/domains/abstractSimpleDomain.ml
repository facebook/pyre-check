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

  val bottom : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool

  val show : t -> string
end

module Make (Element : ELEMENT) = struct
  module rec Base : (BASE with type t := Element.t) = MakeBase (Domain)

  and Domain : (S with type t = Element.t) = struct
    include Element

    let pp formatter value = Format.fprintf formatter "%s" (Element.show value)

    let is_bottom v = v = Element.bottom

    let widen ~iteration:_ ~prev ~next = join prev next

    type _ part += Self : t part

    let transform = Base.transform

    let reduce = Base.reduce

    let partition = Base.partition

    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f -> f#report Self
      | Structure -> [Format.sprintf "Simple(%s)" Element.name]
      | Name part -> (
          match part with
          | Self -> Format.sprintf "Simple(%s).Self" Element.name
          | _ -> Base.introspect op)


    let create parts =
      ListLabels.fold_left
        ~f:(fun result (Part (part, value)) ->
          match part with
          | Self -> join value result
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

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
