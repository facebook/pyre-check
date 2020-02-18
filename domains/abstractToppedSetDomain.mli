(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

module type ELEMENT = sig
  include AbstractSetDomain.ELEMENT

  (* Widen to top when set exceeds max count *)
  val max_count : unit -> int
end

module Make (Element : ELEMENT) : sig
  include AbstractDomainCore.S

  type 'a with_top =
    | Top
    | ASet of 'a

  type _ AbstractDomainCore.part +=
    | Element : Element.t AbstractDomainCore.part
    | Set : Element.t list with_top AbstractDomainCore.part

  val add : Element.t -> t -> t

  val singleton : Element.t -> t

  val elements : t -> Element.t list

  val of_list : Element.t list -> t

  val top : t
end
