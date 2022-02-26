(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ELEMENT = sig
  include AbstractSetDomain.ELEMENT

  (* Widen to top when set exceeds max count *)
  val max_count : unit -> int
end

module Make (Element : ELEMENT) : sig
  include AbstractDomainCore.S

  type _ AbstractDomainCore.part += Element : Element.t AbstractDomainCore.part

  val add : Element.t -> t -> t

  val singleton : Element.t -> t

  val is_top : t -> bool

  (* returns [] if is_top *)
  val elements : t -> Element.t list

  val of_list : Element.t list -> t

  val top : t
end
