(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ELEMENT = sig
  include AbstractSetDomain.ELEMENT

  val less_or_equal : left:t -> right:t -> bool

  (* Called after widen steps to further simplify the set. *)
  val widen : t list -> t list
end

(* A set of abstract elements where elements can be related *)
module Make (Element : ELEMENT) : sig
  include AbstractDomainCore.S

  type _ AbstractDomainCore.part += Element : Element.t AbstractDomainCore.part

  val add : Element.t -> t -> t

  val elements : t -> Element.t list

  val singleton : Element.t -> t

  val of_list : Element.t list -> t

  val count : t -> int
end
