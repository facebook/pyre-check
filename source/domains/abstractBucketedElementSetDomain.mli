(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type BUCKETED_ELEMENT = sig
  include AbstractElementSetDomain.ELEMENT

  type bucket

  val bucket : t -> bucket

  val pp_bucket : Format.formatter -> bucket -> unit

  val compare_bucket : bucket -> bucket -> int
end

(* A set of abstract elements where elements can be related *)
module Make (Element : BUCKETED_ELEMENT) : sig
  include AbstractDomainCore.S

  type _ AbstractDomainCore.part += Element : Element.t AbstractDomainCore.part

  val add : Element.t -> t -> t

  val elements : t -> Element.t list

  val singleton : Element.t -> t

  val of_list : Element.t list -> t
end
