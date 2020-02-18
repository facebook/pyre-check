(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type 'a approximation = {
  element: 'a;
  in_under: bool;
}

module type S = sig
  include AbstractSetDomain.S

  type _ AbstractDomainCore.part +=
    | ElementAndUnder : element approximation AbstractDomainCore.part
    | SetAndUnder : element approximation list AbstractDomainCore.part

  (* Distinct from bottom in that it has no elements present, which will cause joins to
     over-approximate *)
  val empty : t

  val inject : element -> element approximation

  val to_approximation : t -> element approximation list

  val of_approximation : element approximation list -> t

  val add_set : t -> to_add:t -> t
end

module Make (Element : AbstractSetDomain.ELEMENT) : sig
  include S with type element = Element.t
end
