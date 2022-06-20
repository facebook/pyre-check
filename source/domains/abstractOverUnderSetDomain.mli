(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a approximation = {
  element: 'a;
  in_under: bool;
}

module type S = sig
  include AbstractSetDomain.S

  type _ AbstractDomainCore.part += ElementAndUnder : element approximation AbstractDomainCore.part

  (* Distinct from bottom in that it has no elements present, which will cause joins to
     over-approximate *)
  val empty : t

  (* true if the set is bottom or empty *)
  val is_empty : t -> bool

  val inject : element -> element approximation

  val to_approximation : t -> element approximation list

  val of_approximation : element approximation list -> t

  val add_set : t -> to_add:t -> t

  (* Normal join models an either/or outcome, e.g. two distinct paths, where as sequence_join models
     a composition where if an element is in the under approximation on either side, it will be also
     in the result *)
  val sequence_join : t -> t -> t

  (* Copy the elements from the over set to the under set. *)
  val over_to_under : t -> t
end

module MakeWithSet (Set : AbstractSetDomain.SET) : S with type element = Set.element

module Make (Element : AbstractSetDomain.ELEMENT) : S with type element = Element.t
