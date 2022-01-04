(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

[@@@ocamlformat "wrap-comments=false"]

module type PRODUCT_CONFIG = sig
  (* GADT describing the different parts of the product and which abstract domain they are using.
     Must be constant constructors.

     type _ slot =
       | Left: LeftDomain.t AbstractDomainCore.abstract_domain slot
       | Right: RightDomain.t AbstractDomainCore.abstract_domain slot
  *)
  type 'a slot

  (* Cardinality of type 'a slot, i.e., distinct constants. If this is wrong, code will crash.
     E.g. let slots = 2
  *)
  val slots : int

  (* Name of the product slot, e.g., "Left", "Right". Must be distinct for each slot. *)
  val slot_name : 'a slot -> string

  (* The abstract domain of values in a given product slot. E.g.,

     let slot_domain (type a) (a slot) =
       | Left -> (module LeftDomain : AbstractDomainCore.S with type t = a) ...
  *)
  val slot_domain : 'a slot -> 'a AbstractDomainCore.abstract_domain

  (* If a slot is strict, then the entire product is bottom when that slot is bottom. *)
  val strict : 'a slot -> bool
end

(* Product domain whose interpretation and thus semantics depends on strict slots.
   E.g. (X, Y) can be interpreted with different gamma functions.
     y1: { (x, y) | x in X /\ y in Y }
     y2: { (x, Y') | x in X /\ Y' <= Y }
     y3: { (X', Y') | X' <= X /\ Y' <= Y }
   and more
*)
module Make (Config : PRODUCT_CONFIG) : sig
  include AbstractDomainCore.S

  val update : 'a Config.slot -> 'a -> t -> t

  val get : 'a Config.slot -> t -> 'a
end
