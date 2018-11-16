(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module type PRODUCT_CONFIG = sig
  (* GADT describing the different parts of the product and which abstract
     domain they are using, e.g.

     type _ slot =
     | Left: LeftDomain.t AbstractDomain.abstract_domain slot
     | Right: RightDomain.t AbstractDomain.abstract_domain slot
  *)
  type 'a slot

  (* Name of the product slot, e.g., "Left", "Right". Must be distinct for each
     slot. *)
  val slot_name: 'a slot -> string

  (* The abstract domain of values in a given product slot. E.g.,
     let slot_domain (type a) (a slot) =
       | Left -> (module LeftDomain : AbstractDomain.S with type t = a)
       ...
  *)
  val slot_domain: 'a slot -> 'a AbstractDomain.abstract_domain
end


module Make(Config : PRODUCT_CONFIG) : sig
  include AbstractDomain.S

  (* Product parts need to be routed to the proper slot. This can be done by
     wrapping the part in a ProductSlot.
  *)
  type 'a AbstractDomain.part +=
    | ProductSlot: 'b Config.slot * 'a AbstractDomain.part -> 'a AbstractDomain.part

  val singleton: 'a Config.slot -> 'a -> t
  val update: 'a Config.slot -> 'a -> t -> t
  val get: 'a Config.slot -> t -> 'a

  (* An actual abstract value for a particular slot.
  *)
  type element = Element: 'a Config.slot * 'a -> element
  val product: element list -> t
end
