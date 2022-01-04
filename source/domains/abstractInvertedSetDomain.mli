(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ELEMENT = sig
  include AbstractSetDomain.ELEMENT
end

(** A set starting out as the Universe of objects and joins cause it to shrink by intersection **)
module Make (Element : ELEMENT) : sig
  include AbstractDomainCore.S

  type elements = {
    is_universe: bool;
    elements: Element.t list;
  }

  type _ AbstractDomainCore.part += Element : Element.t AbstractDomainCore.part

  val add : Element.t -> t -> t

  val singleton : Element.t -> t

  val elements : t -> elements

  val of_elements : elements -> t
end
