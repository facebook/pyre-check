(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Make (Element : Core.Set.Elt) : sig
  include AbstractDomain.S

  type element = {
    element: Element.t;
    in_under: bool;
  }
  [@@deriving compare]

  type _ AbstractDomain.part +=
    | Element : Element.t AbstractDomain.part
    | Set : Element.t list AbstractDomain.part
    | ElementAndUnder : element AbstractDomain.part
    | SetAndUnder : element list AbstractDomain.part

  val element : Element.t -> element

  val empty : t

  val add : t -> Element.t -> t

  val singleton : Element.t -> t

  val elements : t -> element list

  val of_list : element list -> t
end
