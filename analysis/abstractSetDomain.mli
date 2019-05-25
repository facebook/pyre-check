(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Make (Element : Core.Set.Elt) : sig
  include AbstractDomain.S

  type _ AbstractDomain.part +=
    | Element : Element.t AbstractDomain.part | Set : Element.t list AbstractDomain.part

  val add : t -> Element.t -> t

  val singleton : Element.t -> t

  val elements : t -> Element.t list

  val of_list : Element.t list -> t
end
