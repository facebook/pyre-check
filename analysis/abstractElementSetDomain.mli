(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module type ELEMENT_DOMAIN = sig
  type t [@@deriving show, compare, sexp]

  val less_or_equal : left:t -> right:t -> bool

  (* Called after widen steps to further simplify the set. *)
  val widen : t list -> t list
end

(* A set of abstract elements where elements can be related *)
module Make (Element : ELEMENT_DOMAIN) : sig
  include AbstractDomain.S

  type _ AbstractDomain.part +=
    | Element : Element.t AbstractDomain.part | Set : Element.t list AbstractDomain.part

  val add : t -> Element.t -> t

  val elements : t -> Element.t list

  val singleton : Element.t -> t

  val of_list : Element.t list -> t
end
