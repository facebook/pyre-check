(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module type ELEMENT_DOMAIN = sig
  type t
  [@@deriving show, sexp]

  val less_or_equal: left: t -> right: t -> bool
  val join: t -> t -> t

  type group
  [@@deriving compare, sexp]

  val group: t -> group
end


(* A set of abstract elements where set adding will join elements by group *)
module Make(Element : ELEMENT_DOMAIN) : sig
  include AbstractDomain.S

  val add: t -> Element.t -> t
  val elements: t -> Element.t list
  val singleton: Element.t -> t
end
