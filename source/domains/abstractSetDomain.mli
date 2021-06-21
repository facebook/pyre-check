(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ELEMENT = sig
  type t [@@deriving show]

  val name : string

  val compare : t -> t -> int
end

module type S = sig
  include AbstractDomainCore.S

  type element

  type _ AbstractDomainCore.part += Element : element AbstractDomainCore.part

  val add : element -> t -> t

  val remove : element -> t -> t

  val contains : element -> t -> bool

  val singleton : element -> t

  val elements : t -> element list

  val of_list : element list -> t
end

module Make (Element : ELEMENT) : S with type element = Element.t
