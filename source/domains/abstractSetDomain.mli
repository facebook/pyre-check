(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ELEMENT = sig
  type t [@@deriving show]

  val name : string

  val compare : t -> t -> int
end

module type SET = sig
  type t

  type element

  val empty : t

  val is_empty : t -> bool

  val singleton : element -> t

  val add : element -> t -> t

  val remove : element -> t -> t

  val mem : element -> t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t

  val subset : t -> t -> bool

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val map : (element -> element) -> t -> t

  val filter : (element -> bool) -> t -> t

  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a

  val exists : (element -> bool) -> t -> bool

  val of_list : element list -> t

  val elements : t -> element list

  val show_element : element -> string

  val element_name : string
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

module MakeWithSet (Set : SET) : S with type element = Set.element

module Make (Element : ELEMENT) : S with type element = Element.t
