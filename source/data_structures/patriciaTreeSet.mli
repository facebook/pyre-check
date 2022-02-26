(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ELEMENT = sig
  type t [@@deriving show]

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val is_zero : t -> bool

  val add_one : t -> t

  val subtract_one : t -> t

  val bitwise_and : t -> t -> t

  val bitwise_xor : t -> t -> t

  val bitwise_not : t -> t
end

module type SET = sig
  type element

  type t

  val empty : t

  val is_empty : t -> bool

  val add : element -> t -> t

  val mem : element -> t -> bool

  val singleton : element -> t

  val remove : element -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (element -> unit) -> t -> unit

  val map : (element -> element) -> t -> t

  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (element -> bool) -> t -> bool

  val exists : (element -> bool) -> t -> bool

  val filter : (element -> bool) -> t -> t

  val cardinal : t -> int

  val elements : t -> element list

  val of_list : element list -> t
end

module Make (Element : ELEMENT) : sig
  include SET with type element = Element.t
end

module PatriciaTreeIntSet : sig
  include SET with type element = int
end
