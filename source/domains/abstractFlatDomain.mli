(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ELEMENT = sig
  type t

  val name : string

  val show : t -> string
end

module Make (Element : ELEMENT) : sig
  include AbstractDomainCore.S

  type _ AbstractDomainCore.part += Element : Element.t AbstractDomainCore.part

  val top : t

  val is_top : t -> bool

  val is_equal : t -> Element.t -> bool

  val make : Element.t -> t

  val get : t -> Element.t option
end
