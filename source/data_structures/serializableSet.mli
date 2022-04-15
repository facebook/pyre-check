(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ppx_sexp_conv_lib
module Hash = Core.Hash
module Formatter = Core.Formatter

module type S = sig
  include Caml.Set.S

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val hash_fold_t : Hash.state -> t -> Hash.state

  val pp : Formatter.t -> t -> unit

  val show : t -> string
end

module type OrderedType = sig
  include Caml.Set.OrderedType

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val hash_fold_t : Hash.state -> t -> Hash.state

  val pp : Formatter.t -> t -> unit

  val show : t -> string
end

module Make (Ordered : OrderedType) : S with type elt = Ordered.t
