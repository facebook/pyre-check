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
  include Map.S

  val set : 'a t -> key:key -> data:'a -> 'a t

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val hash_fold_t : (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state

  val pp : (Formatter.t -> 'a -> unit) -> Formatter.t -> 'a t -> unit
end

module type OrderedType = sig
  include Caml.Map.OrderedType

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val hash_fold_t : Hash.state -> t -> Hash.state

  val pp : Formatter.t -> t -> unit
end

module Make (Ordered : OrderedType) : S with type key = Ordered.t
