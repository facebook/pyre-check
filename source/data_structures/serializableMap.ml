(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ppx_sexp_conv_lib
module Hash = Core.Hash
module Formatter = Core.Formatter

module type S = sig
  include Caml.Map.S

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

module Make (Ordered : OrderedType) : S with type key = Ordered.t = struct
  include Caml.Map.Make (Ordered)

  module Key = struct
    include Ordered

    type 'a assoc = t * 'a [@@deriving compare, sexp, show, hash]
  end

  let set map ~key ~data = add key data map

  let t_of_sexp a_of_sexp sexp =
    Core.List.t_of_sexp (Key.assoc_of_sexp a_of_sexp) sexp |> Caml.List.to_seq |> of_seq


  let sexp_of_t sexp_of_a map = bindings map |> Core.List.sexp_of_t (Key.sexp_of_assoc sexp_of_a)

  let hash_fold_t hash_fold_a hash_state map =
    bindings map |> Core.List.hash_fold_t (Key.hash_fold_assoc hash_fold_a) hash_state


  let pp pp_a format map =
    let l el = Format.asprintf "%a" (Key.pp_assoc pp_a) el in
    Format.fprintf format "%s" (List.to_string ~f:l (bindings map))
end
