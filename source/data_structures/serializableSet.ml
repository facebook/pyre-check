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

module Make (Ordered : OrderedType) : S with type elt = Ordered.t = struct
  include Caml.Set.Make (Ordered)

  let t_of_sexp sexp = Core.List.t_of_sexp Ordered.t_of_sexp sexp |> of_list

  let sexp_of_t set = elements set |> Core.List.sexp_of_t Ordered.sexp_of_t

  let hash_fold_t hash_state set =
    elements set |> Core.List.hash_fold_t Ordered.hash_fold_t hash_state


  let show set =
    elements set |> List.map ~f:Ordered.show |> String.concat ~sep:", " |> Format.asprintf "{%s}"


  let pp format set = Format.fprintf format "%s" (show set)
end
