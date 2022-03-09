(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | NamedSource of string
  | NamedSink of string
[@@deriving compare, eq, hash, sexp]

val pp : Format.formatter -> t -> unit

val show : t -> string

module Set : sig
  (* We cannot use `Core.Set` because we need this to be serialazable to shared memory,
   * but `Core.Set.t` uses functions as values in its type.
   * The downside is that we have to define sexp, hash and show manually. *)

  include Stdlib.Set.S with type elt = t

  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

  val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state

  val hash : t -> int

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val filter_sources : t -> t

  val filter_sinks : t -> t
end
