(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = string [@@deriving compare, sexp, hash, yojson]

module T : sig
  type nonrec t = t [@@deriving compare, sexp, hash, yojson]
end

module Map : sig
  include Map.S with type Key.t = t

  module Tree : module type of struct
    include Map.Make_tree (struct
      include T
      include Comparator.Make (T)
    end)
  end
end

module SerializableMap : Data_structures.SerializableMap.S with type key = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

val sanitized : t -> t

val is_sanitized : t -> bool

val equal_sanitized : t -> t -> bool

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

val pp_sanitized : Format.formatter -> t -> unit

val remove_leading_underscores : t -> t

val split_star : t -> string * t

val is_private_name : t -> bool

val is_valid_identifier : t -> bool
