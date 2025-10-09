(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Raw : sig
  type t = {
    relative: string;
    priority: int;
  }
  [@@deriving compare, equal, hash, sexp]

  val empty : t

  val priority_aware_compare : configuration:Configuration.Analysis.t -> t -> t -> int
end

type t = {
  raw: Raw.t;
  qualifier: Reference.t;
  should_type_check: bool;
}
[@@deriving compare, equal, hash, sexp]

val pp : Format.formatter -> t -> unit

val qualifier : t -> Reference.t

val raw : t -> Raw.t

val relative : t -> string

val should_type_check : t -> bool

val create : should_type_check:bool -> Raw.t -> t

val qualifier_from_relative_path : string -> Reference.t

val is_stub : t -> bool

val is_init : t -> bool

val expand_relative_import : from:Reference.t -> t -> Reference.t
