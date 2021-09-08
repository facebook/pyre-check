(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Attach
  | NamedSource of string
  | ParametricSource of {
      source_name: string;
      subkind: string;
    }
[@@deriving compare, eq, sexp, show, hash]

val name : string

val ignore_kind_at_call : t -> bool

module Set : sig
  include Stdlib.Set.S with type elt = t

  val pp : Format.formatter -> t -> unit

  val show : t -> string
end
