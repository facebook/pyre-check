(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module AstReference = Reference

type position = {
  line: int;
  column: int;
}
[@@deriving equal, compare, sexp, show, hash, yojson]

val any_position : position

val position_from_string : string -> (position, string) Result.t

type t = {
  start: position;
  stop: position;
}
[@@deriving equal, compare, sexp, show, hash, yojson]

val create : start:Lexing.position -> stop:Lexing.position -> t

val equal : t -> t -> bool

val any : t

val start : t -> position

val stop : t -> position

val line : t -> int

val column : t -> int

val stop_column : t -> int

val pp_start : Format.formatter -> t -> unit

val pp_line_and_column : Format.formatter -> t -> unit

val from_string : string -> (t, string) Result.t

module T : sig
  type nonrec t = t [@@deriving compare, sexp, hash, to_yojson]
end

module Map : sig
  include Core.Map.S with type Key.t = t

  module Tree : module type of struct
    include Core.Map.Make_tree (struct
      include T
      include Core.Comparator.Make (T)
    end)
  end
end

module SerializableMap : Data_structures.SerializableMap.S with type key = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

module WithPath : sig
  type t = {
    path: string;
    start: position;
    stop: position;
  }
  [@@deriving equal, compare, sexp, hash, to_yojson]

  val any : t

  val pp : Format.formatter -> t -> unit

  val line : t -> int

  val pp_line : Format.formatter -> t -> unit
end

module WithModule : sig
  type t = {
    module_reference: Reference.t;
    start: position;
    stop: position;
  }
  [@@deriving compare, eq, sexp, hash, to_yojson]

  val any : t

  val line : t -> int

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val instantiate : lookup:(Reference.t -> string option) -> t -> WithPath.t

  include Hashable with type t := t
end

val with_path : path:string -> t -> WithPath.t

val with_module : module_reference:Reference.t -> t -> WithModule.t

val strip_module : WithModule.t -> t
