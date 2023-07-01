(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

(** Roots representing parameters, locals, and special return value in models. *)
module Root : sig
  type t =
    | LocalResult (* Special root representing the return value location. *)
    | PositionalParameter of {
        position: int;
        name: Identifier.t;
        positional_only: bool;
      }
    | NamedParameter of { name: Identifier.t }
    | StarParameter of { position: int }
    | StarStarParameter of { excluded: Identifier.t list }
    | Variable of Identifier.t
    | CapturedVariable of Identifier.t
  [@@deriving compare, eq, hash, sexp, show]

  val normalize_parameters : Parameter.t list -> (t * Identifier.t * Parameter.t) list

  val parameter_name : t -> string option

  val to_string : t -> string

  module Set : Caml.Set.S with type elt = t
end

module Path : sig
  type t = Abstract.TreeDomain.Label.t list [@@deriving compare, eq, show]

  val empty : t

  val is_prefix : prefix:t -> t -> bool
end

type t = {
  root: Root.t;
  path: Path.t;
}
[@@deriving show, compare]

val create : Root.t -> Path.t -> t

val extend : t -> path:Path.t -> t

val of_expression : Expression.t -> t option

val get_index : Expression.t -> Abstract.TreeDomain.Label.t

val to_json : t -> Yojson.Safe.t

type argument_match = {
  root: Root.t;
  actual_path: Path.t;
  formal_path: Path.t;
}
[@@deriving compare, show]

(* Will preserve the order in which the arguments were matched to formals. *)
val match_actuals_to_formals
  :  Call.Argument.t list ->
  Root.t list ->
  (Call.Argument.t * argument_match list) list

val dictionary_keys : Abstract.TreeDomain.Label.t
