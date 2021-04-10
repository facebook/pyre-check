(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
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
  [@@deriving compare, eq, show]

  val normalize_parameters : Parameter.t list -> (t * Identifier.t * Parameter.t) list

  val parameter_name : t -> string option
end

type t = {
  root: Root.t;
  path: Abstract.TreeDomain.Label.path;
}
[@@deriving show, eq]

val create : Root.t -> Abstract.TreeDomain.Label.path -> t

val extend : t -> path:Abstract.TreeDomain.Label.path -> t

val of_expression : resolution:Resolution.t -> Expression.t -> t option

val get_index : Expression.t -> Abstract.TreeDomain.Label.t

val to_json : t -> Yojson.Safe.json

val get_global : resolution:Resolution.t -> Expression.t -> Reference.t option

val is_global : resolution:Resolution.t -> Expression.t -> bool

type argument_match = {
  root: Root.t;
  actual_path: Abstract.TreeDomain.Label.path;
  formal_path: Abstract.TreeDomain.Label.path;
}
[@@deriving compare, show]

(* Will preserve the order in which the arguments were matched to formals. *)
val match_actuals_to_formals
  :  Call.Argument.t list ->
  Root.t list ->
  (Expression.t * argument_match list) list

val dictionary_keys : Abstract.TreeDomain.Label.t
