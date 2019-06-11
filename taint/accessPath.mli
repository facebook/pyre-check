(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Expression

(** Roots representing parameters, locals, and special return value in models. *)
module Root : sig
  type t =
    | LocalResult (* Special root representing the return value location. *)
    | PositionalParameter of { position: int; name: Identifier.t }
    | NamedParameter of { name: Identifier.t }
    | StarParameter of { position: int }
    | StarStarParameter of { excluded: Identifier.t list }
    | Variable of Identifier.t
  [@@deriving compare, eq, sexp, show, hash]

  val normalize_parameters : 'a Parameter.t list -> (t * Identifier.t * 'a Parameter.t) list

  val parameter_name : t -> string option
end

type t = {
  root: Root.t;
  path: AbstractTreeDomain.Label.path
}
[@@deriving show]

val create : Root.t -> AbstractTreeDomain.Label.path -> t

val of_expression : Expression.t -> t option

val get_index : Expression.t -> AbstractTreeDomain.Label.t

val to_json : t -> Yojson.Safe.json

val create_receiver
  :  call:Expression.t Expression.Call.t ->
  Expression.t ->
  Expression.t Call.Argument.t

val get_global : resolution:Resolution.t -> Expression.t -> Reference.t option

val is_global : resolution:Resolution.t -> Expression.t -> bool

val is_property : resolution:Resolution.t -> Expression.expression -> bool

type argument_match = {
  root: Root.t;
  actual_path: AbstractTreeDomain.Label.path;
  formal_path: AbstractTreeDomain.Label.path
}
[@@deriving show]

val match_actuals_to_formals
  :  Expression.t Call.Argument.t list ->
  Root.t list ->
  (Expression.t * argument_match list) list
