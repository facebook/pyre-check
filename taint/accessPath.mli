(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Expression


(** Roots representing parameters, locals, and special return value in models. *)
module Root : sig
  type t =
    | LocalResult (* Special root representing the return value location. *)
    | PositionalParameter of { position: int; name: Identifier.t; }
    | NamedParameter of { name: Identifier.t; }
    | StarParameter of { position: int }
    | StarStarParameter of { excluded: Identifier.t list }
    | Variable of Identifier.t
  [@@deriving compare, eq, sexp, show, hash]

  val normalize_parameters: 'a Parameter.t list -> (t * Identifier.t * 'a option) list

  val parameter_name: t -> string option
end


type t = {
  root: Root.t;
  path: AbstractTreeDomain.Label.path;
}

val create: Root.t -> AbstractTreeDomain.Label.path -> t

val of_accesses: Access.t -> t option

val of_expression: Expression.t -> t option

val get_index: Expression.t -> AbstractTreeDomain.Label.t

type normalized_expression =
  | Access of { expression: normalized_expression; member: Identifier.t }
  | Call of {
      callee: normalized_expression;
      arguments: Argument.t list Node.t;
    }
  | Index of {
      expression: normalized_expression;
      index: AbstractTreeDomain.Label.t;
      original: Identifier.t;
      arguments: ((Expression.t Argument.record) list) Node.t;
    }
  | Global of Access.t
  | Local of Identifier.t
  | Expression of Expression.t
[@@deriving eq, show]

val normalize_access: resolution: Resolution.t -> Access.t -> normalized_expression

val as_access: normalized_expression -> Access.t

val to_json: t -> Yojson.Safe.json

type argument_match = {
  root: Root.t;
  actual_path: AbstractTreeDomain.Label.path;
  formal_path: AbstractTreeDomain.Label.path;
}

val match_actuals_to_formals :
  Expression.t Argument.record list
  -> Root.t list
  -> (Expression.t * argument_match list) list
