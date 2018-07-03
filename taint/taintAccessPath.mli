(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre


(* Roots representing parameters, locals, and special return value in models. *)
module Root : sig
  type t =
    | LocalResult  (* Special root representing the return value location. *)
    | Parameter of { name: Identifier.t; position: int; }
    | Variable of Identifier.t
  [@@deriving compare, sexp, show, hash]
end


type t = { root: Root.t; path: TaintAccessPathTree.Label.path; }


val of_accesses: 'a Expression.Record.Access.access list -> t option
val of_expression: Expression.t -> t option


type normalized_expression =
  | Access of { expression: normalized_expression; member: Identifier.t }
  | Call of {
      callee: normalized_expression;
      arguments: Expression.t Expression.Record.Argument.record list;
    }
  | Identifier of Identifier.t
  | Expression of Expression.t
[@@deriving show]

val normalize_access : Expression.t Expression.Access.access list -> normalized_expression
