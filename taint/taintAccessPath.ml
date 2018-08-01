(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre


(** Roots representing parameters, locals, and special return value in models. *)
module Root = struct
  type t =
    | LocalResult (* Special root representing the return value location. *)
    | Parameter of { position: int }
    | Variable of Identifier.t
  [@@deriving compare, sexp, show, hash]
end


type t = {
  root: Root.t;
  path: TaintAccessPathTree.Label.path;
}


let of_access path access_element =
  match path, access_element with
  | Some path, Access.Identifier id ->
      Some (TaintAccessPathTree.Label.Field id :: path)
  | _ -> None


let of_accesses = function
  | Access.Identifier root :: rest ->
      List.fold ~init:(Some []) ~f:of_access rest
      >>| (fun path -> { root = Root.Variable root; path })
  | _ -> None


let of_expression = function
  | { Node.value = Access access; _ } -> of_accesses access
  | _ -> None


type normalized_expression =
  | Access of { expression: normalized_expression; member: Identifier.t }
  | Call of { callee: normalized_expression; arguments: Argument.t list }
  | Identifier of Identifier.t
  | Expression of Expression.t
[@@deriving show]


let normalize_root_access = function
  | Access.Identifier identifier ->
      Identifier identifier
  | Access.Expression expression ->
      Expression expression
  | Access.Call _ ->
      failwith "invalid root (call) in access"


let normalize_access_list left = function
  | Access.Identifier member ->
      Access { expression = left; member }
  | Access.Call arguments ->
      Call { callee = left; arguments = arguments.Node.value }
  | Access.Expression _ ->
      failwith "invalid expr in access (not in root position)"


let normalize_access = function
  | root :: path ->
      List.fold path ~init:(normalize_root_access root) ~f:normalize_access_list
  | _ ->
      failwith "empty access"
