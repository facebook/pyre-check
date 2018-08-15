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
  path: AccessPathTree.Label.path;
}


let of_access path access_element =
  match path, access_element with
  | Some path, Access.Identifier id ->
      Some (AccessPathTree.Label.Field id :: path)
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
  | Call of {
      callee: normalized_expression;
      arguments: Argument.t list Node.t;
    }
  | Global of Identifier.t list
  | Local of Identifier.t
  | Expression of Expression.t
[@@deriving show]


let normalize_access_list left = function
  | Access.Identifier member ->
      Access { expression = left; member }
  | Access.Call arguments ->
      Call { callee = left; arguments = arguments }
  | Access.Expression _ ->
      failwith "invalid expr in access (not in root position)"


let is_local identifier =
  String.is_prefix ~prefix:"$" (Identifier.show identifier)


let rec split_maximal_prefix path =
  match path with
  | [] ->
      ([], [])
  | Access.Identifier identifier :: rest ->
      let (prefix, rest) = split_maximal_prefix rest in
      (identifier :: prefix), rest
  | Access.Call _ :: _ ->
      [], path
  | Access.Expression _ :: _ ->
      failwith "invalid expr in access (not in root position)"


let rec split_root = function
  | Access.Identifier identifier :: rest when is_local identifier ->
      Local identifier, rest
  | Access.Expression expression :: rest ->
      Expression expression, rest
  | Access.Identifier identifier :: rest ->
      let (prefix, rest) = split_maximal_prefix rest in
      Global (identifier :: prefix), rest
  | Access.Call _ :: _ ->
      failwith "invalid root (call) in access"
  | _ ->
      failwith "empty access"


let normalize_access path =
  let (root, rest) = split_root path in
  List.fold rest ~init:root ~f:normalize_access_list


let rec as_access = function
  | Global access ->
      Access.create_from_identifiers access
  | Local identifier ->
      Access.create_from_identifiers [identifier]
  | Expression expression ->
      [Access.Expression expression]
  | Call { callee; arguments; } ->
      let callee_access = as_access callee in
      callee_access @ [Access.Call arguments]
  | Access { expression; member; } ->
      let left = as_access expression in
      left @ [Access.Identifier member]
