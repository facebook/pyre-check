(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre


(* Placeholder for representing parameters, locals, and special return value. *)
module Root = struct
  type t =
    | LocalResult  (* Special root representing the return value location. *)
    | Parameter of { name: Identifier.t; position: int; }
    | Variable of Identifier.t
  [@@deriving compare, sexp, show, hash]
end


type t = { root: Root.t; path: TaintAccessPathTree.Label.path; }


let of_access path_opt access_element =
  match path_opt, access_element with
  | Some path, Record.Access.Identifier id ->
      Some (TaintAccessPathTree.Label.Field id :: path)
  | _ -> None


let of_accesses = function
  | Record.Access.Identifier root :: rest ->
      List.fold_left ~init:(Some []) ~f:of_access rest
      >>| (fun path -> { root=Root.Variable root; path; })
  | _ -> None


let of_expression expression =
  match expression.Node.value with
  | Access a -> of_accesses a
  | _ -> None

type normalized_expression =
  | Access of { expression: normalized_expression; member: Identifier.t }
  | Call of { callee: normalized_expression; arguments: Expression.t Record.Argument.record list }
  | Identifier of Identifier.t
  | Expression of Expression.t
[@@deriving show]


let normalize_root_access = function
    Access.Identifier identifier ->
      Identifier identifier
  | Access.Expression expression ->
      Expression expression
  | Access.Call _ ->
      failwith "invalid root (call) in access"


let normalize_access_list left = function
  | Access.Identifier member ->
      Access { expression = left; member; }
  | Access.Call arguments ->
      Call { callee = left; arguments = arguments.Node.value; }
  | Access.Expression _ ->
      failwith "invalid expr in access (not in root position)"


let normalize_access = function
  | root :: path ->
      let init = normalize_root_access root in
      List.fold path ~f:normalize_access_list ~init
  | _ ->
      failwith "empty access"
