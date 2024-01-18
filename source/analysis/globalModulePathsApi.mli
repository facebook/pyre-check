(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create : (unit -> Ast.ModulePath.t list) -> t

val module_paths : t -> Ast.ModulePath.t list

(* Return all qualifiers backed by actual source (i.e. exclude implicit packages) *)
val explicit_qualifiers : t -> Ast.Reference.t list

(* Return all the projects that should be type checked (exclude dependencies) *)
val type_check_qualifiers : t -> Ast.Reference.t list
