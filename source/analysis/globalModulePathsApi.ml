(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = { module_paths: unit -> Ast.ModulePath.t list }

let create module_paths = { module_paths }

let module_paths { module_paths; _ } = module_paths ()

(* Return all qualifiers backed by actual source (i.e. exclude implicit packages) *)
let explicit_qualifiers api = module_paths api |> List.map ~f:Ast.ModulePath.qualifier

(* Return all the projects that should be type checked (exclude dependencies) *)
let type_check_qualifiers api =
  module_paths api
  |> List.filter ~f:Ast.ModulePath.should_type_check
  |> List.map ~f:Ast.ModulePath.qualifier
