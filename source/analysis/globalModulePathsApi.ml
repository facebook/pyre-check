(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The core analysis engine of Pyre always limits fanouts to O(request),
 * for example pulling in just enough dependency information to type check
 * a single function. The core analysis never relies on a "full" pass being
 * done, such as parsing all source code.
 *
 * This is important because the IDE needs to be lazy and make sure *everything*
 * it does is O(request) so that startup times stay fast in huge codebases.
 *
 * But we do in fact need global information sometimes, for example:
 * - running a full check on all in-project code in CI
 * - static analyses (e.g. Pysa) that require all classes or all symbol names
 *
 * This module provides the ability to list all qualifiers and is used to power
 * all such project-global actions. Importantly it is not possible to create
 * a `GlobalModulePaths.t` from any "normal" read-only Pyre environment, we
 * can only create it in top-level code from read-write environments. This enforces
 * type safety, and makes explicit in the type signatures which functions are using
 * global information.
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
