(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val instantiate_path
  :  build_system:BuildSystem.t ->
  ast_environment:Analysis.AstEnvironment.ReadOnly.t ->
  Ast.Reference.t ->
  string option
(** Given a Python module name, Return path to the corresponding Python source file as a string.
    This API will take into account any potential path translation done by the {!BuildSystem.t}. *)
