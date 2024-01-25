(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create
  :  controls:EnvironmentControls.t ->
  module_path_of_qualifier:(Ast.Reference.t -> Ast.ModulePath.t option) ->
  raw_source_of_qualifier:(Ast.Reference.t -> Parsing.ParseResult.t option) ->
  t

val controls : t -> EnvironmentControls.t

val module_path_of_qualifier : t -> Ast.Reference.t -> Ast.ModulePath.t option

val relative_path_of_qualifier : t -> Ast.Reference.t -> string option

val is_qualifier_tracked : t -> Ast.Reference.t -> bool

val raw_source_of_qualifier : t -> Ast.Reference.t -> Parsing.ParseResult.t option

val processed_source_of_qualifier : t -> Ast.Reference.t -> Ast.Source.t option
