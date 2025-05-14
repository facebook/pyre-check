(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val source_of_qualifier
  :  string_annotation_preserve_location:bool ->
  parse_result_of_qualifier:(Ast.Reference.t -> Parsing.ParseResult.t option) ->
  Ast.Reference.t ->
  Ast.Source.t option
