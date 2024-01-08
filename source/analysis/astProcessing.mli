(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

val get_processed_source
  :  get_raw_source:(Ast.Reference.t -> (Ast.Source.t, Parsing.ParserError.t) Result.t option) ->
  Ast.Reference.t ->
  Ast.Source.t option
