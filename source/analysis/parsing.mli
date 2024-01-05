(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ParserError : sig
  type t = {
    module_path: Ast.ModulePath.t;
    location: Ast.Location.t;
    is_suppressed: bool;
    message: string;
  }
  [@@deriving sexp, compare, hash]
end

val create_source
  :  typecheck_flags:Ast.Source.TypecheckFlags.t ->
  module_path:Ast.ModulePath.t ->
  Ast.Statement.t list ->
  Ast.Source.t

val load_and_parse
  :  controls:EnvironmentControls.t ->
  get_raw_code:(Ast.ModulePath.t -> (string, string) Result.t) ->
  Ast.ModulePath.t ->
  (Ast.Source.t, ParserError.t) Result.t
