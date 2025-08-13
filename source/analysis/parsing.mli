(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module LoadResult : sig
  module Code : sig
    type t = string [@@deriving sexp, compare, hash]
  end

  module Error : sig
    type t = string [@@deriving sexp, compare, hash]
  end

  type t = (Code.t, Error.t) Result.t [@@deriving sexp, compare, hash]
end

module ParseResult : sig
  module Error : sig
    type t = {
      module_path: Ast.ModulePath.t;
      location: Ast.Location.t;
      is_suppressed: bool;
      message: string;
    }
    [@@deriving sexp, compare, hash]
  end

  type t = (Ast.Source.t, Error.t) Result.t [@@deriving sexp, compare, hash]
end

val create_source
  :  typecheck_flags:Ast.Source.TypecheckFlags.t ->
  module_path:Ast.ModulePath.t ->
  Ast.Statement.t list ->
  Ast.Source.t

val parse_result_of_load_result
  :  controls:EnvironmentControls.t ->
  post_process:bool ->
  Ast.ModulePath.t ->
  LoadResult.t ->
  ParseResult.t
