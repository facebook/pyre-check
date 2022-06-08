(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
module Error = AnalysisError

val name : string

val run_on_define : qualifier:Reference.t -> Statement.Define.t Node.t -> Error.t list

val run_for_testing : source:Source.t -> Error.t list

module StatementKey = Int

type defined_locals = Scope.Binding.t Identifier.Map.t

val defined_locals_at_each_statement
  :  Statement.Define.t Node.t ->
  (* TODO(T112570623): Don't store statements in the value. *)
  (Statement.t * defined_locals) StatementKey.Map.t
