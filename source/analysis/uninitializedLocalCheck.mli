(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
module Error = AnalysisError

val check_define : qualifier:Reference.t -> Statement.Define.t Node.t -> Error.t list

val check_module_for_testing : source:Source.t -> Error.t list

module StatementKey = Int

val extract_reads_in_statement : Statement.t -> string Node.t list

(* Get bindings for local identifiers. This cannot live in scope.ml because it deals with
   qualification. *)
val local_bindings : Scope.Scope.t -> Scope.Binding.t Identifier.Map.t

type defined_locals = Scope.Binding.t Identifier.Map.t

val defined_locals_at_each_statement
  :  Statement.Define.t Node.t ->
  (* TODO(T112570623): Don't store statements in the value. *)
  (Statement.t * defined_locals) StatementKey.Map.t
