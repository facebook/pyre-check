(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

val name : string

val run_on_define : qualifier:Reference.t -> Statement.Define.t Node.t -> Error.t list

val run
  :  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  source:Source.t ->
  Error.t list
