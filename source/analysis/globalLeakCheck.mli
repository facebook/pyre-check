(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

val check_qualifier
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  Reference.t ->
  Error.t list option
