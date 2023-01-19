(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

val check_define
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  qualifier:Reference.t ->
  Statement.Define.t Node.t ->
  Error.t list
