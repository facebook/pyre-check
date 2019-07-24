(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Statement

val run
  :  environment:Environment.t ->
  define:Define.t Node.t ->
  existing_model:TaintResult.call_model ->
  TaintResult.Forward.model * TaintResult.result
