(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
module Error = AnalysisError

val name : string

val run
  :  configuration:Configuration.Analysis.t ->
  global_resolution:GlobalResolution.t ->
  source:Source.t ->
  Error.t list
