(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
module Error = AnalysisError

val ignore : Source.t -> Error.t list -> Error.t list

val add_local_mode_errors
  :  define:Statement.Define.t Node.t ->
  Source.t ->
  Error.t list ->
  Error.t list
