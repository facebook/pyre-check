(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Error = AnalysisError

val ignore : Ast.Source.t -> Error.t list -> Error.t list
