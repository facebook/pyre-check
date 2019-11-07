(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
module Error = AnalysisError

val run_on_source : source:Source.t -> Error.t list -> Error.t list

val run : modules:Reference.t list -> TypeEnvironment.ReadOnly.t -> Error.t list
