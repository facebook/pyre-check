(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
module NamedTuples = NamedTuples
module Filter = Filter
module NewType = NewType

val apply_to_ast : Source.t -> Source.t

val apply_to_environment : Environment.t -> GlobalResolution.t -> Source.t -> unit
