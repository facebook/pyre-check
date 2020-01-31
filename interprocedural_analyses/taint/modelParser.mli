(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre

val parse
  :  resolution:Analysis.Resolution.t ->
  ?path:Path.t ->
  ?verify:bool ->
  ?rule_filter:int list ->
  source:string ->
  configuration:Configuration.t ->
  TaintResult.call_model Interprocedural.Callable.Map.t ->
  TaintResult.call_model Interprocedural.Callable.Map.t

val verify_model_syntax : path:Path.t -> source:string -> unit
