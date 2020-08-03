(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre

module ClassDefinitionsCache : sig
  val invalidate : unit -> unit
end

module T : sig
  type parse_result = {
    models: TaintResult.call_model Interprocedural.Callable.Map.t;
    skip_overrides: Ast.Reference.Set.t;
    errors: string list;
  }
end

val parse
  :  resolution:Analysis.Resolution.t ->
  ?path:Path.t ->
  ?rule_filter:int list ->
  source:string ->
  configuration:Configuration.t ->
  TaintResult.call_model Interprocedural.Callable.Map.t ->
  T.parse_result

val verify_model_syntax : path:Path.t -> source:string -> unit
