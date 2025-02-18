(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type analysis =
  | Forward
  | Backward

type t

(* A profiler that does nothing. *)
val disabled : t

val start : callable:Interprocedural.Target.t -> unit -> t

val track_duration : profiler:t -> name:string -> f:(unit -> 'a) -> 'a

val track_statement_analysis
  :  profiler:t ->
  analysis:analysis ->
  statement:Statement.t ->
  f:(unit -> 'a) ->
  'a

val track_expression_analysis
  :  profiler:t ->
  analysis:analysis ->
  expression:Expression.t ->
  f:(unit -> 'a) ->
  'a

val track_model_fetch
  :  profiler:t ->
  analysis:analysis ->
  call_target:Interprocedural.Target.t ->
  f:(unit -> Model.t) ->
  Model.t

module ApplyCallStep : sig
  type t =
    | ApplyCallForArgumentSinks
    | ApplyCallForArgumentSources
    | ApplyCallForReturn
    | ApplyCallEffects
    | CheckIssuesForArgument
    | BuildTaintInTaintOutMapping
    | ApplyTitoForArgument
end

val track_apply_call_step
  :  profiler:t ->
  analysis:analysis ->
  step:ApplyCallStep.t ->
  call_target:Interprocedural.Target.t ->
  location:Location.t ->
  argument:Expression.t option ->
  f:(unit -> 'a) ->
  'a

val stop : max_number_expressions:int -> max_number_apply_call_steps:int -> t -> unit
