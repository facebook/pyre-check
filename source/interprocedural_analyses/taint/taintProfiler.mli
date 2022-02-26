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
val none : t

val create : unit -> t

val track_duration : profiler:t -> name:string -> f:(unit -> 'a) -> 'a

val track_statement_analysis
  :  profiler:t ->
  analysis:analysis ->
  statement:Statement.statement Node.t ->
  f:(unit -> 'a) ->
  'a

val track_model_fetch
  :  profiler:t ->
  analysis:analysis ->
  call_target:Interprocedural.Target.t ->
  f:(unit -> Model.t) ->
  Model.t

val dump : t -> unit
