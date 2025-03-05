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

module type Analysis = sig
  val perf_data_file_name : string

  module ApplyCallStep : sig
    type t [@@deriving sexp, compare]

    val pp_short : Format.formatter -> t -> unit
  end
end

module Make (Analysis : Analysis) : sig
  type t

  (* A profiler that does nothing. *)
  val disabled : t

  val start : callable:Target.t -> unit -> t

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
    call_target:Target.t ->
    f:(unit -> 'a) ->
    'a

  val track_apply_call_step
    :  profiler:t ->
    analysis:analysis ->
    step:Analysis.ApplyCallStep.t ->
    call_target:Target.t option ->
    location:Location.t ->
    argument:Expression.t option ->
    f:(unit -> 'a) ->
    'a

  val stop : max_number_expressions:int -> max_number_apply_call_steps:int -> t -> unit
end
