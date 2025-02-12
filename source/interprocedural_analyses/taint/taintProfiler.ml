(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TaintProfiler: a small helper to track the performance of the forward or
 * backward taint analysis of a callable. *)

open Ast
open Core
open Pyre

type analysis =
  | Forward
  | Backward
[@@deriving eq, sexp, compare]

type step_event = {
  name: string;
  seconds: float;
}

type statement_event = {
  statement: Statement.t;
  analysis: analysis;
  seconds: float;
}

type expression_event = {
  expression: Expression.t;
  analysis: analysis;
  seconds: float;
}

type fetch_model_event = {
  target: Interprocedural.Target.t;
  analysis: analysis;
  seconds: float;
  model_words: int;
}

type expression_timer = {
  timer: Timer.t;
  accumulated: float;
}

type profiler = {
  mutable step_events: step_event list;
  mutable statement_events: statement_event list;
  mutable expression_events: expression_event list;
  mutable fetch_model_events: fetch_model_event list;
  whole_timer: Timer.t;
  mutable current_expression: expression_timer;
}

type t = profiler option

(* A profiler that does nothing. *)
let none = None

let create () =
  Some
    {
      step_events = [];
      statement_events = [];
      expression_events = [];
      fetch_model_events = [];
      whole_timer = Timer.start ();
      current_expression = { timer = Timer.start (); accumulated = 0. };
    }


let track_duration ~profiler ~name ~f =
  match profiler with
  | None -> f ()
  | Some profiler ->
      let timer = Timer.start () in
      let result = f () in
      let seconds = Timer.stop_in_sec timer in
      profiler.step_events <- { name; seconds } :: profiler.step_events;
      result


let track_statement_analysis ~profiler ~analysis ~statement ~f =
  match profiler with
  | None -> f ()
  | Some profiler ->
      let timer = Timer.start () in
      let result = f () in
      let seconds = Timer.stop_in_sec timer in
      profiler.statement_events <- { statement; analysis; seconds } :: profiler.statement_events;
      result


let track_expression_analysis ~profiler ~analysis ~expression ~f =
  match profiler with
  | None -> f ()
  | Some profiler ->
      (* Expressions are nested, therefore we need to stop the timer for the outer
       * expression and accumulate the time spent. *)
      let outer_expression_accumulated_seconds =
        let { timer; accumulated } = profiler.current_expression in
        accumulated +. Timer.stop_in_sec timer
      in
      profiler.current_expression <- { timer = Timer.start (); accumulated = 0. };
      let result = f () in
      let current_expression = profiler.current_expression in
      profiler.expression_events <-
        {
          expression;
          analysis;
          seconds = Timer.stop_in_sec current_expression.timer +. current_expression.accumulated;
        }
        :: profiler.expression_events;
      profiler.current_expression <-
        { timer = Timer.start (); accumulated = outer_expression_accumulated_seconds };
      result


let track_model_fetch ~profiler ~analysis ~call_target ~f =
  match profiler with
  | None -> f ()
  | Some profiler ->
      let timer = Timer.start () in
      let model = f () in
      let seconds = Timer.stop_in_sec timer in
      let model_words = model |> Obj.repr |> Obj.reachable_words in
      profiler.fetch_model_events <-
        { target = call_target; analysis; seconds; model_words } :: profiler.fetch_model_events;
      model


module StatementKey = struct
  type t = {
    statement: Statement.t;
    analysis: analysis;
  }
  [@@deriving sexp, compare]
end

module StatementEventMap = Map.Make (StatementKey)

module ExpressionKey = struct
  type t = {
    expression: Expression.t;
    analysis: analysis;
  }
  [@@deriving sexp, compare]
end

module ExpressionEventMap = Map.Make (ExpressionKey)

module TargetKey = struct
  type t = {
    target: Interprocedural.Target.t;
    analysis: analysis;
  }
  [@@deriving sexp, compare]
end

module FetchModelEventMap = Map.Make (TargetKey)

let dump ~max_number_expressions = function
  | None -> ()
  | Some
      {
        step_events;
        statement_events;
        expression_events;
        fetch_model_events;
        whole_timer;
        current_expression = _;
      } ->
      let total_seconds = Timer.stop_in_sec whole_timer in
      Log.dump "Performance metrics:";
      Log.dump "Total time: %.2fs" total_seconds;

      (* Add implicit step events. *)
      let statement_events_seconds =
        List.fold ~init:0.0 ~f:(fun sofar { statement = _; seconds; _ } -> sofar +. seconds)
      in
      let filter_statement_events analysis =
        List.filter ~f:(fun { statement = _; analysis = event_analysis; _ } ->
            equal_analysis analysis event_analysis)
      in
      let fetch_model_events_seconds =
        List.fold ~init:0.0 ~f:(fun sofar { target = _; seconds; _ } -> sofar +. seconds)
      in
      let filter_model_events analysis =
        List.filter ~f:(fun { target = _; analysis = event_analysis; _ } ->
            equal_analysis analysis event_analysis)
      in
      let step_events =
        {
          name = "Forward analysis - statements";
          seconds = statement_events |> filter_statement_events Forward |> statement_events_seconds;
        }
        :: {
             name = "Backward analysis - statements";
             seconds =
               statement_events |> filter_statement_events Backward |> statement_events_seconds;
           }
        :: {
             name = "Forward analysis - fetch models";
             seconds =
               fetch_model_events |> filter_model_events Forward |> fetch_model_events_seconds;
           }
        :: {
             name = "Backward analysis - fetch models";
             seconds =
               fetch_model_events |> filter_model_events Backward |> fetch_model_events_seconds;
           }
        :: step_events
      in

      Log.dump "Performance per step:";
      Log.dump "| Name | Time | Percent of Total Time |";
      let step_events =
        List.sort
          ~compare:
            (fun { name = _; seconds = left_seconds } { name = _; seconds = right_seconds } ->
            Float.compare right_seconds left_seconds)
          step_events
      in
      let display_step_row { name; seconds } =
        Log.dump "| %s | %.3fs | %.2f%% |" name seconds (seconds /. total_seconds *. 100.0)
      in
      List.iter step_events ~f:display_step_row;

      Log.dump "Performance per statement:";
      let add_statement_event map { statement; analysis; seconds } =
        Map.update map { StatementKey.statement; analysis } ~f:(function
            | None -> [seconds]
            | Some times -> seconds :: times)
      in
      let statement_events =
        statement_events
        |> List.fold ~f:add_statement_event ~init:StatementEventMap.empty
        |> Core.Map.to_alist
        |> List.sort ~compare:(fun (_, left_times) (_, right_times) ->
               let left_seconds = List.fold ~init:0.0 ~f:( +. ) left_times in
               let right_seconds = List.fold ~init:0.0 ~f:( +. ) right_times in
               Float.compare right_seconds left_seconds)
      in
      Log.dump
        "| Line | Column | Analysis | Iterations | Total Time | Percent of Total Time | Statement |";
      let analysis_pp formatter = function
        | Forward -> Format.fprintf formatter "F"
        | Backward -> Format.fprintf formatter "B"
      in
      let display_statement_row
          ({ StatementKey.statement = { location; _ } as statement; analysis }, times)
        =
        let iterations = List.length times in
        let seconds = List.fold ~init:0.0 ~f:( +. ) times in
        Log.dump
          "| %4d | %3d | %a | %2d | %8.3fs | %4.2f%% | %a |"
          location.start.line
          location.start.column
          analysis_pp
          analysis
          iterations
          seconds
          (seconds /. total_seconds *. 100.0)
          Statement.pp
          statement
      in
      List.iter statement_events ~f:display_statement_row;

      Log.dump "Performance per expression:";
      let add_expression_event map { expression; analysis; seconds } =
        Map.update map { ExpressionKey.expression; analysis } ~f:(function
            | None -> [seconds]
            | Some times -> seconds :: times)
      in
      let expression_events =
        expression_events
        |> List.fold ~f:add_expression_event ~init:ExpressionEventMap.empty
        |> Core.Map.to_alist
        |> List.sort ~compare:(fun (_, left_times) (_, right_times) ->
               let left_seconds = List.fold ~init:0.0 ~f:( +. ) left_times in
               let right_seconds = List.fold ~init:0.0 ~f:( +. ) right_times in
               Float.compare right_seconds left_seconds)
      in
      if List.length expression_events > max_number_expressions then
        Log.dump
          "WARNING: Showing the first %d expressions out of %d expressions"
          max_number_expressions
          (List.length expression_events);
      Log.dump
        "| Line | Column | Analysis | Iterations | Total Time | Percent of Total Time | Expression \
         |";
      let display_expression_row
          ({ ExpressionKey.expression = { location; _ } as expression; analysis }, times)
        =
        let iterations = List.length times in
        let seconds = List.fold ~init:0.0 ~f:( +. ) times in
        Log.dump
          "| %4d | %3d | %a | %2d | %8.3fs | %4.2f%% | %a |"
          location.start.line
          location.start.column
          analysis_pp
          analysis
          iterations
          seconds
          (seconds /. total_seconds *. 100.0)
          Expression.pp
          expression
      in
      List.iter (List.take expression_events max_number_expressions) ~f:display_expression_row;

      Log.dump "Performance per callee model:";
      let add_fetch_model_event map ({ target; analysis; _ } as event) =
        Map.update map { TargetKey.target; analysis } ~f:(function
            | None -> [event]
            | Some events -> event :: events)
      in
      let fetch_model_events =
        fetch_model_events
        |> List.fold ~f:add_fetch_model_event ~init:FetchModelEventMap.empty
        |> Core.Map.to_alist
        |> List.sort ~compare:(fun (_, left_events) (_, right_events) ->
               let left_seconds =
                 left_events
                 |> List.map ~f:(fun { seconds; _ } -> seconds)
                 |> List.fold ~init:0.0 ~f:( +. )
               in
               let right_seconds =
                 right_events
                 |> List.map ~f:(fun { seconds; _ } -> seconds)
                 |> List.fold ~init:0.0 ~f:( +. )
               in
               Float.compare right_seconds left_seconds)
      in
      Log.dump
        "| Analysis | Fetch Count | Total Time | Percent of Total Time | Average Model Size \
         (bytes) | Target |";
      let display_model_row ({ TargetKey.target; analysis }, events) =
        let count = List.length events in
        let seconds =
          events |> List.map ~f:(fun { seconds; _ } -> seconds) |> List.fold ~init:0.0 ~f:( +. )
        in
        let average_size =
          events
          |> List.map ~f:(fun { model_words; _ } -> model_words)
          |> List.fold ~init:0 ~f:( + )
          |> fun size -> 8 * size / count
        in
        Log.dump
          "| %a | %3d | %8.3fs | %4.2f%% | %d | %a |"
          analysis_pp
          analysis
          count
          seconds
          (seconds /. total_seconds *. 100.0)
          average_size
          Interprocedural.Target.pp_pretty
          target
      in
      List.iter fetch_model_events ~f:display_model_row
