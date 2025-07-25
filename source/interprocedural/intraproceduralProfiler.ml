(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* IntraproceduralProfiler: a small helper to track the performance of the forward or
 * backward intra-procedural analysis of a callable. *)

(* Core shadows/deprecates the stdlib Unix module. *)
module CamlUnix = Unix
open Ast
open Core
open Pyre

type analysis =
  | Forward
  | Backward
[@@deriving equal, sexp, compare]

module StepEvent = struct
  type t = {
    name: string;
    seconds: float;
  }
end

module StatementEvent = struct
  type t = {
    statement: Statement.t;
    analysis: analysis;
    seconds: float;
  }

  module Key = struct
    type t = {
      statement: Statement.t;
      analysis: analysis;
    }
    [@@deriving sexp, compare]
  end

  module Map = Map.Make (Key)
end

module ExpressionEvent = struct
  type t = {
    expression: Expression.t;
    analysis: analysis;
    seconds: float;
  }

  module Key = struct
    type t = {
      expression: Expression.t;
      analysis: analysis;
    }
    [@@deriving sexp, compare]
  end

  module Map = Map.Make (Key)
end

module FetchModelEvent = struct
  type t = {
    target: Target.t;
    analysis: analysis;
    seconds: float;
    model_words: int;
  }

  module Key = struct
    type t = {
      target: Target.t;
      analysis: analysis;
    }
    [@@deriving sexp, compare]
  end

  module Map = Map.Make (Key)
end

module ExpressionTimer = struct
  type t = {
    timer: Timer.t;
    accumulated: float;
  }
end

module type Analysis = sig
  val perf_data_file_name : string

  module ApplyCallStep : sig
    type t [@@deriving sexp, compare]

    val pp_short : Format.formatter -> t -> unit
  end
end

module Make (Analysis : Analysis) = struct
  module ApplyCallStepEvent = struct
    type t = {
      target: Target.t option;
      location: Location.t;
      analysis: analysis;
      step: Analysis.ApplyCallStep.t;
      argument: Expression.t option;
      seconds: float;
    }

    module Key = struct
      type t = {
        target: Target.t option;
        location: Location.t;
        analysis: analysis;
        step: Analysis.ApplyCallStep.t;
        argument: Expression.t option;
      }
      [@@deriving sexp, compare]
    end

    let key { target; location; analysis; step; argument; _ } =
      { Key.target; location; analysis; step; argument }


    module Map = Map.Make (Key)
  end

  type profiler = {
    mutable step_events: StepEvent.t list;
    mutable statement_events: StatementEvent.t list;
    mutable expression_events: ExpressionEvent.t list;
    mutable fetch_model_events: FetchModelEvent.t list;
    mutable apply_call_step_events: ApplyCallStepEvent.t list;
    whole_timer: Timer.t;
    mutable current_expression: ExpressionTimer.t;
    perf_profiler: PerfProfiler.t option;
  }

  type t = profiler option

  (* A profiler that does nothing. *)
  let disabled = None

  let start ~enable_perf ~callable () =
    Log.dump "Starting to profile the analysis of %a" Target.pp_pretty callable;

    let perf_profiler =
      if enable_perf then
        Some
          (PerfProfiler.start
             ~filename:
               (Format.asprintf
                  "perf.%s.%a.%d.data"
                  Analysis.perf_data_file_name
                  Target.pp_pretty
                  callable
                  (Int.of_float (CamlUnix.time ())))
             ())
      else
        None
    in

    Some
      {
        step_events = [];
        statement_events = [];
        expression_events = [];
        fetch_model_events = [];
        apply_call_step_events = [];
        whole_timer = Timer.start ();
        current_expression = { timer = Timer.start (); accumulated = 0. };
        perf_profiler;
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
          let { ExpressionTimer.timer; accumulated } = profiler.current_expression in
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


  let track_apply_call_step ~profiler ~analysis ~step ~call_target ~location ~argument ~f =
    match profiler with
    | None -> f ()
    | Some profiler ->
        let timer = Timer.start () in
        let result = f () in
        let seconds = Timer.stop_in_sec timer in
        profiler.apply_call_step_events <-
          { target = call_target; location; analysis; step; argument; seconds }
          :: profiler.apply_call_step_events;
        result


  let stop ~max_number_expressions ~max_number_apply_call_steps = function
    | None -> ()
    | Some
        {
          step_events;
          statement_events;
          expression_events;
          fetch_model_events;
          apply_call_step_events;
          whole_timer;
          current_expression = _;
          perf_profiler;
        } ->
        let total_seconds = Timer.stop_in_sec whole_timer in
        (match perf_profiler with
        | Some perf_profiler -> PerfProfiler.stop perf_profiler
        | None -> ());
        Log.dump "Performance metrics:";
        Log.dump "Total time: %.2fs" total_seconds;

        (* Add implicit step events. *)
        let statement_events_seconds =
          List.fold ~init:0.0 ~f:(fun sofar { StatementEvent.seconds; _ } -> sofar +. seconds)
        in
        let filter_statement_events analysis =
          List.filter ~f:(fun { StatementEvent.analysis = event_analysis; _ } ->
              equal_analysis analysis event_analysis)
        in
        let fetch_model_events_seconds =
          List.fold ~init:0.0 ~f:(fun sofar { FetchModelEvent.seconds; _ } -> sofar +. seconds)
        in
        let filter_model_events analysis =
          List.filter ~f:(fun { FetchModelEvent.analysis = event_analysis; _ } ->
              equal_analysis analysis event_analysis)
        in
        let step_events =
          {
            StepEvent.name = "Forward analysis - statements";
            seconds =
              statement_events |> filter_statement_events Forward |> statement_events_seconds;
          }
          :: {
               StepEvent.name = "Backward analysis - statements";
               seconds =
                 statement_events |> filter_statement_events Backward |> statement_events_seconds;
             }
          :: {
               StepEvent.name = "Forward analysis - fetch models";
               seconds =
                 fetch_model_events |> filter_model_events Forward |> fetch_model_events_seconds;
             }
          :: {
               StepEvent.name = "Backward analysis - fetch models";
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
        let display_step_row { StepEvent.name; seconds } =
          Log.dump "| %s | %.3fs | %.2f%% |" name seconds (seconds /. total_seconds *. 100.0)
        in
        List.iter step_events ~f:display_step_row;

        Log.dump "Performance per statement:";
        let add_statement_event map { StatementEvent.statement; analysis; seconds } =
          Map.update map { StatementEvent.Key.statement; analysis } ~f:(function
              | None -> [seconds]
              | Some times -> seconds :: times)
        in
        let statement_events =
          statement_events
          |> List.fold ~f:add_statement_event ~init:StatementEvent.Map.empty
          |> Core.Map.to_alist
          |> List.sort ~compare:(fun (_, left_times) (_, right_times) ->
                 let left_seconds = List.fold ~init:0.0 ~f:( +. ) left_times in
                 let right_seconds = List.fold ~init:0.0 ~f:( +. ) right_times in
                 Float.compare right_seconds left_seconds)
        in
        Log.dump
          "| Line | Column | Analysis | Iterations | Total Time | Percent of Total Time | \
           Statement |";
        let analysis_pp formatter = function
          | Forward -> Format.fprintf formatter "F"
          | Backward -> Format.fprintf formatter "B"
        in
        let display_statement_row
            ({ StatementEvent.Key.statement = { location; _ } as statement; analysis }, times)
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
        let add_expression_event map { ExpressionEvent.expression; analysis; seconds } =
          Map.update map { ExpressionEvent.Key.expression; analysis } ~f:(function
              | None -> [seconds]
              | Some times -> seconds :: times)
        in
        let expression_events =
          expression_events
          |> List.fold ~f:add_expression_event ~init:ExpressionEvent.Map.empty
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
          "| Line | Column | Analysis | Iterations | Total Time | Percent of Total Time | \
           Expression |";
        let display_expression_row
            ({ ExpressionEvent.Key.expression = { location; _ } as expression; analysis }, times)
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

        Log.dump "Performance of fetching callee models:";
        let add_fetch_model_event map ({ FetchModelEvent.target; analysis; _ } as event) =
          Map.update map { FetchModelEvent.Key.target; analysis } ~f:(function
              | None -> [event]
              | Some events -> event :: events)
        in
        let fetch_model_events =
          fetch_model_events
          |> List.fold ~f:add_fetch_model_event ~init:FetchModelEvent.Map.empty
          |> Core.Map.to_alist
          |> List.sort ~compare:(fun (_, left_events) (_, right_events) ->
                 let left_seconds =
                   left_events
                   |> List.map ~f:(fun { FetchModelEvent.seconds; _ } -> seconds)
                   |> List.fold ~init:0.0 ~f:( +. )
                 in
                 let right_seconds =
                   right_events
                   |> List.map ~f:(fun { FetchModelEvent.seconds; _ } -> seconds)
                   |> List.fold ~init:0.0 ~f:( +. )
                 in
                 Float.compare right_seconds left_seconds)
        in
        Log.dump
          "| Analysis | Fetch Count | Total Time | Percent of Total Time | Average Model Size \
           (bytes) | Target |";
        let display_model_row ({ FetchModelEvent.Key.target; analysis }, events) =
          let count = List.length events in
          let seconds =
            events
            |> List.map ~f:(fun { FetchModelEvent.seconds; _ } -> seconds)
            |> List.fold ~init:0.0 ~f:( +. )
          in
          let average_size =
            events
            |> List.map ~f:(fun { FetchModelEvent.model_words; _ } -> model_words)
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
            Target.pp_pretty
            target
        in
        List.iter fetch_model_events ~f:display_model_row;

        Log.dump "Performance per apply call step:";
        let add_apply_call_step_event map ({ ApplyCallStepEvent.seconds; _ } as event) =
          Map.update map (ApplyCallStepEvent.key event) ~f:(function
              | None -> [seconds]
              | Some times -> seconds :: times)
        in
        let apply_call_step_events =
          apply_call_step_events
          |> List.fold ~f:add_apply_call_step_event ~init:ApplyCallStepEvent.Map.empty
          |> Core.Map.to_alist
          |> List.sort ~compare:(fun (_, left_times) (_, right_times) ->
                 let left_seconds = List.fold ~init:0.0 ~f:( +. ) left_times in
                 let right_seconds = List.fold ~init:0.0 ~f:( +. ) right_times in
                 Float.compare right_seconds left_seconds)
        in
        if List.length apply_call_step_events > max_number_apply_call_steps then
          Log.dump
            "WARNING: Showing the first %d apply call steps out of %d steps"
            max_number_apply_call_steps
            (List.length apply_call_step_events);
        Log.dump
          "| Line | Column | Analysis | Iterations | Total Time | Percent of Total Time | Target | \
           Step | Argument |";
        let display_apply_call_step_row
            ({ ApplyCallStepEvent.Key.target; location; analysis; step; argument }, times)
          =
          let iterations = List.length times in
          let seconds = List.fold ~init:0.0 ~f:( +. ) times in
          let pp_option pp formatter = function
            | None -> Format.fprintf formatter "None"
            | Some value -> pp formatter value
          in
          Log.dump
            "| %4d | %3d | %a | %2d | %8.3fs | %4.2f%% | %a | %a | %a"
            location.start.line
            location.start.column
            analysis_pp
            analysis
            iterations
            seconds
            (seconds /. total_seconds *. 100.0)
            (pp_option Target.pp_pretty)
            target
            Analysis.ApplyCallStep.pp_short
            step
            (pp_option Expression.pp)
            argument
        in
        List.iter
          (List.take apply_call_step_events max_number_apply_call_steps)
          ~f:display_apply_call_step_row
end
