(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* MissingFlow: implements an analysis to find missing flows due to unresolved
 * functions called (usually because of missing type information). This works by
 * adding a hardcoded sink on all calls that cannot be resolved.
 *)

open Core
open Pyre
open Domains
open Interprocedural

(* Returns true if the given target is a symbolic target that represents an unknown callee. *)
let is_unknown_callee = function
  | Target.Object name when String.is_prefix name ~prefix:"unknown-callee:" -> true
  | _ -> false


(* Model for an unknown callee, with sinks on all parameters, in order to find missing flows. *)
let unknown_callee_model _ =
  (* Add a model with sinks on *args and **kwargs. *)
  let sink_leaf =
    BackwardTaint.singleton CallInfo.declaration (Sinks.NamedSink "UnknownCallee") Frame.initial
    |> BackwardState.Tree.create_leaf
  in
  let sink_taint =
    BackwardState.assign
      ~root:(AccessPath.Root.StarParameter { position = 0 })
      ~path:[]
      sink_leaf
      BackwardState.empty
    |> BackwardState.assign
         ~root:(AccessPath.Root.StarStarParameter { excluded = [] })
         ~path:[]
         sink_leaf
  in
  (* Add taint-in-taint-out for all parameters. *)
  let local_return =
    BackwardTaint.singleton CallInfo.Tito Sinks.LocalReturn Frame.initial
    |> BackwardState.Tree.create_leaf
  in
  let taint_in_taint_out =
    BackwardState.assign
      ~root:(AccessPath.Root.StarParameter { position = 0 })
      ~path:[]
      local_return
      BackwardState.empty
    |> BackwardState.assign
         ~root:(AccessPath.Root.StarStarParameter { excluded = [] })
         ~path:[]
         local_return
  in
  {
    Model.forward = Model.Forward.empty;
    backward = { sink_taint; taint_in_taint_out };
    sanitizers = Model.Sanitizers.empty;
    modes = Model.ModeSet.singleton Model.Mode.SkipAnalysis;
  }


(* Return the initial set of models, updated for the missing-flows=obscure analysis. *)
let add_obscure_models
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.find_missing_flows; _ }
    ~environment
    ~stubs
    ~initial_models
  =
  let remove_sinks models = Registry.map models ~f:Model.remove_sinks in
  let add_obscure_sinks models =
    let resolution =
      Analysis.TypeCheck.resolution
        (Analysis.TypeEnvironment.ReadOnly.global_resolution environment)
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module Analysis.TypeCheck.DummyContext)
    in
    let add_obscure_sink models callable =
      let model =
        Registry.get models callable
        |> Option.value ~default:Model.empty_model
        |> Model.add_obscure_sink ~resolution ~call_target:callable
        |> Model.remove_obscureness
      in
      Registry.set models ~target:callable ~model
    in
    stubs
    |> Hash_set.filter ~f:(fun callable ->
           Registry.get models callable >>| Model.is_obscure |> Option.value ~default:true)
    |> Hash_set.fold ~f:add_obscure_sink ~init:models
  in
  match find_missing_flows with
  | Some Obscure -> initial_models |> remove_sinks |> add_obscure_sinks
  | Some Type -> initial_models |> remove_sinks
  | None -> initial_models


(* Return the initial set of models, updated for the missing-flows=type analysis. *)
let add_unknown_callee_models
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.find_missing_flows; _ }
    ~call_graph
    ~initial_models
  =
  match find_missing_flows with
  | Some Type ->
      Log.info "Initializing models for unknown callees...";
      let gather_unknown_callees ~target:_ ~callees unknown_callees =
        List.fold
          ~init:unknown_callees
          ~f:(fun unknown_callees target ->
            if is_unknown_callee target then
              Target.Set.add target unknown_callees
            else
              unknown_callees)
          callees
      in
      let unknown_callees =
        CallGraph.WholeProgramCallGraph.fold
          ~init:Target.Set.empty
          ~f:gather_unknown_callees
          call_graph
      in
      let add_model target models =
        Registry.set models ~target ~model:(unknown_callee_model target)
      in
      Target.Set.fold add_model unknown_callees initial_models
  | _ -> initial_models
