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
let is_unknown_callee target =
  match Target.get_regular target with
  | Target.Regular.Object name when String.is_prefix name ~prefix:"unknown-callee:" -> true
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
    BackwardTaint.singleton (CallInfo.tito ()) Sinks.LocalReturn Frame.initial
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
    Model.empty_model with
    backward = { sink_taint; taint_in_taint_out };
    modes = Model.ModeSet.singleton Model.Mode.SkipAnalysis;
  }


(* Return the initial set of models, updated for the missing-flows=obscure analysis. *)
let add_obscure_models
    ~scheduler
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.find_missing_flows; _ }
    ~callables_to_definitions_map
    ~stubs
    ~initial_models
  =
  let stubs = Hash_set.to_list stubs in
  let remove_sinks models =
    SharedModels.map_parallel
      ~scheduler
      ~f:(fun ~target:_ ~model -> Model.remove_sinks model)
      models
  in
  let add_obscure_sinks models =
    (* Update existing models *)
    let models =
      SharedModels.map_parallel_targets
        models
        ~scheduler
        ~f:(fun ~target ~model ->
          if Model.is_obscure model then
            model
            |> Model.add_obscure_sink ~callables_to_definitions_map ~call_target:target
            |> Model.remove_obscureness
          else
            model)
        ~targets:stubs
    in
    (* Add new models when necessary *)
    let targets_with_model = initial_models |> SharedModels.targets |> Target.Set.of_list in
    let stubs_without_model =
      List.filter ~f:(fun callable -> not (Target.Set.mem callable targets_with_model)) stubs
    in
    List.fold
      ~init:(SharedModels.add_only models)
      ~f:(fun models target ->
        Model.empty_model
        |> Model.add_obscure_sink ~callables_to_definitions_map ~call_target:target
        |> Model.remove_obscureness
        |> SharedModels.AddOnly.add models target)
      stubs_without_model
    |> SharedModels.from_add_only
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
        SharedModels.AddOnly.add models target (unknown_callee_model target)
      in
      initial_models
      |> SharedModels.add_only
      |> Target.Set.fold add_model unknown_callees
      |> SharedModels.from_add_only
  | _ -> initial_models
