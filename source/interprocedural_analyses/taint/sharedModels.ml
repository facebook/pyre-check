(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* SharedModels: represents a mapping from targets to their model,
 * stored in the shared memory *)

open Core
open Interprocedural
module T = TaintFixpoint.SharedModels
include T

let targets_with_mode ~scheduler ~mode models =
  let all_targets = T.targets models in
  let models = T.read_only models in
  let policy =
    Scheduler.Policy.fixed_chunk_size
      ~minimum_chunks_per_worker:1
      ~minimum_chunk_size:1
      ~preferred_chunk_size:1000
      ()
  in
  let map targets =
    let fold set target =
      match T.ReadOnly.get models ~cache:false target with
      | Some { Model.modes; _ } when Model.ModeSet.contains mode modes -> target :: set
      | _ -> set
    in
    List.fold ~init:[] ~f:fold targets
  in
  Scheduler.map_reduce
    scheduler
    ~policy
    ~initial:[]
    ~reduce:List.rev_append
    ~map
    ~inputs:all_targets
    ()


let skip_overrides ~scheduler models =
  targets_with_mode ~scheduler ~mode:Model.Mode.SkipOverrides models
  |> List.filter ~f:Target.is_function_or_method
  |> List.map ~f:Target.define_name_exn
  |> Ast.Reference.SerializableSet.of_list


let analyze_all_overrides ~scheduler models =
  models |> targets_with_mode ~scheduler ~mode:Model.Mode.AnalyzeAllOverrides |> Target.Set.of_list


let skip_analysis ~scheduler models =
  models |> targets_with_mode ~scheduler ~mode:Model.Mode.SkipAnalysis |> Target.Set.of_list


let called_when_parameter ~scheduler models =
  models
  |> targets_with_mode ~scheduler ~mode:Model.Mode.CalledWhenParameter
  |> Target.HashSet.of_list


let entrypoints ~scheduler models = targets_with_mode ~scheduler ~mode:Model.Mode.Entrypoint models

let object_targets models =
  models |> T.targets |> List.filter ~f:Target.is_object |> Target.Set.of_list


let initialize_for_parameterized_callables ~higher_order_call_graph_fixpoint initial_models =
  let initial_model = function
    | Interprocedural.Target.Parameterized _ as callable -> (
        (* We assume the higher order call graph should only create parameterized targets. *)
        callable
        |> Target.strip_parameters
        (* Initialize with the user-provided models, which we assume are already in
           `initial_models`. *)
        |> T.get_model (T.read_only initial_models)
        |> function
        | Some model -> Some model
        | None -> Some TaintFixpoint.Analysis.initial_model)
    | Regular _ -> None
  in
  match higher_order_call_graph_fixpoint with
  | None -> initial_models
  | Some higher_order_call_graph_fixpoint ->
      higher_order_call_graph_fixpoint
      |> Interprocedural.CallGraphFixpoint.analyzed_callables
      |> List.fold ~init:(T.add_only initial_models) ~f:(fun models callable ->
             match initial_model callable with
             | None -> models
             | Some model -> T.AddOnly.add models callable model)
      |> from_add_only
