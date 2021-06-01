(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open TypeInferenceData
module TypeEnvironment = Analysis.TypeEnvironment
open Interprocedural

let get_result callable = Fixpoint.get_result callable |> Result.get_result TypeInferenceResult.kind

let make_global_result ~global_resolution ~callables =
  let add_local_result global_result callable =
    callable
    |> get_result
    >>| GlobalResult.add_local_result ~global_resolution global_result
    |> Option.value ~default:global_result
  in
  callables |> List.fold ~init:GlobalResult.empty ~f:add_local_result


let log_performance ~fixpoint_timer ~fixpoint_iterations ~inference_count =
  match fixpoint_iterations with
  | Some iterations ->
      Log.info "Fixpoint iterations: %d" iterations;
      Statistics.performance
        ~name:"Infer fixpoint complete"
        ~phase_name:"Infer fixpoint"
        ~timer:fixpoint_timer
        ~integers:
          [
            "infer fixpoint iterations", iterations;
            "infer heap size", SharedMem.heap_size ();
            "infer inference count", inference_count;
          ]
        ()
  | None -> ()


let report
    ~scheduler:_
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.result_json_path; _ }
    ~environment
    ~filename_lookup:_
    ~callables
    ~skipped_overrides:_
    ~fixpoint_timer
    ~fixpoint_iterations
  =
  let result =
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    let callables = Callable.Set.elements callables in
    make_global_result ~global_resolution ~callables
  in
  log_performance
    ~fixpoint_timer
    ~fixpoint_iterations
    ~inference_count:(GlobalResult.inference_count result);
  if Option.is_some result_json_path then
    Log.warning "Pyre infer does not dump results to disk ignoring result json path";
  [GlobalResult.to_yojson result]
