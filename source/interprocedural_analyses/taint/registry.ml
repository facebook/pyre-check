(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Registry: represents a mapping from targets to their model. *)

open Core
open Ast
open Interprocedural
include TaintFixpoint.Registry

let targets_with_mode models ~mode =
  let collect ~target ~model:{ Model.modes; _ } set =
    if Model.ModeSet.contains mode modes then
      target :: set
    else
      set
  in
  fold ~init:[] ~f:collect models

  
let skip_overrides models =
  targets_with_mode models ~mode:Model.Mode.SkipOverrides
  |> List.filter ~f:Target.is_function_or_method
  |> List.map ~f:Target.define_name
  |> Reference.SerializableSet.of_list


let analyze_all_overrides models =
  targets_with_mode models ~mode:Model.Mode.AnalyzeAllOverrides |> Target.Set.of_list


let skip_analysis models =
  targets_with_mode models ~mode:Model.Mode.SkipAnalysis |> Target.Set.of_list


let entrypoints models = targets_with_mode models ~mode:Model.Mode.Entrypoint

let pp pp_value formatter map =
  let pp_pairs formatter pairs =
    let pp_pair formatter (key, value) =
      Format.fprintf formatter "@[<hv 2>%a -> %a@]" Target.pp key pp_value value
    in
    Format.pp_print_list pp_pair formatter (Target.Map.bindings map)
  in
  Format.fprintf formatter "@[<v 2>{%a}@]" pp_pairs

let show pp_value map =
  Format.asprintf "%a" (pp pp_value) map
