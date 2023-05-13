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
  |> List.map ~f:Target.define_name
  |> Reference.Set.of_list


let skip_analysis models =
  targets_with_mode models ~mode:Model.Mode.SkipAnalysis |> Target.Set.of_list


let entrypoints models = targets_with_mode models ~mode:Model.Mode.Entrypoint
