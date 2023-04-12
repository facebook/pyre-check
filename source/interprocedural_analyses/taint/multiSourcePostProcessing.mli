(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Target = Interprocedural.Target

(* For a multi-source rule, only keep its main issue, based on the taint configuration.
   Additionally, we attach the main issues with the secondary issues. This function updates the
   analysis results that are stored in `fixpoint_state`. *)
val update_multi_source_issues
  :  filename_lookup:(Ast.Reference.t -> string option) ->
  taint_configuration:TaintConfiguration.Heap.t ->
  callables:Target.t list ->
  fixpoint_state:Fixpoint.t ->
  unit
