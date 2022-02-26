(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre
open Domains

include Interprocedural.AnalysisResult.Make (struct
  let name = "taint"

  type result = Issue.t list

  type call_model = Model.t

  let show_call_model = Model.show

  let obscure_model = Model.obscure_model

  let empty_model = Model.empty_model

  let pp_call_model = Model.pp

  let join ~iteration:_ left right = Model.join left right

  let widen = Model.widen

  let reached_fixpoint = Model.reached_fixpoint

  let strip_for_callsite = Model.strip_for_callsite
end)

(* Patch the forward reference to access the final summaries in trace info generation. *)
let has_significant_summary root path target =
  let model =
    Interprocedural.FixpointState.get_model target >>= Interprocedural.AnalysisResult.get_model kind
  in
  match model with
  | None -> false
  | Some { forward; backward; _ } -> (
      match root with
      | AccessPath.Root.LocalResult ->
          let _, tree =
            ForwardState.read_tree_raw ~use_precise_labels:true ~root ~path forward.source_taint
          in
          let taint = ForwardState.Tree.get_root tree in
          not (ForwardTaint.is_bottom taint)
      | _ ->
          let _, tree =
            BackwardState.read_tree_raw ~use_precise_labels:true ~root ~path backward.sink_taint
          in
          let taint = BackwardState.Tree.get_root tree in
          not (BackwardTaint.is_bottom taint))


let () = CallInfo.has_significant_summary := has_significant_summary
