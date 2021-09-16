(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Domains
open Pyre

let json_to_string ~indent json =
  let lines = Yojson.Safe.to_string json |> Yojson.Safe.prettify |> String.split ~on:'\n' in
  match lines with
  | [line] -> line
  | lines ->
      lines
      |> List.map ~f:(fun line -> indent ^ line)
      |> String.concat ~sep:"\n"
      |> fun content -> "\n" ^ content


module Forward = struct
  type model = { source_taint: ForwardState.t }

  let pp_model formatter { source_taint } =
    Format.fprintf
      formatter
      "  Sources: %s"
      (json_to_string ~indent:"    " (ForwardState.to_json source_taint))


  let show_model = Format.asprintf "%a" pp_model

  let empty = { source_taint = ForwardState.empty }

  let is_empty_model { source_taint } = ForwardState.is_empty source_taint

  let obscure = empty

  let join { source_taint = left } { source_taint = right } =
    { source_taint = ForwardState.join left right }


  let widen ~iteration ~previous:{ source_taint = prev } ~next:{ source_taint = next } =
    { source_taint = ForwardState.widen ~iteration ~prev ~next }


  let reached_fixpoint
      ~iteration:_
      ~previous:{ source_taint = previous }
      ~next:{ source_taint = next }
    =
    ForwardState.less_or_equal ~left:next ~right:previous
end

module Backward = struct
  type model = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }

  let pp_model formatter { taint_in_taint_out; sink_taint } =
    Format.fprintf
      formatter
      "  Taint-in-taint-out: %s\n  Sinks: %s"
      (json_to_string ~indent:"    " (BackwardState.to_json taint_in_taint_out))
      (json_to_string ~indent:"    " (BackwardState.to_json sink_taint))


  let show_model = Format.asprintf "%a" pp_model

  let empty = { sink_taint = BackwardState.empty; taint_in_taint_out = BackwardState.empty }

  let is_empty_model { sink_taint; taint_in_taint_out } =
    BackwardState.is_empty sink_taint && BackwardState.is_empty taint_in_taint_out


  let obscure = empty

  let join
      { sink_taint = sink_taint_left; taint_in_taint_out = tito_left }
      { sink_taint = sink_taint_right; taint_in_taint_out = tito_right }
    =
    {
      sink_taint = BackwardState.join sink_taint_left sink_taint_right;
      taint_in_taint_out = BackwardState.join tito_left tito_right;
    }


  let widen
      ~iteration
      ~previous:{ sink_taint = sink_taint_previous; taint_in_taint_out = tito_previous }
      ~next:{ sink_taint = sink_taint_next; taint_in_taint_out = tito_next }
    =
    let sink_taint =
      BackwardState.widen ~iteration ~prev:sink_taint_previous ~next:sink_taint_next
    in
    let taint_in_taint_out = BackwardState.widen ~iteration ~prev:tito_previous ~next:tito_next in
    { sink_taint; taint_in_taint_out }


  let reached_fixpoint
      ~iteration:_
      ~previous:{ sink_taint = sink_taint_previous; taint_in_taint_out = tito_previous }
      ~next:{ sink_taint = sink_taint_next; taint_in_taint_out = tito_next }
    =
    BackwardState.less_or_equal ~left:sink_taint_next ~right:sink_taint_previous
    && BackwardState.less_or_equal ~left:tito_next ~right:tito_previous
end

module Sanitizers = struct
  type model = {
    (* Sanitizers applying to all parameters and the return value. *)
    global: Sanitize.t;
    (* Sanitizers applying to all parameters. *)
    parameters: Sanitize.t;
    (* Map from parameter or return value to sanitizers. *)
    roots: SanitizeRootMap.t;
  }

  let pp_model formatter { global; parameters; roots } =
    Format.fprintf
      formatter
      "  Global Sanitizer: %s\n  Parameters Sanitizer: %s\n  Sanitizers: %s"
      (json_to_string ~indent:"    " (Sanitize.to_json global))
      (json_to_string ~indent:"    " (Sanitize.to_json parameters))
      (json_to_string ~indent:"    " (SanitizeRootMap.to_json roots))


  let show_model = Format.asprintf "%a" pp_model

  let empty =
    { global = Sanitize.empty; parameters = Sanitize.empty; roots = SanitizeRootMap.bottom }


  let is_empty_model { global; parameters; roots } =
    Sanitize.is_empty global && Sanitize.is_empty parameters && SanitizeRootMap.is_bottom roots


  let join
      { global = global_left; parameters = parameters_left; roots = roots_left }
      { global = global_right; parameters = parameters_right; roots = roots_right }
    =
    {
      global = Sanitize.join global_left global_right;
      parameters = Sanitize.join parameters_left parameters_right;
      roots = SanitizeRootMap.join roots_left roots_right;
    }


  let widen ~iteration:_ ~previous ~next = join previous next
end

module Mode = struct
  let name = "modes"

  type t =
    | Obscure
    | SkipAnalysis (* Don't analyze at all *)
    | SkipDecoratorWhenInlining
    | SkipOverrides
  [@@deriving compare]

  let pp formatter = function
    | Obscure -> Format.fprintf formatter "Obscure"
    | SkipAnalysis -> Format.fprintf formatter "SkipAnalysis"
    | SkipDecoratorWhenInlining -> Format.fprintf formatter "SkipDecoratorWhenInlining"
    | SkipOverrides -> Format.fprintf formatter "SkipOverrides"


  let show = Format.asprintf "%a" pp

  let to_json mode = `String (show mode)
end

module ModeSet = struct
  module T = Abstract.SetDomain.Make (Mode)
  include T

  let empty = T.bottom

  let is_empty = T.is_bottom

  let equal left right = T.less_or_equal ~left ~right && T.less_or_equal ~left:right ~right:left

  let to_json modes = `List (modes |> T.elements |> List.map ~f:Mode.to_json)

  let pp_model formatter modes =
    Format.fprintf formatter "  Modes: %s" (json_to_string ~indent:"    " (to_json modes))
end

type call_model = {
  forward: Forward.model;
  backward: Backward.model;
  sanitizers: Sanitizers.model;
  modes: ModeSet.t;
}

let pp_call_model formatter { forward; backward; sanitizers; modes } =
  Format.fprintf
    formatter
    "%a\n%a\n%a\n%a"
    Forward.pp_model
    forward
    Backward.pp_model
    backward
    Sanitizers.pp_model
    sanitizers
    ModeSet.pp_model
    modes


let show_call_model = Format.asprintf "%a" pp_call_model

type result = Flow.issue list

let empty_skip_model =
  {
    forward = Forward.empty;
    backward = Backward.empty;
    sanitizers = Sanitizers.empty;
    modes = ModeSet.singleton SkipAnalysis;
  }


module ResultArgument = struct
  let name = "taint"

  type nonrec result = result

  type nonrec call_model = call_model

  let show_call_model = show_call_model

  let pp_call_model = pp_call_model

  let obscure_model =
    {
      forward = Forward.obscure;
      backward = Backward.obscure;
      sanitizers = Sanitizers.empty;
      modes = ModeSet.singleton Obscure;
    }


  let empty_model =
    {
      forward = Forward.empty;
      backward = Backward.empty;
      sanitizers = Sanitizers.empty;
      modes = ModeSet.empty;
    }


  let is_empty_model ~with_modes { forward; backward; sanitizers; modes } =
    Forward.is_empty_model forward
    && Backward.is_empty_model backward
    && Sanitizers.is_empty_model sanitizers
    && ModeSet.equal with_modes modes


  let should_externalize_model { forward; backward; sanitizers; _ } =
    (not (Forward.is_empty_model forward))
    || (not (Backward.is_empty_model backward))
    || not (Sanitizers.is_empty_model sanitizers)


  let join ~iteration:_ left right =
    {
      forward = Forward.join left.forward right.forward;
      backward = Backward.join left.backward right.backward;
      sanitizers = Sanitizers.join left.sanitizers right.sanitizers;
      modes = ModeSet.join left.modes right.modes;
    }


  let widen ~iteration ~previous ~next =
    {
      forward = Forward.widen ~iteration ~previous:previous.forward ~next:next.forward;
      backward = Backward.widen ~iteration ~previous:previous.backward ~next:next.backward;
      sanitizers = Sanitizers.widen ~iteration ~previous:previous.sanitizers ~next:next.sanitizers;
      modes = ModeSet.widen ~iteration ~prev:previous.modes ~next:next.modes;
    }


  let reached_fixpoint ~iteration ~previous ~next =
    Forward.reached_fixpoint ~iteration ~previous:previous.forward ~next:next.forward
    && Backward.reached_fixpoint ~iteration ~previous:previous.backward ~next:next.backward


  let strip_for_callsite
      {
        forward = { source_taint };
        backward = { sink_taint; taint_in_taint_out };
        sanitizers;
        modes;
      }
    =
    (* Remove positions and other info that are not needed at call site *)
    let source_taint =
      source_taint
      |> ForwardState.transform FlowDetails.Self Map ~f:Domains.FlowDetails.strip_tito_positions
      |> ForwardState.transform ForwardTaint.trace_info Map ~f:Domains.TraceInfo.strip_for_callsite
    in
    let sink_taint =
      sink_taint
      |> BackwardState.transform FlowDetails.Self Map ~f:Domains.FlowDetails.strip_tito_positions
      |> BackwardState.transform
           BackwardTaint.trace_info
           Map
           ~f:Domains.TraceInfo.strip_for_callsite
    in
    let taint_in_taint_out =
      taint_in_taint_out
      |> BackwardState.transform FlowDetails.Self Map ~f:Domains.FlowDetails.strip_tito_positions
      |> BackwardState.transform
           BackwardTaint.trace_info
           Map
           ~f:Domains.TraceInfo.strip_for_callsite
    in
    { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; sanitizers; modes }


  let model_to_json
      ~filename_lookup
      callable
      {
        forward = { source_taint };
        backward = { sink_taint; taint_in_taint_out };
        sanitizers =
          { global = global_sanitizer; parameters = parameters_sanitizer; roots = root_sanitizers };
        modes;
      }
    =
    let callable_name = Interprocedural.Target.external_target_name callable in
    let model_json = ["callable", `String callable_name] in
    let model_json =
      if not (ForwardState.is_empty source_taint) then
        model_json @ ["sources", ForwardState.to_external_json ~filename_lookup source_taint]
      else
        model_json
    in
    let model_json =
      if not (BackwardState.is_empty sink_taint) then
        model_json @ ["sinks", BackwardState.to_external_json ~filename_lookup sink_taint]
      else
        model_json
    in
    let model_json =
      if not (BackwardState.is_empty taint_in_taint_out) then
        model_json @ ["tito", BackwardState.to_external_json ~filename_lookup taint_in_taint_out]
      else
        model_json
    in
    let model_json =
      if not (Sanitize.is_empty global_sanitizer) then
        model_json @ ["global_sanitizer", Sanitize.to_json global_sanitizer]
      else
        model_json
    in
    let model_json =
      if not (Sanitize.is_empty parameters_sanitizer) then
        model_json @ ["parameters_sanitizer", Sanitize.to_json parameters_sanitizer]
      else
        model_json
    in
    let model_json =
      if not (SanitizeRootMap.is_bottom root_sanitizers) then
        model_json @ ["sanitizers", SanitizeRootMap.to_json root_sanitizers]
      else
        model_json
    in
    let model_json =
      if not (ModeSet.is_empty modes) then
        model_json @ ["modes", ModeSet.to_json modes]
      else
        model_json
    in
    `Assoc ["kind", `String "model"; "data", `Assoc model_json]
end

let is_empty_model = ResultArgument.is_empty_model

let should_externalize_model = ResultArgument.should_externalize_model

let model_to_json = ResultArgument.model_to_json

include Interprocedural.AnalysisResult.Make (ResultArgument)

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


let decorators_to_skip models =
  let skippable_decorator_reference (callable, model) =
    match callable, Interprocedural.AnalysisResult.get_model kind model with
    | `Function callable_name, Some { modes; _ }
      when ModeSet.contains Mode.SkipDecoratorWhenInlining modes ->
        Some (Ast.Reference.create callable_name)
    | _ -> None
  in
  Map.to_alist models
  |> List.filter_map ~f:skippable_decorator_reference
  |> Ast.Reference.Set.of_list


let () = TraceInfo.has_significant_summary := has_significant_summary
