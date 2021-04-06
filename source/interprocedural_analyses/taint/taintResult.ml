(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Domains
open Pyre

let json_to_string json =
  Yojson.Safe.to_string json
  |> Yojson.Safe.prettify
  |> String.split ~on:'\n'
  |> List.map ~f:(fun line -> "      " ^ line)
  |> String.concat ~sep:"\n"


module Forward = struct
  type model = { source_taint: ForwardState.t }

  let pp_model formatter { source_taint } =
    Format.fprintf formatter "    Sources:\n%s" (json_to_string (ForwardState.to_json source_taint))


  let show_model = Format.asprintf "%a" pp_model

  let empty = { source_taint = ForwardState.empty }

  let is_empty { source_taint } = ForwardState.is_empty source_taint

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


  let to_json ~filename_lookup { source_taint } =
    ForwardState.to_external_json ~filename_lookup source_taint
end

module Backward = struct
  type model = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }

  let pp_model formatter { taint_in_taint_out; sink_taint } =
    Format.fprintf
      formatter
      "    Taint-in-taint-out:\n%s\n    Sinks:\n%s"
      (json_to_string (BackwardState.to_json taint_in_taint_out))
      (json_to_string (BackwardState.to_json sink_taint))


  let show_model = Format.asprintf "%a" pp_model

  let empty = { sink_taint = BackwardState.empty; taint_in_taint_out = BackwardState.empty }

  let is_empty { sink_taint; taint_in_taint_out } =
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


  let to_json_sinks ~filename_lookup { sink_taint; _ } =
    BackwardState.to_external_json ~filename_lookup sink_taint


  let to_json_tito ~filename_lookup { taint_in_taint_out; _ } =
    BackwardState.to_external_json ~filename_lookup taint_in_taint_out
end

module Mode = struct
  type sanitize_sources =
    | AllSources
    | SpecificSources of Sources.t list
  [@@deriving show, compare, eq]

  type sanitize_sinks =
    | AllSinks
    | SpecificSinks of Sinks.t list
  [@@deriving show, compare, eq]

  type sanitize_tito =
    | AllTito
    | SpecificTito of {
        sanitized_tito_sources: Sources.t list;
        sanitized_tito_sinks: Sinks.t list;
      }
  [@@deriving show, compare, eq]

  type sanitize = {
    sources: sanitize_sources option;
    sinks: sanitize_sinks option;
    tito: sanitize_tito option;
  }
  [@@deriving show, eq]

  type t =
    | SkipAnalysis (* Don't analyze at all *)
    | Sanitize of sanitize
    (* Analyze, but throw away inferred model *)
    | Normal
  [@@deriving show, eq]

  let join left right =
    match left, right with
    | SkipAnalysis, _ -> SkipAnalysis
    | _, SkipAnalysis -> SkipAnalysis
    | Sanitize left, Sanitize right ->
        let sources =
          match left.sources, right.sources with
          | None, Some _ -> right.sources
          | Some _, None -> left.sources
          | Some AllSources, _
          | _, Some AllSources ->
              Some AllSources
          | Some (SpecificSources left_sources), Some (SpecificSources right_sources) ->
              Some
                (SpecificSources
                   (List.dedup_and_sort ~compare:Sources.compare (left_sources @ right_sources)))
          | None, None -> None
        in
        let sinks =
          match left.sinks, right.sinks with
          | None, Some _ -> right.sinks
          | Some _, None -> left.sinks
          | Some AllSinks, _
          | _, Some AllSinks ->
              Some AllSinks
          | Some (SpecificSinks left_sinks), Some (SpecificSinks right_sinks) ->
              Some
                (SpecificSinks
                   (List.dedup_and_sort ~compare:Sinks.compare (left_sinks @ right_sinks)))
          | None, None -> None
        in
        let tito =
          match left.tito, right.tito with
          | None, Some tito
          | Some tito, None ->
              Some tito
          | Some AllTito, _
          | _, Some AllTito ->
              Some AllTito
          | ( Some
                (SpecificTito
                  { sanitized_tito_sources = left_sources; sanitized_tito_sinks = left_sinks }),
              Some
                (SpecificTito
                  { sanitized_tito_sources = right_sources; sanitized_tito_sinks = right_sinks }) )
            ->
              Some
                (SpecificTito
                   {
                     sanitized_tito_sources =
                       List.dedup_and_sort ~compare:Sources.compare (left_sources @ right_sources);
                     sanitized_tito_sinks =
                       List.dedup_and_sort ~compare:Sinks.compare (left_sinks @ right_sinks);
                   })
          | None, None -> None
        in
        Sanitize { sources; sinks; tito }
    | Sanitize _, _ -> left
    | _, Sanitize _ -> right
    | Normal, Normal -> Normal
    [@@deriving show, eq]
end

type call_model = {
  forward: Forward.model;
  backward: Backward.model;
  mode: Mode.t;
}

let pp_call_model formatter { forward; backward; mode } =
  Format.fprintf
    formatter
    "  Forward:\n%a\n  Backward:\n%a\n  Mode:%a\n"
    Forward.pp_model
    forward
    Backward.pp_model
    backward
    Mode.pp
    mode


let show_call_model = Format.asprintf "%a" pp_call_model

type result = Flow.issue list

let empty_skip_model = { forward = Forward.empty; backward = Backward.empty; mode = SkipAnalysis }

module ResultArgument = struct
  let name = "taint"

  type nonrec result = result

  type nonrec call_model = call_model

  let show_call_model = show_call_model

  let pp_call_model = pp_call_model

  let obscure_model = { forward = Forward.obscure; backward = Backward.obscure; mode = Normal }

  let empty_model = { forward = Forward.empty; backward = Backward.empty; mode = Normal }

  let is_empty { forward; backward; _ } = Forward.is_empty forward && Backward.is_empty backward

  let join ~iteration:_ left right =
    {
      forward = Forward.join left.forward right.forward;
      backward = Backward.join left.backward right.backward;
      mode = Mode.join left.mode right.mode;
    }


  let widen ~iteration ~previous ~next =
    {
      forward = Forward.widen ~iteration ~previous:previous.forward ~next:next.forward;
      backward = Backward.widen ~iteration ~previous:previous.backward ~next:next.backward;
      mode = Mode.join previous.mode next.mode;
    }


  let get_errors result = List.map ~f:Flow.generate_error result

  let issues_to_json ~filename_lookup callable result =
    match result with
    | None -> []
    | Some issues ->
        let issue_to_json issue =
          let json = Flow.to_json ~filename_lookup callable issue in
          `Assoc ["kind", `String "issue"; "data", json]
        in
        List.map ~f:issue_to_json issues


  let model_to_json ~filename_lookup callable model =
    let callable_name = Interprocedural.Callable.external_target_name callable in
    let model_json =
      `Assoc
        [
          "callable", `String callable_name;
          "sources", Forward.to_json ~filename_lookup model.forward;
          "sinks", Backward.to_json_sinks ~filename_lookup model.backward;
          "tito", Backward.to_json_tito ~filename_lookup model.backward;
        ]
    in
    `Assoc ["kind", `String "model"; "data", model_json]


  let reached_fixpoint ~iteration ~previous ~next =
    Forward.reached_fixpoint ~iteration ~previous:previous.forward ~next:next.forward
    && Backward.reached_fixpoint ~iteration ~previous:previous.backward ~next:next.backward


  (* Emit both issues and models for external processing *)
  let externalize ~filename_lookup callable result model =
    let issues = issues_to_json ~filename_lookup callable result in
    if is_empty model then
      issues
    else
      model_to_json ~filename_lookup callable model :: issues


  let metadata () =
    let codes = Flow.code_metadata () in
    `Assoc ["codes", codes]


  let statistics () =
    let model_verification_errors =
      ModelVerificationError.get () |> List.map ~f:ModelVerificationError.to_json
    in
    `Assoc ["model_verification_errors", `List model_verification_errors]


  let strip_for_callsite
      { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; mode }
    =
    (* Remove positions and other info that are not needed at call site *)
    let source_taint =
      source_taint
      |> ForwardState.transform
           ForwardTaint.flow_details
           Map
           ~f:Domains.FlowDetails.strip_tito_positions
      |> ForwardState.transform ForwardTaint.trace_info Map ~f:Domains.TraceInfo.strip_for_callsite
    in
    let sink_taint =
      sink_taint
      |> BackwardState.transform
           BackwardTaint.flow_details
           Map
           ~f:Domains.FlowDetails.strip_tito_positions
      |> BackwardState.transform
           BackwardTaint.trace_info
           Map
           ~f:Domains.TraceInfo.strip_for_callsite
    in
    let taint_in_taint_out =
      taint_in_taint_out
      |> BackwardState.transform
           BackwardTaint.flow_details
           Map
           ~f:Domains.FlowDetails.strip_tito_positions
      |> BackwardState.transform
           BackwardTaint.trace_info
           Map
           ~f:Domains.TraceInfo.strip_for_callsite
    in
    { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; mode }
end

include Interprocedural.Result.Make (ResultArgument)

(* Patch the forward reference to access the final summaries in trace info generation. *)
let has_significant_summary root path target =
  let model = Interprocedural.Fixpoint.get_model target >>= Interprocedural.Result.get_model kind in
  match model with
  | None -> false
  | Some { forward; backward; _ } -> (
      match root with
      | AccessPath.Root.LocalResult ->
          let _, tree =
            ForwardState.read_tree_raw ~use_precise_fields:true ~root ~path forward.source_taint
          in
          let taint = ForwardState.Tree.get_root_taint tree in
          not (ForwardTaint.is_bottom taint)
      | _ ->
          let _, tree =
            BackwardState.read_tree_raw ~use_precise_fields:true ~root ~path backward.sink_taint
          in
          let taint = BackwardState.Tree.get_root_taint tree in
          not (BackwardTaint.is_bottom taint) )


let () = TraceInfo.has_significant_summary := has_significant_summary
