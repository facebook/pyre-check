(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open TaintConfiguration
open Domains

type flow = {
  source_taint: ForwardTaint.t;
  sink_taint: BackwardTaint.t;
}
[@@deriving show]

type flows = flow list [@@deriving show]

type candidate = {
  flows: flows;
  location: Location.WithModule.t;
}

type features = {
  breadcrumbs: Features.BreadcrumbSet.t;
  first_indices: Features.FirstIndexSet.t;
  first_fields: Features.FirstFieldSet.t;
}

type partitioned_flow = {
  source_partition: (Sources.t, ForwardTaint.t) Map.Poly.t;
  sink_partition: (Sinks.t, BackwardTaint.t) Map.Poly.t;
}

type issue = {
  code: int;
  flow: flow;
  features: features;
  issue_location: Location.WithModule.t;
  define: Statement.Define.t Node.t;
}

type triggered_sinks = String.Hash_set.t

(* Compute all flows from paths in ~source tree to corresponding paths in ~sink tree, while avoiding
   duplication as much as possible.

   Strategy:

   Let F and B for forward and backward taint respectively. For each path p in B from the root to
   some node with non-empty taint T, we match T with the join of taint in the upward and downward
   closure from node at path p in F. *)
let generate_source_sink_matches ~location ~source_tree ~sink_tree =
  let make_source_sink_matches (path, sink_taint) matches =
    let source_taint =
      ForwardState.Tree.read path source_tree
      |> ForwardState.Tree.collapse
           ~transform:(ForwardTaint.add_breadcrumbs Features.issue_broadening)
    in
    if ForwardTaint.is_bottom source_taint then
      matches
    else
      { source_taint; sink_taint } :: matches
  in
  let flows =
    if ForwardState.Tree.is_empty source_tree then
      []
    else
      BackwardState.Tree.fold BackwardState.Tree.Path ~init:[] ~f:make_source_sink_matches sink_tree
  in
  { location; flows }


type flow_state = {
  matched: flows;
  rest: flows;
}

let get_issue_features { source_taint; sink_taint } =
  let breadcrumbs =
    let source_breadcrumbs =
      ForwardTaint.fold
        Features.BreadcrumbSet.Self
        ~f:Features.BreadcrumbSet.join
        ~init:Features.BreadcrumbSet.bottom
        source_taint
    in
    let sink_breadcrumbs =
      BackwardTaint.fold
        Features.BreadcrumbSet.Self
        ~f:Features.BreadcrumbSet.join
        ~init:Features.BreadcrumbSet.bottom
        sink_taint
    in
    Features.BreadcrumbSet.sequence_join source_breadcrumbs sink_breadcrumbs
  in
  let first_indices =
    let source_indices =
      ForwardTaint.fold
        Features.FirstIndexSet.Self
        ~f:Features.FirstIndexSet.join
        ~init:Features.FirstIndexSet.bottom
        source_taint
    in
    let sink_indices =
      BackwardTaint.fold
        Features.FirstIndexSet.Self
        ~f:Features.FirstIndexSet.join
        ~init:Features.FirstIndexSet.bottom
        sink_taint
    in

    Features.FirstIndexSet.join source_indices sink_indices
  in
  let first_fields =
    let source_fields =
      ForwardTaint.fold
        Features.FirstFieldSet.Self
        ~f:Features.FirstFieldSet.join
        ~init:Features.FirstFieldSet.bottom
        source_taint
    in
    let sink_fields =
      BackwardTaint.fold
        Features.FirstFieldSet.Self
        ~f:Features.FirstFieldSet.join
        ~init:Features.FirstFieldSet.bottom
        sink_taint
    in
    Features.FirstFieldSet.join source_fields sink_fields
  in

  { breadcrumbs; first_indices; first_fields }


let generate_issues ~define { location; flows } =
  let erase_source_subkind = function
    | Sources.ParametricSource { source_name; _ } -> Sources.NamedSource source_name
    | source -> source
  in
  let erase_sink_subkind = function
    | Sinks.ParametricSink { sink_name; _ } -> Sinks.NamedSink sink_name
    | sink -> sink
  in
  let partitions =
    let partition { source_taint; sink_taint } =
      {
        source_partition =
          ForwardTaint.partition ForwardTaint.kind By source_taint ~f:erase_source_subkind;
        sink_partition =
          BackwardTaint.partition BackwardTaint.kind By sink_taint ~f:erase_sink_subkind;
      }
    in
    List.map flows ~f:partition
  in
  let apply_rule_on_flow { Rule.sources; sinks; _ } { source_partition; sink_partition } =
    let add_source_taint source_taint source =
      match Map.Poly.find source_partition (erase_source_subkind source) with
      | Some taint -> ForwardTaint.join source_taint taint
      | None -> source_taint
    in
    let add_sink_taint sink_taint sink =
      match Map.Poly.find sink_partition (erase_sink_subkind sink) with
      | Some taint -> BackwardTaint.join sink_taint taint
      | None -> sink_taint
    in
    let partition_flow =
      {
        source_taint = List.fold sources ~f:add_source_taint ~init:ForwardTaint.bottom;
        sink_taint = List.fold sinks ~f:add_sink_taint ~init:BackwardTaint.bottom;
      }
    in
    if
      ForwardTaint.is_bottom partition_flow.source_taint
      || BackwardTaint.is_bottom partition_flow.sink_taint
    then
      None
    else
      Some partition_flow
  in
  let apply_rule_separate_access_path issues_so_far (rule : Rule.t) =
    let fold_partitions issues candidate =
      match apply_rule_on_flow rule candidate with
      | Some flow ->
          let features = get_issue_features flow in
          { code = rule.code; flow; features; issue_location = location; define } :: issues
      | None -> issues
    in
    List.fold partitions ~init:issues_so_far ~f:fold_partitions
  in
  let apply_rule_merge_access_path rule =
    let fold_partitions flow_so_far candidate =
      match apply_rule_on_flow rule candidate with
      | Some flow ->
          {
            source_taint = ForwardTaint.join flow_so_far.source_taint flow.source_taint;
            sink_taint = BackwardTaint.join flow_so_far.sink_taint flow.sink_taint;
          }
      | None -> flow_so_far
    in
    let flow =
      List.fold
        partitions
        ~init:{ source_taint = ForwardTaint.bottom; sink_taint = BackwardTaint.bottom }
        ~f:fold_partitions
    in
    if ForwardTaint.is_bottom flow.source_taint || BackwardTaint.is_bottom flow.sink_taint then
      None
    else
      let features = get_issue_features flow in
      let issue = { code = rule.code; flow; features; issue_location = location; define } in
      Some issue
  in
  let configuration = TaintConfiguration.get () in
  if configuration.lineage_analysis then
    (* Create different issues for same access path, e.g, Issue{[a] -> [b]}, Issue {[c] -> [d]}. *)
    List.fold configuration.rules ~init:[] ~f:apply_rule_separate_access_path
  else (* Create single issue for same access path, e.g, Issue{[a],[c] -> [b], [d]}. *)
    List.filter_map ~f:apply_rule_merge_access_path configuration.rules


let sinks_regexp = Str.regexp_string "{$sinks}"

let sources_regexp = Str.regexp_string "{$sources}"

let get_name_and_detailed_message { code; flow; _ } =
  let configuration = TaintConfiguration.get () in
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) configuration.rules with
  | None -> failwith "issue with code that has no rule"
  | Some { name; message_format; _ } ->
      let sources =
        Domains.ForwardTaint.kinds flow.source_taint
        |> List.map ~f:Sources.show
        |> String.concat ~sep:", "
      in
      let sinks =
        Domains.BackwardTaint.kinds flow.sink_taint
        |> List.map ~f:Sinks.show
        |> String.concat ~sep:", "
      in
      let message =
        Str.global_replace sources_regexp sources message_format
        |> Str.global_replace sinks_regexp sinks
      in
      name, message


let generate_error ({ code; issue_location; define; _ } as issue) =
  let configuration = TaintConfiguration.get () in
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) configuration.rules with
  | None -> failwith "issue with code that has no rule"
  | Some _ ->
      let name, detail = get_name_and_detailed_message issue in
      let kind = { Interprocedural.Error.name; messages = [detail]; code } in
      Interprocedural.Error.create ~location:issue_location ~define ~kind


let to_json ~filename_lookup callable issue =
  let callable_name = Interprocedural.Target.external_target_name callable in
  let _, detail = get_name_and_detailed_message issue in
  let message = detail in
  let source_traces =
    Domains.ForwardTaint.to_external_json ~filename_lookup issue.flow.source_taint
  in
  let sink_traces = Domains.BackwardTaint.to_external_json ~filename_lookup issue.flow.sink_taint in
  let features =
    let get_feature_json { Abstract.OverUnderSetDomain.element; in_under } breadcrumbs =
      let breadcrumb_json = Features.Breadcrumb.to_json element ~on_all_paths:in_under in
      breadcrumb_json :: breadcrumbs
    in
    Features.BreadcrumbSet.fold
      Features.BreadcrumbSet.ElementAndUnder
      ~f:get_feature_json
      ~init:[]
      issue.features.breadcrumbs
  in
  let features =
    List.concat
      [
        Features.FirstIndex.to_json (Features.FirstIndexSet.elements issue.features.first_indices);
        Features.FirstField.to_json (Features.FirstFieldSet.elements issue.features.first_fields);
        features;
      ]
  in
  let traces =
    `List
      [
        `Assoc ["name", `String "forward"; "roots", source_traces];
        `Assoc ["name", `String "backward"; "roots", sink_traces];
      ]
  in
  let {
    Location.WithPath.path;
    start = { line; column = start_column };
    stop = { column = stop_column; _ };
  }
    =
    Location.WithModule.instantiate ~lookup:filename_lookup issue.issue_location
  in
  let callable_line = Ast.(Location.line issue.define.location) in
  `Assoc
    [
      "callable", `String callable_name;
      "callable_line", `Int callable_line;
      "code", `Int issue.code;
      "line", `Int line;
      "start", `Int start_column;
      "end", `Int stop_column;
      "filename", `String path;
      "message", `String message;
      "traces", traces;
      "features", `List features;
    ]


let code_metadata () =
  let configuration = TaintConfiguration.get () in
  `Assoc
    (List.map configuration.rules ~f:(fun rule -> Format.sprintf "%d" rule.code, `String rule.name))


let compute_triggered_sinks ~triggered_sinks ~location ~source_tree ~sink_tree =
  let partial_sinks_to_taint =
    BackwardState.Tree.collapse
      ~transform:(BackwardTaint.add_breadcrumbs Features.issue_broadening)
      sink_tree
    |> BackwardTaint.partition BackwardTaint.kind ByFilter ~f:(function
           | Sinks.PartialSink { Sinks.kind; label } -> Some { Sinks.kind; label }
           | _ -> None)
  in
  if not (Map.Poly.is_empty partial_sinks_to_taint) then
    let sources =
      source_tree |> ForwardState.Tree.partition ForwardTaint.kind By ~f:Fn.id |> Map.Poly.keys
    in
    let add_triggered_sinks (triggered, candidates) sink =
      let add_triggered_sinks_for_source source =
        TaintConfiguration.get_triggered_sink ~partial_sink:sink ~source
        |> function
        | Some (Sinks.TriggeredPartialSink triggered_sink) ->
            if Hash_set.mem triggered_sinks (Sinks.show_partial_sink sink) then
              (* We have both pairs, let's check the flow directly for this sink being triggered. *)
              let candidate =
                generate_source_sink_matches
                  ~location
                  ~source_tree
                  ~sink_tree:
                    (BackwardState.Tree.create_leaf
                       (BackwardTaint.singleton ~location (Sinks.TriggeredPartialSink sink)))
              in
              None, Some candidate
            else
              Some triggered_sink, None
        | _ -> None, None
      in
      let new_triggered, new_candidates =
        List.map sources ~f:add_triggered_sinks_for_source
        |> List.unzip
        |> fun (triggered_sinks, candidates) ->
        List.filter_opt triggered_sinks, List.filter_opt candidates
      in
      List.rev_append new_triggered triggered, List.rev_append new_candidates candidates
    in
    partial_sinks_to_taint |> Core.Map.Poly.keys |> List.fold ~f:add_triggered_sinks ~init:([], [])
  else
    [], []
