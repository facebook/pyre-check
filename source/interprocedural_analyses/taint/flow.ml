(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
           ~transform:(ForwardTaint.add_local_breadcrumbs (Features.issue_broadening_set ()))
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
    let source_breadcrumbs = ForwardTaint.joined_breadcrumbs source_taint in
    let sink_breadcrumbs = BackwardTaint.joined_breadcrumbs sink_taint in
    Features.BreadcrumbSet.sequence_join source_breadcrumbs sink_breadcrumbs
  in
  let first_indices =
    let source_indices = ForwardTaint.first_indices source_taint in
    let sink_indices = BackwardTaint.first_indices sink_taint in
    Features.FirstIndexSet.join source_indices sink_indices
  in
  let first_fields =
    let source_fields = ForwardTaint.first_fields source_taint in
    let sink_fields = BackwardTaint.first_fields sink_taint in
    Features.FirstFieldSet.join source_fields sink_fields
  in

  { breadcrumbs; first_indices; first_fields }


let generate_issues ~define { location; flows } =
  let partitions =
    let partition { source_taint; sink_taint } =
      {
        source_partition =
          ForwardTaint.partition ForwardTaint.kind By source_taint ~f:(fun kind ->
              kind |> Sources.discard_transforms |> Sources.discard_subkind);
        sink_partition =
          BackwardTaint.partition BackwardTaint.kind By sink_taint ~f:(fun kind ->
              kind |> Sinks.discard_transforms |> Sinks.discard_subkind);
      }
    in
    List.map flows ~f:partition
  in
  let apply_rule_on_flow { Rule.sources; sinks; _ } { source_partition; sink_partition } =
    let add_source_taint source_taint source =
      match Map.Poly.find source_partition (Sources.discard_subkind source) with
      | Some taint -> ForwardTaint.join source_taint taint
      | None -> source_taint
    in
    let add_sink_taint sink_taint sink =
      match Map.Poly.find sink_partition (Sinks.discard_subkind sink) with
      | Some taint -> BackwardTaint.join sink_taint taint
      | None -> sink_taint
    in
    let source_taint = List.fold sources ~f:add_source_taint ~init:ForwardTaint.bottom in
    let sink_taint = List.fold sinks ~f:add_sink_taint ~init:BackwardTaint.bottom in

    let rec apply_sanitizers
        ?(previous_sanitized_sources = Sources.Set.empty)
        ?(previous_sanitized_sinks = Sinks.Set.empty)
        { source_taint; sink_taint }
      =
      (* This needs a fixpoint since refining sinks might sanitize more sources etc.
       * For instance:
       * Sources: {Not[X]@A, Not[X]:Not[Y]@C}
       * Sinks: {X, Not[A]@Y}
       * After one iteration, we still have {Not[X]:Not[Y]@C} and {Not[A]@Y},
       * which can be refined further to an invalid flow.
       *)
      let gather_sanitized_sinks kind sofar =
        let sanitized =
          kind
          |> Sources.extract_sanitize_transforms
          |> Sinks.extract_sanitized_sinks_from_transforms
        in
        match sofar with
        | None -> Some sanitized
        | Some sofar -> Some (Sinks.Set.inter sofar sanitized)
      in
      let sanitized_sinks =
        ForwardTaint.fold ForwardTaint.kind ~init:None ~f:gather_sanitized_sinks source_taint
        |> Option.value ~default:Sinks.Set.empty
      in
      let sink_taint = BackwardTaint.sanitize sanitized_sinks sink_taint in

      let gather_sanitized_sources kind sofar =
        let sanitized =
          kind
          |> Sinks.extract_sanitize_transforms
          |> Sources.extract_sanitized_sources_from_transforms
        in
        match sofar with
        | None -> Some sanitized
        | Some sofar -> Some (Sources.Set.inter sofar sanitized)
      in
      let sanitized_sources =
        BackwardTaint.fold BackwardTaint.kind ~init:None ~f:gather_sanitized_sources sink_taint
        |> Option.value ~default:Sources.Set.empty
      in
      let source_taint = ForwardTaint.sanitize sanitized_sources source_taint in

      if
        Sources.Set.equal sanitized_sources previous_sanitized_sources
        && Sinks.Set.equal sanitized_sinks previous_sanitized_sinks
      then
        { source_taint; sink_taint }
      else
        apply_sanitizers
          ~previous_sanitized_sources:sanitized_sources
          ~previous_sanitized_sinks:sanitized_sinks
          { source_taint; sink_taint }
    in
    let partition_flow = apply_sanitizers { source_taint; sink_taint } in
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
        |> List.map ~f:Sources.discard_sanitize_transforms
        |> List.dedup_and_sort ~compare:Sources.compare
        |> List.map ~f:Sources.show
        |> String.concat ~sep:", "
      in
      let sinks =
        Domains.BackwardTaint.kinds flow.sink_taint
        |> List.map ~f:Sinks.discard_sanitize_transforms
        |> List.dedup_and_sort ~compare:Sinks.compare
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
      let element = Features.BreadcrumbInterned.unintern element in
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
        issue.features.first_indices
        |> Features.FirstIndexSet.elements
        |> List.map ~f:Features.FirstIndexInterned.unintern
        |> Features.FirstIndex.to_json;
        issue.features.first_fields
        |> Features.FirstFieldSet.elements
        |> List.map ~f:Features.FirstFieldInterned.unintern
        |> Features.FirstField.to_json;
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
      ~transform:(BackwardTaint.add_local_breadcrumbs (Features.issue_broadening_set ()))
      sink_tree
    |> BackwardTaint.partition BackwardTaint.kind ByFilter ~f:Sinks.extract_partial_sink
  in
  if not (Map.Poly.is_empty partial_sinks_to_taint) then
    let sources =
      ForwardState.Tree.fold ForwardTaint.kind ~f:List.cons ~init:[] source_tree
      |> List.map ~f:Sources.discard_transforms
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
                       (BackwardTaint.singleton
                          ~location
                          (Sinks.TriggeredPartialSink sink)
                          Frame.initial))
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


let source_can_match_rule = function
  | Sources.Transform { sanitize_local; sanitize_global; base = NamedSource name }
  | Sources.Transform
      { sanitize_local; sanitize_global; base = ParametricSource { source_name = name; _ } } -> (
      let { matching_sinks; _ } = TaintConfiguration.get () in
      match Sources.Map.find_opt (Sources.NamedSource name) matching_sinks with
      | None ->
          (* TODO(T104600511): Filter out sources that are never used in any rule. *)
          false
      | Some sinks ->
          SanitizeTransform.Set.union sanitize_local sanitize_global
          |> Sinks.extract_sanitized_sinks_from_transforms
          |> Sinks.Set.diff sinks
          |> Sinks.Set.is_empty
          |> not)
  | _ -> true


let sink_can_match_rule = function
  | Sinks.Transform { sanitize_local; sanitize_global; base = NamedSink name }
  | Sinks.Transform
      { sanitize_local; sanitize_global; base = ParametricSink { sink_name = name; _ } } -> (
      let { matching_sources; _ } = TaintConfiguration.get () in
      match Sinks.Map.find_opt (NamedSink name) matching_sources with
      | None ->
          (* TODO(T104600511): Filter out sinks that are never used in any rule. *)
          false
      | Some sources ->
          SanitizeTransform.Set.union sanitize_local sanitize_global
          |> Sources.extract_sanitized_sources_from_transforms
          |> Sources.Set.diff sources
          |> Sources.Set.is_empty
          |> not)
  | _ -> true
