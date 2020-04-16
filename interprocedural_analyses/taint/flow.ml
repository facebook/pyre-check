(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Configuration
open Domains
open Pyre

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

type issue = {
  code: int;
  flow: flow;
  features: Features.SimpleSet.t;
  issue_location: Location.WithModule.t;
  define: Statement.Define.t Node.t;
}

type triggered_sinks = (AccessPath.Root.t * Sinks.t) list Location.Table.t

(* Compute all flows from paths in ~source tree to corresponding paths in ~sink tree, while avoiding
   duplication as much as possible.

   Strategy:

   Let F and B for forward and backward taint respectively. For each path p in B from the root to
   some node with non-empty taint T, we match T with the join of taint in the upward and downward
   closure from node at path p in F. *)
let generate_source_sink_matches ~location ~source_tree ~sink_tree =
  let make_source_sink_matches { BackwardState.Tree.path; tip = sink_taint; _ } matches =
    let source_taint = ForwardState.Tree.collapse (ForwardState.Tree.read path source_tree) in
    if ForwardTaint.is_bottom source_taint then
      matches
    else
      { source_taint; sink_taint } :: matches
  in
  let flows =
    if ForwardState.Tree.is_empty source_tree then
      []
    else
      BackwardState.Tree.fold
        BackwardState.Tree.RawPath
        ~init:[]
        ~f:make_source_sink_matches
        sink_tree
  in
  { location; flows }


type flow_state = {
  matched: flows;
  rest: flows;
}

(* partition taint flow t according to sources/sinks filters into matching and rest flows. *)
let partition_flow ?sources ?sinks flow =
  let split ~default partition =
    ( Map.Poly.find partition true |> Option.value ~default,
      Map.Poly.find partition false |> Option.value ~default )
  in
  let included_source_taint, excluded_source_taint =
    match sources with
    | None -> flow.source_taint, ForwardTaint.bottom
    | Some f ->
        ForwardTaint.partition
          ForwardTaint.leaf
          ~f:(fun leaf -> f leaf |> Option.some)
          flow.source_taint
        |> split ~default:ForwardTaint.bottom
  in
  let included_sink_taint, excluded_sink_taint =
    match sinks with
    | None -> flow.sink_taint, BackwardTaint.bottom
    | Some f ->
        BackwardTaint.partition
          BackwardTaint.leaf
          ~f:(fun leaf -> f leaf |> Option.some)
          flow.sink_taint
        |> split ~default:BackwardTaint.bottom
  in
  if ForwardTaint.is_bottom included_source_taint || BackwardTaint.is_bottom included_sink_taint
  then
    { matched = []; rest = [flow] }
  else
    let matched = [{ source_taint = included_source_taint; sink_taint = included_sink_taint }] in
    match
      ForwardTaint.is_bottom excluded_source_taint, BackwardTaint.is_bottom excluded_sink_taint
    with
    | true, true -> { matched; rest = [] }
    | true, false -> { matched; rest = [{ flow with sink_taint = excluded_sink_taint }] }
    | false, true -> { matched; rest = [{ flow with source_taint = excluded_source_taint }] }
    | false, false ->
        {
          matched;
          rest =
            [
              { source_taint = excluded_source_taint; sink_taint = included_sink_taint };
              { flow with sink_taint = excluded_sink_taint };
            ];
        }


let partition_flows ?sources ?sinks flows =
  let accumulate_matches { matched; rest } flow =
    let { matched = new_matching; rest = new_rest } = partition_flow ?sources ?sinks flow in
    { matched = new_matching @ matched; rest = new_rest @ rest }
  in
  List.fold flows ~init:{ matched = []; rest = [] } ~f:accumulate_matches


let get_issue_features { source_taint; sink_taint } =
  let source_features =
    ForwardTaint.fold
      Features.SimpleSet.Self
      ~f:Features.SimpleSet.join
      ~init:Features.SimpleSet.bottom
      source_taint
  in
  let sink_features =
    BackwardTaint.fold
      Features.SimpleSet.Self
      ~f:Features.SimpleSet.join
      ~init:Features.SimpleSet.bottom
      sink_taint
  in
  Features.SimpleSet.sequence_join source_features sink_features


let generate_issues ~define { location; flows } =
  let apply_rule (issues, remaining_flows) { Rule.sources; sinks; code; _ } =
    let any_sources source_list source = List.exists ~f:(( = ) source) source_list in
    let any_sinks sink_list sink = List.exists ~f:(( = ) sink) sink_list in
    let { matched; rest } =
      partition_flows ~sources:(any_sources sources) ~sinks:(any_sinks sinks) remaining_flows
    in
    match matched with
    | [] -> issues, rest
    | matched ->
        let join_flows flows =
          let get_source_taint { source_taint; _ } = source_taint in
          let get_sink_taint { sink_taint; _ } = sink_taint in
          let join_source_taint source_taints =
            List.fold source_taints ~init:ForwardTaint.bottom ~f:ForwardTaint.join
          in
          let join_sink_taint sink_taints =
            List.fold sink_taints ~init:BackwardTaint.bottom ~f:BackwardTaint.join
          in
          {
            source_taint = join_source_taint (List.map flows ~f:get_source_taint);
            sink_taint = join_sink_taint (List.map flows ~f:get_sink_taint);
          }
        in
        let flow = join_flows matched in
        let features = get_issue_features flow in
        let issue = { code; flow; features; issue_location = location; define } in
        issue :: issues, rest
  in
  let configuration = Configuration.get () in
  let issues, _ = List.fold ~f:apply_rule ~init:([], flows) configuration.rules in
  issues


let sinks_regexp = Str.regexp_string "{$sinks}"

let sources_regexp = Str.regexp_string "{$sources}"

let get_name_and_detailed_message { code; flow; _ } =
  let configuration = Configuration.get () in
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) configuration.rules with
  | None -> failwith "issue with code that has no rule"
  | Some { name; message_format; _ } ->
      let sources =
        Domains.ForwardTaint.leaves flow.source_taint
        |> List.map ~f:Sources.show
        |> String.concat ~sep:", "
      in
      let sinks =
        Domains.BackwardTaint.leaves flow.sink_taint
        |> List.map ~f:Sinks.show
        |> String.concat ~sep:", "
      in
      let message =
        Str.global_replace sources_regexp sources message_format
        |> Str.global_replace sinks_regexp sinks
      in
      name, message


let generate_error ({ code; issue_location; define; _ } as issue) =
  let configuration = Configuration.get () in
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) configuration.rules with
  | None -> failwith "issue with code that has no rule"
  | Some _ ->
      let name, detail = get_name_and_detailed_message issue in
      let kind = { Interprocedural.Error.name; messages = [detail]; code } in
      Interprocedural.Error.create ~location:issue_location ~define ~kind


let to_json ~filename_lookup callable issue =
  let callable_name = Interprocedural.Callable.external_target_name callable in
  let _, detail = get_name_and_detailed_message issue in
  let message = detail in
  let source_traces =
    Domains.ForwardTaint.to_external_json ~filename_lookup issue.flow.source_taint
  in
  let sink_traces = Domains.BackwardTaint.to_external_json ~filename_lookup issue.flow.sink_taint in
  let features =
    let get_feature_json { Abstract.OverUnderSetDomain.element; in_under } breadcrumbs =
      let open Features.Simple in
      match element with
      | Breadcrumb breadcrumb ->
          let breadcrumb_json = Features.Breadcrumb.to_json breadcrumb ~on_all_paths:in_under in
          breadcrumb_json :: breadcrumbs
      | _ -> breadcrumbs
    in
    Features.SimpleSet.fold
      Features.SimpleSet.ElementAndUnder
      ~f:get_feature_json
      ~init:[]
      issue.features
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
  let configuration = Configuration.get () in
  `Assoc
    (List.map configuration.rules ~f:(fun rule -> Format.sprintf "%d" rule.code, `String rule.name))


let compute_triggered_sinks ~source_tree ~sink_tree =
  let partial_sinks_to_taint =
    BackwardState.Tree.collapse sink_tree
    |> BackwardTaint.partition BackwardTaint.leaf ~f:(function
           | Sinks.PartialSink { Sinks.kind; label } -> Some { Sinks.kind; label }
           | _ -> None)
  in
  if not (Map.Poly.is_empty partial_sinks_to_taint) then
    let sources =
      source_tree
      |> ForwardState.Tree.partition ForwardTaint.leaf ~f:(fun source -> Some source)
      |> Map.Poly.keys
    in
    let add_triggered_sinks triggered sink =
      let add_triggered_sinks_for_source source =
        Configuration.get_triggered_sink ~partial_sink:sink ~source
        >>= function
        | Sinks.TriggeredPartialSink triggered_sink -> Some triggered_sink
        | _ -> None
      in
      List.filter_map sources ~f:add_triggered_sinks_for_source |> List.rev_append triggered
    in
    partial_sinks_to_taint |> Core.Map.Poly.keys |> List.fold ~f:add_triggered_sinks ~init:[]
  else
    []
