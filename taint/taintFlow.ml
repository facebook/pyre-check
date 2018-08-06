(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open TaintDomains


type flow = {
  source_taint: ForwardTaint.t;
  sink_taint: BackwardTaint.t;
}
[@@deriving sexp]


type flows = flow list


type flow_candidates = {
  candidates: flows;
}


(* Compute all flows from paths in ~source tree to corresponding paths in
   ~sink tree, while avoiding duplication as much as possible.
   Strategy:
   Let F and B for forward and backward taint
   respectively. For each path p in B from the root to some node with non-empty
   taint T, we match T with the join of taint in the upward and downward closure
   from node at path p in F. *)
let generate_source_sink_matches ~source_tree ~sink_tree =
  let make_source_sink_matches ~path ~path_element:_ ~element:sink_taint matches =
    let source_taint = ForwardState.collapse (ForwardState.read_tree path source_tree) in
    if ForwardTaint.is_empty source_taint then
      matches
    else
      { source_taint; sink_taint; } :: matches
  in
  let candidates =
    if ForwardState.is_empty_tree source_tree then
      []
    else
      BackwardState.fold_tree_paths ~init:[] ~f:make_source_sink_matches sink_tree
  in
  { candidates; }


type flow_state = {
  matched: flows;
  rest: flows;
}


(* partition taint flow t according to sources/sinks filters into matching and
   rest flows. *)
let partition_flow ?sources ?sinks flow =
  let included_source_taint, excluded_source_taint =
    match sources with
    | None -> flow.source_taint, ForwardTaint.empty
    | Some f -> ForwardTaint.partition_tf ~f flow.source_taint
  in
  let included_sink_taint, excluded_sink_taint =
    match sinks with
    | None -> flow.sink_taint, BackwardTaint.empty
    | Some f -> BackwardTaint.partition_tf ~f flow.sink_taint
  in
  if ForwardTaint.is_empty included_source_taint
  || BackwardTaint.is_empty included_sink_taint then
    { matched = []; rest = [ flow ]; }
  else
    let matched = [ { source_taint = included_source_taint; sink_taint = included_sink_taint; } ] in
    match
      ForwardTaint.is_empty excluded_source_taint, BackwardTaint.is_empty excluded_sink_taint
    with
    | true, true ->
        { matched; rest = []; }
    | true, false ->
        { matched; rest = [ { flow with sink_taint = excluded_sink_taint; } ]; }
    | false, true ->
        {  matched; rest = [ { flow with source_taint = excluded_source_taint; } ]; }
    | false, false ->
        {
          matched;
          rest = [
            { source_taint = excluded_source_taint; sink_taint = included_sink_taint; };
            { flow with sink_taint = excluded_sink_taint; };
          ]
        }


let partition_flows ?sources ?sinks flows =
  let accumulate_matches { matched; rest; } flow =
    let { matched = new_matching; rest = new_rest } = partition_flow ?sources ?sinks flow in
    { matched = new_matching @ matched; rest = new_rest @ rest; }
  in
  List.fold flows ~init:{ matched = []; rest = []; } ~f:accumulate_matches


type rule = {
  sources: TaintSources.t list;
  sinks: TaintSinks.t list;
  code: int;
  name: string;
}


let rules = [
  {
    sources = [ TaintSources.UserControlled ];
    sinks = [ TaintSinks.RemoteCodeExecution ];
    code = 5001;
    name = "User controlled data may lead to remote code execution.";
  };
  {
    sources = [ TaintSources.TestSource ];
    sinks = [ TaintSinks.TestSink ];
    code = 5002;
    name = "Flow from test source to test sink.";
  };
]


let make_error define location code name flows =
  let get_source_taint { source_taint; _ } = source_taint in
  let get_sink_taint { sink_taint; _ } = sink_taint in
  let join_source_taint source_taints =
    List.fold source_taints ~init:ForwardTaint.empty ~f:ForwardTaint.join
  in
  let join_sink_taint sink_taints =
    List.fold sink_taints ~init:BackwardTaint.empty ~f:BackwardTaint.join
  in
  let join_flows flows =
    {
      source_taint = join_source_taint (List.map flows ~f:get_source_taint);
      sink_taint = join_sink_taint (List.map flows ~f:get_sink_taint);
    }
  in
  let _flow = join_flows flows in
  (* TODO(T32467565) emit trace roots. *)
  let messages = [] in
  let kind = { Interprocedural.Error.name; messages; code; } in
  Interprocedural.Error.create ~location ~define ~kind


let any_sources source_list source =
  List.exists ~f:((=) source) source_list


let any_sinks sink_list sink =
  List.exists ~f:((=) sink) sink_list


let generate_errors ~define ~location { candidates } =
  let apply_rule (errors, remaining_flows) { sources; sinks; code; name; } =
    let { matched; rest; } =
      partition_flows
        ~sources:(any_sources sources)
        ~sinks:(any_sinks sinks)
        remaining_flows
    in
    match matched with
    | [] ->
        (errors, rest)
    | matched ->
        let new_error = make_error define location code name matched in
        new_error :: errors, rest
  in
  let errors, _ = List.fold ~f:apply_rule ~init:([], candidates) rules in
  errors
