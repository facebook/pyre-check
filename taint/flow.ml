(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Domains


type flow = {
  source_taint: ForwardTaint.t;
  sink_taint: BackwardTaint.t;
}
[@@deriving sexp]


type flows = flow list
[@@deriving sexp]


type candidate = {
  flows: flows;
  location: Location.t;
}
[@@deriving sexp]


type issue = {
  code: int;
  flow: flow;
  issue_location: Location.t;
  define: Statement.Define.t Node.t;
}
[@@deriving sexp]


(* Compute all flows from paths in ~source tree to corresponding paths in
   ~sink tree, while avoiding duplication as much as possible.
   Strategy:
   Let F and B for forward and backward taint
   respectively. For each path p in B from the root to some node with non-empty
   taint T, we match T with the join of taint in the upward and downward closure
   from node at path p in F. *)
let generate_source_sink_matches ~location ~source_tree ~sink_tree =
  let make_source_sink_matches matches {BackwardState.Tree.path; tip=sink_taint; _} =
    let source_taint = ForwardState.Tree.collapse (ForwardState.Tree.read path source_tree) in
    if ForwardTaint.is_bottom source_taint then
      matches
    else
      { source_taint; sink_taint; } :: matches
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
  { location; flows; }


type flow_state = {
  matched: flows;
  rest: flows;
}
[@@deriving sexp]


(* partition taint flow t according to sources/sinks filters into matching and
   rest flows. *)
let partition_flow ?sources ?sinks flow =
  let split ~default partition =
    Map.Poly.find partition true |> Option.value ~default,
    Map.Poly.find partition false |> Option.value ~default
  in
  let included_source_taint, excluded_source_taint =
    match sources with
    | None ->
        flow.source_taint, ForwardTaint.bottom
    | Some f ->
        ForwardTaint.partition ForwardTaint.leaf ~f flow.source_taint
        |> split ~default:ForwardTaint.bottom
  in
  let included_sink_taint, excluded_sink_taint =
    match sinks with
    | None ->
        flow.sink_taint, BackwardTaint.bottom
    | Some f ->
        BackwardTaint.partition BackwardTaint.leaf ~f flow.sink_taint
        |> split ~default:BackwardTaint.bottom
  in
  if ForwardTaint.is_bottom included_source_taint
  || BackwardTaint.is_bottom included_sink_taint then
    { matched = []; rest = [ flow ]; }
  else
    let matched = [ { source_taint = included_source_taint; sink_taint = included_sink_taint; } ] in
    match
      ForwardTaint.is_bottom excluded_source_taint, BackwardTaint.is_bottom excluded_sink_taint
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
  sources: Sources.t list;
  sinks: Sinks.t list;
  code: int;
  name: string;
  message_format: string;  (* format *)
}


let rules = [
  {
    sources = [ Sources.UserControlled ];
    sinks = [ Sinks.RemoteCodeExecution ];
    code = 5001;
    name = "Possible shell injection.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.Test ];
    sinks = [ Sinks.Test ];
    code = 5002;
    name = "Test flow.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.UserControlled ];
    sinks = [ Sinks.Thrift ];
    code = 5003;
    name = "User controlled data to thrift.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.Thrift ];
    sinks = [ Sinks.RemoteCodeExecution ];
    code = 5004;
    name = "Thrift return data to remote code execution.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.UserControlled ];
    sinks = [ Sinks.SQL ];
    code = 5005;
    name = "User controlled data to SQL execution.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.Cookies; Sources.PII; Sources.Secrets ];
    sinks = [ Sinks.Logging ];
    code = 5006;
    name = "Restricted data being logged.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.UserControlled ];
    sinks = [ Sinks.XMLParser ];
    code = 5007;
    name = "User data to XML Parser.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.UserControlled ];
    sinks = [ Sinks.XSS ];
    code = 5008;
    name = "XSS";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.Demo ];
    sinks = [ Sinks.Demo ];
    code = 5009;
    name = "Demo flow.";
    message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  };
  {
    sources = [ Sources.UserControlled ];
    sinks = [ Sinks.GetAttr ];
    code = 5010;
    name = "User data to getattr.";
    message_format = "Attacker may control at least one argument to getattr(,)."
  };
]


let generate_issues ~define { location; flows; } =
  let apply_rule (issues, remaining_flows) { sources; sinks; code; _ } =
    let any_sources source_list source =
      List.exists ~f:((=) source) source_list
    in
    let any_sinks sink_list sink =
      List.exists ~f:((=) sink) sink_list
    in
    let { matched; rest; } =
      partition_flows
        ~sources:(any_sources sources)
        ~sinks:(any_sinks sinks)
        remaining_flows
    in
    match matched with
    | [] ->
        (issues, rest)
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
        let issue = {
          code;
          flow;
          issue_location = location;
          define;
        }
        in
        issue :: issues, rest
  in
  let issues, _ = List.fold ~f:apply_rule ~init:([], flows) rules in
  issues


let sinks_regexp = Str.regexp_string "{$sinks}"
let sources_regexp = Str.regexp_string "{$sources}"


let get_name_and_detailed_message { code; flow; _ } =
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) rules with
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


let generate_error ({ code; issue_location; define; _  } as issue) =
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) rules with
  | None -> failwith "issue with code that has no rule"
  | Some _ ->
      let name, detail = get_name_and_detailed_message issue in
      let kind =
        {
          Interprocedural.Error.name;
          messages = [ detail ];
          code;
        }
      in
      Interprocedural.Error.create ~location:issue_location ~define ~kind


let to_json callable issue =
  let callable_name = Interprocedural.Callable.external_target_name callable in
  let name, detail = get_name_and_detailed_message issue in
  let message = Format.sprintf "%s %s" name detail in
  let source_traces = Domains.ForwardTaint.to_json issue.flow.source_taint in
  let sink_traces = Domains.BackwardTaint.to_json issue.flow.sink_taint in
  let traces =
    `List [
      `Assoc [
        "name", `String "forward";
        "roots", source_traces;
      ];
      `Assoc [
        "name", `String "backward";
        "roots", sink_traces;
      ];
    ]
  in
  let issue_location =
    issue.issue_location
    |> Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash)
  in
  let callable_line = Ast.(Location.line issue.define.location) in
  `Assoc [
    "callable", `String callable_name;
    "callable_line", `Int callable_line;
    "code", `Int issue.code;
    "line", `Int (Location.line issue_location);
    "start", `Int (Location.column issue_location);
    "end", `Int (Location.stop_column issue_location);
    "filename", `String (Location.path issue_location);
    "message", `String message;
    "traces", traces;
  ]


let code_metadata () =
  `Assoc (List.map rules ~f:(fun rule -> Format.sprintf "%d" rule.code, `String rule.name))
