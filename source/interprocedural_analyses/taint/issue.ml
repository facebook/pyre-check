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
open Interprocedural
open Pyre

module Flow = struct
  type t = {
    source_taint: ForwardTaint.t;
    sink_taint: BackwardTaint.t;
  }
  [@@deriving show]

  let bottom = { source_taint = ForwardTaint.bottom; sink_taint = BackwardTaint.bottom }

  let is_bottom { source_taint; sink_taint } =
    ForwardTaint.is_bottom source_taint || BackwardTaint.is_bottom sink_taint


  let join
      { source_taint = left_source_taint; sink_taint = left_sink_taint }
      { source_taint = right_source_taint; sink_taint = right_sink_taint }
    =
    {
      source_taint = ForwardTaint.join left_source_taint right_source_taint;
      sink_taint = BackwardTaint.join left_sink_taint right_sink_taint;
    }
end

module SinkHandle = struct
  type t =
    | Call of {
        callee: Target.t;
        index: int;
        parameter: AccessPath.Root.t;
      }
    | Global of {
        callee: Target.t;
        index: int;
      }
    | Return
    | LiteralStringSink of Sinks.t
    | ConditionalTestSink of Sinks.t
  [@@deriving compare]

  let make_call ~call_target:{ CallGraph.CallTarget.target; index; _ } ~root =
    let root =
      (* Ignore extra information in the parameter in order to group issues together. *)
      let open AccessPath.Root in
      match root with
      | LocalResult -> LocalResult
      | PositionalParameter { name; _ } -> NamedParameter { name }
      | NamedParameter { name } -> NamedParameter { name }
      | StarParameter _ -> StarParameter { position = 0 }
      | StarStarParameter _ -> StarStarParameter { excluded = [] }
      | Variable name -> Variable name
    in
    Call { callee = target; index; parameter = root }


  let make_global ~call_target:{ CallGraph.CallTarget.target; index; _ } =
    Global { callee = target; index }


  let master_handle ~callable ~code sink_handle =
    let version = 0 (* Increment the version on format change. *) in
    let sink_handle =
      match sink_handle with
      | Call { callee; index; parameter } ->
          Format.asprintf
            "Call|%s|%d|%s"
            (Target.external_target_name callee)
            index
            (AccessPath.Root.to_string parameter)
      | Global { callee; index } ->
          Format.asprintf "Global|%s|%d" (Target.external_target_name callee) index
      | Return -> "Return"
      | LiteralStringSink sink -> Format.asprintf "LiteralStringSink|%a" Sinks.pp sink
      | ConditionalTestSink sink -> Format.asprintf "ConditionalTestSink|%a" Sinks.pp sink
    in
    let full_handle = Format.asprintf "%s:%d:%d:%s" callable code version sink_handle in
    let hash = full_handle |> Digest.string |> Digest.to_hex in
    let short_handle =
      String.sub
        full_handle
        ~pos:0
        ~len:(min (String.length full_handle) (255 - String.length hash - 1))
    in
    Format.asprintf "%s:%s" short_handle hash
end

module SinkHandleSet = Caml.Set.Make (SinkHandle)

module SinkTreeWithHandle = struct
  type t = {
    sink_tree: BackwardState.Tree.t;
    handle: SinkHandle.t;
  }

  let filter_bottom sink_tree_with_identifiers =
    List.filter
      ~f:(fun { sink_tree; _ } -> not (BackwardState.Tree.is_bottom sink_tree))
      sink_tree_with_identifiers


  let join sink_tree_with_identifiers =
    List.fold
      ~init:BackwardState.Tree.bottom
      ~f:(fun sofar { sink_tree; _ } -> BackwardState.Tree.join sofar sink_tree)
      sink_tree_with_identifiers
end

type t = {
  code: int;
  flow: Flow.t;
  location: Location.WithModule.t;
  sink_handles: SinkHandleSet.t;
  define: Statement.Define.t Node.t;
}

type issue = t

module Handle = struct
  type t = {
    code: int;
    location: Location.WithModule.t;
    callable: Target.t;
  }
  [@@deriving sexp, compare]

  let from_issue { code; location; define; _ } =
    { code; location; callable = Interprocedural.Target.create define }
end

module HandleMap = Map.Make (Handle)

module Candidate = struct
  type t = {
    flows: Flow.t list;
    location: Location.WithModule.t;
    sink_handles: SinkHandleSet.t;
  }

  let join
      { flows = left_flows; location; sink_handles = left_sink_handles }
      { flows = right_flows; sink_handles = right_sink_handles; _ }
    =
    {
      flows = List.rev_append left_flows right_flows;
      location;
      sink_handles = SinkHandleSet.union left_sink_handles right_sink_handles;
    }
end

module TriggeredSinks = struct
  type t = String.Hash_set.t
end

(* Compute all flows from paths in ~source tree to corresponding paths in ~sink tree, while avoiding
   duplication as much as possible.

   Strategy:

   Let F and B for forward and backward taint respectively. For each path p in B from the root to
   some node with non-empty taint T, we match T with the join of taint in the upward and downward
   closure from node at path p in F. *)
let generate_source_sink_matches ~location ~sink_handle ~source_tree ~sink_tree =
  let make_source_sink_matches (path, sink_taint) matches =
    let source_taint =
      ForwardState.Tree.read path source_tree
      |> ForwardState.Tree.collapse
           ~transform:(ForwardTaint.add_local_breadcrumbs (Features.issue_broadening_set ()))
    in
    if ForwardTaint.is_bottom source_taint then
      matches
    else
      { Flow.source_taint; sink_taint } :: matches
  in
  let flows =
    if ForwardState.Tree.is_empty source_tree then
      []
    else
      BackwardState.Tree.fold BackwardState.Tree.Path ~init:[] ~f:make_source_sink_matches sink_tree
  in
  { Candidate.flows; location; sink_handles = SinkHandleSet.singleton sink_handle }


let compute_triggered_sinks ~triggered_sinks ~location ~sink_handle ~source_tree ~sink_tree =
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
                  ~sink_handle
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


module PartitionedFlow = struct
  type t = {
    source_partition: (Sources.t, ForwardTaint.t) Map.Poly.t;
    sink_partition: (Sinks.t, BackwardTaint.t) Map.Poly.t;
  }
end

let generate_issues ~define { Candidate.flows; location; sink_handles } =
  let partitions =
    let partition { Flow.source_taint; sink_taint } =
      {
        PartitionedFlow.source_partition =
          ForwardTaint.partition ForwardTaint.kind By source_taint ~f:(fun kind ->
              kind |> Sources.discard_transforms |> Sources.discard_subkind);
        sink_partition =
          BackwardTaint.partition BackwardTaint.kind By sink_taint ~f:(fun kind ->
              kind |> Sinks.discard_transforms |> Sinks.discard_subkind);
      }
    in
    List.map flows ~f:partition
  in
  let apply_rule_on_flow
      { Rule.sources; sinks; transforms; _ }
      { PartitionedFlow.source_partition; sink_partition }
    =
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
        { Flow.source_taint; sink_taint }
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
        { Flow.source_taint; sink_taint }
      else
        apply_sanitizers
          ~previous_sanitized_sources:sanitized_sources
          ~previous_sanitized_sinks:sanitized_sinks
          { source_taint; sink_taint }
    in
    let partition_flow = apply_sanitizers { source_taint; sink_taint } in
    let apply_ordered_transforms { Flow.source_taint; sink_taint } =
      (* TODO(T90698159): Handle interaction with sanitizing transforms *)
      let taint_by_source_transforms =
        ForwardTaint.partition ForwardTaint.kind By source_taint ~f:Sources.get_ordered_transforms
      in
      let taint_by_sink_transforms =
        BackwardTaint.partition BackwardTaint.kind By sink_taint ~f:Sinks.get_ordered_transforms
      in
      let find_flow source_transforms sink_transforms =
        Map.Poly.find taint_by_source_transforms source_transforms
        >>= fun source_taint ->
        Map.Poly.find taint_by_sink_transforms sink_transforms
        >>| fun sink_taint -> { Flow.source_taint; sink_taint }
      in
      let add_flow sofar (source_transforms, sink_transforms) =
        find_flow source_transforms sink_transforms
        |> Option.value_map ~default:sofar ~f:(Flow.join sofar)
      in
      transform_splits transforms |> List.fold ~init:Flow.bottom ~f:add_flow
    in
    let partition_flow = apply_ordered_transforms partition_flow in
    if Flow.is_bottom partition_flow then
      None
    else
      Some partition_flow
  in
  let apply_rule_separate_access_path issues_so_far (rule : Rule.t) =
    let fold_partitions issues candidate =
      match apply_rule_on_flow rule candidate with
      | Some flow -> { code = rule.code; flow; location; sink_handles; define } :: issues
      | None -> issues
    in
    List.fold partitions ~init:issues_so_far ~f:fold_partitions
  in
  let apply_rule_merge_access_path rule =
    let fold_partitions flow_so_far candidate =
      match apply_rule_on_flow rule candidate with
      | Some flow -> Flow.join flow_so_far flow
      | None -> flow_so_far
    in
    let flow =
      List.fold
        partitions
        ~init:{ Flow.source_taint = ForwardTaint.bottom; sink_taint = BackwardTaint.bottom }
        ~f:fold_partitions
    in
    if Flow.is_bottom flow then
      None
    else
      Some { code = rule.code; flow; location; sink_handles; define }
  in
  let group_by_handle map issue =
    (* SAPP invariant: There should be a single issue per issue handle.
     * The configuration might have multiple rules with the same code due to
     * multi source-sink rules, hence we need to merge issues here. *)
    let handle = Handle.from_issue issue in
    HandleMap.update map handle ~f:(function
        | None -> issue
        | Some previous_issue -> { issue with flow = Flow.join issue.flow previous_issue.flow })
  in
  let configuration = TaintConfiguration.get () in
  if configuration.lineage_analysis then
    (* Create different issues for same access path, e.g, Issue{[a] -> [b]}, Issue {[c] -> [d]}. *)
    (* Note that this breaks a SAPP invariant because there might be multiple issues with the same
       handle. This is fine because in that configuration we do not use SAPP. *)
    List.fold configuration.rules ~init:[] ~f:apply_rule_separate_access_path
  else (* Create single issue for same access path, e.g, Issue{[a],[c] -> [b], [d]}. *)
    List.filter_map ~f:apply_rule_merge_access_path configuration.rules
    |> List.fold ~init:HandleMap.empty ~f:group_by_handle
    |> HandleMap.data


module Candidates = struct
  type t = Candidate.t Location.WithModule.Table.t

  let create () = Location.WithModule.Table.create ()

  let add_candidate candidates ({ Candidate.location; _ } as candidate) =
    Location.WithModule.Table.update candidates location ~f:(function
        | None -> candidate
        | Some current_candidate -> Candidate.join current_candidate candidate)


  let check_flow candidates ~location ~sink_handle ~source_tree ~sink_tree =
    generate_source_sink_matches ~location ~sink_handle ~source_tree ~sink_tree
    |> add_candidate candidates


  let check_triggered_flows
      candidates
      ~triggered_sinks
      ~location
      ~sink_handle
      ~source_tree
      ~sink_tree
    =
    let triggered, new_candidates =
      compute_triggered_sinks ~triggered_sinks ~sink_handle ~location ~source_tree ~sink_tree
    in
    List.iter triggered ~f:(fun sink -> Hash_set.add triggered_sinks (Sinks.show_partial_sink sink));
    List.iter new_candidates ~f:(add_candidate candidates)


  let generate_issues candidates ~define =
    let accumulate ~key:_ ~data:candidate issues =
      let new_issues = generate_issues ~define candidate in
      List.rev_append new_issues issues
    in
    Location.WithModule.Table.fold candidates ~f:accumulate ~init:[]
end

type features = {
  breadcrumbs: Features.BreadcrumbSet.t;
  first_indices: Features.FirstIndexSet.t;
  first_fields: Features.FirstFieldSet.t;
}

let get_issue_features { Flow.source_taint; sink_taint } =
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


let sinks_regexp = Str.regexp_string "{$sinks}"

let sources_regexp = Str.regexp_string "{$sources}"

let transforms_regexp = Str.regexp_string "{$transforms}"

let get_name_and_detailed_message { code; flow; _ } =
  let configuration = TaintConfiguration.get () in
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) configuration.rules with
  | None -> failwith "issue with code that has no rule"
  | Some { name; message_format; transforms; _ } ->
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
      let transforms = List.map transforms ~f:TaintTransform.show |> String.concat ~sep:", " in
      let message =
        Str.global_replace sources_regexp sources message_format
        |> Str.global_replace sinks_regexp sinks
        |> Str.global_replace transforms_regexp transforms
      in
      name, message


let to_error ({ code; location; define; _ } as issue) =
  let configuration = TaintConfiguration.get () in
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) configuration.rules with
  | None -> failwith "issue with code that has no rule"
  | Some _ ->
      let name, detail = get_name_and_detailed_message issue in
      let kind = { Error.name; messages = [detail]; code } in
      Error.create ~location ~define ~kind


let to_json ~filename_lookup callable issue =
  let callable_name = Target.external_target_name callable in
  let _, message = get_name_and_detailed_message issue in
  let source_traces =
    Domains.ForwardTaint.to_external_json ~filename_lookup issue.flow.source_taint
  in
  let sink_traces = Domains.BackwardTaint.to_external_json ~filename_lookup issue.flow.sink_taint in
  let features = get_issue_features issue.flow in
  let json_features =
    let get_feature_json { Abstract.OverUnderSetDomain.element; in_under } breadcrumbs =
      let element = Features.BreadcrumbInterned.unintern element in
      let breadcrumb_json = Features.Breadcrumb.to_json element ~on_all_paths:in_under in
      breadcrumb_json :: breadcrumbs
    in
    Features.BreadcrumbSet.fold
      Features.BreadcrumbSet.ElementAndUnder
      ~f:get_feature_json
      ~init:[]
      features.breadcrumbs
  in
  let json_features =
    List.concat
      [
        features.first_indices
        |> Features.FirstIndexSet.elements
        |> List.map ~f:Features.FirstIndexInterned.unintern
        |> Features.FirstIndex.to_json;
        features.first_fields
        |> Features.FirstFieldSet.elements
        |> List.map ~f:Features.FirstFieldInterned.unintern
        |> Features.FirstField.to_json;
        json_features;
      ]
  in
  let traces : Yojson.Safe.json =
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
    Location.WithModule.instantiate ~lookup:filename_lookup issue.location
  in
  let callable_line = Ast.(Location.line issue.define.location) in
  let master_handles =
    SinkHandleSet.fold
      (fun sink_handle handles ->
        `String (SinkHandle.master_handle ~callable:callable_name ~code:issue.code sink_handle)
        :: handles)
      issue.sink_handles
      []
  in
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
      "features", `List json_features;
      "master_handles", `List master_handles;
    ]


let code_metadata () =
  let configuration = TaintConfiguration.get () in
  `Assoc
    (List.map configuration.rules ~f:(fun rule -> Format.sprintf "%d" rule.code, `String rule.name))


let source_can_match_rule = function
  | Sources.Transform { local; global; base = NamedSource name }
  | Sources.Transform { local; global; base = ParametricSource { source_name = name; _ } } -> (
      let { matching_sinks; _ } = TaintConfiguration.get () in
      let source =
        match local.ordered @ global.ordered with
        | [] -> Sources.NamedSource name
        | ordered ->
            Sources.Transform
              {
                local = TaintTransforms.empty;
                global = { TaintTransforms.empty with ordered };
                base = Sources.NamedSource name;
              }
      in
      match Sources.Map.find_opt source matching_sinks with
      | None ->
          (* TODO(T104600511): Filter out sources that are never used in any rule. *)
          false
      | Some sinks ->
          SanitizeTransform.Set.union local.sanitize global.sanitize
          |> Sinks.extract_sanitized_sinks_from_transforms
          |> Sinks.Set.diff sinks
          |> Sinks.Set.is_empty
          |> not)
  | _ -> true


let sink_can_match_rule = function
  | Sinks.Transform { local; global; base = NamedSink name }
  | Sinks.Transform { local; global; base = ParametricSink { sink_name = name; _ } } -> (
      let { matching_sources; _ } = TaintConfiguration.get () in
      let sink =
        match local.ordered @ global.ordered with
        | [] -> Sinks.NamedSink name
        | ordered ->
            Sinks.Transform
              {
                local = TaintTransforms.empty;
                global = { TaintTransforms.empty with ordered };
                base = Sinks.NamedSink name;
              }
      in
      match Sinks.Map.find_opt sink matching_sources with
      | None ->
          (* TODO(T104600511): Filter out sinks that are never used in any rule. *)
          false
      | Some sources ->
          SanitizeTransform.Set.union local.sanitize global.sanitize
          |> Sources.extract_sanitized_sources_from_transforms
          |> Sources.Set.diff sources
          |> Sources.Set.is_empty
          |> not)
  | _ -> true
