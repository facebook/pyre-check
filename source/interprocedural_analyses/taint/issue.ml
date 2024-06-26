(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Issue: implements the logic that matches sources against sinks, using the
 * current set of rules, and convert them into issues.
 * It also defines a handle that uniquely represents issues.
 *)

open Core
open Ast
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

module LocationSet = Stdlib.Set.Make (Location.WithModule)

type t = {
  flow: Flow.t;
  handle: IssueHandle.t;
  locations: LocationSet.t;
  define: Statement.Define.t Node.t;
}

let join
    { flow = flow_left; handle; locations = locations_left; define }
    { flow = flow_right; handle = _; locations = locations_right; define = _ }
  =
  {
    flow = Flow.join flow_left flow_right;
    handle;
    locations = LocationSet.union locations_left locations_right;
    define;
  }


let canonical_location { locations; _ } =
  Option.value_exn ~message:"issue has no location" (LocationSet.min_elt_opt locations)


(* Define how to group issue candidates for a given function. *)
module CandidateKey = struct
  module T = struct
    type t = {
      location: Location.WithModule.t;
      sink_handle: IssueHandle.Sink.t;
    }
    [@@deriving compare, sexp, hash]
  end

  include T
  include Hashable.Make (T)
end

module Candidate = struct
  type t = {
    flows: Flow.t list;
    key: CandidateKey.t;
  }

  let is_empty = function
    | { flows = []; _ } -> true
    | _ -> false


  let join { flows = left_flows; key } { flows = right_flows; _ } =
    { flows = List.rev_append left_flows right_flows; key }
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
      ForwardState.Tree.read_refined path source_tree
      |> ForwardState.Tree.collapse ~breadcrumbs:(Features.issue_broadening_set ())
    in
    if ForwardTaint.is_bottom source_taint then
      matches
    else
      { Flow.source_taint; sink_taint } :: matches
  in
  let flows =
    if ForwardState.Tree.is_empty source_tree || BackwardState.Tree.is_empty sink_tree then
      []
    else
      BackwardState.Tree.fold
        BackwardState.Tree.RefinedPath
        ~init:[]
        ~f:make_source_sink_matches
        sink_tree
  in
  { Candidate.flows; key = { location; sink_handle } }


module PartitionedFlow = struct
  type t = {
    source_partition: (Sources.t, ForwardTaint.t) Map.Poly.t;
    sink_partition: (Sinks.t, BackwardTaint.t) Map.Poly.t;
  }
end

let generate_issues
    ~taint_configuration
    ~define
    { Candidate.flows; key = { location; sink_handle } }
  =
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
      { Rule.sources; sinks; transforms; filters; _ }
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
        ?(previous_single_base_source = None)
        ?(previous_single_base_sink = None)
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
          |> (fun { sinks; _ } -> sinks)
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
      let sink_taint = BackwardTaint.sanitize_taint_kinds sanitized_sinks sink_taint in

      let gather_sanitized_sources kind sofar =
        let sanitized =
          kind
          |> Sinks.extract_sanitize_transforms
          |> (fun { sources; _ } -> sources)
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
      let source_taint = ForwardTaint.sanitize_taint_kinds sanitized_sources source_taint in

      (* If all sources have the same base, we can remove sink flows that sanitize
       * that base (and vice versa). *)
      let gather_base_sources kind sofar =
        Sources.Set.add
          (kind |> Sources.discard_sanitize_transforms |> Sources.discard_subkind)
          sofar
      in
      let single_base_source =
        ForwardTaint.fold
          ForwardTaint.kind
          ~init:Sources.Set.empty
          ~f:gather_base_sources
          source_taint
        |> Sources.Set.as_singleton
      in
      let sink_taint =
        match single_base_source with
        | Some (Sources.NamedSource source) ->
            let sanitize_transforms =
              SanitizeTransform.Source.Named source
              |> SanitizeTransform.SourceSet.singleton
              |> SanitizeTransformSet.from_sources
            in
            BackwardTaint.transform
              BackwardTaint.kind
              Filter
              ~f:(fun kind -> not (Sinks.contains_sanitize_transforms kind sanitize_transforms))
              sink_taint
        | _ -> sink_taint
      in

      let gather_base_sinks kind sofar =
        Sinks.Set.add (kind |> Sinks.discard_sanitize_transforms |> Sinks.discard_subkind) sofar
      in
      let single_base_sink =
        BackwardTaint.fold BackwardTaint.kind ~init:Sinks.Set.empty ~f:gather_base_sinks sink_taint
        |> Sinks.Set.as_singleton
      in
      let source_taint =
        match single_base_sink with
        | Some (Sinks.NamedSink sink) ->
            let sanitize_transforms =
              SanitizeTransform.Sink.Named sink
              |> SanitizeTransform.SinkSet.singleton
              |> SanitizeTransformSet.from_sinks
            in
            ForwardTaint.transform
              ForwardTaint.kind
              Filter
              ~f:(fun kind -> not (Sources.contains_sanitize_transforms kind sanitize_transforms))
              source_taint
        | _ -> source_taint
      in

      if
        Sources.Set.equal sanitized_sources previous_sanitized_sources
        && Sinks.Set.equal sanitized_sinks previous_sanitized_sinks
        && Option.equal Sources.equal single_base_source previous_single_base_source
        && Option.equal Sinks.equal single_base_sink previous_single_base_sink
      then
        { Flow.source_taint; sink_taint }
      else
        apply_sanitizers
          ~previous_sanitized_sources:sanitized_sources
          ~previous_sanitized_sinks:sanitized_sinks
          ~previous_single_base_source:single_base_source
          ~previous_single_base_sink:single_base_sink
          { source_taint; sink_taint }
    in
    let apply_transforms { Flow.source_taint; sink_taint } =
      let taint_by_source_transforms =
        ForwardTaint.partition ForwardTaint.kind By source_taint ~f:Sources.get_named_transforms
      in
      let taint_by_sink_transforms =
        BackwardTaint.partition BackwardTaint.kind By sink_taint ~f:Sinks.get_named_transforms
      in
      let find_flow source_transforms sink_transforms =
        Map.Poly.find taint_by_source_transforms source_transforms
        >>= fun source_taint ->
        Map.Poly.find taint_by_sink_transforms sink_transforms
        >>| fun sink_taint -> { Flow.source_taint; sink_taint }
      in
      let add_and_sanitize_flow sofar (source_transforms, sink_transforms) =
        find_flow source_transforms sink_transforms
        >>| apply_sanitizers
        |> Option.value_map ~default:sofar ~f:(Flow.join sofar)
      in
      Rule.transform_splits transforms |> List.fold ~init:Flow.bottom ~f:add_and_sanitize_flow
    in
    let apply_filters { Flow.source_taint; sink_taint } =
      let source_taint =
        match filters with
        | Some { maximum_source_distance = Some maximum_source_distance; _ } ->
            ForwardTaint.prune_maximum_length maximum_source_distance source_taint
        | _ -> source_taint
      in
      let sink_taint =
        match filters with
        | Some { maximum_sink_distance = Some maximum_sink_distance; _ } ->
            BackwardTaint.prune_maximum_length maximum_sink_distance sink_taint
        | _ -> sink_taint
      in
      { Flow.source_taint; sink_taint }
    in
    let partition_flow = { source_taint; sink_taint } |> apply_transforms |> apply_filters in
    if Flow.is_bottom partition_flow then
      None
    else
      Some partition_flow
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
      Some
        {
          flow;
          handle =
            { code = rule.code; callable = Target.create (Node.value define); sink = sink_handle };
          locations = LocationSet.singleton location;
          define;
        }
  in
  let group_by_handle map issue =
    (* SAPP invariant: There should be a single issue per issue handle.
     * The configuration might have multiple rules with the same code due to
     * multi source-sink rules, hence we need to merge issues here. *)
    let update = function
      | None -> Some issue
      | Some previous_issue -> Some (join previous_issue issue)
    in
    IssueHandle.SerializableMap.update issue.handle update map
  in
  (* Create single issue for same access path, e.g, Issue{[a],[c] -> [b], [d]}. *)
  List.filter_map ~f:apply_rule_merge_access_path taint_configuration.TaintConfiguration.Heap.rules
  |> List.fold ~init:IssueHandle.SerializableMap.empty ~f:group_by_handle
  |> IssueHandle.SerializableMap.data


(* A map from triggered sink kinds (which is a string) to their triggers. A triggered sink here
   means we found one source, and must find the other source, in order to file an issue for a
   multi-source. *)
module TriggeredSinkHashMap = struct
  module Hashable = Core.Hashable.Make (String)
  module HashMap = Hashable.Table

  module Trigger = struct
    (* Information about why a triggered sink is created: the sources that triggered them, the
       source traces, as well as the locations that the sources flow into. The source traces will be
       shown as subtraces *)
    type t = {
      source: Sources.TriggeringSource.t;
      source_trace: Domains.ExtraTraceFirstHop.t;
      argument_location: Location.t;
    }

    let create_taint
        ~call_info
        ~partial_sink
        ~argument_location
        { source; source_trace; argument_location = trigger_location }
      =
      if Location.equal argument_location trigger_location then
        (* Only generate triggered taint on the non-fullfilled parameter. E.g., in `foo(a=source(),
           b)`, we only want to create a triggered sink on `b`, not `a`. The flow on `a` triggers
           creating a taint on `b`. *)
        None
      else
        Some
          (BackwardTaint.singleton
             call_info
             (Sinks.TriggeredPartialSink { partial_sink; triggering_source = source })
             Frame.initial
          |> BackwardTaint.transform
               ExtraTraceFirstHop.Set.Self
               Map
               ~f:(ExtraTraceFirstHop.Set.add source_trace))
  end

  type t = Trigger.t list HashMap.t

  let create () = HashMap.create ()

  let is_empty = Core.Hashtbl.is_empty

  let mem = Core.Hashtbl.mem

  let add
      ~argument_location
      ~triggered_sink:{ Sinks.PartialSink.Triggered.partial_sink; triggering_source }
      ~extra_trace
      map
    =
    let trigger =
      { Trigger.source = triggering_source; source_trace = extra_trace; argument_location }
    in
    let update = function
      | Some existing_triggers -> trigger :: existing_triggers
      | None -> [trigger]
    in
    Core.Hashtbl.update map partial_sink ~f:update


  (* Turn the given partial sink into a triggered sink taint at certain location, based on what has
     triggered this partial sink. *)
  let create_triggered_sink_taint ~argument_location ~call_info ~partial_sink map =
    Core.Hashtbl.find map partial_sink
    >>| List.filter_map ~f:(Trigger.create_taint ~call_info ~partial_sink ~argument_location)
    >>| Algorithms.fold_balanced ~f:BackwardTaint.join ~init:BackwardTaint.bottom
    |> Option.value ~default:BackwardTaint.bottom
end

(* A map from locations to the triggered sinks that are created during the forward analysis on each
   expression, but should be propagated by the backward analysis. *)
module TriggeredSinkLocationMap = struct
  module ExpressionMap = Stdlib.Map.Make (Ast.Expression)

  type t = BackwardState.Tree.t ExpressionMap.t Location.Table.t

  let create () = Location.Table.create ()

  let add ~location ~expression ~taint_tree map =
    let update_expression_map = function
      | Some existing_tree -> Some (BackwardState.Tree.join existing_tree taint_tree)
      | None -> Some taint_tree
    in
    Hashtbl.update map location ~f:(function
        | Some existing_map -> ExpressionMap.update expression update_expression_map existing_map
        | None -> ExpressionMap.singleton expression taint_tree)


  let get ~location ~expression map =
    Hashtbl.find map location
    |> Option.value ~default:ExpressionMap.empty
    |> ExpressionMap.find_opt expression
    |> Option.value ~default:BackwardState.Tree.bottom
end

let compute_triggered_flows
    ~pyre_in_context
    ~taint_configuration:{ TaintConfiguration.Heap.partial_sink_converter; _ }
    ~triggered_sinks_for_call
    ~location
    ~sink_handle
    ~source_tree
    ~sink_tree
    ~callee
    ~port
  =
  let partial_sinks =
    if ForwardState.Tree.is_bottom source_tree then
      []
    else
      BackwardState.Tree.fold
        BackwardTaint.kind
        ~f:(fun sink sofar ->
          match Sinks.extract_partial_sink sink with
          | Some partial_sink -> partial_sink :: sofar
          | None -> sofar)
        ~init:[]
        sink_tree
  in
  let call_infos_and_sources =
    if List.is_empty partial_sinks then
      []
    else
      ForwardState.Tree.reduce
        ForwardTaint.kind
        ~using:(Context (ForwardTaint.call_info, Acc))
        ~f:(fun call_info source sofar -> (call_info, source) :: sofar)
        ~init:[]
        source_tree
  in
  let generate_issue_or_attach_subtraces ~call_info ~source ~partial_sink triggered_sink =
    if TriggeredSinkHashMap.mem triggered_sinks_for_call partial_sink then
      (* Since another flow has already been found, both flows are found and hence we emit an issue.
         No need to add subtraces for the discovered flow, since it is already an issue. *)
      let triggered_sink_taint =
        triggered_sinks_for_call
        |> TriggeredSinkHashMap.create_triggered_sink_taint
             ~argument_location:(Location.strip_module location)
             ~call_info:CallInfo.declaration
             ~partial_sink
        |> BackwardTaint.apply_call
             ~pyre_in_context
             ~location
             ~callee
             ~arguments:[]
             ~path:[]
             ~port
             ~is_class_method:false
             ~is_static_method:false
             ~call_info_intervals:Domains.ClassIntervals.top
      in
      if BackwardTaint.is_bottom triggered_sink_taint then
        None
      else
        Some
          (generate_source_sink_matches
             ~location
             ~sink_handle
             ~source_tree
             ~sink_tree:(BackwardState.Tree.create_leaf triggered_sink_taint))
    else (* Remember the discovered flow so that later it can be attached as a subtrace. *)
      let extra_trace =
        {
          ExtraTraceFirstHop.call_info;
          leaf_kind = Source source;
          message = Some (Format.asprintf "Subtrace for source %s" (Sources.show source));
        }
      in
      let () =
        TriggeredSinkHashMap.add
          triggered_sinks_for_call
          ~argument_location:(Location.strip_module location)
          ~triggered_sink
          ~extra_trace
      in
      None
  in
  let check_source_sink_flows ~call_info ~source ~partial_sink =
    TaintConfiguration.PartialSinkConverter.get_triggered_sinks_if_matched
      ~partial_sink
      ~source
      partial_sink_converter
    |> Sinks.PartialSink.Triggered.Set.elements
    (* One flow is found. If both flows are present, turn the flow into an issue. Otherwise, record
       the flow so that it can be attached later. *)
    |> List.filter_map ~f:(generate_issue_or_attach_subtraces ~call_info ~source ~partial_sink)
  in
  partial_sinks
  |> List.cartesian_product call_infos_and_sources
  |> List.map ~f:(fun ((call_info, source), partial_sink) ->
         check_source_sink_flows ~call_info ~source ~partial_sink)
  |> List.concat


module Candidates = struct
  type issue = t

  type t = Candidate.t CandidateKey.Table.t

  let create () = CandidateKey.Table.create ()

  let add_candidate candidates ({ Candidate.key; _ } as candidate) =
    if not (Candidate.is_empty candidate) then
      Core.Hashtbl.update candidates key ~f:(function
          | None -> candidate
          | Some current_candidate -> Candidate.join current_candidate candidate)


  (* Check for issues in flows from the `source_tree` to the `sink_tree`, updating
   * issue `candidates`. *)
  let check_flow candidates ~location ~sink_handle ~source_tree ~sink_tree =
    generate_source_sink_matches ~location ~sink_handle ~source_tree ~sink_tree
    |> add_candidate candidates


  (* Check for issues for combined source rules.
   * For flows where both sources are present, this adds the flow to issue `candidates`.
   * If only one source is present, this creates a triggered sink in `triggered_sinks_for_call`.
   *)
  let check_triggered_flows
      candidates
      ~pyre_in_context
      ~taint_configuration
      ~triggered_sinks_for_call
      ~location
      ~sink_handle
      ~source_tree
      ~sink_tree
      ~callee
      ~port
    =
    let new_candidates =
      compute_triggered_flows
        ~pyre_in_context
        ~taint_configuration
        ~triggered_sinks_for_call
        ~sink_handle
        ~location
        ~source_tree
        ~sink_tree
        ~callee
        ~port
    in
    List.iter new_candidates ~f:(add_candidate candidates)


  let generate_issues candidates ~taint_configuration ~define =
    let add_or_merge_issue issues_so_far new_issue =
      IssueHandle.SerializableMap.update
        new_issue.handle
        (function
          | Some existing_issue -> Some (join existing_issue new_issue)
          | None -> Some new_issue)
        issues_so_far
    in
    let accumulate ~key:_ ~data:candidate issues =
      let new_issues = generate_issues ~taint_configuration ~define candidate in
      List.fold new_issues ~init:issues ~f:add_or_merge_issue
    in
    Core.Hashtbl.fold candidates ~f:accumulate ~init:IssueHandle.SerializableMap.empty
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

let get_name_and_detailed_message
    ~taint_configuration:{ TaintConfiguration.Heap.rules; _ }
    { flow; handle = { code; _ }; _ }
  =
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) rules with
  | None -> failwith "issue with code that has no rule"
  | Some { name; message_format; transforms; _ } ->
      let sources =
        ForwardTaint.kinds flow.source_taint
        |> List.map ~f:Sources.discard_transforms
        |> List.dedup_and_sort ~compare:Sources.compare
        |> List.map ~f:Sources.show
        |> String.concat ~sep:", "
      in
      let sinks =
        BackwardTaint.kinds flow.sink_taint
        |> List.map ~f:Sinks.discard_transforms
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


let to_error
    ~taint_configuration:({ TaintConfiguration.Heap.rules; _ } as taint_configuration)
    ({ handle = { code; _ }; define; _ } as issue)
  =
  match List.find ~f:(fun { code = rule_code; _ } -> code = rule_code) rules with
  | None -> failwith "issue with code that has no rule"
  | Some _ ->
      let name, detail = get_name_and_detailed_message ~taint_configuration issue in
      let kind = { Error.name; messages = [detail]; code } in
      let location = canonical_location issue in
      Error.create ~location ~define ~kind


let to_json ~taint_configuration ~expand_overrides ~is_valid_callee ~resolve_module_path issue =
  let callable_name = Target.external_name issue.handle.callable in
  let _, message = get_name_and_detailed_message ~taint_configuration issue in
  let source_traces =
    ForwardTaint.to_json
      ~expand_overrides
      ~is_valid_callee
      ~trace_kind:(Some TraceKind.Source)
      ~resolve_module_path:(Some resolve_module_path)
      ~export_leaf_names:ExportLeafNames.Always
      issue.flow.source_taint
  in
  let sink_traces =
    BackwardTaint.to_json
      ~expand_overrides
      ~is_valid_callee
      ~trace_kind:(Some TraceKind.Sink)
      ~resolve_module_path:(Some resolve_module_path)
      ~export_leaf_names:ExportLeafNames.Always
      issue.flow.sink_taint
  in
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
  let traces : Yojson.Safe.t =
    `List
      [
        `Assoc ["name", `String "forward"; "roots", source_traces];
        `Assoc ["name", `String "backward"; "roots", sink_traces];
      ]
  in
  let filename_lookup qualifier =
    resolve_module_path qualifier >>= fun { RepositoryPath.filename; _ } -> filename
  in
  let {
    Location.WithPath.path;
    start = { line; column = start_column };
    stop = { column = stop_column; _ };
  }
    =
    canonical_location issue |> Location.WithModule.instantiate ~lookup:filename_lookup
  in
  let callable_line = Ast.(Location.line issue.define.location) in
  let sink_handle = IssueHandle.Sink.to_json issue.handle.sink in
  let master_handle = IssueHandle.master_handle issue.handle in
  `Assoc
    [
      "callable", `String callable_name;
      "callable_line", `Int callable_line;
      "code", `Int issue.handle.code;
      "line", `Int line;
      "start", `Int start_column;
      "end", `Int stop_column;
      "filename", `String path;
      "message", `String message;
      "traces", traces;
      "features", `List json_features;
      "sink_handle", sink_handle;
      "master_handle", `String master_handle;
    ]
