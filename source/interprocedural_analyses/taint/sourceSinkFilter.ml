(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* SourceSinkFilter: implements logic that dictates whether a source or sink
 * should be propagated or removed. This is used to implement the
 * `--source`/`--sink`/`--rule` command line options.
 *)

module MatchingSanitizeTransforms = struct
  type t = {
    transforms: SanitizeTransformSet.t;
    (* For a sink / source, if all of its potential matching sources / sinks are sanitized, can we
       remove the sink / source? If yes then `sanitizable = true`. This is an optimization to stop
       propagating taint early, and hence using `sanitizable = false` is always sound. *)
    sanitizable: bool;
  }
  [@@deriving show]

  let empty = { transforms = SanitizeTransformSet.empty; sanitizable = true }

  let unsanitizable = { transforms = SanitizeTransformSet.empty; sanitizable = false }

  let add_source { transforms; sanitizable } = function
    | Sources.NamedSource name
    | Sources.ParametricSource { source_name = name; _ } ->
        {
          transforms =
            SanitizeTransformSet.add_source (SanitizeTransform.Source.Named name) transforms;
          sanitizable;
        }
    | Sources.Transform { global; local; _ }
      when List.exists ~f:TaintTransform.is_named_transform global
           || List.exists ~f:TaintTransform.is_named_transform local ->
        (* Be sound. Since a rule could allow a sink to match some source after going through named
           transforms, we don't remove the sink even if all of its matching sources are
           sanitized. *)
        { transforms; sanitizable = false }
    | Sources.Transform _ -> { transforms; sanitizable }
    | Sources.Attach -> failwith "unexpected source in `matching_sources`"


  let add_sink { transforms; sanitizable } = function
    | Sinks.NamedSink name
    | Sinks.ParametricSink { sink_name = name; _ } ->
        {
          transforms = SanitizeTransformSet.add_sink (SanitizeTransform.Sink.Named name) transforms;
          sanitizable;
        }
    | Sinks.Transform { global; local; _ }
      when List.exists ~f:TaintTransform.is_named_transform global
           || List.exists ~f:TaintTransform.is_named_transform local ->
        (* Be sound. Since a rule could allow a source to match some sink after going through named
           transforms, we don't remove the source even if all of its matching sinks are
           sanitized. *)
        { transforms; sanitizable = false }
    | Sinks.PartialSink _ -> { transforms; sanitizable = false }
    | Sinks.Transform _ -> { transforms; sanitizable }
    | Sinks.LocalReturn
    | Sinks.ParameterUpdate _
    | Sinks.Attach
    | Sinks.AddFeatureToArgument
    | Sinks.ExtraTraceSink ->
        failwith "unexpected sink in `matching_sinks`"


  let from_sources sources = Sources.Set.fold (Core.Fn.flip add_source) sources empty

  let from_sinks sinks = Sinks.Set.fold (Core.Fn.flip add_sink) sinks empty
end

type t = {
  matching_sources: Sources.Set.t Sinks.Map.t;
  matching_sinks: Sinks.Set.t Sources.Map.t;
  possible_tito_transforms: TaintTransforms.Set.t;
  (* These are the same as `matching_sources`/`matching_sinks`, but storing
   * sources and sinks as `SanitizeTransform.t`. *)
  matching_source_sanitize_transforms: MatchingSanitizeTransforms.t Sinks.Map.t;
  matching_sink_sanitize_transforms: MatchingSanitizeTransforms.t Sources.Map.t;
  maximum_source_distances: int Sources.Map.t;
  maximum_sink_distances: int Sinks.Map.t;
}

(* Filters everything. *)
let all =
  {
    matching_sources = Sinks.Map.empty;
    matching_sinks = Sources.Map.empty;
    possible_tito_transforms = TaintTransforms.Set.empty;
    matching_source_sanitize_transforms = Sinks.Map.empty;
    matching_sink_sanitize_transforms = Sources.Map.empty;
    maximum_source_distances = Sources.Map.empty;
    maximum_sink_distances = Sinks.Map.empty;
  }


let filter_rules ~filtered_rule_codes ~filtered_sources ~filtered_sinks ~filtered_transforms rules =
  let rules =
    match filtered_rule_codes with
    | Some rule_codes ->
        List.filter rules ~f:(fun { Rule.code; _ } -> Rule.CodeSet.mem code rule_codes)
    | None -> rules
  in
  let rules =
    match filtered_sources with
    | Some filtered_sources ->
        let should_keep_source = function
          | Sources.NamedSource name
          | Sources.ParametricSource { source_name = name; _ } ->
              Sources.Set.mem (Sources.NamedSource name) filtered_sources
          | _ -> true
        in
        List.filter_map rules ~f:(fun rule ->
            let rule_sources = List.filter ~f:should_keep_source rule.sources in
            if not (List.is_empty rule_sources) then
              Some { rule with sources = rule_sources }
            else
              None)
    | None -> rules
  in
  let rules =
    match filtered_sinks with
    | Some filtered_sinks ->
        let should_keep_sink = function
          | Sinks.NamedSink name
          | Sinks.ParametricSink { sink_name = name; _ } ->
              Sinks.Set.mem (Sinks.NamedSink name) filtered_sinks
          | _ -> true
        in
        List.filter_map rules ~f:(fun rule ->
            let rule_sinks = List.filter ~f:should_keep_sink rule.sinks in
            if not (List.is_empty rule_sinks) then
              Some { rule with sinks = rule_sinks }
            else
              None)
    | None -> rules
  in
  let rules =
    match filtered_transforms with
    | Some filtered_transforms ->
        let should_keep_transform = List.mem filtered_transforms ~equal:TaintTransform.equal in
        List.filter rules ~f:(fun rule -> List.for_all rule.transforms ~f:should_keep_transform)
    | None -> rules
  in
  rules


let matching_kinds_from_rules ~rules =
  let add_sources_sinks (matching_sources, matching_sinks) (sources, sinks) =
    let sinks_set = Sinks.Set.of_list sinks in
    let sources_set = Sources.Set.of_list sources in
    let update_matching_sources matching_sources sink =
      Sinks.Map.update
        sink
        (function
          | None -> Some sources_set
          | Some sources -> Some (Sources.Set.union sources sources_set))
        matching_sources
    in
    let update_matching_sinks matching_sinks source =
      Sources.Map.update
        source
        (function
          | None -> Some sinks_set
          | Some sinks -> Some (Sinks.Set.union sinks sinks_set))
        matching_sinks
    in
    let matching_sources = List.fold ~f:update_matching_sources ~init:matching_sources sinks in
    let matching_sinks = List.fold ~f:update_matching_sinks ~init:matching_sinks sources in
    matching_sources, matching_sinks
  in
  let add_combined_source_rule sofar { Rule.sources; sinks; transforms; _ } =
    match transforms with
    | [TaintTransform.TriggeredPartialSink _] ->
        (* In combined source rules, sources can directly match the partial sinks. *)
        add_sources_sinks sofar (sources, sinks)
    | _ -> sofar
  in
  let add_rule sofar ({ Rule.sources; sinks; transforms; _ } as rule) =
    let update sofar (source_transforms, sink_transforms) =
      let sources =
        if List.is_empty source_transforms then
          sources
        else
          List.map sources ~f:(fun base ->
              Sources.make_transform ~local:TaintTransforms.empty ~global:source_transforms ~base)
      in
      let sinks =
        if List.is_empty sink_transforms then
          sinks
        else
          List.map sinks ~f:(fun base ->
              Sinks.make_transform ~local:TaintTransforms.empty ~global:sink_transforms ~base)
      in
      add_sources_sinks sofar (sources, sinks)
    in
    let sofar = add_combined_source_rule sofar rule in
    transforms |> Rule.transform_splits |> List.fold ~init:sofar ~f:update
  in
  List.fold ~f:add_rule ~init:(Sinks.Map.empty, Sources.Map.empty) rules


(* For a TITO to extend to an actual issue, the transforms in it must be a substring (contiguous
   subsequence) of transforms appearing in a rule. In addition to optimization, this is used for
   ensuring termination. We do not consider arbitrarily long transform sequences in the analysis. *)
let possible_tito_transforms_from_rules ~rules =
  let rec suffixes l = l :: Option.value_map (List.tl l) ~default:[] ~f:suffixes in
  let prefixes l = List.rev l |> suffixes |> List.map ~f:List.rev in
  let substrings l = List.concat_map (prefixes l) ~f:suffixes in
  rules
  |> List.concat_map ~f:(fun { Rule.transforms; _ } -> substrings transforms)
  |> List.map ~f:TaintTransforms.get_named_transforms
  (* Transform `TriggeredPartialSink` can't be tito transforms. *)
  |> List.map ~f:TaintTransforms.of_named_transforms
  |> TaintTransforms.Set.of_list


let add_extra_trace_sinks possible_tito_transforms matching_source_sanitize_transforms =
  let add_sink tito_transform so_far =
    let sink = Sinks.make_transform ~local:[] ~global:tito_transform ~base:Sinks.ExtraTraceSink in
    Sinks.Map.add sink MatchingSanitizeTransforms.unsanitizable so_far
  in
  TaintTransforms.Set.fold add_sink possible_tito_transforms matching_source_sanitize_transforms


module MaximumDistance = struct
  type t =
    | Unlimited
    | Limited of int

  let to_option = function
    | Unlimited -> None
    | Limited value -> Some value
end

let maximum_kind_distance_from_rules ~rules =
  let add_rule
      (maximum_source_distances, maximum_sink_distances)
      { Rule.sources; sinks; filters; _ }
    =
    let update_source maximum_source_distances source =
      match filters with
      | Some { maximum_source_distance = Some maximum_source_distance; _ } ->
          Sources.Map.update
            source
            (function
              | None -> Some (MaximumDistance.Limited maximum_source_distance)
              | Some MaximumDistance.Unlimited -> Some MaximumDistance.Unlimited
              | Some (MaximumDistance.Limited existing_distance) ->
                  Some (MaximumDistance.Limited (Int.max existing_distance maximum_source_distance)))
            maximum_source_distances
      | _ ->
          Sources.Map.update
            source
            (fun _ -> Some MaximumDistance.Unlimited)
            maximum_source_distances
    in
    let update_sink maximum_sink_distances sink =
      match filters with
      | Some { maximum_sink_distance = Some maximum_sink_distance; _ } ->
          Sinks.Map.update
            sink
            (function
              | None -> Some (MaximumDistance.Limited maximum_sink_distance)
              | Some MaximumDistance.Unlimited -> Some MaximumDistance.Unlimited
              | Some (MaximumDistance.Limited existing_distance) ->
                  Some (MaximumDistance.Limited (Int.max existing_distance maximum_sink_distance)))
            maximum_sink_distances
      | _ -> Sinks.Map.update sink (fun _ -> Some MaximumDistance.Unlimited) maximum_sink_distances
    in
    (* TODO: handle transforms *)
    let maximum_source_distances =
      List.fold ~init:maximum_source_distances ~f:update_source sources
    in
    let maximum_sink_distances = List.fold ~init:maximum_sink_distances ~f:update_sink sinks in
    maximum_source_distances, maximum_sink_distances
  in
  let maximum_source_distances, maximum_sink_distances =
    List.fold ~f:add_rule ~init:(Sources.Map.empty, Sinks.Map.empty) rules
  in
  let maximum_source_distances =
    Sources.Map.filter_map (fun _ -> MaximumDistance.to_option) maximum_source_distances
  in
  let maximum_sink_distances =
    Sinks.Map.filter_map (fun _ -> MaximumDistance.to_option) maximum_sink_distances
  in
  maximum_source_distances, maximum_sink_distances


let create ~rules ~filtered_rule_codes ~filtered_sources ~filtered_sinks ~filtered_transforms =
  let rules =
    filter_rules ~filtered_rule_codes ~filtered_sources ~filtered_sinks ~filtered_transforms rules
  in
  let matching_sources, matching_sinks = matching_kinds_from_rules ~rules in
  let possible_tito_transforms = possible_tito_transforms_from_rules ~rules in
  let matching_source_sanitize_transforms =
    matching_sources
    |> Sinks.Map.map MatchingSanitizeTransforms.from_sources
    (* To prevent the special sink `ExtraTraceSink` from being removed, it should have a record in
       the map. *)
    |> add_extra_trace_sinks possible_tito_transforms
  in
  let matching_sink_sanitize_transforms =
    Sources.Map.map MatchingSanitizeTransforms.from_sinks matching_sinks
  in
  let maximum_source_distances, maximum_sink_distances = maximum_kind_distance_from_rules ~rules in
  {
    matching_sources;
    matching_sinks;
    possible_tito_transforms;
    matching_source_sanitize_transforms;
    matching_sink_sanitize_transforms;
    maximum_source_distances;
    maximum_sink_distances;
  }


let matching_source_sanitize_transforms
    { matching_source_sanitize_transforms; _ }
    ~non_sanitize_transforms
    ~base
  =
  let base = Sinks.discard_subkind base in
  let sink =
    Sinks.make_transform ~local:TaintTransforms.empty ~global:non_sanitize_transforms ~base
  in
  Sinks.Map.find_opt sink matching_source_sanitize_transforms


let matching_sink_sanitize_transforms
    { matching_sink_sanitize_transforms; _ }
    ~non_sanitize_transforms
    ~base
  =
  let base = Sources.discard_subkind base in
  let source =
    Sources.make_transform ~local:TaintTransforms.empty ~global:non_sanitize_transforms ~base
  in
  Sources.Map.find_opt source matching_sink_sanitize_transforms


let should_keep_source filter source =
  match source with
  | Sources.Attach -> true
  | Sources.NamedSource _
  | Sources.ParametricSource _ ->
      matching_sink_sanitize_transforms filter ~non_sanitize_transforms:[] ~base:source
      |> Option.is_some
  | Sources.Transform { local; global; base } -> (
      let transforms = TaintTransforms.merge ~local ~global in
      let non_sanitize_transforms = TaintTransforms.get_non_sanitize_transforms transforms in
      match matching_sink_sanitize_transforms filter ~non_sanitize_transforms ~base with
      | None -> false
      | Some { sanitizable = false; _ } -> true
      | Some { transforms = matching_sinks; sanitizable = true } ->
          not
            (SanitizeTransformSet.less_or_equal
               ~left:matching_sinks
               ~right:(TaintTransforms.get_sanitize_transforms transforms)))


let should_keep_sink ({ possible_tito_transforms; _ } as filter) sink =
  match sink with
  | Sinks.Attach
  | Sinks.AddFeatureToArgument ->
      true
  | Sinks.NamedSink _
  | Sinks.ParametricSink _
  | Sinks.PartialSink _
  | Sinks.ExtraTraceSink ->
      matching_source_sanitize_transforms filter ~non_sanitize_transforms:[] ~base:sink
      |> Option.is_some
  | Sinks.LocalReturn
  | Sinks.ParameterUpdate _ ->
      true
  | Sinks.Transform { local; global; base = LocalReturn }
  | Sinks.Transform { local; global; base = ParameterUpdate _ } ->
      let transforms =
        TaintTransforms.merge ~local ~global |> TaintTransforms.get_non_sanitize_transforms
      in
      TaintTransforms.Set.mem transforms possible_tito_transforms
  | Sinks.Transform { local; global; base } -> (
      let transforms = TaintTransforms.merge ~local ~global in
      let non_sanitize_transforms = TaintTransforms.get_non_sanitize_transforms transforms in
      match matching_source_sanitize_transforms filter ~non_sanitize_transforms ~base with
      | None -> false
      | Some { sanitizable = false; _ } -> true
      | Some { transforms = matching_sources; sanitizable = true } ->
          not
            (SanitizeTransformSet.less_or_equal
               ~left:matching_sources
               ~right:(TaintTransforms.get_sanitize_transforms transforms)))


let matching_sources { matching_sources; _ } = matching_sources

let matching_sinks { matching_sinks; _ } = matching_sinks

let possible_tito_transforms { possible_tito_transforms; _ } = possible_tito_transforms

let maximum_source_distance { maximum_source_distances; _ } source =
  Sources.Map.find_opt
    (source |> Sources.discard_sanitize_transforms |> Sources.discard_subkind)
    maximum_source_distances


let maximum_sink_distance { maximum_sink_distances; _ } sink =
  Sinks.Map.find_opt
    (sink |> Sinks.discard_sanitize_transforms |> Sinks.discard_subkind)
    maximum_sink_distances
