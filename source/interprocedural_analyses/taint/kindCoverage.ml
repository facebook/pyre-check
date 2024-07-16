(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* The kinds that are defined by a user, which are composed of sources, sinks, and transforms. They
   are used for computing the rule coverage. *)
module Sources = struct
  include Sources

  let rec from_source = function
    | Sources.Attach -> None
    | Sources.NamedSource _ as source -> Some source
    | Sources.ParametricSource { source_name; _ } -> Some (Sources.NamedSource source_name)
    | Sources.Transform _ as source -> from_source (Sources.discard_transforms source)
end

module Sinks = struct
  include Sinks

  let rec from_sink ~partial_sink_converter = function
    | Sinks.Attach -> None
    | Sinks.PartialSink partial_sink as sink ->
        Some
          (partial_sink_converter
          |> TaintConfiguration.PartialSinkConverter.to_triggered_sinks ~partial_sink
          (* Any triggered sink that has the same partial sink kind. *)
          |> Sinks.PartialSink.Triggered.Set.elements
          |> List.map ~f:(fun triggered -> Sinks.TriggeredPartialSink triggered)
          |> Set.of_list
          |> Set.add sink (* The partial sink itself is also covered. *))
    | Sinks.TriggeredPartialSink _ as sink -> Some (Set.singleton sink)
    | Sinks.LocalReturn -> None
    | Sinks.NamedSink _ as sink -> Some (Set.singleton sink)
    | Sinks.ParametricSink { sink_name; _ } -> Some (Set.singleton (Sinks.NamedSink sink_name))
    | Sinks.ParameterUpdate _ -> None
    | Sinks.AddFeatureToArgument -> None
    | Sinks.Transform _ as sink -> from_sink ~partial_sink_converter (Sinks.discard_transforms sink)
    | Sinks.ExtraTraceSink -> None


  let from_sinks ~partial_sink_converter sinks =
    List.fold
      ~f:(fun so_far sink ->
        match from_sink ~partial_sink_converter sink with
        | Some sinks -> Sinks.Set.union so_far sinks
        | None -> so_far)
      sinks
      ~init:Sinks.Set.empty
end

module Transforms = struct
  module Set = Data_structures.SerializableSet.Make (TaintTransform)

  let from_transform = function
    | (TaintTransform.Named _ | TaintTransform.TriggeredPartialSink _) as transform ->
        Some transform
    | TaintTransform.Sanitize _ ->
        (* Sanitizers are not used in rules, although they are internally treated as taint
           transforms. *)
        None


  let from_transforms transforms = transforms |> List.filter_map ~f:from_transform |> Set.of_list
end

type t = {
  sources: Sources.Set.t;
  sinks: Sinks.Set.t;
  transforms: Transforms.Set.t;
}
[@@deriving eq, show, compare, sexp, hash]

let empty =
  { sources = Sources.Set.empty; sinks = Sinks.Set.empty; transforms = Transforms.Set.empty }


let from_model
    ~partial_sink_converter
    {
      Model.forward = { generations };
      Model.backward = { taint_in_taint_out; sink_taint };
      Model.parameter_sources = { parameter_sources };
      Model.sanitizers = _;
      Model.modes = _;
    }
  =
  let collect_sinks =
    Domains.BackwardState.fold
      Domains.BackwardTaint.kind
      ~f:(fun sink so_far -> Sinks.Set.add sink so_far)
      ~init:Sinks.Set.empty
  in
  let collect_sources =
    Domains.ForwardState.fold
      Domains.ForwardTaint.kind
      ~f:(fun source so_far -> Sources.Set.add source so_far)
      ~init:Sources.Set.empty
  in
  let sources =
    Sources.Set.union (collect_sources generations) (collect_sources parameter_sources)
  in
  let sinks = Sinks.Set.union (collect_sinks taint_in_taint_out) (collect_sinks sink_taint) in
  let source_transforms =
    Sources.Set.fold
      (fun source so_far ->
        source
        |> Sources.get_non_sanitize_transforms
        |> Transforms.from_transforms
        |> Transforms.Set.union so_far)
      sources
      Transforms.Set.empty
  in
  let sink_transforms =
    Sinks.Set.fold
      (fun sink so_far ->
        sink
        |> Sinks.get_non_sanitize_transforms
        |> Transforms.from_transforms
        |> Transforms.Set.union so_far)
      sinks
      Transforms.Set.empty
  in
  {
    sources = sources |> Sources.Set.filter_map Sources.from_source;
    sinks = sinks |> Sinks.Set.elements |> Sinks.from_sinks ~partial_sink_converter;
    transforms = Transforms.Set.union source_transforms sink_transforms;
  }


let from_rule ~partial_sink_converter { Rule.sources; sinks; transforms; _ } =
  {
    sources = sources |> Sources.Set.of_list |> Sources.Set.filter_map Sources.from_source;
    sinks = Sinks.from_sinks ~partial_sink_converter sinks;
    transforms =
      (* Not consider transforms from sources or sinks, since those should not have transforms. *)
      Transforms.Set.of_list transforms;
  }


let intersect
    { sources = sources_left; sinks = sinks_left; transforms = transforms_left }
    { sources = sources_right; sinks = sinks_right; transforms = transforms_right }
  =
  {
    sources = Sources.Set.inter sources_left sources_right;
    sinks = Sinks.Set.inter sinks_left sinks_right;
    transforms = Transforms.Set.inter transforms_left transforms_right;
  }


let union
    { sources = sources_left; sinks = sinks_left; transforms = transforms_left }
    { sources = sources_right; sinks = sinks_right; transforms = transforms_right }
  =
  {
    sources = Sources.Set.union sources_left sources_right;
    sinks = Sinks.Set.union sinks_left sinks_right;
    transforms = Transforms.Set.union transforms_left transforms_right;
  }


let to_json { sources; sinks; transforms } =
  let sources_json =
    `List
      (sources |> Sources.Set.elements |> List.map ~f:(fun source -> `String (Sources.show source)))
  in
  let sinks_json =
    `List (sinks |> Sinks.Set.elements |> List.map ~f:(fun sink -> `String (Sinks.show sink)))
  in
  if Transforms.Set.is_empty transforms then
    `Assoc ["sources", sources_json; "sinks", sinks_json]
  else
    let transforms_json =
      `List
        (transforms
        |> Transforms.Set.elements
        |> List.map ~f:(fun transform -> `String (TaintTransform.show transform)))
    in
    `Assoc ["sources", sources_json; "sinks", sinks_json; "transforms", transforms_json]
