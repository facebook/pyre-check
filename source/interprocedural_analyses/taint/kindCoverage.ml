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

  let rec from_sink sink =
    match sink with
    | Sinks.Attach -> None
    | Sinks.PartialSink _ as sink -> Some (Set.singleton sink)
    | Sinks.LocalReturn -> None
    | Sinks.NamedSink _ as sink -> Some (Set.singleton sink)
    | Sinks.ParametricSink { sink_name; _ } -> Some (Set.singleton (Sinks.NamedSink sink_name))
    | Sinks.ParameterUpdate _ -> None
    | Sinks.AddFeatureToArgument -> None
    | Sinks.Transform _ as sink -> from_sink (Sinks.discard_transforms sink)
    | Sinks.ExtraTraceSink -> None


  let from_sinks sinks =
    List.fold
      ~f:(fun so_far sink ->
        match from_sink sink with
        | Some sinks -> Sinks.Set.union so_far sinks
        | None -> so_far)
      sinks
      ~init:Sinks.Set.empty
end

module NamedTransforms = struct
  module Set = Data_structures.SerializableSet.Make (TaintTransform)

  let from_transform = function
    | TaintTransform.Named _ as transform -> Some transform
    | TaintTransform.TriggeredPartialSink _
    | TaintTransform.Sanitize _ ->
        (* Sanitizers are not used in rules, although they are internally treated as taint
           transforms. *)
        None


  let from_transforms transforms = transforms |> List.filter_map ~f:from_transform |> Set.of_list
end

type t = {
  sources: Sources.Set.t;
  sinks: Sinks.Set.t;
  named_transforms: NamedTransforms.Set.t;
      (* The kind coverage only cares about `Named` transforms. `Sanitize` transforms are used
         internally for dropping taint. `TriggeredPartialSink` transforms are used to represent
         partial sinks that have a flow, which is irrelevant with the kind coverage. *)
}
[@@deriving equal, show, compare, sexp, hash]

let empty =
  {
    sources = Sources.Set.empty;
    sinks = Sinks.Set.empty;
    named_transforms = NamedTransforms.Set.empty;
  }


let from_model
    {
      Model.forward = { generations };
      Model.backward = { taint_in_taint_out; sink_taint };
      Model.parameter_sources = { parameter_sources };
      _;
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
  let source_named_transforms =
    Sources.Set.fold
      (fun source so_far ->
        source
        |> Sources.get_named_transforms
        |> NamedTransforms.from_transforms
        |> NamedTransforms.Set.union so_far)
      sources
      NamedTransforms.Set.empty
  in
  let sink_named_transforms =
    Sinks.Set.fold
      (fun sink so_far ->
        sink
        |> Sinks.get_named_transforms
        |> NamedTransforms.from_transforms
        |> NamedTransforms.Set.union so_far)
      sinks
      NamedTransforms.Set.empty
  in
  {
    sources = sources |> Sources.Set.filter_map Sources.from_source;
    sinks = sinks |> Sinks.Set.elements |> Sinks.from_sinks;
    named_transforms = NamedTransforms.Set.union source_named_transforms sink_named_transforms;
  }


let from_rule { Rule.sources; sinks; transforms; _ } =
  {
    sources = sources |> Sources.Set.of_list |> Sources.Set.filter_map Sources.from_source;
    sinks = Sinks.from_sinks sinks;
    named_transforms =
      (* Not consider transforms from sources or sinks, since those should not have transforms. *)
      NamedTransforms.from_transforms transforms;
  }


let intersect
    { sources = sources_left; sinks = sinks_left; named_transforms = named_transforms_left }
    { sources = sources_right; sinks = sinks_right; named_transforms = named_transforms_right }
  =
  {
    sources = Sources.Set.inter sources_left sources_right;
    sinks = Sinks.Set.inter sinks_left sinks_right;
    named_transforms = NamedTransforms.Set.inter named_transforms_left named_transforms_right;
  }


let union
    { sources = sources_left; sinks = sinks_left; named_transforms = named_transforms_left }
    { sources = sources_right; sinks = sinks_right; named_transforms = named_transforms_right }
  =
  {
    sources = Sources.Set.union sources_left sources_right;
    sinks = Sinks.Set.union sinks_left sinks_right;
    named_transforms = NamedTransforms.Set.union named_transforms_left named_transforms_right;
  }


let to_json { sources; sinks; named_transforms } =
  let sources_json =
    `List
      (sources |> Sources.Set.elements |> List.map ~f:(fun source -> `String (Sources.show source)))
  in
  let sinks_json =
    `List (sinks |> Sinks.Set.elements |> List.map ~f:(fun sink -> `String (Sinks.show sink)))
  in
  if NamedTransforms.Set.is_empty named_transforms then
    `Assoc ["sources", sources_json; "sinks", sinks_json]
  else
    let transforms_json =
      `List
        (named_transforms
        |> NamedTransforms.Set.elements
        |> List.map ~f:(fun transform -> `String (TaintTransform.show transform)))
    in
    `Assoc ["sources", sources_json; "sinks", sinks_json; "transforms", transforms_json]
