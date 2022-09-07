(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
module Json = Yojson.Safe

module Rule = struct
  type t = {
    sources: Sources.t list;
    sinks: Sinks.t list;
    transforms: TaintTransform.t list;
    code: int;
    name: string;
    message_format: string; (* format *)
  }
  [@@deriving compare, show]
end

type literal_string_sink = {
  pattern: Re2.t;
  sink_kind: Sinks.t;
}

type implicit_sinks = {
  conditional_test: Sinks.t list;
  literal_string_sinks: literal_string_sink list;
}

let empty_implicit_sinks = { conditional_test = []; literal_string_sinks = [] }

type literal_string_source = {
  pattern: Re2.t;
  source_kind: Sources.t;
}

type implicit_sources = { literal_strings: literal_string_source list }

let empty_implicit_sources = { literal_strings = [] }

type analysis_model_constraints = {
  maximum_model_width: int;
  maximum_return_access_path_length: int;
  maximum_overrides_to_analyze: int option;
  maximum_trace_length: int option;
  maximum_tito_depth: int option;
}

let default_analysis_model_constraints =
  {
    maximum_model_width = 25;
    maximum_return_access_path_length = 10;
    maximum_overrides_to_analyze = None;
    maximum_trace_length = None;
    maximum_tito_depth = None;
  }


type partial_sink_converter = (Sources.t list * Sinks.t) list String.Map.Tree.t

(* Given a rule to find flows of the form:
 *   source -> T1 -> T2 -> T3 -> ... -> Tn -> sink
 * Following are different ways we can find matching flows:
 *   source -> T1:T2:T3:...:Tn:sink
 *   T1:source -> T2:T3:...:Tn:sink
 *   T2:T1:source -> T3:...:Tn:sink
 *   ...
 *   Tn:...:T3:T2:T1:source -> sink
 *)
let transform_splits transforms =
  let rec split ~result ~prefix ~suffix =
    let result = (prefix, suffix) :: result in
    match suffix with
    | [] -> result
    | next :: suffix -> split ~result ~prefix:(next :: prefix) ~suffix
  in
  split ~result:[] ~prefix:[] ~suffix:transforms


module IntSet = Stdlib.Set.Make (Int)

let filter_rules ~filtered_rule_codes ~filtered_sources ~filtered_sinks ~filtered_transforms rules =
  let rules =
    match filtered_rule_codes with
    | Some rule_codes -> List.filter rules ~f:(fun { Rule.code; _ } -> IntSet.mem code rule_codes)
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


module SourceSinkFilter = struct
  type t = {
    matching_sources: Sources.Set.t Sinks.Map.t;
    matching_sinks: Sinks.Set.t Sources.Map.t;
    possible_tito_transforms: TaintTransforms.Set.t;
  }

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
    let add_rule sofar { Rule.sources; sinks; transforms; _ } =
      let update sofar (source_transforms, sink_transforms) =
        let sources =
          if List.is_empty source_transforms then
            sources
          else
            List.map sources ~f:(fun base ->
                Sources.Transform
                  {
                    base;
                    global = TaintTransforms.of_named_transforms source_transforms;
                    local = TaintTransforms.empty;
                  })
        in
        let sinks =
          if List.is_empty sink_transforms then
            sinks
          else
            List.map sinks ~f:(fun base ->
                Sinks.Transform
                  {
                    base;
                    global = TaintTransforms.of_named_transforms sink_transforms;
                    local = TaintTransforms.empty;
                  })
        in
        add_sources_sinks sofar (sources, sinks)
      in
      transform_splits transforms |> List.fold ~init:sofar ~f:update
    in
    List.fold ~f:add_rule ~init:(Sinks.Map.empty, Sources.Map.empty) rules


  (* For a TITO to extend to an actual issue, the transforms in it must be a substring (contiguous
     subsequence) of transforms appearing in a rule. In addition to optimization, this is used for
     ensuring termination. We do not consider arbitrarily long transform sequences in the analysis. *)
  let possible_tito_transforms_from_rules ~rules =
    let rec suffixes l = l :: Option.value_map (List.tl l) ~default:[] ~f:suffixes in
    let prefixes l = List.rev l |> suffixes |> List.map ~f:List.rev in
    let substrings l = List.concat_map (prefixes l) ~f:suffixes in
    List.concat_map rules ~f:(fun { Rule.transforms; _ } -> substrings transforms)
    |> List.map ~f:TaintTransforms.of_named_transforms
    |> TaintTransforms.Set.of_list


  let create ~rules ~filtered_rule_codes ~filtered_sources ~filtered_sinks ~filtered_transforms =
    let rules =
      filter_rules ~filtered_rule_codes ~filtered_sources ~filtered_sinks ~filtered_transforms rules
    in
    let matching_sources, matching_sinks = matching_kinds_from_rules ~rules in
    let possible_tito_transforms = possible_tito_transforms_from_rules ~rules in
    { matching_sources; matching_sinks; possible_tito_transforms }


  let should_keep_source { matching_sinks; _ } = function
    | Sources.Transform { local; global; base = NamedSource name }
    | Sources.Transform { local; global; base = ParametricSource { source_name = name; _ } } -> (
        let transforms = TaintTransforms.merge ~local ~global in
        let source =
          match TaintTransforms.get_named_transforms transforms with
          | [] -> Sources.NamedSource name
          | named_transforms ->
              Sources.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms named_transforms;
                  base = Sources.NamedSource name;
                }
        in
        match Sources.Map.find_opt source matching_sinks with
        | None -> false
        | Some sinks ->
            TaintTransforms.get_sanitize_transforms transforms
            |> (fun { sinks; _ } -> sinks)
            |> Sinks.extract_sanitized_sinks_from_transforms
            |> Sinks.Set.diff sinks
            |> Sinks.Set.is_empty
            |> not)
    | Sources.NamedSource name
    | Sources.ParametricSource { source_name = name; _ } ->
        Sources.Map.mem (Sources.NamedSource name) matching_sinks
    | _ -> true


  let should_keep_sink { matching_sources; possible_tito_transforms; _ } = function
    | Sinks.Transform { local; global; base = NamedSink name }
    | Sinks.Transform { local; global; base = ParametricSink { sink_name = name; _ } } -> (
        let transforms = TaintTransforms.merge ~local ~global in
        let sink =
          match TaintTransforms.get_named_transforms transforms with
          | [] -> Sinks.NamedSink name
          | named_transforms ->
              Sinks.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms named_transforms;
                  base = Sinks.NamedSink name;
                }
        in
        match Sinks.Map.find_opt sink matching_sources with
        | None -> false
        | Some sources ->
            TaintTransforms.get_sanitize_transforms transforms
            |> (fun { sources; _ } -> sources)
            |> Sources.extract_sanitized_sources_from_transforms
            |> Sources.Set.diff sources
            |> Sources.Set.is_empty
            |> not)
    | Sinks.Transform { local; global; base = LocalReturn }
    | Sinks.Transform { local; global; base = ParameterUpdate _ } ->
        let transforms =
          TaintTransforms.merge ~local ~global |> TaintTransforms.discard_sanitize_transforms
        in
        TaintTransforms.Set.mem transforms possible_tito_transforms
    | Sinks.NamedSink name
    | Sinks.ParametricSink { sink_name = name; _ } ->
        Sinks.Map.mem (Sinks.NamedSink name) matching_sources
    | _ -> true


  let matching_sources { matching_sources; _ } = matching_sources

  let matching_sinks { matching_sinks; _ } = matching_sinks

  let possible_tito_transforms { possible_tito_transforms; _ } = possible_tito_transforms
end

let filter_implicit_sources ~source_sink_filter { literal_strings } =
  {
    literal_strings =
      List.filter literal_strings ~f:(fun { source_kind; _ } ->
          SourceSinkFilter.should_keep_source source_sink_filter source_kind);
  }


let filter_implicit_sinks ~source_sink_filter { conditional_test; literal_string_sinks } =
  {
    conditional_test =
      List.filter conditional_test ~f:(SourceSinkFilter.should_keep_sink source_sink_filter);
    literal_string_sinks =
      List.filter literal_string_sinks ~f:(fun { sink_kind; _ } ->
          SourceSinkFilter.should_keep_sink source_sink_filter sink_kind);
  }


type t = {
  sources: AnnotationParser.source_or_sink list;
  sinks: AnnotationParser.source_or_sink list;
  transforms: TaintTransform.t list;
  filtered_sources: Sources.Set.t option;
  filtered_sinks: Sinks.Set.t option;
  filtered_transforms: TaintTransform.t list option;
  features: string list;
  rules: Rule.t list;
  filtered_rule_codes: IntSet.t option;
  implicit_sinks: implicit_sinks;
  implicit_sources: implicit_sources;
  partial_sink_converter: partial_sink_converter;
  partial_sink_labels: string list String.Map.Tree.t;
  find_missing_flows: Configuration.MissingFlowKind.t option;
  dump_model_query_results_path: PyrePath.t option;
  analysis_model_constraints: analysis_model_constraints;
  lineage_analysis: bool;
  source_sink_filter: SourceSinkFilter.t option;
}

let empty =
  {
    sources = [];
    sinks = [];
    transforms = [];
    filtered_sources = None;
    filtered_sinks = None;
    filtered_transforms = None;
    features = [];
    rules = [];
    filtered_rule_codes = None;
    partial_sink_converter = String.Map.Tree.empty;
    implicit_sinks = empty_implicit_sinks;
    implicit_sources = empty_implicit_sources;
    partial_sink_labels = String.Map.Tree.empty;
    find_missing_flows = None;
    dump_model_query_results_path = None;
    analysis_model_constraints = default_analysis_model_constraints;
    lineage_analysis = false;
    source_sink_filter = None;
  }


module ConfigurationSharedMemory =
  Memory.WithCache.Make
    (Memory.SingletonKey)
    (struct
      type nonrec t = t

      let prefix = Prefix.make ()

      let description = "Taint configuration"
    end)

module Error = struct
  type kind =
    | FileNotFound
    | FileRead
    | InvalidJson of string
    | NoConfigurationFound
    | UnexpectedJsonType of {
        json: Json.t;
        message: string;
        section: string option;
      }
    | MissingKey of {
        key: string;
        section: string;
      }
    | UnknownKey of {
        key: string;
        section: string;
      }
    | UnsupportedSource of string
    | UnsupportedSink of string
    | UnsupportedTransform of string
    | UnexpectedCombinedSourceRule of Json.t
    | PartialSinkDuplicate of string
    | InvalidLabelMultiSink of {
        label: string;
        sink: string;
        labels: string list;
      }
    | InvalidMultiSink of string
    | RuleCodeDuplicate of int
    | OptionDuplicate of string
    | SourceDuplicate of string
    | SinkDuplicate of string
    | TransformDuplicate of string
    | FeatureDuplicate of string
  [@@deriving equal]

  type t = {
    kind: kind;
    path: PyrePath.t option;
  }
  [@@deriving equal]

  let create ~path ~kind = { kind; path = Some path }

  let pp_kind formatter = function
    | FileNotFound -> Format.fprintf formatter "File not found"
    | FileRead -> Format.fprintf formatter "Could not read file"
    | InvalidJson error -> Format.fprintf formatter "%s" error
    | NoConfigurationFound ->
        Format.fprintf formatter "No `.config` was found in the taint directories"
    | UnexpectedJsonType { json; message; section } ->
        let json =
          match json with
          | `Null -> ""
          | _ -> Format.sprintf ": `%s`" (Json.to_string json)
        in
        let section =
          match section with
          | Some section -> Format.sprintf " for section `%s`" section
          | None -> ""
        in
        Format.fprintf formatter "%s%s%s" message json section
    | MissingKey { key; section } ->
        Format.fprintf formatter "Required key `%s` is missing in section `%s`" key section
    | UnknownKey { key; section } ->
        Format.fprintf formatter "Unknown key `%s` encountered in section `%s`" key section
    | UnsupportedSource source -> Format.fprintf formatter "Unsupported taint source `%s`" source
    | UnsupportedSink sink -> Format.fprintf formatter "Unsupported taint sink `%s`" sink
    | UnsupportedTransform transform ->
        Format.fprintf formatter "Unsupported taint transform `%s`" transform
    | UnexpectedCombinedSourceRule json ->
        Format.fprintf
          formatter
          "Combined source rules must be of the form {\"a\": [\"SourceA\"], \"b\": [\"SourceB\"]}, \
           got `%s`"
          (Json.to_string json)
    | PartialSinkDuplicate partial_sink ->
        Format.fprintf
          formatter
          "Partial sinks must be unique - an entry for `%s` already exists"
          partial_sink
    | InvalidLabelMultiSink { label; sink; labels } ->
        Format.fprintf
          formatter
          "`%s` is an invalid label For multi sink `%s` (choices: `%s`)"
          label
          sink
          (String.concat labels ~sep:", ")
    | InvalidMultiSink sink -> Format.fprintf formatter "`%s` is not a multi sink" sink
    | RuleCodeDuplicate code ->
        Format.fprintf formatter "Multiple rules share the same code `%d`" code
    | OptionDuplicate name ->
        Format.fprintf formatter "Multiple values were passed in for option `%s`" name
    | SourceDuplicate name -> Format.fprintf formatter "Duplicate entry for source: `%s`" name
    | SinkDuplicate name -> Format.fprintf formatter "Duplicate entry for sink: `%s`" name
    | TransformDuplicate name -> Format.fprintf formatter "Duplicate entry for transform: `%s`" name
    | FeatureDuplicate name -> Format.fprintf formatter "Duplicate entry for feature: `%s`" name


  let code = function
    | FileNotFound -> 1
    | FileRead -> 2
    | InvalidJson _ -> 3
    | NoConfigurationFound -> 4
    | UnexpectedJsonType _ -> 5
    | MissingKey _ -> 6
    | UnknownKey _ -> 7
    | UnsupportedSource _ -> 8
    | UnsupportedSink _ -> 9
    | UnexpectedCombinedSourceRule _ -> 10
    | PartialSinkDuplicate _ -> 11
    | InvalidLabelMultiSink _ -> 12
    | InvalidMultiSink _ -> 13
    | RuleCodeDuplicate _ -> 14
    | OptionDuplicate _ -> 15
    | SourceDuplicate _ -> 16
    | SinkDuplicate _ -> 16
    | FeatureDuplicate _ -> 18
    | UnsupportedTransform _ -> 19
    | TransformDuplicate _ -> 20


  let show_kind = Format.asprintf "%a" pp_kind

  let pp formatter = function
    | { path = None; kind } -> pp_kind formatter kind
    | { path = Some path; kind } -> Format.fprintf formatter "%a: %a" PyrePath.pp path pp_kind kind


  let show = Format.asprintf "%a" pp

  let to_json { path; kind } =
    let path =
      match path with
      | None -> `Null
      | Some path -> `String (PyrePath.absolute path)
    in
    `Assoc ["description", `String (show_kind kind); "path", path; "code", `Int (code kind)]
end

module PartialSinkConverter = struct
  let mangle { Sinks.kind; label } = Format.sprintf "%s$%s" kind label

  let add map ~first_sources ~first_sinks ~second_sources ~second_sinks =
    let add map (first_sink, second_sink) =
      (* Trigger second sink when the first sink matches a source, and vice versa. *)
      String.Map.Tree.add_multi
        map
        ~key:(mangle first_sink)
        ~data:(first_sources, Sinks.TriggeredPartialSink second_sink)
      |> String.Map.Tree.add_multi
           ~key:(mangle second_sink)
           ~data:(second_sources, Sinks.TriggeredPartialSink first_sink)
    in
    List.cartesian_product first_sinks second_sinks |> List.fold ~f:add ~init:map


  let merge left right =
    String.Map.Tree.merge
      ~f:
        (fun ~key:_ -> function
          | `Left value
          | `Right value ->
              Some value
          | `Both (left, right) -> Some (left @ right))
      left
      right


  let get_triggered_sink sink_to_sources ~partial_sink ~source =
    match mangle partial_sink |> String.Map.Tree.find sink_to_sources with
    | Some source_and_sink_list ->
        List.find source_and_sink_list ~f:(fun (supported_sources, _) ->
            List.exists supported_sources ~f:(Sources.equal source))
        >>| snd
    | _ -> None
end

let from_json_list source_json_list =
  let open Result in
  let json_exception_to_error ~path ?section f =
    try f () with
    | Json.Util.Type_error (message, json)
    | Json.Util.Undefined (message, json) ->
        Error [Error.create ~path ~kind:(Error.UnexpectedJsonType { json; message; section })]
  in
  let json_bool_member ~path key value ~default =
    json_exception_to_error ~path ~section:key (fun () ->
        Json.Util.member key value
        |> Yojson.Safe.Util.to_bool_option
        |> Option.value ~default
        |> Result.return)
  in
  let json_string_member ~path key value =
    json_exception_to_error ~path ~section:key (fun () ->
        Json.Util.member key value |> Json.Util.to_string |> Result.return)
  in
  let json_integer_member ~path key value =
    json_exception_to_error ~path ~section:key (fun () ->
        Json.Util.member key value |> Json.Util.to_int |> Result.return)
  in
  let member name json =
    try Json.Util.member name json with
    | Not_found -> `Null
  in
  let array_member ~path ?section name json =
    match member name json with
    | `Null -> Ok []
    | json ->
        json_exception_to_error ~path ?section (fun () -> Json.Util.to_list json |> Result.return)
  in
  let json_string_list ~path ?section json =
    json_exception_to_error ~path ?section (fun () ->
        Json.Util.to_list json |> List.map ~f:Json.Util.to_string |> Result.return)
  in
  let parse_kind ~path ?section json =
    match member "kind" json with
    | `Null -> Ok AnnotationParser.Named
    | `String "parametric" -> Ok AnnotationParser.Parametric
    | json ->
        Error
          [
            Error.create
              ~path
              ~kind:(Error.UnexpectedJsonType { json; message = "Unexpected kind"; section });
          ]
  in
  let check_keys ~path ~section ~required_keys ~valid_keys ~current_keys =
    let valid_keys_hash_set = String.Hash_set.of_list valid_keys in
    let current_keys_hash_set = String.Hash_set.of_list current_keys in
    let check_required_key_present key =
      if not (Hash_set.mem current_keys_hash_set key) then
        Error (Error.create ~path ~kind:(Error.MissingKey { key; section }))
      else
        Ok ()
    in
    let check_key_is_valid key =
      if not (Hash_set.mem valid_keys_hash_set key) then
        Error (Error.create ~path ~kind:(Error.UnknownKey { key; section }))
      else
        Ok ()
    in
    List.map current_keys ~f:check_key_is_valid
    |> List.rev_append (List.map required_keys ~f:check_required_key_present)
    |> Result.combine_errors_unit
  in
  let parse_source_or_sink_annotation ~path ~section json =
    check_keys
      ~path
      ~section
      ~required_keys:["name"]
      ~current_keys:(Json.Util.keys json)
      ~valid_keys:["name"; "kind"; "comment"]
    >>= fun () ->
    json_string_member ~path "name" json
    >>= fun name -> parse_kind ~path json >>| fun kind -> { AnnotationParser.name; kind }
  in
  let parse_source_annotations (path, json) =
    array_member ~path "sources" json
    >>= fun json ->
    List.map ~f:(parse_source_or_sink_annotation ~path ~section:"sources") json
    |> Result.combine_errors
    |> Result.map_error ~f:List.concat
  in
  let parse_sink_annotations (path, json) =
    array_member ~path "sinks" json
    >>= fun json ->
    List.map ~f:(parse_source_or_sink_annotation ~path ~section:"sinks") json
    |> Result.combine_errors
    |> Result.map_error ~f:List.concat
  in
  let parse_transform ~path ~section json =
    check_keys
      ~path
      ~section
      ~required_keys:["name"]
      ~current_keys:(Json.Util.keys json)
      ~valid_keys:["name"; "comment"]
    >>= fun () ->
    json_string_member ~path "name" json >>= fun name -> Ok (TaintTransform.Named name)
  in
  let parse_transforms (path, json) =
    array_member ~path "transforms" json
    >>= fun json ->
    List.map ~f:(parse_transform ~path ~section:"transforms") json
    |> Result.combine_errors
    |> Result.map_error ~f:List.concat
  in
  let parse_features (path, json) =
    array_member ~path "features" json
    >>= fun json ->
    List.map ~f:(json_string_member ~path "name") json
    |> Result.combine_errors
    |> Result.map_error ~f:List.concat
  in
  let seen_rules = Int.Hash_set.create () in
  let validate_code_uniqueness ~path code =
    if Hash_set.mem seen_rules code then
      Error [Error.create ~path ~kind:(Error.RuleCodeDuplicate code)]
    else (
      Hash_set.add seen_rules code;
      Ok ())
  in
  let parse_source_reference ~path ~allowed_sources source =
    AnnotationParser.parse_source ~allowed:allowed_sources source
    |> Result.map_error ~f:(fun _ -> Error.create ~path ~kind:(Error.UnsupportedSource source))
  in
  let parse_sink_reference ~path ~allowed_sinks sink =
    AnnotationParser.parse_sink ~allowed:allowed_sinks sink
    |> Result.map_error ~f:(fun _ -> Error.create ~path ~kind:(Error.UnsupportedSink sink))
  in
  let parse_transform_reference ~path ~allowed_transforms transform =
    AnnotationParser.parse_transform ~allowed:allowed_transforms transform
    |> Result.map_error ~f:(fun _ ->
           Error.create ~path ~kind:(Error.UnsupportedTransform transform))
  in
  let parse_rules ~allowed_sources ~allowed_sinks ~allowed_transforms (path, json) =
    let parse_rule json =
      let required_keys = ["name"; "code"; "sources"; "sinks"; "message_format"] in
      let valid_keys = "oncall" :: "comment" :: "transforms" :: required_keys in
      check_keys
        ~path
        ~section:"rules"
        ~required_keys
        ~valid_keys
        ~current_keys:(Json.Util.keys json)
      >>= fun () ->
      Json.Util.member "sources" json
      |> json_string_list ~path ~section:"rules"
      >>= fun sources ->
      List.map ~f:(parse_source_reference ~path ~allowed_sources) sources
      |> Result.combine_errors
      >>= fun sources ->
      Json.Util.member "sinks" json
      |> json_string_list ~path ~section:"rules"
      >>= fun sinks ->
      List.map ~f:(parse_sink_reference ~path ~allowed_sinks) sinks
      |> Result.combine_errors
      >>= fun sinks ->
      (match member "transforms" json with
      | `Null -> Ok []
      | transforms -> json_string_list ~path ~section:"rules" transforms)
      >>= fun transforms ->
      List.map ~f:(parse_transform_reference ~path ~allowed_transforms) transforms
      |> Result.combine_errors
      >>= fun transforms ->
      json_string_member ~path "name" json
      >>= fun name ->
      json_string_member ~path "message_format" json
      >>= fun message_format ->
      json_integer_member ~path "code" json
      >>= fun code ->
      validate_code_uniqueness ~path code
      >>| fun () -> { Rule.sources; sinks; transforms; name; code; message_format }
    in
    array_member ~path "rules" json
    >>= fun rules ->
    List.map ~f:parse_rule rules |> Result.combine_errors |> Result.map_error ~f:List.concat
  in
  let parse_combined_source_rules ~allowed_sources (path, json) =
    let parse_combined_source_rule sofar json =
      sofar
      >>= fun (rules, partial_sink_converter, partial_sink_labels) ->
      json_string_member ~path "name" json
      >>= fun name ->
      json_string_member ~path "message_format" json
      >>= fun message_format ->
      json_integer_member ~path "code" json
      >>= fun code ->
      validate_code_uniqueness ~path code
      >>= fun () ->
      let sources = Json.Util.member "sources" json in
      let keys = Json.Util.keys sources in
      match keys with
      | [first; second] ->
          let parse_sources sources =
            (match sources with
            | `String source -> Ok [source]
            | `List _ -> json_string_list ~path sources
            | _ -> Error [Error.create ~path ~kind:(Error.UnexpectedCombinedSourceRule json)])
            >>= fun sources ->
            List.map ~f:(parse_source_reference ~path ~allowed_sources) sources
            |> Result.combine_errors
          in
          Json.Util.member first sources
          |> parse_sources
          >>= fun first_sources ->
          Json.Util.member second sources
          |> parse_sources
          >>= fun second_sources ->
          json_string_member ~path "partial_sink" json
          >>= fun partial_sink ->
          if String.Map.Tree.mem partial_sink_labels partial_sink then
            Error [Error.create ~path ~kind:(Error.PartialSinkDuplicate partial_sink)]
          else
            let partial_sink_labels =
              String.Map.Tree.set partial_sink_labels ~key:partial_sink ~data:[first; second]
            in
            let create_partial_sink label sink =
              match String.Map.Tree.find partial_sink_labels sink with
              | Some labels when not (List.mem ~equal:String.equal labels label) ->
                  Error
                    [Error.create ~path ~kind:(Error.InvalidLabelMultiSink { label; sink; labels })]
              | None -> Error [Error.create ~path ~kind:(Error.InvalidMultiSink sink)]
              | _ -> Ok { Sinks.kind = sink; label }
            in
            create_partial_sink first partial_sink
            >>= fun first_sink ->
            create_partial_sink second partial_sink
            >>| fun second_sink ->
            ( {
                Rule.sources = first_sources;
                sinks = [Sinks.TriggeredPartialSink first_sink];
                transforms = [];
                name;
                code;
                message_format;
              }
              ::
              {
                Rule.sources = second_sources;
                sinks = [Sinks.TriggeredPartialSink second_sink];
                transforms = [];
                name;
                code;
                message_format;
              }
              :: rules,
              PartialSinkConverter.add
                partial_sink_converter
                ~first_sources
                ~first_sinks:[first_sink]
                ~second_sources
                ~second_sinks:[second_sink],
              partial_sink_labels )
      | _ -> Error [Error.create ~path ~kind:(Error.UnexpectedCombinedSourceRule json)]
    in
    array_member ~path "combined_source_rules" json
    >>= List.fold
          ~init:(Ok ([], String.Map.Tree.empty, String.Map.Tree.empty))
          ~f:parse_combined_source_rule
  in
  let parse_implicit_sinks ~allowed_sinks (path, json) =
    match member "implicit_sinks" json with
    | `Null -> Ok empty_implicit_sinks
    | implicit_sinks ->
        check_keys
          ~path
          ~section:"implicit_sinks"
          ~required_keys:[]
          ~valid_keys:["conditional_test"; "literal_strings"]
          ~current_keys:(Json.Util.keys implicit_sinks)
        >>= fun () ->
        (match member "conditional_test" implicit_sinks with
        | `Null -> Ok []
        | conditional_test ->
            json_string_list ~path conditional_test
            >>= fun sinks ->
            List.map ~f:(parse_sink_reference ~path ~allowed_sinks) sinks |> Result.combine_errors)
        >>= fun conditional_test ->
        array_member ~path "literal_strings" implicit_sinks
        >>= fun literal_strings ->
        List.map
          ~f:(fun json ->
            json_string_member ~path "kind" json
            >>= fun sink ->
            parse_sink_reference ~path ~allowed_sinks sink
            |> Result.map_error ~f:(fun error -> [error])
            >>= fun sink_kind ->
            json_string_member ~path "regexp" json
            >>| fun pattern -> { sink_kind; pattern = Re2.create_exn pattern })
          literal_strings
        |> Result.combine_errors
        |> Result.map_error ~f:List.concat
        >>| fun literal_string_sinks -> { conditional_test; literal_string_sinks }
  in
  let parse_implicit_sources ~allowed_sources (path, json) =
    match member "implicit_sources" json with
    | `Null -> Ok { literal_strings = [] }
    | implicit_sources ->
        check_keys
          ~path
          ~section:"implicit_sources"
          ~required_keys:[]
          ~valid_keys:["conditional_test"; "literal_strings"]
          ~current_keys:(Json.Util.keys implicit_sources)
        >>= fun () ->
        array_member ~path "literal_strings" implicit_sources
        >>= fun literal_strings ->
        List.map
          ~f:(fun json ->
            json_string_member ~path "kind" json
            >>= fun source ->
            parse_source_reference ~path ~allowed_sources source
            |> Result.map_error ~f:(fun error -> [error])
            >>= fun source_kind ->
            json_string_member ~path "regexp" json
            >>| fun pattern -> { source_kind; pattern = Re2.create_exn pattern })
          literal_strings
        |> Result.combine_errors
        |> Result.map_error ~f:List.concat
        >>| fun literal_strings -> { literal_strings }
  in
  List.map source_json_list ~f:parse_source_annotations
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.concat
  >>= fun sources ->
  List.map source_json_list ~f:parse_sink_annotations
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.concat
  >>= fun sinks ->
  List.map source_json_list ~f:parse_transforms
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.concat
  >>= fun transforms ->
  List.map source_json_list ~f:parse_features
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.concat
  >>= fun features ->
  List.map
    source_json_list
    ~f:(parse_rules ~allowed_sources:sources ~allowed_sinks:sinks ~allowed_transforms:transforms)
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.concat
  >>= fun rules ->
  List.map source_json_list ~f:(parse_combined_source_rules ~allowed_sources:sources)
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.unzip3
  >>= fun (generated_combined_rules, partial_sink_converters, partial_sink_labels) ->
  let generated_combined_rules = List.concat generated_combined_rules in
  let partial_sink_converter =
    List.fold partial_sink_converters ~init:String.Map.Tree.empty ~f:PartialSinkConverter.merge
  in
  let partial_sink_labels =
    List.fold partial_sink_labels ~init:String.Map.Tree.empty ~f:PartialSinkConverter.merge
  in

  let merge_implicit_sinks left right =
    {
      conditional_test = left.conditional_test @ right.conditional_test;
      literal_string_sinks = left.literal_string_sinks @ right.literal_string_sinks;
    }
  in
  List.map source_json_list ~f:(parse_implicit_sinks ~allowed_sinks:sinks)
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.fold ~init:empty_implicit_sinks ~f:merge_implicit_sinks
  >>= fun implicit_sinks ->
  let parse_integer_option name =
    let parse_single_json (path, json) =
      match member "options" json with
      | `Null -> Ok None
      | options -> (
          match member name options with
          | `Null -> Ok None
          | `Int value -> Ok (Some value)
          | json ->
              Error
                (Error.create
                   ~path
                   ~kind:
                     (Error.UnexpectedJsonType
                        { json; message = "Expected integer, got"; section = Some "options" })))
    in
    List.map source_json_list ~f:parse_single_json
    |> Result.combine_errors
    >>| List.filter_map ~f:Fn.id
    >>= function
    | [] -> Ok None
    | [value] -> Ok (Some value)
    | _ -> Error [{ Error.path = None; kind = Error.OptionDuplicate name }]
  in
  parse_integer_option "maximum_overrides_to_analyze"
  >>= fun maximum_overrides_to_analyze ->
  parse_integer_option "maximum_trace_length"
  >>= fun maximum_trace_length ->
  parse_integer_option "maximum_tito_depth"
  >>= fun maximum_tito_depth ->
  let merge_implicit_sources left right =
    { literal_strings = left.literal_strings @ right.literal_strings }
  in
  List.map source_json_list ~f:(parse_implicit_sources ~allowed_sources:sources)
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.fold ~init:empty_implicit_sources ~f:merge_implicit_sources
  >>= fun implicit_sources ->
  let parse_lineage_analysis (path, json) =
    json_bool_member ~path "lineage_analysis" json ~default:false
  in
  List.map ~f:parse_lineage_analysis source_json_list
  |> Result.combine_errors
  |> Result.map_error ~f:List.concat
  >>| List.exists ~f:Fn.id
  >>| fun lineage_analysis ->
  let rules = List.rev_append rules generated_combined_rules in
  {
    sources;
    sinks;
    transforms;
    filtered_sources = None;
    filtered_sinks = None;
    filtered_transforms = None;
    features;
    rules;
    filtered_rule_codes = None;
    partial_sink_converter;
    implicit_sinks;
    implicit_sources;
    partial_sink_labels;
    find_missing_flows = None;
    dump_model_query_results_path = None;
    analysis_model_constraints =
      {
        default_analysis_model_constraints with
        maximum_overrides_to_analyze;
        maximum_trace_length;
        maximum_tito_depth;
      };
    lineage_analysis;
    source_sink_filter =
      Some
        (SourceSinkFilter.create
           ~rules
           ~filtered_rule_codes:None
           ~filtered_sources:None
           ~filtered_sinks:None
           ~filtered_transforms:None);
  }


let validate ({ sources; sinks; transforms; features; _ } as configuration) =
  let ensure_list_unique ~get_name ~get_error elements =
    let seen = String.Hash_set.create () in
    let ensure_unique element =
      let element = get_name element in
      if Hash_set.mem seen element then
        Error [{ Error.path = None; kind = get_error element }]
      else (
        Hash_set.add seen element;
        Ok ())
    in
    List.map elements ~f:ensure_unique
    |> Result.combine_errors_unit
    |> Result.map_error ~f:List.concat
  in
  Result.combine_errors_unit
    [
      ensure_list_unique
        ~get_name:(fun { AnnotationParser.name; _ } -> name)
        ~get_error:(fun name -> Error.SourceDuplicate name)
        sources;
      ensure_list_unique
        ~get_name:(fun { AnnotationParser.name; _ } -> name)
        ~get_error:(fun name -> Error.SinkDuplicate name)
        sinks;
      ensure_list_unique
        ~get_name:TaintTransform.show
        ~get_error:(fun name -> Error.TransformDuplicate name)
        transforms;
      ensure_list_unique
        ~get_name:Fn.id
        ~get_error:(fun name -> Error.FeatureDuplicate name)
        features;
    ]
  |> Result.map_error ~f:List.concat
  |> Result.map ~f:(fun () -> configuration)


exception TaintConfigurationError of Error.t list

let exception_on_error = function
  | Ok configuration -> configuration
  | Error errors -> raise (TaintConfigurationError errors)


let register configuration =
  let key = Memory.SingletonKey.key in
  let () =
    if ConfigurationSharedMemory.mem key then
      ConfigurationSharedMemory.remove_batch (ConfigurationSharedMemory.KeySet.singleton key)
  in
  ConfigurationSharedMemory.add key configuration


let default =
  let sources =
    List.map
      ~f:(fun name -> { AnnotationParser.name; kind = Named })
      ["Demo"; "Test"; "UserControlled"; "PII"; "Secrets"; "Cookies"]
  in
  let sinks =
    List.map
      ~f:(fun name -> { AnnotationParser.name; kind = Named })
      [
        "Demo";
        "FileSystem";
        "GetAttr";
        "Logging";
        "RemoteCodeExecution";
        "SQL";
        "Test";
        "XMLParser";
        "XSS";
      ]
  in
  let transforms =
    List.map ~f:(fun name -> TaintTransform.Named name) ["DemoTransform"; "TestTransform"]
  in
  let rules =
    [
      {
        Rule.sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "RemoteCodeExecution"];
        transforms = [];
        code = 5001;
        name = "Possible shell injection.";
        message_format =
          "Possible remote code execution due to [{$sources}] data reaching [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "Test"; Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "Test"];
        transforms = [];
        code = 5002;
        name = "Test flow.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "SQL"];
        transforms = [];
        code = 5005;
        name = "User controlled data to SQL execution.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources =
          [Sources.NamedSource "Cookies"; Sources.NamedSource "PII"; Sources.NamedSource "Secrets"];
        sinks = [Sinks.NamedSink "Logging"];
        transforms = [];
        code = 5006;
        name = "Restricted data being logged.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "XMLParser"];
        transforms = [];
        code = 5007;
        name = "User data to XML Parser.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "XSS"];
        transforms = [];
        code = 5008;
        name = "XSS";
        message_format = "Possible XSS due to [{$sources}] data reaching [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "Demo"];
        sinks = [Sinks.NamedSink "Demo"];
        transforms = [];
        code = 5009;
        name = "Demo flow.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "GetAttr"];
        transforms = [];
        code = 5010;
        name = "User data to getattr.";
        message_format = "Attacker may control at least one argument to getattr(,).";
      };
      {
        sources = [Sources.NamedSource "Test"];
        sinks = [Sinks.NamedSink "Test"];
        transforms = [TaintTransform.Named "TestTransform"];
        code = 5011;
        name = "Flow with one transform.";
        message_format =
          "Data from [{$sources}] source(s) via [{$transforms}] may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "Test"];
        sinks = [Sinks.NamedSink "Test"];
        transforms = [TaintTransform.Named "TestTransform"; TaintTransform.Named "DemoTransform"];
        code = 5011;
        name = "Flow with two transforms.";
        message_format =
          "Data from [{$sources}] source(s) via [{$transforms}] may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "Demo"];
        sinks = [Sinks.NamedSink "Demo"];
        transforms = [];
        code = 6001;
        name = "Duplicate demo flow.";
        message_format =
          "Possible remote code execution due to [{$sources}] data reaching [{$sinks}] sink(s)";
      };
    ]
  in
  {
    sources;
    sinks;
    transforms;
    filtered_sources = None;
    filtered_sinks = None;
    filtered_transforms = None;
    features =
      [
        "copy";
        "default";
        "object";
        "special_source";
        "special_sink";
        "string_concat_lhs";
        "string_concat_rhs";
      ];
    rules;
    filtered_rule_codes = None;
    partial_sink_converter = String.Map.Tree.empty;
    partial_sink_labels = String.Map.Tree.empty;
    implicit_sinks = empty_implicit_sinks;
    implicit_sources = empty_implicit_sources;
    find_missing_flows = None;
    dump_model_query_results_path = None;
    analysis_model_constraints = default_analysis_model_constraints;
    lineage_analysis = false;
    source_sink_filter =
      Some
        (SourceSinkFilter.create
           ~rules
           ~filtered_rule_codes:None
           ~filtered_sources:None
           ~filtered_sinks:None
           ~filtered_transforms:None);
  }


let obscure_flows_configuration configuration =
  let rules =
    [
      {
        Rule.sources =
          List.map ~f:(fun { name = source; _ } -> Sources.NamedSource source) configuration.sources;
        sinks = [Sinks.NamedSink "Obscure"];
        transforms = [];
        code = 9001;
        name = "Obscure flow.";
        message_format = "Data from [{$sources}] source(s) may reach an obscure model";
      };
    ]
  in
  {
    configuration with
    rules;
    find_missing_flows = Some Obscure;
    source_sink_filter =
      Some
        (SourceSinkFilter.create
           ~rules
           ~filtered_rule_codes:None
           ~filtered_sources:configuration.filtered_sources
           ~filtered_sinks:None
           ~filtered_transforms:None);
  }


let missing_type_flows_configuration configuration =
  let rules =
    [
      {
        Rule.sources =
          List.map ~f:(fun { name = source; _ } -> Sources.NamedSource source) configuration.sources;
        sinks = [Sinks.NamedSink "UnknownCallee"];
        transforms = [];
        code = 9002;
        name = "Unknown callee flow.";
        message_format = "Data from [{$sources}] source(s) may flow to an unknown callee";
      };
    ]
  in
  {
    configuration with
    rules;
    find_missing_flows = Some Type;
    source_sink_filter =
      Some
        (SourceSinkFilter.create
           ~rules
           ~filtered_rule_codes:None
           ~filtered_sources:configuration.filtered_sources
           ~filtered_sinks:None
           ~filtered_transforms:None);
  }


let apply_missing_flows configuration = function
  | Configuration.MissingFlowKind.Obscure -> obscure_flows_configuration configuration
  | Configuration.MissingFlowKind.Type -> missing_type_flows_configuration configuration


let get () =
  match ConfigurationSharedMemory.get Memory.SingletonKey.key with
  | None -> default
  | Some configuration -> configuration


let from_taint_model_paths taint_model_paths =
  let open Result in
  let file_paths =
    PyrePath.get_matching_files_recursively ~suffix:".config" ~paths:taint_model_paths
  in
  let parse_json path =
    if not (PyrePath.file_exists path) then
      Error (Error.create ~path ~kind:Error.FileNotFound)
    else
      let content =
        path
        |> File.create
        |> File.content
        |> Result.of_option ~error:(Error.create ~path ~kind:FileRead)
      in
      try content >>| Json.from_string >>| fun json -> path, json with
      | Yojson.Json_error parse_error ->
          Error (Error.create ~path ~kind:(Error.InvalidJson parse_error))
  in
  let configurations = file_paths |> List.map ~f:parse_json |> Result.combine_errors in
  match configurations with
  | Error errors -> Error errors
  | Ok [] -> Error [{ Error.path = None; kind = NoConfigurationFound }]
  | Ok configurations -> from_json_list configurations >>= validate


let with_command_line_options
    configuration
    ~rule_filter
    ~source_filter
    ~sink_filter
    ~transform_filter
    ~find_missing_flows
    ~dump_model_query_results_path
    ~maximum_trace_length
    ~maximum_tito_depth
  =
  let open Result in
  (match source_filter with
  | None -> Ok configuration
  | Some source_filter ->
      let parse_source_reference source =
        AnnotationParser.parse_source ~allowed:configuration.sources source
        |> Result.map_error ~f:(fun _ ->
               { Error.path = None; kind = Error.UnsupportedSource source })
      in
      source_filter
      |> List.map ~f:parse_source_reference
      |> Result.all
      |> Result.map_error ~f:(fun error -> [error])
      >>| Sources.Set.of_list
      >>| Option.some
      >>| fun filtered_sources -> { configuration with filtered_sources })
  >>= fun configuration ->
  (match sink_filter with
  | None -> Ok configuration
  | Some sink_filter ->
      let parse_sink_reference sink =
        AnnotationParser.parse_sink ~allowed:configuration.sinks sink
        |> Result.map_error ~f:(fun _ -> { Error.path = None; kind = Error.UnsupportedSink sink })
      in
      sink_filter
      |> List.map ~f:parse_sink_reference
      |> Result.all
      |> Result.map_error ~f:(fun error -> [error])
      >>| Sinks.Set.of_list
      >>| Option.some
      >>| fun filtered_sinks -> { configuration with filtered_sinks })
  >>= fun configuration ->
  (match transform_filter with
  | None -> Ok configuration
  | Some transform_filter ->
      let parse_transform_reference transform =
        AnnotationParser.parse_transform ~allowed:configuration.transforms transform
        |> Result.map_error ~f:(fun _ ->
               { Error.path = None; kind = Error.UnsupportedTransform transform })
      in
      transform_filter
      |> List.map ~f:parse_transform_reference
      |> Result.all
      |> Result.map_error ~f:(fun error -> [error])
      >>| Option.some
      >>| fun filtered_transforms -> { configuration with filtered_transforms })
  >>| fun configuration ->
  let configuration =
    match find_missing_flows with
    | Some Configuration.MissingFlowKind.Obscure -> obscure_flows_configuration configuration
    | Some Configuration.MissingFlowKind.Type -> missing_type_flows_configuration configuration
    | None -> configuration
  in
  let configuration = { configuration with dump_model_query_results_path } in
  let configuration =
    match maximum_trace_length with
    | None -> configuration
    | Some _ ->
        let analysis_model_constraints =
          { configuration.analysis_model_constraints with maximum_trace_length }
        in
        { configuration with analysis_model_constraints }
  in
  let configuration =
    match maximum_tito_depth with
    | None -> configuration
    | Some _ ->
        let analysis_model_constraints =
          { configuration.analysis_model_constraints with maximum_tito_depth }
        in
        { configuration with analysis_model_constraints }
  in
  let configuration =
    match rule_filter with
    | None -> configuration
    | Some rule_filter ->
        let filtered_rule_codes = IntSet.of_list rule_filter in
        { configuration with filtered_rule_codes = Some filtered_rule_codes }
  in
  let rules =
    filter_rules
      ~filtered_rule_codes:configuration.filtered_rule_codes
      ~filtered_sources:configuration.filtered_sources
      ~filtered_sinks:configuration.filtered_sinks
      ~filtered_transforms:configuration.filtered_transforms
      configuration.rules
  in
  let source_sink_filter =
    SourceSinkFilter.create
      ~rules
      ~filtered_rule_codes:configuration.filtered_rule_codes
      ~filtered_sources:configuration.filtered_sources
      ~filtered_sinks:configuration.filtered_sinks
      ~filtered_transforms:configuration.filtered_transforms
  in
  let implicit_sources =
    filter_implicit_sources ~source_sink_filter configuration.implicit_sources
  in
  let implicit_sinks = filter_implicit_sinks ~source_sink_filter configuration.implicit_sinks in
  {
    configuration with
    rules;
    implicit_sources;
    implicit_sinks;
    source_sink_filter = Some source_sink_filter;
  }


let source_can_match_rule { source_sink_filter; _ } source =
  match source_sink_filter with
  | Some source_sink_filter -> SourceSinkFilter.should_keep_source source_sink_filter source
  | None -> true


let sink_can_match_rule { source_sink_filter; _ } sink =
  match source_sink_filter with
  | Some source_sink_filter -> SourceSinkFilter.should_keep_sink source_sink_filter sink
  | None -> true


let code_metadata () =
  let { rules; _ } = get () in
  `Assoc (List.map rules ~f:(fun rule -> Format.sprintf "%d" rule.code, `String rule.name))


let conditional_test_sinks () =
  match get () with
  | { implicit_sinks = { conditional_test; _ }; _ } -> conditional_test


let literal_string_sinks () =
  match get () with
  | { implicit_sinks = { literal_string_sinks; _ }; _ } -> literal_string_sinks


let get_triggered_sink ~partial_sink ~source =
  let { partial_sink_converter; _ } = get () in
  PartialSinkConverter.get_triggered_sink partial_sink_converter ~partial_sink ~source


let is_missing_flow_analysis kind =
  match get () with
  | { find_missing_flows; _ } ->
      Option.equal Configuration.MissingFlowKind.equal (Some kind) find_missing_flows


let get_maximum_model_width () =
  match get () with
  | { analysis_model_constraints = { maximum_model_width; _ }; _ } -> maximum_model_width


let literal_string_sources () =
  let { implicit_sources; _ } = get () in
  implicit_sources.literal_strings


let get_maximum_overrides_to_analyze () =
  let { analysis_model_constraints = { maximum_overrides_to_analyze; _ }; _ } = get () in
  maximum_overrides_to_analyze


let maximum_return_access_path_width = 5

let maximum_return_access_path_depth = 3

let maximum_tito_positions = 50

let maximum_tree_depth_after_widening = 4

let maximum_tito_leaves = 5
