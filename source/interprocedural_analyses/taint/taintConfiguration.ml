(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
module SharedMemory = Memory
module Json = Yojson.Safe

module Rule = struct
  type t = {
    sources: Sources.t list;
    sinks: Sinks.t list;
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

type missing_flows_kind =
  (* Find missing flows through obscure models. *)
  | Obscure
  (* Find missing flows due to missing type information. *)
  | Type
[@@deriving compare, show]

let missing_flows_kind_from_string = function
  | "obscure" -> Some Obscure
  | "type" -> Some Type
  | _ -> None


type t = {
  sources: AnnotationParser.source_or_sink list;
  sinks: AnnotationParser.source_or_sink list;
  features: string list;
  rules: Rule.t list;
  implicit_sinks: implicit_sinks;
  implicit_sources: implicit_sources;
  partial_sink_converter: partial_sink_converter;
  partial_sink_labels: string list String.Map.Tree.t;
  matching_sources: Sources.Set.t Sinks.Map.t;
  matching_sinks: Sinks.Set.t Sources.Map.t;
  find_missing_flows: missing_flows_kind option;
  dump_model_query_results_path: PyrePath.t option;
  analysis_model_constraints: analysis_model_constraints;
  lineage_analysis: bool;
}

let empty =
  {
    sources = [];
    sinks = [];
    features = [];
    rules = [];
    partial_sink_converter = String.Map.Tree.empty;
    implicit_sinks = empty_implicit_sinks;
    implicit_sources = empty_implicit_sources;
    partial_sink_labels = String.Map.Tree.empty;
    matching_sources = Sinks.Map.empty;
    matching_sinks = Sources.Map.empty;
    find_missing_flows = None;
    dump_model_query_results_path = None;
    analysis_model_constraints = default_analysis_model_constraints;
    lineage_analysis = false;
  }


(* There's only a single taint configuration *)
let key = "root"

module ConfigurationSharedMemory =
  SharedMemory.WithCache.Make
    (struct
      include String

      type out = string

      let from_string = ident
    end)
    (struct
      type nonrec t = t

      let prefix = Prefix.make ()

      let description = "Taint configuration"

      let unmarshall value = Marshal.from_string value 0
    end)

exception
  MalformedConfiguration of {
    path: string;
    parse_error: string;
  }

let matching_kinds_from_rules rules =
  let add_rule (matching_sources, matching_sinks) { Rule.sources; sinks; _ } =
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
  List.fold ~f:add_rule ~init:(Sinks.Map.empty, Sources.Map.empty) rules


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

let parse source_jsons =
  let json_bool_member key value ~default =
    Yojson.Safe.Util.member key value |> Yojson.Safe.Util.to_bool_option |> Option.value ~default
  in
  let member name json =
    try Json.Util.member name json with
    | Not_found -> `Null
  in
  let array_member name json =
    match member name json with
    | `Null -> []
    | json -> Json.Util.to_list json
  in
  let kind json =
    match member "kind" json with
    | `Null -> AnnotationParser.Named
    | `String "parametric" -> AnnotationParser.Parametric
    | unexpected -> failwith (Format.sprintf "Unexpected kind %s" (Json.Util.to_string unexpected))
  in
  let check_keys ~required_keys ~valid_keys ~current_keys ~section =
    let valid_keys_hash_set = String.Hash_set.of_list valid_keys in
    let current_keys_hash_set = String.Hash_set.of_list current_keys in
    let check_required_key_present key =
      if not (Hash_set.mem current_keys_hash_set key) then
        failwith (Format.sprintf "Required key `%s` is not found in section `%s`" key section)
    in
    let check_key_is_valid key =
      if not (Hash_set.mem valid_keys_hash_set key) then
        Log.error "Unknown key `%s` encountered in section `%s`" key section
    in
    List.iter current_keys ~f:check_key_is_valid;
    List.iter required_keys ~f:check_required_key_present
  in
  let parse_lineage_analysis json = json_bool_member "lineage_analysis" json ~default:false in
  let parse_string_list json = Json.Util.to_list json |> List.map ~f:Json.Util.to_string in
  let parse_source_or_sink section json =
    check_keys
      ~required_keys:["name"]
      ~current_keys:(Json.Util.keys json)
      ~valid_keys:["name"; "comment"]
      ~section;
    let name = Json.Util.member "name" json |> Json.Util.to_string in
    { AnnotationParser.name; kind = kind json }
  in
  let parse_sources json =
    array_member "sources" json |> List.map ~f:(parse_source_or_sink "sources")
  in
  let parse_sinks json = array_member "sinks" json |> List.map ~f:(parse_source_or_sink "sinks") in
  let parse_features json =
    let parse_feature json = Json.Util.member "name" json |> Json.Util.to_string in
    array_member "features" json |> List.map ~f:parse_feature
  in
  let seen_rules = Int.Hash_set.create () in
  let validate_code_uniqueness code =
    if Hash_set.mem seen_rules code then
      failwith (Format.sprintf "Multiple rules share the same code `%d`." code);
    Hash_set.add seen_rules code
  in
  let parse_rules ~allowed_sources ~allowed_sinks json =
    let parse_rule json =
      let required_keys = ["name"; "code"; "sources"; "sinks"; "message_format"] in
      let valid_keys = "oncall" :: "comment" :: required_keys in
      check_keys ~required_keys ~valid_keys ~current_keys:(Json.Util.keys json) ~section:"rules";
      let sources =
        Json.Util.member "sources" json
        |> parse_string_list
        |> List.map ~f:(AnnotationParser.parse_source ~allowed:allowed_sources)
        |> Core.Result.all
        |> Core.Result.ok_or_failwith
      in
      let sinks =
        Json.Util.member "sinks" json
        |> parse_string_list
        |> List.map ~f:(AnnotationParser.parse_sink ~allowed:allowed_sinks)
        |> Core.Result.all
        |> Core.Result.ok_or_failwith
      in
      let name = Json.Util.member "name" json |> Json.Util.to_string in
      let message_format = Json.Util.member "message_format" json |> Json.Util.to_string in
      let code = Json.Util.member "code" json |> Json.Util.to_int in
      validate_code_uniqueness code;
      { Rule.sources; sinks; name; code; message_format }
    in
    array_member "rules" json |> List.map ~f:parse_rule
  in
  let parse_combined_source_rules ~allowed_sources json =
    let parse_combined_source_rule (rules, partial_sink_converter, partial_sink_labels) json =
      let name = Json.Util.member "name" json |> Json.Util.to_string in
      let message_format = Json.Util.member "message_format" json |> Json.Util.to_string in
      let code = Json.Util.member "code" json |> Json.Util.to_int in
      validate_code_uniqueness code;
      let sources = Json.Util.member "sources" json in
      let keys = Json.Util.keys sources in
      match keys with
      | [first; second] ->
          let parse_sources sources =
            match sources with
            | `String source -> [AnnotationParser.parse_source ~allowed:allowed_sources source]
            | `List sources ->
                List.map sources ~f:Json.Util.to_string
                |> List.map ~f:(AnnotationParser.parse_source ~allowed:allowed_sources)
            | _ -> failwith "Expected a string or list of strings for combined rule sources."
          in
          let first_sources =
            Json.Util.member first sources
            |> parse_sources
            |> Core.Result.all
            |> Core.Result.ok_or_failwith
          in
          let second_sources =
            Json.Util.member second sources
            |> parse_sources
            |> Core.Result.all
            |> Core.Result.ok_or_failwith
          in

          let sinks, partial_sink_labels =
            let partial_sink = Json.Util.member "partial_sink" json |> Json.Util.to_string in
            if String.Map.Tree.mem partial_sink_labels partial_sink then
              failwith
                (Format.sprintf
                   "Partial sinks must be unique - an entry for `%s` already exists."
                   partial_sink)
            else
              ( [partial_sink],
                String.Map.Tree.set partial_sink_labels ~key:partial_sink ~data:[first; second] )
          in
          let create_partial_sink label sink =
            begin
              match String.Map.Tree.find partial_sink_labels sink with
              | Some labels when not (List.mem ~equal:String.equal labels label) ->
                  failwith
                    (Format.sprintf
                       "Error when parsing configuration: `%s` is an invalid label For multi sink \
                        `%s` (choices: `%s`)"
                       label
                       sink
                       (String.concat labels ~sep:", "))
              | None ->
                  failwith
                    (Format.sprintf
                       "Error when parsing configuration: `%s` is not a multi sink."
                       sink)
              | _ -> ()
            end;
            { Sinks.kind = sink; label }
          in
          let first_sinks = List.map sinks ~f:(create_partial_sink first) in
          let second_sinks = List.map sinks ~f:(create_partial_sink second) in
          ( {
              Rule.sources = first_sources;
              sinks = List.map first_sinks ~f:(fun sink -> Sinks.TriggeredPartialSink sink);
              name;
              code;
              message_format;
            }
            ::
            {
              Rule.sources = second_sources;
              sinks = List.map second_sinks ~f:(fun sink -> Sinks.TriggeredPartialSink sink);
              name;
              code;
              message_format;
            }
            :: rules,
            PartialSinkConverter.add
              partial_sink_converter
              ~first_sources
              ~first_sinks
              ~second_sources
              ~second_sinks,
            partial_sink_labels )
      | _ -> failwith "Combined source rules must be of the form {\"a\": SourceA, \"b\": SourceB}"
    in
    array_member "combined_source_rules" json
    |> List.fold
         ~init:([], String.Map.Tree.empty, String.Map.Tree.empty)
         ~f:parse_combined_source_rule
  in
  let parse_implicit_sinks ~allowed_sinks json =
    match member "implicit_sinks" json with
    | `Null -> empty_implicit_sinks
    | implicit_sinks ->
        check_keys
          ~required_keys:[]
          ~valid_keys:["conditional_test"; "literal_strings"]
          ~current_keys:(Json.Util.keys implicit_sinks)
          ~section:"implicit_sinks";
        let conditional_test =
          match member "conditional_test" implicit_sinks with
          | `Null -> []
          | conditional_test ->
              Json.Util.to_list conditional_test
              |> List.map ~f:(fun json ->
                     Json.Util.to_string json
                     |> AnnotationParser.parse_sink ~allowed:allowed_sinks
                     |> Core.Result.ok_or_failwith)
        in
        let literal_string_sinks =
          match member "literal_strings" implicit_sinks with
          | `Null -> []
          | literal_strings ->
              Json.Util.to_list literal_strings
              |> List.map ~f:(fun json ->
                     let sink_kind =
                       Json.Util.member "kind" json
                       |> Json.Util.to_string
                       |> AnnotationParser.parse_sink ~allowed:allowed_sinks
                       |> Core.Result.ok_or_failwith
                     in
                     let pattern =
                       Json.Util.member "regexp" json |> Json.Util.to_string |> Re2.create_exn
                     in
                     { sink_kind; pattern })
        in
        { conditional_test; literal_string_sinks }
  in
  let parse_implicit_sources ~allowed_sources json =
    match member "implicit_sources" json with
    | `Null -> { literal_strings = [] }
    | implicit_sources ->
        check_keys
          ~required_keys:[]
          ~valid_keys:["conditional_test"; "literal_strings"]
          ~current_keys:(Json.Util.keys implicit_sources)
          ~section:"implicit_sources";
        let literal_strings =
          array_member "literal_strings" implicit_sources
          |> List.map ~f:(fun json ->
                 let source_kind =
                   Json.Util.member "kind" json
                   |> Json.Util.to_string
                   |> AnnotationParser.parse_source ~allowed:allowed_sources
                   |> Core.Result.ok_or_failwith
                 in
                 let pattern =
                   Json.Util.member "regexp" json |> Json.Util.to_string |> Re2.create_exn
                 in
                 { source_kind; pattern })
        in
        { literal_strings }
  in
  let sources = List.concat_map source_jsons ~f:parse_sources in
  let sinks = List.concat_map source_jsons ~f:parse_sinks in
  let features = List.concat_map source_jsons ~f:parse_features in
  let rules =
    List.concat_map source_jsons ~f:(parse_rules ~allowed_sources:sources ~allowed_sinks:sinks)
  in
  let generated_combined_rules, partial_sink_converter, partial_sink_labels =
    List.map source_jsons ~f:(parse_combined_source_rules ~allowed_sources:sources)
    |> List.unzip3
    |> fun (generated_combined_rules, partial_sink_converters, partial_sink_labels) ->
    ( List.concat generated_combined_rules,
      List.fold partial_sink_converters ~init:String.Map.Tree.empty ~f:PartialSinkConverter.merge,
      List.fold partial_sink_labels ~init:String.Map.Tree.empty ~f:PartialSinkConverter.merge )
  in

  let merge_implicit_sinks left right =
    {
      conditional_test = left.conditional_test @ right.conditional_test;
      literal_string_sinks = left.literal_string_sinks @ right.literal_string_sinks;
    }
  in
  let implicit_sinks =
    List.map source_jsons ~f:(parse_implicit_sinks ~allowed_sinks:sinks)
    |> List.fold ~init:empty_implicit_sinks ~f:merge_implicit_sinks
  in
  let parse_integer_option name =
    let parse_single_json json =
      match member "options" json with
      | `Null -> None
      | options -> (
          match member name options with
          | `Null -> None
          | value -> Some (Json.Util.to_int value))
    in
    List.filter_map source_jsons ~f:parse_single_json
    |> function
    | [] -> None
    | [value] -> Some value
    | _ -> failwith (Format.asprintf "Multiple values were passed in for `%s`." name)
  in
  let maximum_overrides_to_analyze = parse_integer_option "maximum_overrides_to_analyze" in
  let maximum_trace_length = parse_integer_option "maximum_trace_length" in
  let maximum_tito_depth = parse_integer_option "maximum_tito_depth" in
  let merge_implicit_sources left right =
    { literal_strings = left.literal_strings @ right.literal_strings }
  in
  let implicit_sources =
    List.map source_jsons ~f:(parse_implicit_sources ~allowed_sources:sources)
    |> List.fold ~init:empty_implicit_sources ~f:merge_implicit_sources
  in
  let lineage_analysis = List.exists ~f:parse_lineage_analysis source_jsons in
  let rules = List.rev_append rules generated_combined_rules in
  let matching_sources, matching_sinks = matching_kinds_from_rules rules in
  {
    sources;
    sinks;
    features;
    rules;
    partial_sink_converter;
    implicit_sinks;
    implicit_sources;
    partial_sink_labels;
    matching_sources;
    matching_sinks;
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
  }


let validate { sources; sinks; features; _ } =
  let ensure_list_unique ~kind ~get_name elements =
    let seen = String.Hash_set.create () in
    let ensure_unique element =
      let element = get_name element in
      if Hash_set.mem seen element then
        failwith (Format.sprintf "Duplicate entry for %s: `%s`" kind element);
      Hash_set.add seen element
    in
    List.iter elements ~f:ensure_unique
  in
  ensure_list_unique ~kind:"source" ~get_name:(fun { AnnotationParser.name; _ } -> name) sources;
  ensure_list_unique ~kind:"sink" ~get_name:(fun { AnnotationParser.name; _ } -> name) sinks;
  ensure_list_unique ~kind:"feature" ~get_name:ident features


let register configuration =
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
  let rules =
    [
      {
        Rule.sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "RemoteCodeExecution"];
        code = 5001;
        name = "Possible shell injection.";
        message_format =
          "Possible remote code execution due to [{$sources}] data reaching [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "Test"; Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "Test"];
        code = 5002;
        name = "Test flow.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "SQL"];
        code = 5005;
        name = "User controlled data to SQL execution.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources =
          [Sources.NamedSource "Cookies"; Sources.NamedSource "PII"; Sources.NamedSource "Secrets"];
        sinks = [Sinks.NamedSink "Logging"];
        code = 5006;
        name = "Restricted data being logged.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "XMLParser"];
        code = 5007;
        name = "User data to XML Parser.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "XSS"];
        code = 5008;
        name = "XSS";
        message_format = "Possible XSS due to [{$sources}] data reaching [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "Demo"];
        sinks = [Sinks.NamedSink "Demo"];
        code = 5009;
        name = "Demo flow.";
        message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
      };
      {
        sources = [Sources.NamedSource "UserControlled"];
        sinks = [Sinks.NamedSink "GetAttr"];
        code = 5010;
        name = "User data to getattr.";
        message_format = "Attacker may control at least one argument to getattr(,).";
      };
      {
        sources = [Sources.NamedSource "Demo"];
        sinks = [Sinks.NamedSink "Demo"];
        code = 6001;
        name = "Duplicate demo flow.";
        message_format =
          "Possible remote code execution due to [{$sources}] data reaching [{$sinks}] sink(s)";
      };
    ]
  in
  let matching_sources, matching_sinks = matching_kinds_from_rules rules in
  {
    sources;
    sinks;
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
    partial_sink_converter = String.Map.Tree.empty;
    partial_sink_labels = String.Map.Tree.empty;
    implicit_sinks = empty_implicit_sinks;
    implicit_sources = empty_implicit_sources;
    matching_sources;
    matching_sinks;
    find_missing_flows = None;
    dump_model_query_results_path = None;
    analysis_model_constraints = default_analysis_model_constraints;
    lineage_analysis = false;
  }


let obscure_flows_configuration configuration =
  let rules =
    [
      {
        Rule.sources =
          List.map ~f:(fun { name = source; _ } -> Sources.NamedSource source) configuration.sources;
        sinks = [Sinks.NamedSink "Obscure"];
        code = 9001;
        name = "Obscure flow.";
        message_format = "Data from [{$sources}] source(s) may reach an obscure model";
      };
    ]
  in
  let matching_sources, matching_sinks = matching_kinds_from_rules rules in
  { configuration with rules; matching_sources; matching_sinks; find_missing_flows = Some Obscure }


let missing_type_flows_configuration configuration =
  let rules =
    [
      {
        Rule.sources =
          List.map ~f:(fun { name = source; _ } -> Sources.NamedSource source) configuration.sources;
        sinks = [Sinks.NamedSink "UnknownCallee"];
        code = 9002;
        name = "Unknown callee flow.";
        message_format = "Data from [{$sources}] source(s) may flow to an unknown callee";
      };
    ]
  in
  let matching_sources, matching_sinks = matching_kinds_from_rules rules in
  { configuration with rules; matching_sources; matching_sinks; find_missing_flows = Some Type }


let apply_missing_flows configuration = function
  | Obscure -> obscure_flows_configuration configuration
  | Type -> missing_type_flows_configuration configuration


let get () =
  match ConfigurationSharedMemory.get key with
  | None -> default
  | Some configuration -> configuration


let create
    ~rule_filter
    ~find_missing_flows
    ~dump_model_query_results_path
    ~maximum_trace_length
    ~maximum_tito_depth
    ~taint_model_paths
  =
  let file_paths =
    PyrePath.get_matching_files_recursively ~suffix:".config" ~paths:taint_model_paths
  in
  let parse_configuration config_file =
    if not (PyrePath.file_exists config_file) then
      raise
        (MalformedConfiguration
           { path = PyrePath.absolute config_file; parse_error = "File not found" })
    else
      try
        config_file
        |> File.create
        |> File.content
        |> Option.value ~default:""
        |> Json.from_string
        |> Option.some
      with
      | Yojson.Json_error parse_error
      | Failure parse_error ->
          raise (MalformedConfiguration { path = PyrePath.absolute config_file; parse_error })
  in
  let configurations = file_paths |> List.filter_map ~f:parse_configuration in
  if List.is_empty configurations then
    raise (Invalid_argument "No `.config` was found in the taint directories.");
  let configuration = parse configurations in
  validate configuration;
  let configuration =
    match find_missing_flows with
    | Some Obscure -> obscure_flows_configuration configuration
    | Some Type -> missing_type_flows_configuration configuration
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
  match rule_filter with
  | None -> configuration
  | Some rule_filter ->
      let codes_to_keep = Int.Set.of_list rule_filter in
      let { rules; _ } = configuration in
      let rules = List.filter rules ~f:(fun { code; _ } -> Set.mem codes_to_keep code) in
      { configuration with rules }


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
      Option.equal [%compare.equal: missing_flows_kind] (Some kind) find_missing_flows


let get_maximum_model_width () =
  match get () with
  | { analysis_model_constraints = { maximum_model_width; _ }; _ } -> maximum_model_width


let literal_string_sources () =
  let { implicit_sources; _ } = get () in
  implicit_sources.literal_strings


let maximum_return_access_path_width = 5

let maximum_return_access_path_depth = 3

let maximum_tito_positions = 50

let maximum_tree_depth_after_widening = 4

let maximum_tito_leaves = 5
