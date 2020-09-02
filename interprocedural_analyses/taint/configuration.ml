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
  [@@deriving eq, show]
end

type implicit_sinks = { conditional_test: Sinks.t list }

let empty_implicit_sinks = { conditional_test = [] }

type analysis_model_constraints = {
  maximum_model_width: int;
  maximum_complex_access_path_length: int;
  maximum_overrides_to_analyze: int option;
}

let default_analysis_model_constraints =
  {
    maximum_model_width = 25;
    maximum_complex_access_path_length = 10;
    maximum_overrides_to_analyze = None;
  }


type partial_sink_converter = (Sources.t * Sinks.t) list String.Map.Tree.t

type t = {
  sources: string list;
  sinks: string list;
  features: string list;
  rules: Rule.t list;
  implicit_sinks: implicit_sinks;
  partial_sink_converter: partial_sink_converter;
  acceptable_sink_labels: string list String.Map.Tree.t;
  find_obscure_flows: bool;
  analysis_model_constraints: analysis_model_constraints;
}

let empty =
  {
    sources = [];
    sinks = [];
    features = [];
    rules = [];
    partial_sink_converter = String.Map.Tree.empty;
    implicit_sinks = empty_implicit_sinks;
    acceptable_sink_labels = String.Map.Tree.empty;
    find_obscure_flows = false;
    analysis_model_constraints = default_analysis_model_constraints;
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

module PartialSinkConverter = struct
  let mangle { Sinks.kind; label } = Format.sprintf "%s$%s" kind label

  let add map ~first_source ~first_sinks ~second_source ~second_sinks =
    let add map (first_sink, second_sink) =
      (* Trigger second sink when the first sink matches a source, and vice versa. *)
      String.Map.Tree.add_multi
        map
        ~key:(mangle first_sink)
        ~data:(first_source, Sinks.TriggeredPartialSink second_sink)
      |> String.Map.Tree.add_multi
           ~key:(mangle second_sink)
           ~data:(second_source, Sinks.TriggeredPartialSink first_sink)
    in
    List.cartesian_product first_sinks second_sinks |> List.fold ~f:add ~init:map


  let merge left right =
    String.Map.Tree.merge
      ~f:(fun ~key:_ -> function
        | `Left value
        | `Right value ->
            Some value
        | `Both (left, right) -> Some (left @ right))
      left
      right


  let get_triggered_sink sink_to_sources ~partial_sink ~source =
    match mangle partial_sink |> String.Map.Tree.find sink_to_sources with
    | Some source_and_sink_list ->
        List.find source_and_sink_list ~f:(fun (supported_source, _) ->
            Sources.equal source supported_source)
        >>| snd
    | _ -> None
end

let parse source_jsons =
  let member name json =
    try Json.Util.member name json with
    | Not_found -> `Null
  in
  let array_member name json =
    match member name json with
    | `Null -> []
    | json -> Json.Util.to_list json
  in
  let parse_string_list json = Json.Util.to_list json |> List.map ~f:Json.Util.to_string in
  let parse_sources json =
    let parse_source json = Json.Util.member "name" json |> Json.Util.to_string in
    array_member "sources" json |> List.map ~f:parse_source
  in
  let parse_sinks json =
    let parse_sink (sinks, acceptable_sink_labels) json =
      let sink = Json.Util.member "name" json |> Json.Util.to_string in
      let acceptable_sink_labels =
        if List.exists ~f:(String.equal "multi_sink_labels") (Json.Util.keys json) then
          Json.Util.member "multi_sink_labels" json
          |> Json.Util.to_list
          |> List.map ~f:Json.Util.to_string
          |> fun data -> String.Map.Tree.set acceptable_sink_labels ~key:sink ~data
        else
          acceptable_sink_labels
      in
      sink :: sinks, acceptable_sink_labels
    in
    array_member "sinks" json |> List.fold ~init:([], String.Map.Tree.empty) ~f:parse_sink
  in
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
  let parse_rules ~allowed_sources ~allowed_sinks ~acceptable_sink_labels json =
    let parse_rule json =
      let sources =
        Json.Util.member "sources" json
        |> parse_string_list
        |> List.map ~f:(Sources.parse ~allowed:allowed_sources)
      in
      let validate sink =
        (* Ensure that the sink used for a normal rule is not a multi sink. *)
        if String.Map.Tree.mem acceptable_sink_labels sink then
          failwith (Format.sprintf "Multi sink `%s` can't be used for a regular rule." sink);
        sink
      in
      let sinks =
        Json.Util.member "sinks" json
        |> parse_string_list
        |> List.map ~f:validate
        |> List.map ~f:(Sinks.parse ~allowed:allowed_sinks)
      in
      let name = Json.Util.member "name" json |> Json.Util.to_string in
      let message_format = Json.Util.member "message_format" json |> Json.Util.to_string in
      let code = Json.Util.member "code" json |> Json.Util.to_int in
      validate_code_uniqueness code;
      { Rule.sources; sinks; name; code; message_format }
    in
    array_member "rules" json |> List.map ~f:parse_rule
  in
  let parse_combined_source_rules ~allowed_sources ~acceptable_sink_labels json =
    let parse_combined_source_rule (rules, partial_sink_converter) json =
      let name = Json.Util.member "name" json |> Json.Util.to_string in
      let message_format = Json.Util.member "message_format" json |> Json.Util.to_string in
      let code = Json.Util.member "code" json |> Json.Util.to_int in
      validate_code_uniqueness code;
      let sources = Json.Util.member "sources" json in
      let keys = Json.Util.keys sources in
      match keys with
      | [first; second] ->
          let first_source =
            Json.Util.member first sources
            |> Json.Util.to_string
            |> Sources.parse ~allowed:allowed_sources
          in
          let second_source =
            Json.Util.member second sources
            |> Json.Util.to_string
            |> Sources.parse ~allowed:allowed_sources
          in

          let sinks = Json.Util.member "sinks" json |> parse_string_list in
          let create_partial_sink label sink =
            begin
              match String.Map.Tree.find acceptable_sink_labels sink with
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
              Rule.sources = [first_source];
              sinks = List.map first_sinks ~f:(fun sink -> Sinks.TriggeredPartialSink sink);
              name;
              code;
              message_format;
            }
            :: {
                 Rule.sources = [second_source];
                 sinks = List.map second_sinks ~f:(fun sink -> Sinks.TriggeredPartialSink sink);
                 name;
                 code;
                 message_format;
               }
            :: rules,
            PartialSinkConverter.add
              partial_sink_converter
              ~first_source
              ~first_sinks
              ~second_source
              ~second_sinks )
      | _ -> failwith "Combined source rules must be of the form {\"a\": SourceA, \"b\": SourceB}"
    in
    array_member "combined_source_rules" json
    |> List.fold ~init:([], String.Map.Tree.empty) ~f:parse_combined_source_rule
  in
  let parse_implicit_sinks ~allowed_sinks json =
    match member "implicit_sinks" json with
    | `Null -> empty_implicit_sinks
    | implicit_sinks ->
        let conditional_test =
          array_member "conditional_test" implicit_sinks
          |> List.map ~f:(fun json ->
                 Json.Util.to_string json |> Sinks.parse ~allowed:allowed_sinks)
        in
        { conditional_test }
  in
  let sources = List.concat_map source_jsons ~f:parse_sources in
  let sinks, acceptable_sink_labels =
    List.map source_jsons ~f:parse_sinks
    |> List.unzip
    |> fun (sinks, acceptable_sink_labels) ->
    ( List.concat sinks,
      List.fold acceptable_sink_labels ~init:String.Map.Tree.empty ~f:PartialSinkConverter.merge )
  in
  let features = List.concat_map source_jsons ~f:parse_features in
  let rules =
    List.concat_map
      source_jsons
      ~f:(parse_rules ~allowed_sources:sources ~allowed_sinks:sinks ~acceptable_sink_labels)
  in
  let generated_combined_rules, partial_sink_converter =
    List.map
      source_jsons
      ~f:(parse_combined_source_rules ~allowed_sources:sources ~acceptable_sink_labels)
    |> List.unzip
    |> fun (generated_combined_rules, partial_sink_converters) ->
    ( List.concat generated_combined_rules,
      List.fold partial_sink_converters ~init:String.Map.Tree.empty ~f:PartialSinkConverter.merge )
  in

  let merge_implicit_sinks left right =
    { conditional_test = left.conditional_test @ right.conditional_test }
  in
  let implicit_sinks =
    List.map source_jsons ~f:(parse_implicit_sinks ~allowed_sinks:sinks)
    |> List.fold ~init:{ conditional_test = [] } ~f:merge_implicit_sinks
  in
  let maximum_overrides_to_analyze =
    let parse_overrides_to_analyze json =
      match member "options" json with
      | `Null -> None
      | options -> (
          match member "maximum_overrides_to_analyze" options with
          | `Null -> None
          | overrides_to_analyze -> Some (Json.Util.to_int overrides_to_analyze) )
    in
    List.filter_map source_jsons ~f:parse_overrides_to_analyze
    |> function
    | [] -> None
    | [maximum_overrides_to_analyze] -> Some maximum_overrides_to_analyze
    | _ -> failwith "Multiple values were passed in for overrides to analyze."
  in
  {
    sources;
    sinks;
    features;
    rules = List.rev_append rules generated_combined_rules;
    partial_sink_converter;
    implicit_sinks;
    acceptable_sink_labels;
    find_obscure_flows = false;
    analysis_model_constraints =
      { default_analysis_model_constraints with maximum_overrides_to_analyze };
  }


let validate { sources; sinks; features; _ } =
  let ensure_list_unique ~kind elements =
    let seen = String.Hash_set.create () in
    let ensure_unique element =
      if Hash_set.mem seen element then
        failwith (Format.sprintf "Duplicate entry for %s: `%s`" kind element);
      Hash_set.add seen element
    in
    List.iter elements ~f:ensure_unique
  in
  ensure_list_unique ~kind:"source" sources;
  ensure_list_unique ~kind:"sink" sinks;
  ensure_list_unique ~kind:"feature" features


let register configuration =
  let () =
    if ConfigurationSharedMemory.mem key then
      ConfigurationSharedMemory.remove_batch (ConfigurationSharedMemory.KeySet.singleton key)
  in
  ConfigurationSharedMemory.add key configuration


let default =
  {
    sources = ["Demo"; "Test"; "UserControlled"; "PII"; "Secrets"; "Cookies"];
    sinks =
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
      ];
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
    rules =
      [
        {
          sources = [Sources.NamedSource "UserControlled"];
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
            [
              Sources.NamedSource "Cookies"; Sources.NamedSource "PII"; Sources.NamedSource "Secrets";
            ];
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
      ];
    partial_sink_converter = String.Map.Tree.empty;
    implicit_sinks = empty_implicit_sinks;
    acceptable_sink_labels = String.Map.Tree.empty;
    find_obscure_flows = false;
    analysis_model_constraints = default_analysis_model_constraints;
  }


let obscure_flows_configuration configuration =
  {
    configuration with
    rules =
      [
        {
          sources = List.map ~f:(fun source -> Sources.NamedSource source) configuration.sources;
          sinks = [Sinks.NamedSink "Obscure"];
          code = 6002;
          name = "Obscure flow.";
          message_format = "Data from [{$sources}] source(s) may reach an obscure model";
        };
      ];
    find_obscure_flows = true;
  }


let get () =
  match ConfigurationSharedMemory.get key with
  | None -> default
  | Some configuration -> configuration


let create ~rule_filter ~find_obscure_flows ~paths =
  let file_paths = Path.get_matching_files_recursively ~suffix:".config" ~paths in
  let parse_configuration config_file =
    if not (Path.file_exists config_file) then
      raise
        (MalformedConfiguration { path = Path.absolute config_file; parse_error = "File not found" })
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
          raise (MalformedConfiguration { path = Path.absolute config_file; parse_error })
  in
  let configurations = file_paths |> List.filter_map ~f:parse_configuration in
  if List.is_empty configurations then
    raise (Invalid_argument "No `.config` was found in the taint directories.");
  let configuration = parse configurations in
  validate configuration;
  let configuration =
    if find_obscure_flows then
      obscure_flows_configuration configuration
    else
      configuration
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
  | { implicit_sinks = { conditional_test }; _ } -> conditional_test


let get_triggered_sink ~partial_sink ~source =
  let { partial_sink_converter; _ } = get () in
  PartialSinkConverter.get_triggered_sink partial_sink_converter ~partial_sink ~source


let get_maximum_model_width () =
  match get () with
  | { analysis_model_constraints = { maximum_model_width; _ }; _ } -> maximum_model_width


let maximum_return_access_path_width = 5

let maximum_return_access_path_depth = 3
