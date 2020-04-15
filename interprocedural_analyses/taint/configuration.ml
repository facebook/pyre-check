(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
}

let analysis_model_constraints =
  { maximum_model_width = 25; maximum_complex_access_path_length = 10 }


type partial_sink_converter = (Sources.t * Sinks.t) list String.Map.Tree.t

type t = {
  sources: string list;
  sinks: string list;
  features: string list;
  rules: Rule.t list;
  implicit_sinks: implicit_sinks;
  partial_sink_converter: partial_sink_converter;
  acceptable_sink_labels: string list String.Map.Tree.t;
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

let parse source =
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
        if List.exists ~f:(( = ) "multi_sink_labels") (Json.Util.keys json) then
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
      { Rule.sources; sinks; name; code; message_format }
    in
    array_member "rules" json |> List.map ~f:parse_rule
  in
  let parse_combined_source_rules ~allowed_sources ~acceptable_sink_labels json =
    let parse_combined_source_rule (rules, partial_sink_converter) json =
      let name = Json.Util.member "name" json |> Json.Util.to_string in
      let message_format = Json.Util.member "message_format" json |> Json.Util.to_string in
      let code = Json.Util.member "code" json |> Json.Util.to_int in
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
  let json = Json.from_string source in
  let sources = parse_sources json in
  let sinks, acceptable_sink_labels = parse_sinks json in
  let features = parse_features json in
  let rules =
    parse_rules ~allowed_sources:sources ~allowed_sinks:sinks ~acceptable_sink_labels json
  in
  let generated_combined_rules, partial_sink_converter =
    parse_combined_source_rules json ~allowed_sources:sources ~acceptable_sink_labels
  in

  let implicit_sinks = parse_implicit_sinks ~allowed_sinks:sinks json in
  {
    sources;
    sinks;
    features;
    rules = List.rev_append rules generated_combined_rules;
    partial_sink_converter;
    implicit_sinks;
    acceptable_sink_labels;
  }


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
      ];
    partial_sink_converter = String.Map.Tree.empty;
    implicit_sinks = empty_implicit_sinks;
    acceptable_sink_labels = String.Map.Tree.empty;
  }


let get () =
  match ConfigurationSharedMemory.get key with
  | None -> default
  | Some configuration -> configuration


let create ~rule_filter ~paths =
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
        |> parse
        |> Option.some
      with
      | Yojson.Json_error parse_error
      | Failure parse_error ->
          raise (MalformedConfiguration { path = Path.absolute config_file; parse_error })
  in
  let merge_implicit_sinks left right =
    { conditional_test = left.conditional_test @ right.conditional_test }
  in
  let merge left right =
    {
      sources = left.sources @ right.sources;
      sinks = left.sinks @ right.sinks;
      features = left.features @ right.features;
      rules = left.rules @ right.rules;
      partial_sink_converter =
        PartialSinkConverter.merge left.partial_sink_converter right.partial_sink_converter;
      implicit_sinks = merge_implicit_sinks left.implicit_sinks right.implicit_sinks;
      acceptable_sink_labels =
        String.Map.Tree.merge
          left.acceptable_sink_labels
          right.acceptable_sink_labels
          ~f:(fun ~key:_ ->
          function
          | `Both (left_labels, right_labels) -> Some (left_labels @ right_labels)
          | `Left labels
          | `Right labels ->
              Some labels);
    }
  in
  let configurations = file_paths |> List.filter_map ~f:parse_configuration in
  if List.is_empty configurations then
    raise (Invalid_argument "No `.config` was found in the taint directories.");
  let ({ rules; _ } as configuration) = List.fold_left configurations ~f:merge ~init:empty in
  match rule_filter with
  | None -> configuration
  | Some rule_filter ->
      let codes_to_keep = Int.Set.of_list rule_filter in
      let rules = List.filter rules ~f:(fun { code; _ } -> Set.mem codes_to_keep code) in
      { configuration with rules }


let conditional_test_sinks () =
  match get () with
  | { implicit_sinks = { conditional_test }; _ } -> conditional_test


let get_triggered_sink ~partial_sink ~source =
  let { partial_sink_converter; _ } = get () in
  PartialSinkConverter.get_triggered_sink partial_sink_converter ~partial_sink ~source
