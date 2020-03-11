(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
module SharedMemory = Memory
module Json = Yojson.Safe

type rule = {
  sources: Sources.t list;
  sinks: Sinks.t list;
  code: int;
  name: string;
  message_format: string; (* format *)
}

type implicit_sinks = { conditional_test: Sinks.t list }

let empty_implicit_sinks = { conditional_test = [] }

type t = {
  sources: string list;
  sinks: string list;
  features: string list;
  rules: rule list;
  implicit_sinks: implicit_sinks;
}

let empty =
  { sources = []; sinks = []; features = []; rules = []; implicit_sinks = empty_implicit_sinks }


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
  let parse_sources json =
    let parse_source json = Json.Util.member "name" json |> Json.Util.to_string in
    array_member "sources" json |> List.map ~f:parse_source
  in
  let parse_sinks json =
    let parse_sink json = Json.Util.member "name" json |> Json.Util.to_string in
    array_member "sinks" json |> List.map ~f:parse_sink
  in
  let parse_features json =
    let parse_feature json = Json.Util.member "name" json |> Json.Util.to_string in
    array_member "features" json |> List.map ~f:parse_feature
  in
  let parse_rules ~allowed_sources ~allowed_sinks json =
    let parse_string_list json = Json.Util.to_list json |> List.map ~f:Json.Util.to_string in
    let parse_rule json =
      let sources =
        Json.Util.member "sources" json
        |> parse_string_list
        |> List.map ~f:(Sources.parse ~allowed:allowed_sources)
      in
      let sinks =
        Json.Util.member "sinks" json
        |> parse_string_list
        |> List.map ~f:(Sinks.parse ~allowed:allowed_sinks)
      in
      let name = Json.Util.member "name" json |> Json.Util.to_string in
      let message_format = Json.Util.member "message_format" json |> Json.Util.to_string in
      let code = Json.Util.member "code" json |> Json.Util.to_int in
      { sources; sinks; name; code; message_format }
    in
    array_member "rules" json |> List.map ~f:parse_rule
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
  let sinks = parse_sinks json in
  let features = parse_features json in
  let rules = parse_rules ~allowed_sources:sources ~allowed_sinks:sinks json in
  let implicit_sinks = parse_implicit_sinks ~allowed_sinks:sinks json in
  { sources; sinks; features; rules; implicit_sinks }


let register configuration =
  let () =
    if ConfigurationSharedMemory.mem key then
      ConfigurationSharedMemory.remove_batch (ConfigurationSharedMemory.KeySet.singleton key)
  in
  ConfigurationSharedMemory.add key configuration


let default =
  {
    sources = [];
    sinks = [];
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
          sources = [Sources.UserControlled];
          sinks = [Sinks.RemoteCodeExecution];
          code = 5001;
          name = "Possible shell injection.";
          message_format =
            "Possible remote code execution due to [{$sources}] data reaching [{$sinks}] sink(s)";
        };
        {
          sources = [Sources.Test; Sources.UserControlled];
          sinks = [Sinks.Test];
          code = 5002;
          name = "Test flow.";
          message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
        };
        {
          sources = [Sources.UserControlled];
          sinks = [Sinks.SQL];
          code = 5005;
          name = "User controlled data to SQL execution.";
          message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
        };
        {
          sources = [Sources.Cookies; Sources.PII; Sources.Secrets];
          sinks = [Sinks.Logging];
          code = 5006;
          name = "Restricted data being logged.";
          message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
        };
        {
          sources = [Sources.UserControlled];
          sinks = [Sinks.XMLParser];
          code = 5007;
          name = "User data to XML Parser.";
          message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
        };
        {
          sources = [Sources.UserControlled];
          sinks = [Sinks.XSS];
          code = 5008;
          name = "XSS";
          message_format = "Possible XSS due to [{$sources}] data reaching [{$sinks}] sink(s)";
        };
        {
          sources = [Sources.Demo];
          sinks = [Sinks.Demo];
          code = 5009;
          name = "Demo flow.";
          message_format = "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)";
        };
        {
          sources = [Sources.UserControlled];
          sinks = [Sinks.GetAttr];
          code = 5010;
          name = "User data to getattr.";
          message_format = "Attacker may control at least one argument to getattr(,).";
        };
      ];
    implicit_sinks = empty_implicit_sinks;
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
      | Yojson.Json_error parse_error ->
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
      implicit_sinks = merge_implicit_sinks left.implicit_sinks right.implicit_sinks;
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
