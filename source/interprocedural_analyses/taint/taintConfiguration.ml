(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TaintConfiguration: stores all configuration options related to the taint
 * analysis. This is parsed from taint configuration files (`.config`) in JSON.
 * Command line options can also alter the configuration. Among others, it
 * stores the set of sources, sinks and rules.
 *
 * This can be used as a traditional ocaml value using the `Heap` module, and
 * stored in shared memory using the `SharedMemory` module.
 *)

open Core
open Pyre
module Json = Yojson.Safe

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

module ModelConstraints = struct
  type t = {
    (* This limits the width of the source tree in the model for a callable, i.e
     * the number of output paths in the return value.
     *
     * For instance:
     * ```
     * def foo():
     *   return {"a": source(), "b": source(), "c": source()}
     * ```
     *
     * The source tree for `foo` has a width of 3. Above the provided threshold, we
     * would collapse the taint and consider the whole dictionary tainted.
     *)
    maximum_model_source_tree_width: int;
    (* This limits the width of the sink tree in the model for a callable, i.e
     * the number of input paths leading to a sink for a given parameter.
     *
     * For instance:
     * ```
     * def foo(arg):
     *   sink(arg[1])
     *   sink(arg[2])
     *   sink(arg[3])
     * ```
     *
     * The sink tree for `foo` and parameter `arg` has a width of 3.
     * Above the provided threshold, we would collapse the taint and consider that the
     * whole argument leads to a sink.
     *)
    maximum_model_sink_tree_width: int;
    (* This limits the width of the tito tree in the model for a callable, i.e
     * the number of input paths propagated to the return value, for a given parameter.
     *
     * For instance:
     * ```
     * def foo(arg):
     *   return '%s:%s:%s' % (arg.a, arg.b, arg.c)
     * ```
     *
     * The tito tree for `foo` and parameter `arg` has a width of 3.
     * Above the provided threshold, we would collapse the taint and consider that the
     * taint on the whole argument is propagated to the return value.
     *)
    maximum_model_tito_tree_width: int;
    (* This limits the depth of the source, sink and tito trees within loops, i.e the
     * length of source, sink and tito paths for each variables.
     *
     * For instance:
     * ```
     * def foo():
     *   variable = MyClass()
     *   for x in generate():
     *     variable.a.b.c = source()
     *   return result
     * ```
     *
     * The source tree for `variable` has a depth of 3 (i.e, `a` -> `b` -> `c`).
     * Within a loop, we limit the depth to the provided threshold. For instance,
     * if that threshold is 1, we would consider that `variable.a` is tainted.
     *)
    maximum_tree_depth_after_widening: int;
    (* This limits the width of the return access path tree in the model for a callable,
     * i.e the number of output paths propagated to the return value, for a given parameter.
     *
     * For instance:
     * ```
     * def foo(arg):
     *   return {'a': arg, 'b': arg, 'c': arg}
     * ```
     *
     * The return access path tree for `foo` and parameter `arg` has a width of 3.
     * Above the provided threshold, we would collapse the taint and consider that the
     * whole return value is tainted whenever `arg` is tainted.
     *)
    maximum_return_access_path_width: int;
    (* This limits the depth of the return access path tree within loops, i.e the
     * length of output paths propagated to the return value, for a given parameter.
     *
     * For instance:
     * ```
     * def foo(arg):
     *   result = MyClass()
     *   for x in generate():
     *     result.a.b.c = arg
     *   return result
     * ```
     *
     * The return access path tree for `foo` and parameter `arg` has a depth  of 3
     * (i.e, `a` -> `b` -> `c`). Within a loop, we limit the depth to the provided
     * threshold. For instance, if that threshold is 2, we would cut the output path
     * to just `a.b`.
     *)
    maximum_return_access_path_depth_after_widening: int;
    (* This limits the depth of the taint tree after applying taint-in-taint-out,
     * i.e the length of paths for taint propagated from a parameter to the return
     * value.
     *
     * For instance:
     * ```
     * def identity(arg): return arg
     *
     * def foo():
     *   input = {'a': {'b': {'c': source()}}}
     *   output = identity(input)
     * ```
     *
     * The taint tree for `input` has a depth of 3 (i.e, `a` -> `b` -> `c`).
     * When the taint is propagated to the return value of `identity`, we limit
     * the resulting taint tree to the given depth. For instance, if that threshold
     * is 1, we would consider that `output['a']` is tainted.
     *
     * This is also applied for sinks in the backward analysis:
     * ```
     * def foo(arg):
     *   output = identity(arg)
     *   sink(output['a']['b']['c'])
     * ```
     * With a threshold of 1, we would consider that `output['a']` leads to a sink.
     *)
    maximum_tito_collapse_depth: int;
    (* This limits the number of positions to keep track of when propagating taint.
     *
     * When taint is propagated through a function and returned (i.e,
     * taint-in-taint-out), we will keep track of the position of the argument,
     * and display it in the trace.
     *
     * For instance:
     * ```
     * def foo():
     *   x = source()
     *   y = tito(x)
     *            ^
     *   z = {"a": y}
     *             ^
     *   sink(z)
     * ```
     *
     * In this example, we have 2 tito positions. Above the provided threshold,
     * we simply discard all positions. The taint is still propagated.
     *)
    maximum_tito_positions: int;
    (* This limits the number of method overrides that we consider at a call site.
     *
     * If the number of overrides is above that threshold, we simply consider that
     * the method has no override.
     *)
    maximum_overrides_to_analyze: int option;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
  }

  let default =
    {
      (* DOCUMENTATION_CONFIGURATION_START *)
      maximum_model_source_tree_width = 25;
      maximum_model_sink_tree_width = 25;
      maximum_model_tito_tree_width = 5;
      maximum_tree_depth_after_widening = 4;
      maximum_return_access_path_width = 10;
      maximum_return_access_path_depth_after_widening = 4;
      maximum_tito_collapse_depth = 4;
      maximum_tito_positions = 50;
      (* DOCUMENTATION_CONFIGURATION_END *)
      maximum_overrides_to_analyze = None;
      maximum_trace_length = None;
      maximum_tito_depth = None;
    }


  let override_with
      ~maximum_model_source_tree_width
      ~maximum_model_sink_tree_width
      ~maximum_model_tito_tree_width
      ~maximum_tree_depth_after_widening
      ~maximum_return_access_path_width
      ~maximum_return_access_path_depth_after_widening
      ~maximum_tito_collapse_depth
      ~maximum_tito_positions
      ~maximum_overrides_to_analyze
      ~maximum_trace_length
      ~maximum_tito_depth
      constraints
    =
    {
      maximum_model_source_tree_width =
        Option.value
          maximum_model_source_tree_width
          ~default:constraints.maximum_model_source_tree_width;
      maximum_model_sink_tree_width =
        Option.value
          maximum_model_sink_tree_width
          ~default:constraints.maximum_model_sink_tree_width;
      maximum_model_tito_tree_width =
        Option.value
          maximum_model_tito_tree_width
          ~default:constraints.maximum_model_tito_tree_width;
      maximum_tree_depth_after_widening =
        Option.value
          maximum_tree_depth_after_widening
          ~default:constraints.maximum_tree_depth_after_widening;
      maximum_return_access_path_width =
        Option.value
          maximum_return_access_path_width
          ~default:constraints.maximum_return_access_path_width;
      maximum_return_access_path_depth_after_widening =
        Option.value
          maximum_return_access_path_depth_after_widening
          ~default:constraints.maximum_return_access_path_depth_after_widening;
      maximum_tito_collapse_depth =
        Option.value maximum_tito_collapse_depth ~default:constraints.maximum_tito_collapse_depth;
      maximum_tito_positions =
        Option.value maximum_tito_positions ~default:constraints.maximum_tito_positions;
      maximum_overrides_to_analyze =
        (match maximum_overrides_to_analyze with
        | None -> constraints.maximum_overrides_to_analyze
        | Some _ -> maximum_overrides_to_analyze);
      maximum_trace_length =
        (match maximum_trace_length with
        | None -> constraints.maximum_trace_length
        | Some _ -> maximum_trace_length);
      maximum_tito_depth =
        (match maximum_tito_depth with
        | None -> constraints.maximum_tito_depth
        | Some _ -> maximum_tito_depth);
    }
end

type partial_sink_converter = (Sources.t list * Sinks.t) list String.Map.Tree.t

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


(** Taint configuration, stored in the ocaml heap. *)
module Heap = struct
  type t = {
    sources: AnnotationParser.source_or_sink list;
    sinks: AnnotationParser.source_or_sink list;
    transforms: TaintTransform.t list;
    filtered_sources: Sources.Set.t option;
    filtered_sinks: Sinks.Set.t option;
    filtered_transforms: TaintTransform.t list option;
    features: string list;
    rules: Rule.t list;
    filtered_rule_codes: Rule.CodeSet.t option;
    implicit_sinks: implicit_sinks;
    implicit_sources: implicit_sources;
    partial_sink_converter: partial_sink_converter;
    partial_sink_labels: string list String.Map.Tree.t;
    find_missing_flows: Configuration.MissingFlowKind.t option;
    dump_model_query_results_path: PyrePath.t option;
    analysis_model_constraints: ModelConstraints.t;
    lineage_analysis: bool;
    source_sink_filter: SourceSinkFilter.t;
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
      analysis_model_constraints = ModelConstraints.default;
      lineage_analysis = false;
      source_sink_filter = SourceSinkFilter.all;
    }


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
            [
              Sources.NamedSource "Cookies"; Sources.NamedSource "PII"; Sources.NamedSource "Secrets";
            ];
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
      analysis_model_constraints = ModelConstraints.default;
      lineage_analysis = false;
      source_sink_filter =
        SourceSinkFilter.create
          ~rules
          ~filtered_rule_codes:None
          ~filtered_sources:None
          ~filtered_sinks:None
          ~filtered_transforms:None;
    }
end

(** Taint configuration, stored in shared memory. *)
module SharedMemory = struct
  module T =
    Memory.WithCache.Make
      (Memory.SingletonKey)
      (struct
        type t = Heap.t

        let prefix = Prefix.make ()

        let description = "Taint configuration"
      end)

  type t = Handle

  let from_heap configuration =
    let key = Memory.SingletonKey.key in
    let () =
      if T.mem key then
        T.remove_batch (T.KeySet.singleton key)
    in
    let () = T.add key configuration in
    Handle


  let get Handle =
    match T.get Memory.SingletonKey.key with
    | None -> failwith "taint configuration not in shared memory"
    | Some configuration -> configuration


  (* Get the current registered taint configuration.
   * Prefer to use `get` whenever possible. *)
  let get_global () =
    if Memory.is_initialized () then
      T.get Memory.SingletonKey.key
    else
      None
end

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

(** Parse json files to create a taint configuration. *)
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
  parse_integer_option "maximum_model_source_tree_width"
  >>= fun maximum_model_source_tree_width ->
  parse_integer_option "maximum_model_sink_tree_width"
  >>= fun maximum_model_sink_tree_width ->
  parse_integer_option "maximum_model_tito_tree_width"
  >>= fun maximum_model_tito_tree_width ->
  parse_integer_option "maximum_tree_depth_after_widening"
  >>= fun maximum_tree_depth_after_widening ->
  parse_integer_option "maximum_return_access_path_width"
  >>= fun maximum_return_access_path_width ->
  parse_integer_option "maximum_return_access_path_depth_after_widening"
  >>= fun maximum_return_access_path_depth_after_widening ->
  parse_integer_option "maximum_tito_collapse_depth"
  >>= fun maximum_tito_collapse_depth ->
  parse_integer_option "maximum_tito_positions"
  >>= fun maximum_tito_positions ->
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
    Heap.sources;
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
      ModelConstraints.override_with
        ~maximum_model_source_tree_width
        ~maximum_model_sink_tree_width
        ~maximum_model_tito_tree_width
        ~maximum_tree_depth_after_widening
        ~maximum_return_access_path_width
        ~maximum_return_access_path_depth_after_widening
        ~maximum_tito_collapse_depth
        ~maximum_tito_positions
        ~maximum_overrides_to_analyze
        ~maximum_trace_length
        ~maximum_tito_depth
        ModelConstraints.default;
    lineage_analysis;
    source_sink_filter =
      SourceSinkFilter.create
        ~rules
        ~filtered_rule_codes:None
        ~filtered_sources:None
        ~filtered_sinks:None
        ~filtered_transforms:None;
  }


(** Perform additional checks on the taint configuration. *)
let validate ({ Heap.sources; sinks; transforms; features; _ } as configuration) =
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


let obscure_flows_configuration configuration =
  let rules =
    [
      {
        Rule.sources =
          List.map
            ~f:(fun { name = source; _ } -> Sources.NamedSource source)
            configuration.Heap.sources;
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
      SourceSinkFilter.create
        ~rules
        ~filtered_rule_codes:None
        ~filtered_sources:configuration.filtered_sources
        ~filtered_sinks:None
        ~filtered_transforms:None;
  }


let missing_type_flows_configuration configuration =
  let rules =
    [
      {
        Rule.sources =
          List.map
            ~f:(fun { name = source; _ } -> Sources.NamedSource source)
            configuration.Heap.sources;
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
      SourceSinkFilter.create
        ~rules
        ~filtered_rule_codes:None
        ~filtered_sources:configuration.filtered_sources
        ~filtered_sinks:None
        ~filtered_transforms:None;
  }


let apply_missing_flows configuration = function
  | Configuration.MissingFlowKind.Obscure -> obscure_flows_configuration configuration
  | Configuration.MissingFlowKind.Type -> missing_type_flows_configuration configuration


(** Create a taint configuration by finding `.config` files in the given directories. *)
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


(** Update a taint configuration with the given command line options. *)
let with_command_line_options
    configuration
    ~rule_filter
    ~source_filter
    ~sink_filter
    ~transform_filter
    ~find_missing_flows
    ~dump_model_query_results_path
    ~maximum_model_source_tree_width
    ~maximum_model_sink_tree_width
    ~maximum_model_tito_tree_width
    ~maximum_tree_depth_after_widening
    ~maximum_return_access_path_width
    ~maximum_return_access_path_depth_after_widening
    ~maximum_tito_collapse_depth
    ~maximum_tito_positions
    ~maximum_overrides_to_analyze
    ~maximum_trace_length
    ~maximum_tito_depth
  =
  let open Result in
  (match source_filter with
  | None -> Ok configuration
  | Some source_filter ->
      let parse_source_reference source =
        AnnotationParser.parse_source ~allowed:configuration.Heap.sources source
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
  let analysis_model_constraints =
    ModelConstraints.override_with
      ~maximum_model_source_tree_width
      ~maximum_model_sink_tree_width
      ~maximum_model_tito_tree_width
      ~maximum_tree_depth_after_widening
      ~maximum_return_access_path_width
      ~maximum_return_access_path_depth_after_widening
      ~maximum_tito_collapse_depth
      ~maximum_tito_positions
      ~maximum_overrides_to_analyze
      ~maximum_trace_length
      ~maximum_tito_depth
      configuration.analysis_model_constraints
  in
  let configuration =
    match rule_filter with
    | None -> configuration
    | Some rule_filter ->
        let filtered_rule_codes = Rule.CodeSet.of_list rule_filter in
        { configuration with filtered_rule_codes = Some filtered_rule_codes }
  in
  let rules =
    SourceSinkFilter.filter_rules
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
    analysis_model_constraints;
    rules;
    implicit_sources;
    implicit_sinks;
    source_sink_filter;
  }


let code_metadata { Heap.rules; _ } =
  `Assoc (List.map rules ~f:(fun rule -> Format.sprintf "%d" rule.code, `String rule.name))


let conditional_test_sinks { Heap.implicit_sinks = { conditional_test; _ }; _ } = conditional_test

let literal_string_sinks { Heap.implicit_sinks = { literal_string_sinks; _ }; _ } =
  literal_string_sinks


let get_triggered_sink { Heap.partial_sink_converter; _ } ~partial_sink ~source =
  PartialSinkConverter.get_triggered_sink partial_sink_converter ~partial_sink ~source


let is_missing_flow_analysis { Heap.find_missing_flows; _ } kind =
  Option.equal Configuration.MissingFlowKind.equal (Some kind) find_missing_flows


let literal_string_sources { Heap.implicit_sources = { literal_strings; _ }; _ } = literal_strings

let maximum_model_source_tree_width
    { Heap.analysis_model_constraints = { maximum_model_source_tree_width; _ }; _ }
  =
  maximum_model_source_tree_width


let maximum_model_sink_tree_width
    { Heap.analysis_model_constraints = { maximum_model_sink_tree_width; _ }; _ }
  =
  maximum_model_sink_tree_width


let maximum_model_tito_tree_width
    { Heap.analysis_model_constraints = { maximum_model_tito_tree_width; _ }; _ }
  =
  maximum_model_tito_tree_width


let maximum_tree_depth_after_widening
    { Heap.analysis_model_constraints = { maximum_tree_depth_after_widening; _ }; _ }
  =
  maximum_tree_depth_after_widening


let maximum_return_access_path_width
    { Heap.analysis_model_constraints = { maximum_return_access_path_width; _ }; _ }
  =
  maximum_return_access_path_width


let maximum_return_access_path_depth_after_widening
    { Heap.analysis_model_constraints = { maximum_return_access_path_depth_after_widening; _ }; _ }
  =
  maximum_return_access_path_depth_after_widening


let maximum_tito_positions { Heap.analysis_model_constraints = { maximum_tito_positions; _ }; _ } =
  maximum_tito_positions


let maximum_overrides_to_analyze
    { Heap.analysis_model_constraints = { maximum_overrides_to_analyze; _ }; _ }
  =
  maximum_overrides_to_analyze


let maximum_trace_length { Heap.analysis_model_constraints = { maximum_trace_length; _ }; _ } =
  maximum_trace_length


let maximum_tito_depth { Heap.analysis_model_constraints = { maximum_tito_depth; _ }; _ } =
  maximum_tito_depth


let runtime_check_invariants () =
  (* This is enabled in tests or when using `--check-invariants`. *)
  Sys.getenv "PYSA_CHECK_INVARIANTS" |> Option.is_some
