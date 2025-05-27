(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

type raw_code = string

type t = {
  configuration: Configuration.Analysis.t;
  populate_call_graph: bool;
  in_memory_sources: (Ast.ModulePath.t * raw_code) list option;
  no_validation_on_class_lookup_failure: bool;
  use_lazy_module_tracking: bool;
  string_annotation_preserve_location: bool;
}

let create
    ?(populate_call_graph = false)
    ?in_memory_sources
    ?(no_validation_on_class_lookup_failure = false)
    ?(use_lazy_module_tracking = false)
    ?(string_annotation_preserve_location = true)
    configuration
  =
  {
    configuration;
    populate_call_graph;
    in_memory_sources;
    no_validation_on_class_lookup_failure;
    use_lazy_module_tracking;
    string_annotation_preserve_location;
  }


let create_for_overlay parent = { parent with populate_call_graph = false }

let configuration { configuration; _ } = configuration

let populate_call_graph { populate_call_graph; _ } = populate_call_graph

let use_lazy_module_tracking { use_lazy_module_tracking; _ } = use_lazy_module_tracking

let string_annotation_preserve_location { string_annotation_preserve_location; _ } =
  string_annotation_preserve_location


let no_validation_on_class_lookup_failure { no_validation_on_class_lookup_failure; _ } =
  no_validation_on_class_lookup_failure


let track_dependencies { configuration = { Configuration.Analysis.track_dependencies; _ }; _ } =
  track_dependencies


let debug { configuration = { Configuration.Analysis.debug; _ }; _ } = debug

let in_memory_sources { in_memory_sources; _ } = in_memory_sources

let assert_allow_updates controls =
  if not (track_dependencies controls) then
    failwith "Environments without dependency tracking cannot be updated";
  if Option.is_some (in_memory_sources controls) then
    failwith "Environments created using in-memory sources cannot be updated";
  ()


module TypeCheckControls = struct
  type t = {
    include_type_errors: bool;
    include_local_annotations: bool;
    include_readonly_errors: bool;
    include_strict_override_errors: bool;
    include_strict_any_errors: bool;
    include_unawaited_awaitable_errors: bool;
    debug: bool;
    include_suppressed_errors: bool;
    no_validation_on_class_lookup_failure: bool;
  }
end

let type_check_controls
    {
      configuration =
        {
          Configuration.Analysis.debug;
          store_type_errors;
          store_type_check_resolution;
          enable_readonly_analysis;
          enable_strict_override_check;
          enable_strict_any_check;
          enable_unawaited_awaitable_analysis;
          include_suppressed_errors;
          _;
        };
      no_validation_on_class_lookup_failure;
      _;
    }
  =
  TypeCheckControls.
    {
      debug;
      include_type_errors = store_type_errors;
      include_local_annotations = store_type_check_resolution;
      include_readonly_errors = enable_readonly_analysis;
      include_strict_override_errors = enable_strict_override_check;
      include_strict_any_errors = enable_strict_any_check;
      include_unawaited_awaitable_errors = enable_unawaited_awaitable_analysis;
      include_suppressed_errors;
      no_validation_on_class_lookup_failure;
    }
