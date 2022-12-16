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
}

let create
    ?(populate_call_graph = false)
    ?in_memory_sources
    ?(no_validation_on_class_lookup_failure = false)
    ?(use_lazy_module_tracking = false)
    configuration
  =
  {
    configuration;
    populate_call_graph;
    in_memory_sources;
    no_validation_on_class_lookup_failure;
    use_lazy_module_tracking;
  }


let create_for_overlay parent = { parent with populate_call_graph = false }

let configuration { configuration; _ } = configuration

let populate_call_graph { populate_call_graph; _ } = populate_call_graph

let use_lazy_module_tracking { use_lazy_module_tracking; _ } = use_lazy_module_tracking

let no_validation_on_class_lookup_failure { no_validation_on_class_lookup_failure; _ } =
  no_validation_on_class_lookup_failure


let track_dependencies { configuration = { Configuration.Analysis.incremental_style; _ }; _ } =
  match incremental_style with
  | Configuration.Analysis.Shallow -> false
  | Configuration.Analysis.FineGrained -> true


let debug { configuration = { Configuration.Analysis.debug; _ }; _ } = debug

let in_memory_sources { in_memory_sources; _ } = in_memory_sources

let assert_allow_updates controls =
  if not (track_dependencies controls) then
    failwith "Environments without dependency tracking cannot be updated";
  if Option.is_some (in_memory_sources controls) then
    failwith "Environments created using in-memory sources cannot be updated";
  ()


module PythonVersionInfo = struct
  type t = {
    major_version: int;
    minor_version: int;
    micro_version: int;
  }
end

let python_version_info
    {
      configuration =
        {
          Configuration.Analysis.python_major_version = major_version;
          python_minor_version = minor_version;
          python_micro_version = micro_version;
          _;
        };
      _;
    }
  =
  { PythonVersionInfo.major_version; minor_version; micro_version }


module TypeCheckControls = struct
  type t = {
    constraint_solving_style: Configuration.Analysis.constraint_solving_style;
    include_type_errors: bool;
    include_local_annotations: bool;
    include_readonly_errors: bool;
    include_unawaited_awaitable_errors: bool;
    debug: bool;
  }
end

let type_check_controls
    {
      configuration =
        {
          Configuration.Analysis.debug;
          store_type_errors;
          store_type_check_resolution;
          constraint_solving_style;
          enable_readonly_analysis;
          enable_unawaited_awaitable_analysis;
          _;
        };
      _;
    }
  =
  TypeCheckControls.
    {
      debug;
      constraint_solving_style;
      include_type_errors = store_type_errors;
      include_local_annotations = store_type_check_resolution;
      include_readonly_errors = enable_readonly_analysis;
      include_unawaited_awaitable_errors = enable_unawaited_awaitable_analysis;
    }
