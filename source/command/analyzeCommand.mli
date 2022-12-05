(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ExitStatus : sig
  type t =
    | CheckStatus of CheckCommand.ExitStatus.t
    | TaintConfigurationError
    | ModelVerificationError

  val exit_code : t -> int
end

module AnalyzeConfiguration : sig
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    dump_call_graph: PyrePath.t option;
    dump_model_query_results: PyrePath.t option;
    find_missing_flows: Configuration.MissingFlowKind.t option;
    inline_decorators: bool;
    maximum_model_source_tree_width: int option;
    maximum_model_sink_tree_width: int option;
    maximum_model_tito_tree_width: int option;
    maximum_tree_depth_after_widening: int option;
    maximum_return_access_path_width: int option;
    maximum_return_access_path_depth_after_widening: int option;
    maximum_tito_collapse_depth: int option;
    maximum_tito_positions: int option;
    maximum_overrides_to_analyze: int option;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
    no_verify: bool;
    verify_dsl: bool;
    repository_root: PyrePath.t option;
    rule_filter: int list option;
    source_filter: string list option;
    sink_filter: string list option;
    transform_filter: string list option;
    save_results_to: PyrePath.t option;
    output_format: Configuration.TaintOutputFormat.t;
    strict: bool;
    taint_model_paths: PyrePath.t list;
    use_cache: bool;
    check_invariants: bool;
    limit_entrypoints: bool;
  }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val command : Command.t
