(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module AnalyzeConfiguration : sig
  type t = {
    base: NewCommandStartup.BaseConfiguration.t;
    dump_call_graph: bool;
    dump_model_query_results: bool;
    find_missing_flows: string option;
    inline_decorators: bool;
    maximum_tito_depth: int option;
    maximum_trace_length: int option;
    no_verify: bool;
    repository_root: PyrePath.t option;
    rule_filter: int list option;
    save_results_to: PyrePath.t option;
    strict: bool;
    taint_model_paths: PyrePath.t list;
    use_cache: bool;
  }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val command : Command.t
