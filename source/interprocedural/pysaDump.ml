(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let pysa_dump_env = lazy (Sys.getenv "PYSA_DUMP")

let pysa_dump_call_graph_env = lazy (Sys.getenv "PYSA_DUMP_CALL_GRAPH")

let pysa_dump_higher_order_call_graph_env = lazy (Sys.getenv "PYSA_DUMP_HIGHER_ORDER_CALL_GRAPH")

let pysa_dump_taint_env = lazy (Sys.getenv "PYSA_DUMP_TAINT")

let pysa_dump_cfg_env = lazy (Sys.getenv "PYSA_DUMP_CFG")

let pysa_dump_perf_env = lazy (Sys.getenv "PYSA_DUMP_PERF")

let perf_higher_order_call_graph_env = lazy (Sys.getenv "PYSA_DUMP_PERF_HIGHER_ORDER_CALL_GRAPH")

(* A phase fires when the master switch is on (in-source `pysa_dump()` or `PYSA_DUMP` matching the
   fully-qualified name), or when the phase-specific in-source call or environment variable
   matches. *)
let should_dump ~define ~callable ~in_source_phase ~phase_env =
  let qualified_name = Ast.Reference.show (Target.define_name_exn callable) in
  let matches_env env =
    match Lazy.force env with
    | Some value -> String.equal value qualified_name
    | None -> false
  in
  Ast.Statement.Define.pysa_dump define
  || in_source_phase define
  || matches_env pysa_dump_env
  || matches_env phase_env


let should_dump_call_graph ~define ~callable =
  should_dump
    ~define
    ~callable
    ~in_source_phase:Ast.Statement.Define.pysa_dump_call_graph
    ~phase_env:pysa_dump_call_graph_env


let should_dump_higher_order_call_graph ~define ~callable =
  should_dump
    ~define
    ~callable
    ~in_source_phase:Ast.Statement.Define.pysa_dump_higher_order_call_graph
    ~phase_env:pysa_dump_higher_order_call_graph_env


let should_dump_taint ~define ~callable =
  should_dump
    ~define
    ~callable
    ~in_source_phase:Ast.Statement.Define.pysa_dump_taint
    ~phase_env:pysa_dump_taint_env


let should_dump_cfg ~define ~callable =
  should_dump
    ~define
    ~callable
    ~in_source_phase:Ast.Statement.Define.pysa_dump_cfg
    ~phase_env:pysa_dump_cfg_env


let should_dump_perf ~define ~callable =
  should_dump
    ~define
    ~callable
    ~in_source_phase:Ast.Statement.Define.pysa_dump_perf
    ~phase_env:pysa_dump_perf_env


let should_dump_perf_higher_order_call_graph ~define ~callable =
  should_dump
    ~define
    ~callable
    ~in_source_phase:Ast.Statement.Define.pysa_dump_perf_higher_order_call_graph
    ~phase_env:perf_higher_order_call_graph_env
