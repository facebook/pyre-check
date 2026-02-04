(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

(* Exposed for rare use cases, such as resolving the callees of decorators. *)
val resolve_callees_from_type_external
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  return_type:Type.t lazy_t ->
  ?dunder_call:bool ->
  Expression.t ->
  CallGraph.CallCallees.t

val call_graph_of_define
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  attribute_targets:Target.HashSet.t ->
  callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
  callables_to_decorators_map:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  check_invariants:bool ->
  qualifier:Reference.t ->
  callable:Target.t ->
  define:Ast.Statement.Define.t Node.t ->
  CallGraph.DefineCallGraph.t

(* This must be called *once* before analyzing a statement in a control flow graph. *)
val preprocess_statement
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  callable:Target.t ->
  Ast.Statement.t ->
  Ast.Statement.t

(* This must be called *once* before analyzing a generator. *)
val preprocess_generator
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  callable:Target.t ->
  Ast.Expression.Comprehension.Generator.t ->
  Ast.Statement.Assign.t * PyrePysaApi.InContext.t

val preprocess_parameter_default_value
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  callable:Target.t ->
  Ast.Expression.t ->
  Ast.Expression.t

val call_graph_of_callable
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  attribute_targets:Target.HashSet.t ->
  callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
  callables_to_decorators_map:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  check_invariants:bool ->
  callable:Target.t ->
  CallGraph.DefineCallGraph.t

val call_graph_of_decorated_callable
  :  debug:bool ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
  callables_to_decorators_map:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  callable:Target.t ->
  body:CallableToDecoratorsMap.DecoratedDefineBody.t ->
  CallGraph.DefineCallGraph.t

(* The result of finding higher order function callees inside a callable. *)
module HigherOrderCallGraph : sig
  type t = {
    returned_callables: CallGraph.CallTarget.Set.t;
    call_graph: CallGraph.DefineCallGraph.t;
        (* Higher order function callees (i.e., parameterized targets) and potentially regular
           callees. *)
  }
  [@@deriving equal, show]

  val empty : t

  val merge : t -> t -> t

  val save_to_directory
    :  scheduler:Scheduler.t ->
    static_analysis_configuration:Configuration.StaticAnalysis.t ->
    resolve_qualifier:(Target.t -> Reference.t option) ->
    resolve_module_path:(Reference.t -> RepositoryPath.t option) option ->
    get_call_graph:(Target.t -> t option) ->
    json_kind:NewlineDelimitedJson.Kind.t ->
    filename_prefix:string ->
    callables:Target.t list ->
    unit

  val is_empty : t -> bool

  val to_json_alist : t -> (string * Yojson.Safe.t) list

  module State : sig
    type t

    val empty : t

    val of_list : (Analysis.TaintAccessPath.Root.t * CallGraph.CallTarget.Set.t) list -> t

    val initialize_from_roots
      :  pyre_api:PyrePysaApi.ReadOnly.t ->
      callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
      (Analysis.TaintAccessPath.Root.t * Target.t) list ->
      t

    val initialize_from_callable
      :  pyre_api:PyrePysaApi.ReadOnly.t ->
      callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
      Target.t ->
      t
  end
end

val default_scheduler_policy : Scheduler.Policy.t

val debug_higher_order_call_graph : Ast.Statement.Define.t -> bool

val higher_order_call_graph_of_define
  :  define_call_graph:CallGraph.DefineCallGraph.t ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  skip_analysis_targets:Target.HashSet.t ->
  called_when_parameter:Target.HashSet.t ->
  qualifier:Reference.t ->
  callable:Target.t ->
  define:Ast.Statement.Define.t Node.t ->
  initial_state:HigherOrderCallGraph.State.t ->
  get_callee_model:(Target.t -> HigherOrderCallGraph.t option) ->
  profiler:CallGraphProfiler.t ->
  maximum_target_depth:int ->
  maximum_parameterized_targets_at_call_site:int option ->
  HigherOrderCallGraph.t

(** Build the whole call graph of the program.

    The overrides must be computed first because we depend on a global shared memory graph to
    include overrides in the call graph. Without it, we'll underanalyze and have an inconsistent
    fixpoint. *)
val build_whole_program_call_graph
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  resolve_module_path:(Reference.t -> RepositoryPath.t option) option ->
  callables_to_definitions_map:CallablesSharedMemory.ReadOnly.t ->
  callables_to_decorators_map:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
  global_constants:GlobalConstants.SharedMemory.ReadOnly.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  store_shared_memory:bool ->
  attribute_targets:Target.Set.t ->
  skip_analysis_targets:Target.HashSet.t ->
  check_invariants:bool ->
  definitions:Target.t list ->
  create_dependency_for:CallGraph.AllTargetsUseCase.t ->
  CallGraph.SharedMemory.call_graphs
