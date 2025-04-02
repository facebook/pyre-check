(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Data_structures
open Ast
open Expression
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment

(** Represents type information about the return type of a call. *)
module ReturnType : sig
  type t = {
    is_boolean: bool;
    is_integer: bool;
    is_float: bool;
    is_enumeration: bool;
  }
  [@@deriving eq, show]

  val any : t

  val none : t

  val bool : t

  val integer : t

  val from_annotation : pyre_api:PyrePysaEnvironment.ReadOnly.t -> Type.t -> t

  val to_json : t -> Yojson.Safe.t
end

(** A specific target of a given call, with extra information. *)
module CallTarget : sig
  type t = {
    target: Target.t;
    (* True if the call has an implicit receiver.
     * For instance, `x.foo()` should be treated as `C.foo(x)`. *)
    implicit_receiver: bool;
    (* True if this is an implicit call to the `__call__` method. *)
    implicit_dunder_call: bool;
    (* The textual order index of the call in the function. *)
    index: int;
    (* The return type of the call expression, or `None` for object targets. *)
    return_type: ReturnType.t option;
    (* The class of the receiver object at this call site, if any. *)
    receiver_class: string option; (* True if calling a class method. *)
    is_class_method: bool;
    (* True if calling a static method. *)
    is_static_method: bool;
  }
  [@@deriving eq, show, compare]

  module Set : sig
    type call_target = t

    type t [@@deriving show, eq]

    val of_list : call_target list -> t

    val join : t -> t -> t

    val bottom : t

    val is_bottom : t -> bool
  end

  val target : t -> Target.t

  val create
    :  ?implicit_receiver:bool ->
    ?implicit_dunder_call:bool ->
    ?index:int ->
    ?return_type:ReturnType.t option ->
    ?receiver_class:string ->
    ?is_class_method:bool ->
    ?is_static_method:bool ->
    Target.t ->
    t

  (* For testing purpose only. *)
  val create_regular
    :  ?implicit_receiver:bool ->
    ?implicit_dunder_call:bool ->
    ?index:int ->
    ?return_type:ReturnType.t option ->
    ?receiver_class:string ->
    ?is_class_method:bool ->
    ?is_static_method:bool ->
    Target.Regular.t ->
    t

  val to_json : t -> Yojson.Safe.t
end

module ImplicitArgument : sig
  type t =
    | CalleeBase
    | Callee
    | None
  [@@deriving show]

  val implicit_argument : ?is_implicit_new:bool -> CallTarget.t -> t
end

module Unresolved : sig
  type bypassing_decorators =
    | NonMethodAttribute
    | CannotFindAttribute
    | CannotResolveExports
    | CannotFindParentClass
    | UnknownBaseType
    | UnknownCallCallee
    | UnknownIdentifierCallee
    | UnknownCalleeAST
  [@@deriving eq, show]

  type reason =
    | BypassingDecorators of bypassing_decorators
    | UnrecognizedCallee
    | AnonymousCallableType
    | UnknownConstructorCallable
    | AnyTopCallableClass
    | UnknownCallableProtocol
    | UnknownCallableClass
    | LambdaArgument
    | NoRecordInCallGraph

  type t =
    | True of reason
    | False
  [@@deriving eq, show]

  val is_unresolved : t -> bool
end

(** Information about an argument being a callable. *)
module HigherOrderParameter : sig
  type t = {
    index: int;
    call_targets: CallTarget.t list;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: Unresolved.t;
  }
  [@@deriving eq, show]

  val to_json : t -> Yojson.Safe.t
end

(** Mapping from a parameter index to its HigherOrderParameter, if any. *)
module HigherOrderParameterMap : sig
  type t [@@deriving eq, show]

  val empty : t

  val is_empty : t -> bool

  val from_list : HigherOrderParameter.t list -> t

  val to_list : t -> HigherOrderParameter.t list

  val first_index : t -> HigherOrderParameter.t option

  val to_json : t -> Yojson.Safe.t
end

(** An aggregate of all possible callees at a call site. *)
module CallCallees : sig
  module RecognizedCall : sig
    type t =
      | True
      | False
      | Unknown
  end

  type t = {
    (* Normal call targets. *)
    call_targets: CallTarget.t list;
    (* Call targets for calls to the `__new__` class method. *)
    new_targets: CallTarget.t list;
    (* Call targets for calls to the `__init__` instance method. *)
    init_targets: CallTarget.t list;
    (* Call targets for the calls to artificially created callables that call the decorators. Only
       used by call graph building. *)
    decorated_targets: CallTarget.t list;
    (* Information about arguments that are callables, and possibly called. *)
    higher_order_parameters: HigherOrderParameterMap.t;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: Unresolved.t;
    recognized_call: RecognizedCall.t;
  }
  [@@deriving eq, show]

  val create
    :  ?call_targets:CallTarget.t list ->
    ?new_targets:CallTarget.t list ->
    ?init_targets:CallTarget.t list ->
    ?decorated_targets:CallTarget.t list ->
    ?higher_order_parameters:HigherOrderParameterMap.t ->
    ?unresolved:Unresolved.t ->
    ?recognized_call:RecognizedCall.t ->
    unit ->
    t

  (* When `debug` is true, log the message. *)
  val unresolved : ?debug:bool -> reason:Unresolved.reason -> message:string -> unit -> t

  val is_partially_resolved : t -> bool

  val pp_option : Format.formatter -> t option -> unit

  val is_mapping_method : t -> bool

  val is_sequence_method : t -> bool

  val is_string_method : t -> bool

  val is_object_new : CallTarget.t list -> bool

  val is_object_init : CallTarget.t list -> bool

  val to_json : t -> Yojson.Safe.t
end

(** An aggregrate of all possible callees for a given attribute access. *)
module AttributeAccessCallees : sig
  type t = {
    property_targets: CallTarget.t list;
    global_targets: CallTarget.t list;
    (* True if the attribute access should also be considered a regular attribute.
     * For instance, if the object has type `Union[A, B]` where only `A` defines a property. *)
    is_attribute: bool;
    (* Function-typed runtime values that the attribute access may evaluate into. *)
    callable_targets: CallTarget.t list;
    (* Call targets for the calls to artificially created callables that call the decorators. Only
       used by call graph building. *)
    decorated_targets: CallTarget.t list;
  }
  [@@deriving eq, show]

  val create
    :  ?property_targets:CallTarget.t list ->
    ?global_targets:CallTarget.t list ->
    ?callable_targets:CallTarget.t list ->
    ?is_attribute:bool ->
    ?decorated_targets:CallTarget.t list ->
    unit ->
    t

  val empty : t

  val to_json : t -> Yojson.Safe.t
end

(** An aggregate of all possible callees for a given identifier expression, i.e `foo`. *)
module IdentifierCallees : sig
  type t = {
    global_targets: CallTarget.t list;
    nonlocal_targets: CallTarget.t list;
    (* Function-typed runtime values that the identifier may evaluate into. *)
    callable_targets: CallTarget.t list;
    (* Call targets for the calls to artificially created callables that call the decorators. Only
       used by call graph building. *)
    decorated_targets: CallTarget.t list;
  }
  [@@deriving eq, show]

  val create
    :  ?global_targets:CallTarget.t list ->
    ?nonlocal_targets:CallTarget.t list ->
    ?callable_targets:CallTarget.t list ->
    ?decorated_targets:CallTarget.t list ->
    unit ->
    t

  val to_json : t -> Yojson.Safe.t
end

(** An aggregate of callees for formatting strings. *)
module StringFormatCallees : sig
  type t = {
    (* Implicit callees for any expression that is stringified. *)
    stringify_targets: CallTarget.t list;
    (* Artificial callees for distinguishing f-strings within a function. *)
    f_string_targets: CallTarget.t list;
  }
  [@@deriving eq, show]

  val from_stringify_targets : CallTarget.t list -> t

  val from_f_string_targets : CallTarget.t list -> t

  val to_json : t -> Yojson.Safe.t
end

(** An aggregate of all possible callees for an arbitrary expression. *)
module ExpressionCallees : sig
  type t = {
    call: CallCallees.t option;
    attribute_access: AttributeAccessCallees.t option;
    identifier: IdentifierCallees.t option;
    string_format: StringFormatCallees.t option;
  }
  [@@deriving eq, show]

  val from_call : CallCallees.t -> t

  val from_attribute_access : AttributeAccessCallees.t -> t

  val from_identifier : IdentifierCallees.t -> t

  val from_string_format : StringFormatCallees.t -> t

  val to_json : t -> Yojson.Safe.t
end

(** An aggregate of all possible callees for an arbitrary location.

    Note that multiple expressions might have the same location. *)
module LocationCallees : sig
  type t =
    | Singleton of ExpressionCallees.t
    | Compound of ExpressionCallees.t SerializableStringMap.t
  [@@deriving eq, show]

  val to_json : t -> Yojson.Safe.t
end

(* Exposed for rare use cases, such as resolving the callees of decorators. *)
val resolve_callees_from_type_external
  :  pyre_in_context:PyrePysaEnvironment.InContext.t ->
  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  return_type:Type.t lazy_t ->
  ?dunder_call:bool ->
  Expression.t ->
  CallCallees.t

(** The call graph of a function or method definition. This is for testing purpose only. *)
module DefineCallGraphForTest : sig
  type t [@@deriving eq, show]

  val empty : t

  val add : t -> location:Ast.Location.t -> callees:LocationCallees.t -> t

  val equal_ignoring_types : t -> t -> bool
end

module AllTargetsUseCase : sig
  type t =
    | TaintAnalysisDependency
    | CallGraphDependency
    | Everything
end

(** The call graph of a function or method definition. *)
module DefineCallGraph : sig
  type t [@@deriving show, eq]

  val empty : t

  val add_callees
    :  expression_identifier:string ->
    location:Location.t ->
    callees:ExpressionCallees.t ->
    t ->
    t

  (** Return all callees of the call graph, depending on the use case. *)
  val all_targets : use_case:AllTargetsUseCase.t -> t -> Target.t list

  val for_test : t -> DefineCallGraphForTest.t

  val resolve_call
    :  t ->
    location:Ast.Location.t ->
    call:Ast.Expression.Call.t ->
    CallCallees.t option

  val resolve_attribute_access
    :  t ->
    location:Ast.Location.t ->
    attribute:string ->
    AttributeAccessCallees.t option

  val resolve_identifier
    :  t ->
    location:Ast.Location.t ->
    identifier:string ->
    IdentifierCallees.t option

  val resolve_string_format : t -> location:Ast.Location.t -> StringFormatCallees.t option

  (* Ensure the taint analysis does not use these targets. *)
  val drop_decorated_targets : t -> t
end

module DecoratorDefine : sig
  type t = {
    define: Ast.Statement.Define.t;
    (* An artificial define that returns the call to the decorators. *)
    callable: Target.t;
    (* `Target` representation of the above define. *)
    call_graph: DefineCallGraph.t; (* Call graph of the above define. *)
  }
  [@@deriving show, eq]
end

(* A map from each callable to its decorators. *)
module CallableToDecoratorsMap : sig
  module SharedMemory : sig
    type t

    module ReadOnly : sig
      type t

      val get_decorators : t -> Target.t -> Expression.t list option
    end

    val read_only : t -> ReadOnly.t

    val empty : unit -> t

    (* We assume `DecoratorPreprocessing.setup_preprocessing` is called before since we use its
       shared memory here. *)
    val create
      :  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
      scheduler:Scheduler.t ->
      scheduler_policy:Scheduler.Policy.t ->
      Target.t list ->
      t

    val cleanup : t -> unit

    val save_decorator_counts_to_directory
      :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
      scheduler:Scheduler.t ->
      t ->
      unit
  end
end

val call_graph_of_define
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  pyre_api:PyrePysaEnvironment.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  attribute_targets:Target.HashSet.t ->
  decorators:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
  qualifier:Reference.t ->
  define:Ast.Statement.Define.t ->
  DefineCallGraph.t

val redirect_expressions
  :  pyre_in_context:PyrePysaEnvironment.InContext.t ->
  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
  location:Location.t ->
  Expression.expression ->
  Expression.expression

val redirect_assignments : Statement.t -> Statement.t

val call_graph_of_callable
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  pyre_api:PyrePysaEnvironment.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
  attribute_targets:Target.HashSet.t ->
  decorators:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
  callable:Target.t ->
  DefineCallGraph.t

(* The result of finding higher order function callees inside a callable. *)
module HigherOrderCallGraph : sig
  type t = {
    returned_callables: CallTarget.Set.t;
    call_graph: DefineCallGraph.t;
        (* Higher order function callees (i.e., parameterized targets) and potentially regular
           callees. *)
  }
  [@@deriving eq, show]

  val empty : t

  val merge : t -> t -> t

  val save_to_directory
    :  scheduler:Scheduler.t ->
    static_analysis_configuration:Configuration.StaticAnalysis.t ->
    callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
    resolve_module_path:(Reference.t -> RepositoryPath.t option) option ->
    get_call_graph:(Target.t -> t option) ->
    json_kind:NewlineDelimitedJson.Kind.t ->
    filename_prefix:string ->
    callables:Target.t list ->
    unit

  module State : sig
    type t

    val empty : t

    val initialize_from_roots
      :  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
      (TaintAccessPath.Root.t * Target.t) list ->
      t

    val initialize_from_callable
      :  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
      Target.t ->
      t
  end
end

val debug_higher_order_call_graph : Ast.Statement.Define.t -> bool

val higher_order_call_graph_of_define
  :  define_call_graph:DefineCallGraph.t ->
  pyre_api:PyrePysaEnvironment.ReadOnly.t ->
  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
  callable:Target.t option ->
  qualifier:Reference.t ->
  define:Ast.Statement.Define.t ->
  initial_state:HigherOrderCallGraph.State.t ->
  get_callee_model:(Target.t -> HigherOrderCallGraph.t option) ->
  profiler:CallGraphProfiler.t ->
  maximum_target_depth:int ->
  maximum_parameterized_targets_at_call_site:int option ->
  HigherOrderCallGraph.t

module DecoratorResolution : sig
  type t =
    | Decorators of DecoratorDefine.t
    | PropertySetterUnsupported
    | Undecorated
      (* A callable is `Undecorated` if it does not have any decorator, or all of its decorators are
         ignored. *)
  [@@deriving show, eq]

  (**
   * For any target that might be decorated, return the `ResolvedExpression` for the expression that calls the decorators.
   *
   * For instance:
   * ```
   * @decorator
   * @decorator_factory(1, 2)
   * def foo(): pass
   * ```
   * would resolve into expression `decorator(decorator_factory(1, 2)(foo))`, along with its callees that are stored in `call_graph`.
   *)
  val resolve_exn
    :  ?debug:bool ->
    pyre_in_context:PyrePysaEnvironment.InContext.t ->
    override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
    callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
    decorators:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
    Target.t ->
    t

  module Results : sig
    type t

    val empty : t

    val resolve_batch_exn
      :  debug:bool ->
      pyre_api:PyrePysaEnvironment.ReadOnly.t ->
      scheduler:Scheduler.t ->
      scheduler_policy:Scheduler.Policy.t ->
      override_graph:OverrideGraph.SharedMemory.t ->
      callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
      decorators:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
      Target.t list ->
      t

    val register_decorator_defines
      :  decorator_resolution:t ->
      Target.CallablesSharedMemory.t ->
      Target.CallablesSharedMemory.t

    val decorated_targets : t -> Target.t list
  end
end

(** Whole-program call graph, stored in the ocaml heap. This is a mapping from a callable to all its
    callees. *)
module WholeProgramCallGraph : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val of_alist_exn : (Target.t * Target.t list) list -> t

  val add_or_exn : callable:Target.t -> callees:Target.t list -> t -> t

  val fold : t -> init:'a -> f:(target:Target.t -> callees:Target.t list -> 'a -> 'a) -> 'a

  val to_target_graph : t -> TargetGraph.t

  val merge_disjoint : t -> t -> t
end

(** Call graphs of callables, stored in the shared memory. This is a mapping from a callable to its
    `DefineCallGraph.t`. *)
module SharedMemory : sig
  type t

  module ReadOnly : sig
    type t

    val get : t -> cache:bool -> callable:Target.t -> DefineCallGraph.t option
  end

  val callables : t -> Target.t list

  val read_only : t -> ReadOnly.t

  val cleanup : t -> unit

  val save_to_cache : t -> unit

  val load_from_cache : unit -> (t, SaveLoadSharedMemory.Usage.t) result

  type call_graphs = {
    whole_program_call_graph: WholeProgramCallGraph.t;
    define_call_graphs: t;
  }

  val default_scheduler_policy : Scheduler.Policy.t

  (** Build the whole call graph of the program.

      The overrides must be computed first because we depend on a global shared memory graph to
      include overrides in the call graph. Without it, we'll underanalyze and have an inconsistent
      fixpoint. *)
  val build_whole_program_call_graph
    :  scheduler:Scheduler.t ->
    static_analysis_configuration:Configuration.StaticAnalysis.t ->
    pyre_api:PyrePysaEnvironment.ReadOnly.t ->
    resolve_module_path:(Reference.t -> RepositoryPath.t option) option ->
    callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
    override_graph:OverrideGraph.SharedMemory.ReadOnly.t option ->
    store_shared_memory:bool ->
    attribute_targets:Target.Set.t ->
    decorators:CallableToDecoratorsMap.SharedMemory.ReadOnly.t ->
    decorator_resolution:DecoratorResolution.Results.t ->
    skip_analysis_targets:Target.Set.t ->
    definitions:Target.t list ->
    create_dependency_for:AllTargetsUseCase.t ->
    call_graphs
end
