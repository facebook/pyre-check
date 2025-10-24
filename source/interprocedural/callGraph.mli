(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

(** Represents type information about the return type of a call. *)
module ReturnType : sig
  type t = Analysis.PyrePysaEnvironment.ScalarTypeProperties.t [@@deriving equal, show]

  val unknown : t

  val none : t

  val bool : t

  val integer : t

  val to_json : t -> Yojson.Safe.t
end

module Indexer : sig
  type t

  val create : unit -> t
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
  [@@deriving equal, show, compare]

  module Set : Abstract.SetDomain.S with type element = t

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

  val map_target : f:(Target.t -> Target.t) -> t -> t

  val regenerate_index : indexer:Indexer.t -> t -> t
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
  [@@deriving equal, show]

  type reason =
    | BypassingDecorators of bypassing_decorators
    | UnrecognizedCallee
    | AnonymousCallableType
    | UnknownCallableFromType
    | UnknownConstructorCallable
    | AnyTopCallableClass
    | UnknownCallableProtocol
    | UnknownCallableClass
    | LambdaArgument
    | NoRecordInCallGraph

  type t =
    | True of reason
    | False
  [@@deriving equal, show]

  val is_unresolved : t -> bool

  val join : t -> t -> t
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
  [@@deriving equal, show]

  val to_json : t -> Yojson.Safe.t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

(** Mapping from a parameter index to its HigherOrderParameter, if any. *)
module HigherOrderParameterMap : sig
  type t [@@deriving equal, show]

  val empty : t

  val is_empty : t -> bool

  val from_list : HigherOrderParameter.t list -> t

  val to_list : t -> HigherOrderParameter.t list

  val find_opt : t -> int -> HigherOrderParameter.t option

  val add : t -> HigherOrderParameter.t -> t

  val join : t -> t -> t

  val first_index : t -> HigherOrderParameter.t option

  val to_json : t -> Yojson.Safe.t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

module ShimTarget : sig
  type t = {
    call_targets: CallTarget.t list;
    decorated_targets: CallTarget.t list;
    argument_mapping: Shims.ShimArgumentMapping.t;
  }
  [@@deriving equal, show]

  val to_json : t -> Yojson.Safe.t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
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
    shim_target: ShimTarget.t option;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: Unresolved.t;
    recognized_call: RecognizedCall.t;
  }
  [@@deriving equal, show]

  val create
    :  ?call_targets:CallTarget.t list ->
    ?new_targets:CallTarget.t list ->
    ?init_targets:CallTarget.t list ->
    ?decorated_targets:CallTarget.t list ->
    ?higher_order_parameters:HigherOrderParameterMap.t ->
    ?shim_target:ShimTarget.t option ->
    ?unresolved:Unresolved.t ->
    ?recognized_call:RecognizedCall.t ->
    unit ->
    t

  (* When `debug` is true, log the message. *)
  val unresolved : ?debug:bool -> reason:Unresolved.reason -> message:string Lazy.t -> unit -> t

  val default_to_unresolved
    :  ?debug:bool ->
    reason:Unresolved.reason ->
    message:string Lazy.t ->
    t option ->
    t

  val is_partially_resolved : t -> bool

  val join : t -> t -> t

  val pp_option : Format.formatter -> t option -> unit

  val is_mapping_method : t -> bool

  val is_sequence_method : t -> bool

  val is_string_method : t -> bool

  val is_object_new : CallTarget.t list -> bool

  val is_object_init : CallTarget.t list -> bool

  val to_json : t -> Yojson.Safe.t

  val map_target : f:(Target.t -> Target.t) -> t -> t

  val should_redirect_to_decorated : t -> bool

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
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
  [@@deriving equal, show]

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

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
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
  [@@deriving equal, show]

  val create
    :  ?global_targets:CallTarget.t list ->
    ?nonlocal_targets:CallTarget.t list ->
    ?callable_targets:CallTarget.t list ->
    ?decorated_targets:CallTarget.t list ->
    unit ->
    t

  val to_json : t -> Yojson.Safe.t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

(** Artificial callees for distinguishing f-strings within a function. *)
module FormatStringArtificialCallees : sig
  type t = { targets: CallTarget.t list } [@@deriving equal, show]

  val from_f_string_targets : CallTarget.t list -> t

  val to_json : t -> Yojson.Safe.t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

(** Implicit callees for any expression that is stringified. *)
module FormatStringStringifyCallees : sig
  type t = { targets: CallTarget.t list } [@@deriving equal, show]

  val from_stringify_targets : CallTarget.t list -> t

  val to_json : t -> Yojson.Safe.t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

module DefineCallees : sig
  type t = {
    define_targets: CallTarget.t list;
    decorated_targets: CallTarget.t list;
  }
  [@@deriving equal, show]

  val create
    :  ?define_targets:CallTarget.t list ->
    ?decorated_targets:CallTarget.t list ->
    unit ->
    t

  val to_json : t -> Yojson.Safe.t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

(* Artificial callees on returned expressions. *)
module ReturnShimCallees : sig
  type argument_mapping =
    | ReturnExpression
    | ReturnExpressionElement

  type t = {
    call_targets: CallTarget.t list;
    arguments: argument_mapping list;
  }
  [@@deriving equal, show]

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

(** An aggregate of all possible callees for an arbitrary expression. *)
module ExpressionCallees : sig
  type t =
    | Call of CallCallees.t
    | AttributeAccess of AttributeAccessCallees.t
    | Identifier of IdentifierCallees.t
    | FormatStringArtificial of FormatStringArtificialCallees.t
    | FormatStringStringify of FormatStringStringifyCallees.t
    | Define of DefineCallees.t
    | Return of ReturnShimCallees.t
  [@@deriving equal, show]

  val from_call : CallCallees.t -> t

  val from_attribute_access : AttributeAccessCallees.t -> t

  val from_identifier : IdentifierCallees.t -> t

  val from_format_string_artificial : FormatStringArtificialCallees.t -> t

  val from_format_string_stringify : FormatStringStringifyCallees.t -> t

  val from_define : DefineCallees.t -> t

  val from_return : ReturnShimCallees.t -> t

  val to_json : t -> Yojson.Safe.t

  val dedup_and_sort : t -> t

  val equal_ignoring_types : t -> t -> bool

  val regenerate_call_indices : indexer:Indexer.t -> t -> t
end

(** The call graph of a function or method definition. This is for testing purpose only. *)
module DefineCallGraphForTest : sig
  type t [@@deriving equal, show]

  val empty : t

  val from_expected : (string * ExpressionCallees.t) list -> t

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
  type t [@@deriving show, equal]

  val empty : t

  val is_empty : t -> bool

  val equal_ignoring_types : t -> t -> bool

  (** Return all callees of the call graph, depending on the use case. *)
  val all_targets : use_case:AllTargetsUseCase.t -> t -> Target.t list

  val for_test : t -> DefineCallGraphForTest.t

  val copy : t -> t

  val merge : t -> t -> t

  val resolve_call
    :  t ->
    location:Ast.Location.t ->
    call:Ast.Expression.Call.t ->
    CallCallees.t option

  val resolve_attribute_access
    :  t ->
    location:Ast.Location.t ->
    attribute_access:Ast.Expression.Name.Attribute.t ->
    AttributeAccessCallees.t option

  val resolve_identifier
    :  t ->
    location:Ast.Location.t ->
    identifier:string ->
    IdentifierCallees.t option

  val resolve_format_string_artificial
    :  t ->
    location:Ast.Location.t ->
    FormatStringArtificialCallees.t option

  val resolve_format_string_stringify
    :  t ->
    location:Ast.Location.t ->
    FormatStringStringifyCallees.t option

  val resolve_define : t -> define_location:Location.t -> DefineCallees.t option

  val resolve_return : t -> statement_location:Location.t -> ReturnShimCallees.t option

  module OnExistingCallees : sig
    type t =
      | WarnThenJoin
      | Join
      | Fail
      | Replace
  end

  val add_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    expression_for_logging:Expression.t option ->
    expression_identifier:ExpressionIdentifier.t ->
    callees:ExpressionCallees.t ->
    t ->
    t

  val add_call_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    location:Ast.Location.t ->
    call:Ast.Expression.Call.t ->
    callees:CallCallees.t ->
    t ->
    t

  val add_identifier_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    location:Ast.Location.t ->
    identifier:string ->
    callees:IdentifierCallees.t ->
    t ->
    t

  val add_define_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    define:Ast.Statement.Define.t ->
    define_location:Ast.Location.t ->
    callees:DefineCallees.t ->
    t ->
    t

  val add_return_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    return_expression:Ast.Expression.Expression.t ->
    statement_location:Ast.Location.t ->
    callees:ReturnShimCallees.t ->
    t ->
    t

  val add_attribute_access_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    location:Ast.Location.t ->
    attribute_access:Ast.Expression.Name.Attribute.t ->
    callees:AttributeAccessCallees.t ->
    t ->
    t

  val add_format_string_articifial_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    location:Ast.Location.t ->
    format_string:Ast.Expression.Expression.expression ->
    callees:FormatStringArtificialCallees.t ->
    t ->
    t

  val add_format_string_stringify_callees
    :  debug:bool ->
    caller:Target.t ->
    on_existing_callees:OnExistingCallees.t ->
    location:Ast.Location.t ->
    substring:Ast.Expression.Expression.expression ->
    callees:FormatStringStringifyCallees.t ->
    t ->
    t

  val set_attribute_access_callees
    :  error_if_new:bool ->
    location:Ast.Location.t ->
    attribute_access:Ast.Expression.Name.Attribute.t ->
    callees:AttributeAccessCallees.t ->
    t ->
    t

  val set_call_callees
    :  error_if_new:bool ->
    location:Ast.Location.t ->
    call:Ast.Expression.Call.t ->
    callees:CallCallees.t ->
    t ->
    t

  val set_identifier_callees
    :  error_if_new:bool ->
    location:Ast.Location.t ->
    identifier:string ->
    identifier_callees:IdentifierCallees.t ->
    t ->
    t

  val set_define_callees
    :  error_if_new:bool ->
    define_location:Ast.Location.t ->
    callees:DefineCallees.t ->
    t ->
    t

  val filter_empty_attribute_access : t -> t

  val map_target
    :  f:(Target.t -> Target.t) ->
    map_call_if:(CallCallees.t -> bool) ->
    map_return_if:(ReturnShimCallees.t -> bool) ->
    t ->
    t

  (* Ensure the taint analysis does not use these targets. *)
  val drop_decorated_targets : t -> t

  val dedup_and_sort : t -> t

  val regenerate_call_indices : indexer:Indexer.t -> t -> t

  val to_json : t -> Yojson.Safe.t

  val save_to_directory
    :  scheduler:Scheduler.t ->
    static_analysis_configuration:Configuration.StaticAnalysis.t ->
    resolve_qualifier:(Target.t -> Ast.Reference.t option) ->
    resolve_module_path:(Ast.Reference.t -> RepositoryPath.t option) option ->
    get_call_graph:(Target.t -> t option) ->
    json_kind:NewlineDelimitedJson.Kind.t ->
    filename_prefix:string ->
    callables:Target.t list ->
    unit
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

  val number_edges : t -> int
end

(** Call graphs of callables, stored in the shared memory. This is a mapping from a callable to its
    `DefineCallGraph.t`. *)
module SharedMemory : sig
  type t

  module ReadOnly : sig
    type t

    val get : t -> cache:bool -> callable:Target.t -> DefineCallGraph.t option
  end

  module AddOnly : sig
    type t

    val create_empty : t -> t

    val add : t -> Target.t -> DefineCallGraph.t -> t

    val merge_same_handle_disjoint_keys : smaller:t -> larger:t -> t
  end

  val create : unit -> t

  val add_only : t -> AddOnly.t

  val from_add_only : AddOnly.t -> t

  val read_only : t -> ReadOnly.t

  val cleanup : t -> unit

  val callables : t -> Target.t list

  val save_to_cache : t -> unit

  val load_from_cache : unit -> (t, SaveLoadSharedMemory.Usage.t) result

  type call_graphs = {
    whole_program_call_graph: WholeProgramCallGraph.t;
    define_call_graphs: t;
  }
end

module MakeSaveCallGraph (CallGraph : sig
  type t

  val name : string

  val is_empty : t -> bool

  val to_json_alist : t -> (string * Yojson.Safe.t) list
end) : sig
  val save_to_directory
    :  scheduler:Scheduler.t ->
    static_analysis_configuration:Configuration.StaticAnalysis.t ->
    resolve_qualifier:(Target.t -> Ast.Reference.t option) ->
    resolve_module_path:(Ast.Reference.t -> RepositoryPath.t option) option ->
    get_call_graph:(Target.t -> CallGraph.t option) ->
    json_kind:NewlineDelimitedJson.Kind.t ->
    filename_prefix:string ->
    callables:Target.t list ->
    unit
end
