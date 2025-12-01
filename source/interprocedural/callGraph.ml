(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* CallGraph: defines the call graph of a callable (function or method), which
 * stores the set of calles for each call site.
 *)

open Core
open Data_structures
open Ast
open Statement
open Expression
open Pyre

module JsonHelper = struct
  let add_optional name value to_json bindings =
    match value with
    | Some value -> (name, to_json value) :: bindings
    | None -> bindings


  let add name value bindings = (name, value) :: bindings

  let add_flag_if name value condition bindings =
    if condition then
      add name value bindings
    else
      bindings


  let add_list name elements to_json bindings =
    match elements with
    | [] -> bindings
    | _ -> (name, `List (List.map ~f:to_json elements)) :: bindings
end

(** Represents type information about the return type of a call. *)
module ReturnType = struct
  type t = Analysis.PyrePysaEnvironment.ScalarTypeProperties.t
  [@@deriving compare, equal, sexp, hash, show]

  let unknown = Analysis.PyrePysaEnvironment.ScalarTypeProperties.unknown

  let none = Analysis.PyrePysaEnvironment.ScalarTypeProperties.none

  let bool = Analysis.PyrePysaEnvironment.ScalarTypeProperties.bool

  let integer = Analysis.PyrePysaEnvironment.ScalarTypeProperties.integer

  let to_json value =
    let add_string_if name condition elements =
      if condition then `String name :: elements else elements
    in
    let module ScalarTypeProperties = Analysis.PyrePysaEnvironment.ScalarTypeProperties in
    []
    |> add_string_if "boolean" (ScalarTypeProperties.is_boolean value)
    |> add_string_if "integer" (ScalarTypeProperties.is_integer value)
    |> add_string_if "float" (ScalarTypeProperties.is_float value)
    |> add_string_if "enum" (ScalarTypeProperties.is_enumeration value)
    |> fun elements -> `List elements
end

(* Produce call targets with a textual order index.
 *
 * The index is the number of times a given function or method was previously called,
 * respecting the execution flow.
 *
 * ```
 * def f():
 *   a = source_with_hop() # index=0
 *   sink_with_hop(x=a) # index=0
 *   sink_with_hop(y=a) # index=1
 *   b = source_with_hop() # index=1
 *   sink_with_hop(z=a) # index=2
 * ```
 *)
module Indexer = struct
  type t = {
    indices: int Target.HashMap.t;
    mutable seen_targets: Target.Set.t;
  }

  let create () = { indices = Target.HashMap.create (); seen_targets = Target.Set.empty }

  let generate_fresh_indices indexer =
    Target.Set.iter (Hashtbl.incr indexer.indices) indexer.seen_targets;
    indexer.seen_targets <- Target.Set.empty


  let get_index ~indexer original_target =
    let target_for_index = Target.for_issue_handle original_target in
    let index = Hashtbl.find indexer.indices target_for_index |> Option.value ~default:0 in
    indexer.seen_targets <- Target.Set.add target_for_index indexer.seen_targets;
    index
end

(** A specific target of a given call, with extra information. *)
module CallTarget = struct
  module T = struct
    type t = {
      target: Target.t;
      (* True if the call has an implicit receiver, such as calling an instance or a class method.
         For instance, `x.foo(0)` should be treated as `C.foo(x, 0)`. As another example, `C.foo(0)`
         should be treated as `C.foo(C, 0)`. *)
      implicit_receiver: bool;
      (* True if this is an implicit call to the `__call__` method. *)
      implicit_dunder_call: bool;
      (* The textual order index of the call in the function. *)
      index: int;
      (* The return type of the call expression, or `None` for object targets. *)
      return_type: ReturnType.t option;
      (* The class of the receiver object at this call site, if any. *)
      receiver_class: string option;
      (* True if calling a class method. *)
      is_class_method: bool;
      (* True if calling a static method. *)
      is_static_method: bool;
    }
    [@@deriving compare, equal, show { with_path = false }, sexp, hash]
  end

  include T

  module Set = Abstract.SetDomain.MakeWithSet (struct
    module Element = T
    (* TODO: Use `CallCallees` instead of `CallTarget` to distinguish new and init targets. *)

    include Data_structures.SerializableSet.Make (Element)

    type element = Element.t

    let show_element = Element.show

    let element_name = "target"
  end)

  let target { target; _ } = target

  let equal_ignoring_indices left right = equal left { right with index = left.index }

  let dedup_and_sort targets =
    targets
    |> List.sort ~compare
    |> List.remove_consecutive_duplicates ~which_to_keep:`First ~equal:equal_ignoring_indices


  let default =
    {
      target = "<default_call_target>" |> Reference.create |> Target.create_object;
      implicit_receiver = false;
      implicit_dunder_call = false;
      index = 0;
      return_type = Some ReturnType.unknown;
      receiver_class = None;
      is_class_method = false;
      is_static_method = false;
    }


  let create
      ?(implicit_receiver = default.implicit_receiver)
      ?(implicit_dunder_call = default.implicit_dunder_call)
      ?(index = default.index)
      ?(return_type = default.return_type)
      ?receiver_class
      ?(is_class_method = default.is_class_method)
      ?(is_static_method = default.is_static_method)
      target
    =
    {
      target;
      implicit_receiver;
      implicit_dunder_call;
      index;
      return_type;
      receiver_class;
      is_class_method;
      is_static_method;
    }


  let create_regular
      ?(implicit_receiver = default.implicit_receiver)
      ?(implicit_dunder_call = default.implicit_dunder_call)
      ?(index = default.index)
      ?(return_type = default.return_type)
      ?receiver_class
      ?(is_class_method = default.is_class_method)
      ?(is_static_method = default.is_static_method)
      target
    =
    target
    |> Target.from_regular
    |> create
         ~implicit_receiver
         ~implicit_dunder_call
         ~index
         ~return_type
         ?receiver_class
         ~is_class_method
         ~is_static_method


  let equal_ignoring_types
      {
        target = target_left;
        implicit_receiver = implicit_receiver_left;
        implicit_dunder_call = implicit_dunder_call_left;
        index = index_left;
        return_type = _;
        receiver_class = _;
        is_class_method = is_class_method_left;
        is_static_method = is_static_method_left;
      }
      {
        target = target_right;
        implicit_receiver = implicit_receiver_right;
        implicit_dunder_call = implicit_dunder_call_right;
        index = index_right;
        return_type = _;
        receiver_class = _;
        is_class_method = is_class_method_right;
        is_static_method = is_static_method_right;
      }
    =
    Target.equal target_left target_right
    && Bool.equal implicit_receiver_left implicit_receiver_right
    && Bool.equal implicit_dunder_call_left implicit_dunder_call_right
    && Int.equal index_left index_right
    && Bool.equal is_class_method_left is_class_method_right
    && Bool.equal is_static_method_left is_static_method_right


  let to_json
      {
        target;
        implicit_receiver;
        implicit_dunder_call;
        index;
        return_type;
        receiver_class;
        is_class_method;
        is_static_method;
      }
    =
    ["index", `Int index; "target", `String (Target.external_name target)]
    |> JsonHelper.add_flag_if "implicit_receiver" (`Bool true) implicit_receiver
    |> JsonHelper.add_flag_if "implicit_dunder_call" (`Bool true) implicit_dunder_call
    |> JsonHelper.add_optional "return_type" return_type ReturnType.to_json
    |> JsonHelper.add_optional "receiver_class" receiver_class (fun name -> `String name)
    |> JsonHelper.add_flag_if "is_class_method" (`Bool true) is_class_method
    |> JsonHelper.add_flag_if "is_static_method" (`Bool true) is_static_method
    |> fun bindings -> `Assoc (List.rev bindings)


  let map_target ~f ({ target; _ } as call_target) = { call_target with target = f target }

  let map_receiver_class ~f ({ receiver_class; _ } as call_target) =
    { call_target with receiver_class = Option.map ~f receiver_class }


  let regenerate_index ~indexer ({ target; _ } as call_target) =
    { call_target with index = Indexer.get_index ~indexer target }
end

module ImplicitArgument = struct
  (* At some call sites, there exist "implicit" arguments. That is, they do not appear inside the
     parentheses like normal arguments. But for analysis purposes, we need to make them explicit. *)
  type t =
    | CalleeBase
      (* The implicit argument is the base expression inside the callee expression, such as `self`
         at call site `self.m(1)`. *)
    | Callee
      (* The implicit argument is the entire callee expression, such as `c` in at call site `c(1)`
         where `c` is an object instance of a callable class. A more complicated case is when
         creating an object `SomeClass(1)`. Here `SomeClass` is an implicit argument to the
         (implicit) call `object.__new__`. *)
    | None (* No implicit argument. *)
  [@@deriving show]

  let implicit_argument
      ?(is_implicit_new = false)
      { CallTarget.implicit_receiver; implicit_dunder_call; _ }
    =
    if implicit_receiver then
      if implicit_dunder_call then
        Callee
      else
        CalleeBase
    else if is_implicit_new then
      (* Since `__new__` are static methods, `implicit_receiver` is false. However, there exists an
         implicit argument because `__new__()` "takes the class of which an instance was requested
         as its first argument".

         Note that this implicit argument is added only when `__new__` is "implicitly" called. For
         example, `SomeClass(a)` implicitly calls `SomeClass.__new__(SomeClass, a)`. By contrast,
         directly calling `object.__new__(SomeClass)` does not add an implicit argument. *)
      CalleeBase
    else
      None
end

module Unresolved = struct
  type bypassing_decorators =
    | NonMethodAttribute
    | CannotFindAttribute
    | CannotResolveExports
    | CannotFindParentClass
    | UnknownBaseType
    | UnknownCallCallee
    | UnknownIdentifierCallee
    | UnknownCalleeAST
  [@@deriving equal, show, to_yojson]

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
    (* reasons from pyrefly *)
    | UnexpectedPyreflyTarget
    | EmptyPyreflyTarget
    | UnknownClassField
    | UnsupportedFunctionTarget
    | UnexpectedDefiningClass
    | UnexpectedInitMethod
    | UnexpectedNewMethod
    | UnexpectedCalleeExpression
    | UnresolvedMagicDunderAttr
    | Mixed
  [@@deriving equal, show, to_yojson]

  type t =
    | True of reason
    | False
  [@@deriving equal, show]

  let is_unresolved = function
    | True _ -> true
    | False -> false


  let join left right =
    match left, right with
    | True _, True _ -> left
    | True _, False -> left
    | False, True _ -> right
    | False, False -> False


  let reason_from_string = function
    | "UnrecognizedCallee" -> Some UnrecognizedCallee
    | "AnonymousCallableType" -> Some AnonymousCallableType
    | "UnknownCallableFromType" -> Some UnknownCallableFromType
    | "UnknownConstructorCallable" -> Some UnknownConstructorCallable
    | "AnyTopCallableClass" -> Some AnyTopCallableClass
    | "UnknownCallableProtocol" -> Some UnknownCallableProtocol
    | "UnknownCallableClass" -> Some UnknownCallableClass
    | "LambdaArgument" -> Some LambdaArgument
    | "NoRecordInCallGraph" -> Some NoRecordInCallGraph
    | "UnexpectedPyreflyTarget" -> Some UnexpectedPyreflyTarget
    | "EmptyPyreflyTarget" -> Some EmptyPyreflyTarget
    | "UnknownClassField" -> Some UnknownClassField
    | "UnsupportedFunctionTarget" -> Some UnsupportedFunctionTarget
    | "UnexpectedDefiningClass" -> Some UnexpectedDefiningClass
    | "UnexpectedInitMethod" -> Some UnexpectedInitMethod
    | "UnexpectedNewMethod" -> Some UnexpectedNewMethod
    | "UnexpectedCalleeExpression" -> Some UnexpectedCalleeExpression
    | "UnresolvedMagicDunderAttr" -> Some UnresolvedMagicDunderAttr
    | "Mixed" -> Some Mixed
    | _ -> None


  let to_json = function
    | True reason -> reason_to_yojson reason
    | False -> `Bool false
end

module AllTargetsUseCase = struct
  type t =
    | TaintAnalysisDependency
    | CallGraphDependency
    | Everything
end

(** Information about an argument being a callable. *)
module HigherOrderParameter = struct
  type t = {
    index: int;
    call_targets: CallTarget.t list;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: Unresolved.t;
  }
  [@@deriving equal, show { with_path = false }]

  let all_targets ~use_case:_ { call_targets; _ } = List.map ~f:CallTarget.target call_targets

  let equal_ignoring_types
      { index = index_left; call_targets = call_targets_left; unresolved = unresolved_left }
      { index = index_right; call_targets = call_targets_right; unresolved = unresolved_right }
    =
    Int.equal index_left index_right
    && List.equal CallTarget.equal_ignoring_types call_targets_left call_targets_right
    && Unresolved.equal unresolved_left unresolved_right


  let join
      { index; call_targets = call_targets_left; unresolved = unresolved_left }
      { index = _; call_targets = call_targets_right; unresolved = unresolved_right }
    =
    {
      index;
      call_targets = List.rev_append call_targets_left call_targets_right;
      unresolved = Unresolved.join unresolved_left unresolved_right;
    }


  let dedup_and_sort { index; call_targets; unresolved } =
    { index; call_targets = CallTarget.dedup_and_sort call_targets; unresolved }


  let to_json { index; call_targets; unresolved } =
    ["parameter_index", `Int index; "calls", `List (List.map ~f:CallTarget.to_json call_targets)]
    |> JsonHelper.add_flag_if
         "unresolved"
         (Unresolved.to_json unresolved)
         (Unresolved.is_unresolved unresolved)
    |> fun bindings -> `Assoc bindings


  let map_call_target ~f ({ call_targets; _ } as higher_order_parameter) =
    { higher_order_parameter with call_targets = List.map ~f call_targets }


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let regenerate_call_indices ~indexer ({ call_targets; _ } as higher_order_parameter) =
    {
      higher_order_parameter with
      call_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) call_targets;
    }
end

(** Mapping from a parameter index to its HigherOrderParameter, if any. *)
module HigherOrderParameterMap = struct
  module Map = SerializableMap.Make (Int)

  type t = HigherOrderParameter.t Map.t

  let empty = Map.empty

  let is_empty = Map.is_empty

  let pp = Map.pp HigherOrderParameter.pp

  let show = Format.asprintf "%a" pp

  let equal = Map.equal HigherOrderParameter.equal

  let equal_ignoring_types = Map.equal HigherOrderParameter.equal_ignoring_types

  let find_opt map index = Map.find_opt index map

  let join left right =
    Map.union (fun _ left right -> Some (HigherOrderParameter.join left right)) left right


  let dedup_and_sort map = Map.map HigherOrderParameter.dedup_and_sort map

  let all_targets ~use_case map =
    Map.fold
      (fun _ higher_order_parameter targets ->
        List.rev_append targets (HigherOrderParameter.all_targets ~use_case higher_order_parameter))
      map
      []


  let add map ({ HigherOrderParameter.index; _ } as higher_order_parameter) =
    Map.update
      index
      (function
        | None -> Some higher_order_parameter
        | Some existing -> Some (HigherOrderParameter.join existing higher_order_parameter))
      map


  let from_list list = List.fold list ~init:Map.empty ~f:add

  let to_list map = Map.data map

  let first_index map =
    Map.min_binding_opt map >>| fun (_, higher_order_parameter) -> higher_order_parameter


  let to_json map =
    map |> Map.data |> List.map ~f:HigherOrderParameter.to_json |> fun elements -> `List elements


  let map_call_target ~f = Map.map (HigherOrderParameter.map_call_target ~f)

  let map_target ~f = Map.map (HigherOrderParameter.map_target ~f)

  let regenerate_call_indices ~indexer =
    Map.map (HigherOrderParameter.regenerate_call_indices ~indexer)
end

module ShimTarget = struct
  type t = {
    call_targets: CallTarget.t list;
    decorated_targets: CallTarget.t list;
    argument_mapping: Shims.ShimArgumentMapping.t;
  }
  [@@deriving equal, show { with_path = false }]

  let join
      {
        call_targets = left_call_targets;
        decorated_targets = left_decorated_targets;
        argument_mapping = left_argument_mapping;
      }
      {
        call_targets = right_call_targets;
        decorated_targets = right_decorated_targets;
        argument_mapping = right_argument_mapping;
      }
    =
    if not (Shims.ShimArgumentMapping.equal left_argument_mapping right_argument_mapping) then
      failwith "Could NOT join shims with different argument mapping"
    else
      {
        call_targets = List.rev_append left_call_targets right_call_targets;
        decorated_targets = List.rev_append left_decorated_targets right_decorated_targets;
        argument_mapping = left_argument_mapping;
      }


  let dedup_and_sort { call_targets; decorated_targets; argument_mapping } =
    {
      call_targets = CallTarget.dedup_and_sort call_targets;
      decorated_targets = CallTarget.dedup_and_sort decorated_targets;
      argument_mapping;
    }


  let all_targets ~use_case { call_targets; decorated_targets; _ } =
    (match use_case with
    | AllTargetsUseCase.TaintAnalysisDependency -> call_targets
    | AllTargetsUseCase.CallGraphDependency
    | AllTargetsUseCase.Everything ->
        List.rev_append call_targets decorated_targets)
    |> List.map ~f:CallTarget.target


  let equal_ignoring_types
      {
        call_targets = left_call_targets;
        decorated_targets = left_decorated_targets;
        argument_mapping = left_argument_mapping;
      }
      {
        call_targets = right_call_targets;
        decorated_targets = right_decorated_targets;
        argument_mapping = right_argument_mapping;
      }
    =
    List.equal CallTarget.equal_ignoring_types left_call_targets right_call_targets
    && List.equal CallTarget.equal_ignoring_types left_decorated_targets right_decorated_targets
    && Shims.ShimArgumentMapping.equal left_argument_mapping right_argument_mapping


  let to_json { call_targets; decorated_targets; argument_mapping } =
    []
    |> JsonHelper.add_list "calls" call_targets CallTarget.to_json
    |> JsonHelper.add_list "decorated_targets" decorated_targets CallTarget.to_json
    |> List.cons ("argument_mapping", Shims.ShimArgumentMapping.to_json argument_mapping)
    |> fun bindings -> `Assoc (List.rev bindings)


  let map_call_target ~f ({ call_targets; _ } as shim_target) =
    { shim_target with call_targets = List.map ~f call_targets }


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let regenerate_call_indices ~indexer ({ call_targets; _ } as shim_target) =
    {
      shim_target with
      call_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) call_targets;
    }


  let drop_decorated_targets shim_target = { shim_target with decorated_targets = [] }
end

(** An aggregate of all possible callees at a call site. *)
module CallCallees = struct
  module RecognizedCall = struct
    type t =
      | True
      | False
      | Unknown
    [@@deriving equal, show { with_path = false }, to_yojson]

    let join left right =
      match left, right with
      | True, True -> True
      | False, False -> False
      | _, _ -> Unknown


    let output_to_json = function
      | False -> false
      | True
      | Unknown ->
          true


    let redirect_to_decorated = function
      | True -> false
      | Unknown (* It is unclear whether it is better to redirect or not. *)
      | False ->
          true
  end

  type t = {
    (* Normal call targets. *)
    call_targets: CallTarget.t list;
    (* Call targets for calls to the `__new__` class method. *)
    new_targets: CallTarget.t list;
    (* Call targets for calls to the `__init__` instance method. *)
    init_targets: CallTarget.t list;
    (* A decorated target, which is an artificially created callable that calls the decorators,
       represents its returned callables, instead of itself, unlike other targets. This is used to
       set the dependencies in the call graph fixpoint so that when a decorated target returns new
       callables, those can be picked up by whoever depends on the decorated target. *)
    decorated_targets: CallTarget.t list;
    (* Information about arguments that are callables, and possibly called. *)
    higher_order_parameters: HigherOrderParameterMap.t;
    shim_target: ShimTarget.t option;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: Unresolved.t;
    recognized_call: RecognizedCall.t;
  }
  [@@deriving equal, show { with_path = false }]

  let empty =
    {
      call_targets = [];
      new_targets = [];
      init_targets = [];
      decorated_targets = [];
      higher_order_parameters = HigherOrderParameterMap.empty;
      shim_target = None;
      unresolved = Unresolved.False;
      recognized_call = RecognizedCall.False;
    }


  let is_empty = equal empty

  let create
      ?(call_targets = [])
      ?(new_targets = [])
      ?(init_targets = [])
      ?(decorated_targets = [])
      ?(higher_order_parameters = HigherOrderParameterMap.empty)
      ?(shim_target = None)
      ?(unresolved = Unresolved.False)
      ?(recognized_call = RecognizedCall.False)
      ()
    =
    {
      call_targets;
      new_targets;
      init_targets;
      decorated_targets;
      higher_order_parameters;
      shim_target;
      unresolved;
      recognized_call;
    }


  (* When `debug` is true, log the message. *)
  let unresolved ?(debug = false) ~reason ~message () =
    if debug then
      (* Use `dump` so that the log can also be printed when testing. Use `Lazy.t` to improve
         performance since `debug` is almost always false in production. *)
      Log.dump "Unresolved call: %s" (Lazy.force message);
    {
      call_targets = [];
      new_targets = [];
      init_targets = [];
      decorated_targets = [];
      higher_order_parameters = HigherOrderParameterMap.empty;
      shim_target = None;
      unresolved = Unresolved.True reason;
      recognized_call = RecognizedCall.False;
    }


  let default_to_unresolved ?(debug = false) ~reason ~message = function
    | Some value -> value
    | None -> unresolved ~debug ~reason ~message ()


  let is_partially_resolved = function
    | { call_targets = _ :: _; _ } -> true
    | { new_targets = _ :: _; _ } -> true
    | { init_targets = _ :: _; _ } -> true
    | _ -> false


  let pp_option formatter = function
    | None -> Format.fprintf formatter "None"
    | Some callees -> pp formatter callees


  let join
      {
        call_targets = left_call_targets;
        new_targets = left_new_targets;
        init_targets = left_init_targets;
        decorated_targets = left_decorator_targets;
        higher_order_parameters = left_higher_order_parameters;
        shim_target = left_shim_target;
        unresolved = left_unresolved;
        recognized_call = left_recognized_call;
      }
      {
        call_targets = right_call_targets;
        new_targets = right_new_targets;
        init_targets = right_init_targets;
        decorated_targets = right_decorator_targets;
        higher_order_parameters = right_higher_order_parameters;
        shim_target = right_shim_target;
        unresolved = right_unresolved;
        recognized_call = right_recognized_call;
      }
    =
    let call_targets = List.rev_append left_call_targets right_call_targets in
    let new_targets = List.rev_append left_new_targets right_new_targets in
    let init_targets = List.rev_append left_init_targets right_init_targets in
    let decorated_targets = List.rev_append left_decorator_targets right_decorator_targets in
    let higher_order_parameters =
      HigherOrderParameterMap.join left_higher_order_parameters right_higher_order_parameters
    in
    let shim_target = Option.merge ~f:ShimTarget.join left_shim_target right_shim_target in
    let unresolved = Unresolved.join left_unresolved right_unresolved in
    let recognized_call = RecognizedCall.join left_recognized_call right_recognized_call in
    {
      call_targets;
      new_targets;
      init_targets;
      decorated_targets;
      higher_order_parameters;
      shim_target;
      unresolved;
      recognized_call;
    }


  let dedup_and_sort
      {
        call_targets;
        new_targets;
        init_targets;
        decorated_targets;
        higher_order_parameters;
        shim_target;
        unresolved;
        recognized_call;
      }
    =
    let call_targets = CallTarget.dedup_and_sort call_targets in
    let new_targets = CallTarget.dedup_and_sort new_targets in
    let init_targets = CallTarget.dedup_and_sort init_targets in
    let decorated_targets = CallTarget.dedup_and_sort decorated_targets in
    let higher_order_parameters = HigherOrderParameterMap.dedup_and_sort higher_order_parameters in
    let shim_target = Option.map ~f:ShimTarget.dedup_and_sort shim_target in
    {
      call_targets;
      new_targets;
      init_targets;
      decorated_targets;
      higher_order_parameters;
      shim_target;
      unresolved;
      recognized_call;
    }


  let all_targets
      ~use_case
      {
        call_targets;
        new_targets;
        init_targets;
        higher_order_parameters;
        shim_target;
        decorated_targets;
        _;
      }
    =
    let include_decorated_targets =
      match use_case with
      | AllTargetsUseCase.TaintAnalysisDependency -> false
      | _ -> true
    in
    call_targets
    |> List.rev_append new_targets
    |> List.rev_append init_targets
    |> (if include_decorated_targets then List.rev_append decorated_targets else Fn.id)
    |> List.map ~f:CallTarget.target
    |> List.rev_append (HigherOrderParameterMap.all_targets ~use_case higher_order_parameters)
    |> List.rev_append (shim_target >>| ShimTarget.all_targets ~use_case |> Option.value ~default:[])


  let equal_ignoring_types
      {
        call_targets = call_targets_left;
        new_targets = new_targets_left;
        init_targets = init_targets_left;
        decorated_targets = decorator_targets_left;
        higher_order_parameters = higher_order_parameters_left;
        shim_target = shim_target_left;
        unresolved = unresolved_left;
        recognized_call = recognized_call_left;
      }
      {
        call_targets = call_targets_right;
        new_targets = new_targets_right;
        init_targets = init_targets_right;
        decorated_targets = decorator_targets_right;
        higher_order_parameters = higher_order_parameters_right;
        shim_target = shim_target_right;
        unresolved = unresolved_right;
        recognized_call = recognized_call_right;
      }
    =
    List.equal CallTarget.equal_ignoring_types call_targets_left call_targets_right
    && List.equal CallTarget.equal_ignoring_types new_targets_left new_targets_right
    && List.equal CallTarget.equal_ignoring_types init_targets_left init_targets_right
    && List.equal CallTarget.equal_ignoring_types decorator_targets_left decorator_targets_right
    && HigherOrderParameterMap.equal_ignoring_types
         higher_order_parameters_left
         higher_order_parameters_right
    && Option.equal ShimTarget.equal_ignoring_types shim_target_left shim_target_right
    && Unresolved.equal unresolved_left unresolved_right
    && RecognizedCall.equal recognized_call_left recognized_call_right


  let is_method_of_class ~is_class_name callees =
    let is_call_target call_target =
      match
        Target.get_regular call_target.CallTarget.target, call_target.CallTarget.receiver_class
      with
      | Target.Regular.Method { class_name; _ }, Some receiver_class
      | Target.Regular.Override { class_name; _ }, Some receiver_class ->
          (* Is it not enough to check the class name, since methods can be inherited.
           * For instance, `__iter__` is not defined on `Mapping`, but is defined in the parent class `Iterable`. *)
          is_class_name class_name || is_class_name receiver_class
      | _ -> false
    in
    match callees with
    | { call_targets = []; _ } -> false
    | { call_targets; _ } -> List.for_all call_targets ~f:is_call_target


  let is_mapping_method callees =
    let is_class_name = function
      | "dict"
      | "builtins.dict"
      | "typing.Mapping"
      | "typing.MutableMapping"
      | "TypedDictionary" (* pyre1 *)
      | "NonTotalTypedDictionary" (* pyre1 *)
      | "_typeshed._type_checker_internal.TypedDictFallback" (* pyrefly *)
      | "collections.OrderedDict"
      | "collections.defaultdict" ->
          true
      | _ -> false
    in
    is_method_of_class ~is_class_name callees


  let is_sequence_method callees =
    let is_class_name = function
      | "list"
      | "builtins.list"
      | "typing.Sequence"
      | "typing.MutableSequence"
      | "collections.deque"
      | "tuple"
      | "builtins.tuple" ->
          true
      | _ -> false
    in
    is_method_of_class ~is_class_name callees


  let is_string_method callees =
    let is_class_name = function
      | "str"
      | "builtins.str" ->
          true
      | _ -> false
    in
    is_method_of_class ~is_class_name callees


  let is_object_new = function
    | [] -> (* Unresolved call, assume it's object.__new__ *) true
    | [{ CallTarget.target; _ }] -> (
        match Target.get_regular target with
        | Target.Regular.Method { class_name = "object"; method_name = "__new__"; kind = Normal }
        | Target.Regular.Method
            { class_name = "builtins.object"; method_name = "__new__"; kind = Normal } ->
            true
        | _ -> false)
    | _ -> false


  let is_object_init = function
    | [] -> (* Unresolved call, assume it's object.__init__ *) true
    | [{ CallTarget.target; _ }] -> (
        match Target.get_regular target with
        | Target.Regular.Method { class_name = "object"; method_name = "__init__"; kind = Normal }
        | Target.Regular.Method
            { class_name = "builtins.object"; method_name = "__init__"; kind = Normal } ->
            true
        | _ -> false)
    | _ -> false


  let to_json
      {
        call_targets;
        new_targets;
        init_targets;
        decorated_targets;
        higher_order_parameters;
        shim_target;
        unresolved;
        recognized_call;
      }
    =
    let bindings =
      []
      |> JsonHelper.add_list "calls" call_targets CallTarget.to_json
      |> JsonHelper.add_list "new_calls" new_targets CallTarget.to_json
      |> JsonHelper.add_list "init_calls" init_targets CallTarget.to_json
      |> JsonHelper.add_list "decorated_targets" decorated_targets CallTarget.to_json
    in
    let bindings =
      if not (HigherOrderParameterMap.is_empty higher_order_parameters) then
        ("higher_order_parameters", HigherOrderParameterMap.to_json higher_order_parameters)
        :: bindings
      else
        bindings
    in
    let bindings =
      match shim_target with
      | Some shim_target -> ("shim", ShimTarget.to_json shim_target) :: bindings
      | None -> bindings
    in
    let bindings =
      bindings
      |> JsonHelper.add_flag_if
           "unresolved"
           (Unresolved.to_json unresolved)
           (Unresolved.is_unresolved unresolved)
      |> JsonHelper.add_flag_if
           "recognized_call"
           (RecognizedCall.to_yojson recognized_call)
           (RecognizedCall.output_to_json recognized_call)
    in
    `Assoc (List.rev bindings)


  let map_call_target
      ~f
      ({ call_targets; new_targets; init_targets; higher_order_parameters; shim_target; _ } as
      call_callees)
    =
    {
      call_callees with
      call_targets = List.map ~f call_targets;
      higher_order_parameters = HigherOrderParameterMap.map_call_target ~f higher_order_parameters;
      shim_target = Option.map ~f:(ShimTarget.map_call_target ~f) shim_target;
      new_targets = List.map ~f new_targets;
      init_targets = List.map ~f init_targets;
    }


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let should_redirect_to_decorated { recognized_call; _ } =
    RecognizedCall.redirect_to_decorated recognized_call


  let drop_decorated_targets ({ shim_target; _ } as call_callees) =
    {
      call_callees with
      decorated_targets = [];
      shim_target = Option.map ~f:ShimTarget.drop_decorated_targets shim_target;
    }


  let regenerate_call_indices
      ~indexer
      ({
         call_targets;
         new_targets;
         init_targets;
         higher_order_parameters;
         shim_target;
         decorated_targets =
           _ (* No need to regenerate because they will not be used in taint analysis. *);
         _;
       } as call_callees)
    =
    {
      call_callees with
      call_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) call_targets;
      new_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) new_targets;
      init_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) init_targets;
      higher_order_parameters =
        HigherOrderParameterMap.regenerate_call_indices ~indexer higher_order_parameters;
      shim_target = Option.map ~f:(ShimTarget.regenerate_call_indices ~indexer) shim_target;
    }
end

(** An aggregrate of all possible callees for a given attribute access. *)
module AttributeAccessCallees = struct
  type t = {
    property_targets: CallTarget.t list;
    global_targets: CallTarget.t list;
    (* True if that there is at least one case (i.e., execution flow) where this is a regular
       attribute access.
     * For instance, if the object has type `Union[A, B]` where only `A` defines a property. *)
    is_attribute: bool;
    (* Function-typed runtime values that the attribute access may evaluate into. *)
    if_called: CallCallees.t;
  }
  [@@deriving equal, show { with_path = false }]

  let empty =
    {
      property_targets = [];
      global_targets = [];
      is_attribute = true;
      if_called = CallCallees.empty;
    }


  let is_empty attribute_access_callees = equal attribute_access_callees empty

  let create
      ?(property_targets = empty.property_targets)
      ?(global_targets = empty.global_targets)
      ?(if_called = CallCallees.empty)
      ?(is_attribute = empty.is_attribute)
      ()
    =
    { property_targets; global_targets; is_attribute; if_called }


  let dedup_and_sort { property_targets; global_targets; is_attribute; if_called } =
    {
      property_targets = CallTarget.dedup_and_sort property_targets;
      global_targets = CallTarget.dedup_and_sort global_targets;
      is_attribute;
      if_called = CallCallees.dedup_and_sort if_called;
    }


  let join
      {
        property_targets = left_property_targets;
        global_targets = left_global_targets;
        is_attribute = left_is_attribute;
        if_called = left_if_called;
      }
      {
        property_targets = right_property_targets;
        global_targets = right_global_targets;
        is_attribute = right_is_attribute;
        if_called = right_if_called;
      }
    =
    {
      property_targets = List.rev_append left_property_targets right_property_targets;
      global_targets = List.rev_append left_global_targets right_global_targets;
      is_attribute = left_is_attribute || right_is_attribute;
      if_called = CallCallees.join left_if_called right_if_called;
    }


  let all_targets ~use_case { property_targets; global_targets; if_called; is_attribute = _ } =
    match use_case with
    | AllTargetsUseCase.CallGraphDependency ->
        CallCallees.all_targets ~use_case if_called
        (* A property could (in theory) return a callable. *)
        |> List.rev_append (List.map ~f:CallTarget.target property_targets)
    | AllTargetsUseCase.TaintAnalysisDependency ->
        List.rev_append property_targets global_targets |> List.map ~f:CallTarget.target
    | AllTargetsUseCase.Everything ->
        CallCallees.all_targets ~use_case if_called
        |> List.rev_append (List.map ~f:CallTarget.target global_targets)
        |> List.rev_append (List.map ~f:CallTarget.target property_targets)


  let equal_ignoring_types
      {
        property_targets = property_targets_left;
        global_targets = global_targets_left;
        is_attribute = is_attribute_left;
        if_called = if_called_left;
      }
      {
        property_targets = property_targets_right;
        global_targets = global_targets_right;
        is_attribute = is_attribute_right;
        if_called = if_called_right;
      }
    =
    List.equal CallTarget.equal_ignoring_types property_targets_left property_targets_right
    && List.equal CallTarget.equal_ignoring_types global_targets_left global_targets_right
    && Bool.equal is_attribute_left is_attribute_right
    && CallCallees.equal_ignoring_types if_called_left if_called_right


  let to_json { property_targets; global_targets; is_attribute; if_called } =
    []
    |> JsonHelper.add_list "properties" property_targets CallTarget.to_json
    |> JsonHelper.add_list "globals" global_targets CallTarget.to_json
    |> JsonHelper.add_flag_if "is_attribute" (`Bool true) is_attribute
    |> JsonHelper.add_flag_if
         "if_called"
         (CallCallees.to_json if_called)
         (not (CallCallees.is_empty if_called))
    |> fun bindings -> `Assoc (List.rev bindings)


  let map_call_target ~f ({ if_called; _ } as attribute_callees) =
    { attribute_callees with if_called = CallCallees.map_call_target ~f if_called }


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let drop_decorated_targets ({ if_called; _ } as attribute_access_callees) =
    { attribute_access_callees with if_called = CallCallees.drop_decorated_targets if_called }


  let regenerate_call_indices
      ~indexer
      ({
         property_targets;
         global_targets;
         if_called = _ (* No need to regenerate because they will not be used in taint analysis. *);
         is_attribute = _;
       } as attribute_callees)
    =
    {
      attribute_callees with
      property_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) property_targets;
      global_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) global_targets;
    }
end

(** An aggregate of all possible callees for a given identifier expression, i.e `foo`. *)
module IdentifierCallees = struct
  type t = {
    global_targets: CallTarget.t list;
    nonlocal_targets: CallTarget.t list;
    (* Function-typed runtime values that the identifier may evaluate into. *)
    if_called: CallCallees.t;
  }
  [@@deriving equal, show { with_path = false }]

  let create ?(global_targets = []) ?(nonlocal_targets = []) ?(if_called = CallCallees.empty) () =
    { global_targets; nonlocal_targets; if_called }


  let dedup_and_sort { global_targets; nonlocal_targets; if_called } =
    {
      global_targets = CallTarget.dedup_and_sort global_targets;
      nonlocal_targets = CallTarget.dedup_and_sort nonlocal_targets;
      if_called = CallCallees.dedup_and_sort if_called;
    }


  let join
      {
        global_targets = left_global_targets;
        nonlocal_targets = left_nonlocal_targets;
        if_called = left_if_called;
      }
      {
        global_targets = right_global_targets;
        nonlocal_targets = right_nonlocal_targets;
        if_called = right_if_called;
      }
    =
    {
      global_targets = List.rev_append left_global_targets right_global_targets;
      nonlocal_targets = List.rev_append left_nonlocal_targets right_nonlocal_targets;
      if_called = CallCallees.join left_if_called right_if_called;
    }


  let all_targets ~use_case { global_targets; nonlocal_targets; if_called } =
    match use_case with
    | AllTargetsUseCase.CallGraphDependency -> CallCallees.all_targets ~use_case if_called
    | AllTargetsUseCase.TaintAnalysisDependency -> []
    | AllTargetsUseCase.Everything ->
        CallCallees.all_targets ~use_case if_called
        |> List.rev_append (List.map ~f:CallTarget.target nonlocal_targets)
        |> List.rev_append (List.map ~f:CallTarget.target global_targets)


  let to_json { global_targets; nonlocal_targets; if_called } =
    []
    |> JsonHelper.add_list "globals" global_targets CallTarget.to_json
    |> JsonHelper.add_list "nonlocals" nonlocal_targets CallTarget.to_json
    |> JsonHelper.add_flag_if
         "if_called"
         (CallCallees.to_json if_called)
         (not (CallCallees.is_empty if_called))
    |> fun bindings -> `Assoc (List.rev bindings)


  let map_call_target ~f ({ if_called; _ } as identifier_callees) =
    { identifier_callees with if_called = CallCallees.map_call_target ~f if_called }


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let drop_decorated_targets ({ if_called; _ } as identifier_callees) =
    { identifier_callees with if_called = CallCallees.drop_decorated_targets if_called }


  let regenerate_call_indices ~indexer { global_targets; nonlocal_targets; if_called } =
    {
      global_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) global_targets;
      nonlocal_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) nonlocal_targets;
      (* Those are not used in the taint analysis, therefore they don't need indices. *)
      if_called;
    }
end

(** Artificial callees for distinguishing f-strings within a function. *)
module FormatStringArtificialCallees = struct
  type t = { targets: CallTarget.t list } [@@deriving equal, show { with_path = false }]

  let dedup_and_sort { targets } = { targets = CallTarget.dedup_and_sort targets }

  let join { targets = left_targets } { targets = right_targets } =
    { targets = List.rev_append left_targets right_targets }


  let all_targets ~use_case { targets } =
    (match use_case with
    | AllTargetsUseCase.CallGraphDependency -> []
    | AllTargetsUseCase.TaintAnalysisDependency -> []
    | AllTargetsUseCase.Everything -> targets)
    |> List.map ~f:CallTarget.target


  let from_f_string_targets targets = { targets }

  let to_json { targets } = `List (List.map ~f:CallTarget.to_json targets)

  let map_call_target ~f { targets } = { targets = List.map ~f targets }

  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let drop_decorated_targets = Fn.id

  let regenerate_call_indices ~indexer { targets } =
    { targets = List.map ~f:(CallTarget.regenerate_index ~indexer) targets }
end

(** Implicit callees for any expression that is stringified. *)
module FormatStringStringifyCallees = struct
  type t = { targets: CallTarget.t list } [@@deriving equal, show { with_path = false }]

  let dedup_and_sort { targets } = { targets = CallTarget.dedup_and_sort targets }

  let join { targets = left_targets } { targets = right_targets } =
    { targets = List.rev_append left_targets right_targets }


  let all_targets ~use_case { targets } =
    (match use_case with
    | AllTargetsUseCase.CallGraphDependency -> []
    | AllTargetsUseCase.TaintAnalysisDependency -> targets
    | AllTargetsUseCase.Everything -> targets)
    |> List.map ~f:CallTarget.target


  let from_stringify_targets targets = { targets }

  let to_json { targets } = `List (List.map ~f:CallTarget.to_json targets)

  let map_call_target ~f { targets } = { targets = List.map ~f targets }

  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let drop_decorated_targets = Fn.id

  let regenerate_call_indices ~indexer { targets } =
    { targets = List.map ~f:(CallTarget.regenerate_index ~indexer) targets }
end

module DefineCallees = struct
  type t = {
    define_targets: CallTarget.t list;
    decorated_targets: CallTarget.t list;
  }
  [@@deriving equal, show { with_path = false }]

  let create ?(define_targets = []) ?(decorated_targets = []) () =
    { define_targets; decorated_targets }


  let join
      { define_targets = left_define_targets; decorated_targets = left_decorated_targets }
      { define_targets = right_define_targets; decorated_targets = right_decorated_targets }
    =
    {
      define_targets = List.rev_append left_define_targets right_define_targets;
      decorated_targets = List.rev_append left_decorated_targets right_decorated_targets;
    }


  let equal_ignoring_types
      { define_targets = left_define_targets; decorated_targets = left_decorated_targets }
      { define_targets = right_define_targets; decorated_targets = right_decorated_targets }
    =
    List.equal CallTarget.equal_ignoring_types left_define_targets right_define_targets
    && List.equal CallTarget.equal_ignoring_types left_decorated_targets right_decorated_targets


  let dedup_and_sort { define_targets; decorated_targets } =
    {
      define_targets = CallTarget.dedup_and_sort define_targets;
      decorated_targets = CallTarget.dedup_and_sort decorated_targets;
    }


  let all_targets ~use_case { define_targets; decorated_targets } =
    (match use_case with
    | AllTargetsUseCase.CallGraphDependency -> decorated_targets
    | AllTargetsUseCase.TaintAnalysisDependency ->
        (* Taint analysis does not need to know this. *) []
    | AllTargetsUseCase.Everything -> List.rev_append define_targets decorated_targets)
    |> List.map ~f:CallTarget.target


  let to_json { define_targets; decorated_targets } =
    let bindings =
      []
      |> JsonHelper.add_list "define_targets" define_targets CallTarget.to_json
      |> JsonHelper.add_list "decorated_targets" decorated_targets CallTarget.to_json
    in
    `Assoc bindings


  let map_call_target ~f { define_targets; decorated_targets } =
    { define_targets = List.map ~f define_targets; decorated_targets }


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let drop_decorated_targets callees = { callees with decorated_targets = [] }

  let regenerate_call_indices ~indexer:_ callees =
    (* No need to regenerate because these will not be used in taint analysis. *) callees
end

(* Artificial callees on returned expressions. *)
module ReturnShimCallees = struct
  type argument_mapping =
    | ReturnExpression
    | ReturnExpressionElement
  [@@deriving equal, show { with_path = false }]

  type t = {
    call_targets: CallTarget.t list;
    arguments: argument_mapping list;
  }
  [@@deriving equal, show { with_path = false }]

  let dedup_and_sort ({ call_targets; _ } as return_callees) =
    { return_callees with call_targets = CallTarget.dedup_and_sort call_targets }


  let all_targets ~use_case:_ { call_targets; _ } = List.map ~f:CallTarget.target call_targets

  let to_json { call_targets; arguments } =
    let bindings =
      []
      |> JsonHelper.add_list "call_targets" call_targets CallTarget.to_json
      |> JsonHelper.add_list "arguments_mapping" arguments (fun argument_mapping ->
             `String (show_argument_mapping argument_mapping))
    in
    `Assoc bindings


  let map_call_target ~f { call_targets; arguments } =
    { call_targets = List.map ~f call_targets; arguments }


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let regenerate_call_indices ~indexer ({ call_targets; _ } as return_callees) =
    {
      return_callees with
      call_targets = List.map ~f:(CallTarget.regenerate_index ~indexer) call_targets;
    }
end

(** An aggregate of all possible callees for an arbitrary expression. *)
module ExpressionCallees = struct
  type t =
    | Call of CallCallees.t
    | AttributeAccess of AttributeAccessCallees.t
    | Identifier of IdentifierCallees.t
    | FormatStringArtificial of FormatStringArtificialCallees.t
    | FormatStringStringify of FormatStringStringifyCallees.t
    | Define of DefineCallees.t
    | Return of ReturnShimCallees.t
  [@@deriving equal, show { with_path = false }]

  let from_call callees = Call callees

  let from_attribute_access callees = AttributeAccess callees

  let from_identifier callees = Identifier callees

  let from_format_string_artificial callees = FormatStringArtificial callees

  let from_format_string_stringify callees = FormatStringStringify callees

  let from_define callees = Define callees

  let from_return callees = Return callees

  let join left right =
    match left, right with
    | Call left, Call right -> Call (CallCallees.join left right)
    | AttributeAccess left, AttributeAccess right ->
        AttributeAccess (AttributeAccessCallees.join left right)
    | Identifier left, Identifier right -> Identifier (IdentifierCallees.join left right)
    | FormatStringArtificial left, FormatStringArtificial right ->
        FormatStringArtificial (FormatStringArtificialCallees.join left right)
    | FormatStringStringify left, FormatStringStringify right ->
        FormatStringStringify (FormatStringStringifyCallees.join left right)
    | Define left, Define right -> Define (DefineCallees.join left right)
    | _ ->
        Format.asprintf
          "Trying to join different callee types:\n\tleft: %a\n\tright: %a"
          pp
          left
          pp
          right
        |> failwith


  let dedup_and_sort = function
    | Call callees -> Call (CallCallees.dedup_and_sort callees)
    | AttributeAccess callees -> AttributeAccess (AttributeAccessCallees.dedup_and_sort callees)
    | Identifier callees -> Identifier (IdentifierCallees.dedup_and_sort callees)
    | FormatStringArtificial callees ->
        FormatStringArtificial (FormatStringArtificialCallees.dedup_and_sort callees)
    | FormatStringStringify callees ->
        FormatStringStringify (FormatStringStringifyCallees.dedup_and_sort callees)
    | Define callees -> Define (DefineCallees.dedup_and_sort callees)
    | Return callees -> Return (ReturnShimCallees.dedup_and_sort callees)


  let all_targets ~use_case = function
    | Call callees -> CallCallees.all_targets ~use_case callees
    | AttributeAccess callees -> AttributeAccessCallees.all_targets ~use_case callees
    | Identifier callees -> IdentifierCallees.all_targets ~use_case callees
    | FormatStringArtificial callees -> FormatStringArtificialCallees.all_targets ~use_case callees
    | FormatStringStringify callees -> FormatStringStringifyCallees.all_targets ~use_case callees
    | Define callees -> DefineCallees.all_targets ~use_case callees
    | Return callees -> ReturnShimCallees.all_targets ~use_case callees


  let is_empty_attribute_access_callees = function
    | AttributeAccess callees -> AttributeAccessCallees.is_empty callees
    | _ -> false


  let equal_ignoring_types left right =
    match left, right with
    | Call left, Call right -> CallCallees.equal_ignoring_types left right
    | AttributeAccess left, AttributeAccess right ->
        AttributeAccessCallees.equal_ignoring_types left right
    | Identifier left, Identifier right -> IdentifierCallees.equal left right
    | FormatStringArtificial left, FormatStringArtificial right ->
        FormatStringArtificialCallees.equal left right
    | FormatStringStringify left, FormatStringStringify right ->
        FormatStringStringifyCallees.equal left right
    | Define left, Define right -> DefineCallees.equal_ignoring_types left right
    | _ -> false


  let to_json = function
    | Call callees -> `Assoc ["call", CallCallees.to_json callees]
    | AttributeAccess callees -> `Assoc ["attribute_access", AttributeAccessCallees.to_json callees]
    | Identifier callees -> `Assoc ["identifier", IdentifierCallees.to_json callees]
    | FormatStringArtificial callees ->
        `Assoc ["format_string_artificial", FormatStringArtificialCallees.to_json callees]
    | FormatStringStringify callees ->
        `Assoc ["format_string_stringify", FormatStringStringifyCallees.to_json callees]
    | Define callees -> `Assoc ["define", DefineCallees.to_json callees]
    | Return callees -> `Assoc ["return", ReturnShimCallees.to_json callees]


  let map_call_target ~f ~map_call_if ~map_return_if = function
    | Call callees ->
        Call (if map_call_if callees then CallCallees.map_call_target ~f callees else callees)
    | AttributeAccess callees -> AttributeAccess (AttributeAccessCallees.map_call_target ~f callees)
    | Identifier callees -> Identifier (IdentifierCallees.map_call_target ~f callees)
    | FormatStringArtificial callees ->
        FormatStringArtificial (FormatStringArtificialCallees.map_call_target ~f callees)
    | FormatStringStringify callees ->
        FormatStringStringify (FormatStringStringifyCallees.map_call_target ~f callees)
    | Define callees -> Define (DefineCallees.map_call_target ~f callees)
    | Return callees ->
        Return
          (if map_return_if callees then ReturnShimCallees.map_call_target ~f callees else callees)


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let drop_decorated_targets = function
    | Call callees -> Call (CallCallees.drop_decorated_targets callees)
    | AttributeAccess callees ->
        AttributeAccess (AttributeAccessCallees.drop_decorated_targets callees)
    | Identifier callees -> Identifier (IdentifierCallees.drop_decorated_targets callees)
    | FormatStringArtificial callees ->
        FormatStringArtificial (FormatStringArtificialCallees.drop_decorated_targets callees)
    | FormatStringStringify callees ->
        FormatStringStringify (FormatStringStringifyCallees.drop_decorated_targets callees)
    | Define callees -> Define (DefineCallees.drop_decorated_targets callees)
    | Return callees -> Return callees


  let regenerate_call_indices ~indexer callees =
    Indexer.generate_fresh_indices indexer;
    match callees with
    | Call callees -> Call (CallCallees.regenerate_call_indices ~indexer callees)
    | AttributeAccess callees ->
        AttributeAccess (AttributeAccessCallees.regenerate_call_indices ~indexer callees)
    | Identifier callees -> Identifier (IdentifierCallees.regenerate_call_indices ~indexer callees)
    | FormatStringArtificial callees ->
        FormatStringArtificial
          (FormatStringArtificialCallees.regenerate_call_indices ~indexer callees)
    | FormatStringStringify callees ->
        FormatStringStringify
          (FormatStringStringifyCallees.regenerate_call_indices ~indexer callees)
    | Define callees -> Define (DefineCallees.regenerate_call_indices ~indexer callees)
    | Return callees -> Return (ReturnShimCallees.regenerate_call_indices ~indexer callees)
end

let log ~debug format =
  if debug then
    Log.dump format
  else
    Log.log ~section:`CallGraph format


(** The call graph of a function or method definition. This is for testing purpose only. *)
module DefineCallGraphForTest = struct
  type t = ExpressionCallees.t String.Map.t [@@deriving equal]

  let pp formatter call_graph =
    let pp_pair formatter (expression_identifier, callees) =
      Format.fprintf formatter "@,%s -> %a" expression_identifier ExpressionCallees.pp callees
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    call_graph |> Map.to_alist |> Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs


  let from_expected =
    List.fold ~init:String.Map.empty ~f:(fun sofar (expression_identifier, callees) ->
        Map.set sofar ~key:expression_identifier ~data:callees)


  let show = Format.asprintf "%a" pp

  let empty = String.Map.empty

  let equal_ignoring_types = Map.equal ExpressionCallees.equal_ignoring_types
end

module MakeSaveCallGraph (CallGraph : sig
  type t

  val name : string

  val is_empty : t -> bool

  val to_json_alist : t -> (string * Yojson.Safe.t) list
end) =
struct
  let filename_and_path ~resolve_qualifier ~resolve_module_path callable
      : (string * Yojson.Safe.t) list
    =
    let bindings = ["callable", `String (Target.external_name callable)] in
    let resolve_module_path = Option.value ~default:(fun _ -> None) resolve_module_path in
    callable
    |> resolve_qualifier
    >>= resolve_module_path
    >>| (function
          | { RepositoryPath.filename = Some filename; _ } ->
              ("filename", `String filename) :: bindings
          | { path; _ } ->
              ("filename", `String "*") :: ("path", `String (PyrePath.absolute path)) :: bindings)
    |> Option.value ~default:bindings


  let save_to_directory
      ~scheduler
      ~static_analysis_configuration:
        {
          Configuration.StaticAnalysis.dump_call_graph;
          save_results_to;
          output_format;
          configuration = { local_root; _ };
          _;
        }
      ~resolve_qualifier
      ~resolve_module_path
      ~get_call_graph
      ~json_kind
      ~filename_prefix
      ~callables
    =
    let call_graph_to_json callable =
      match get_call_graph callable with
      | Some call_graph when not (CallGraph.is_empty call_graph) ->
          [
            {
              NewlineDelimitedJson.Line.kind = json_kind;
              data =
                `Assoc
                  (filename_and_path ~resolve_qualifier ~resolve_module_path callable
                  @ CallGraph.to_json_alist call_graph);
            };
          ]
      | _ -> []
    in
    let () =
      match save_results_to with
      | Some directory ->
          Log.info "Writing the %s to `%s`" CallGraph.name (PyrePath.absolute directory);
          let () =
            match output_format with
            | Configuration.TaintOutputFormat.Json ->
                NewlineDelimitedJson.write_file
                  ~path:
                    (PyrePath.append directory ~element:(Format.asprintf "%s.json" filename_prefix))
                  ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
                  ~to_json_lines:call_graph_to_json
                  callables
            | Configuration.TaintOutputFormat.ShardedJson ->
                NewlineDelimitedJson.remove_sharded_files ~directory ~filename_prefix;
                NewlineDelimitedJson.write_sharded_files
                  ~scheduler
                  ~directory
                  ~filename_prefix
                  ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
                  ~to_json_lines:call_graph_to_json
                  callables
          in
          ()
      | None -> ()
    in
    match dump_call_graph with
    | Some path ->
        Log.warning "Emitting the contents of the call graph to `%s`" (PyrePath.absolute path);
        NewlineDelimitedJson.write_file
          ~path
          ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
          ~to_json_lines:call_graph_to_json
          callables
    | None -> ()
end

(** The call graph of a function or method definition. *)
module DefineCallGraph = struct
  type t = ExpressionCallees.t ExpressionIdentifier.Map.t [@@deriving equal]

  let to_json call_graph =
    let bindings =
      ExpressionIdentifier.Map.fold
        (fun key data sofar ->
          (ExpressionIdentifier.json_key key, ExpressionCallees.to_json data) :: sofar)
        call_graph
        []
    in
    `Assoc (List.rev bindings)


  let pp formatter call_graph =
    let pp_pair formatter (key, value) =
      Format.fprintf formatter "@,%a -> %a" ExpressionIdentifier.pp key ExpressionCallees.pp value
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    call_graph
    |> ExpressionIdentifier.Map.to_alist
    |> Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs


  let show = Format.asprintf "%a" pp

  let empty = ExpressionIdentifier.Map.empty

  let is_empty = ExpressionIdentifier.Map.is_empty

  include MakeSaveCallGraph (struct
    type nonrec t = t

    let name = "call graphs"

    let is_empty = is_empty

    let to_json_alist call_graph = ["calls", to_json call_graph]
  end)

  let copy = Fn.id

  let for_test map =
    map
    |> ExpressionIdentifier.Map.to_alist
    |> List.map ~f:(fun (expression_identifier, callees) ->
           ExpressionIdentifier.json_key expression_identifier, callees)
    |> String.Map.of_alist_exn


  let equal_ignoring_types = ExpressionIdentifier.Map.equal ExpressionCallees.equal_ignoring_types

  let merge =
    ExpressionIdentifier.Map.merge (fun _ left right ->
        match left, right with
        | Some left, Some right -> Some (ExpressionCallees.join left right)
        | Some left, None -> Some left
        | None, Some right -> Some right
        | None, None -> None)


  module OnExistingCallees = struct
    type t =
      | WarnThenJoin
      | Join
      | Fail
      | Replace
  end

  let add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_for_logging
      ~expression_identifier
      ~callees
    =
    let () =
      match expression_for_logging with
      | Some expression_for_logging ->
          log
            ~debug
            "Resolved callees at `%a` for expression `%a`:@,%a "
            Location.pp
            (ExpressionIdentifier.location expression_identifier)
            Expression.pp
            expression_for_logging
            ExpressionCallees.pp
            callees
      | None -> ()
    in
    ExpressionIdentifier.Map.update expression_identifier (function
        | None -> Some callees
        | Some existing_callees when ExpressionCallees.equal existing_callees callees ->
            Some existing_callees
        | Some existing_callees -> (
            match on_existing_callees with
            | OnExistingCallees.WarnThenJoin ->
                (* TODO(T228078886): We should error here since it means we are visiting the same
                   expression twice and getting different results. However, this is hard to fix due
                   to the amount of AST lowering we perform. Let's just give a warning for now. *)
                Log.warning
                  "Invariant error: When trying to add callees for expression %a in callable %a, \
                   found different existing callees. This bug in Pysa might introduce false \
                   positives."
                  Target.pp_external
                  caller
                  ExpressionIdentifier.pp_json_key
                  expression_identifier;
                Some (ExpressionCallees.join existing_callees callees)
            | OnExistingCallees.Join -> Some (ExpressionCallees.join existing_callees callees)
            | OnExistingCallees.Fail ->
                Format.asprintf
                  "Invariant error: When trying to add callees for expression %a in callable %a, \
                   found different existing callees."
                  Target.pp_external
                  caller
                  ExpressionIdentifier.pp_json_key
                  expression_identifier
                |> failwith
            | OnExistingCallees.Replace -> Some callees))


  let set_callees ~error_if_new ~expression_identifier ~callees =
    ExpressionIdentifier.Map.update expression_identifier (function
        | None ->
            if error_if_new then
              failwith "unexpected: no pre-existing call graph edge"
            else
              Some callees
        | Some _ -> Some callees)


  let add_call_callees ~debug ~caller ~on_existing_callees ~location ~call ~callees =
    add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_identifier:(ExpressionIdentifier.of_call ~location call)
      ~expression_for_logging:(Some (Node.create_with_default_location (Expression.Call call)))
      ~callees:(ExpressionCallees.from_call callees)


  let add_identifier_callees ~debug ~caller ~on_existing_callees ~location ~identifier ~callees =
    add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_identifier:(ExpressionIdentifier.of_identifier ~location identifier)
      ~expression_for_logging:
        (Some (Node.create_with_default_location (Expression.Name (Name.Identifier identifier))))
      ~callees:(ExpressionCallees.from_identifier callees)


  let add_define_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~define:{ Define.signature = { name; _ }; _ }
      ~define_location
      ~callees
    =
    add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_identifier:(ExpressionIdentifier.of_define_statement define_location)
      ~expression_for_logging:
        (Some
           (Node.create_with_default_location
              (Expression.Name (Name.Identifier (Format.asprintf "def %a(): ..." Reference.pp name)))))
      ~callees:(ExpressionCallees.from_define callees)


  let add_return_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~return_expression
      ~statement_location
      ~callees
    =
    add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_identifier:(ExpressionIdentifier.of_return_statement statement_location)
      ~expression_for_logging:(Some return_expression)
      ~callees:(ExpressionCallees.from_return callees)


  let set_call_callees ~error_if_new ~location ~call ~callees =
    set_callees
      ~error_if_new
      ~expression_identifier:(ExpressionIdentifier.of_call ~location call)
      ~callees:(ExpressionCallees.from_call callees)


  let set_identifier_callees ~error_if_new ~location ~identifier ~identifier_callees =
    set_callees
      ~error_if_new
      ~expression_identifier:(ExpressionIdentifier.of_identifier ~location identifier)
      ~callees:(ExpressionCallees.from_identifier identifier_callees)


  let add_attribute_access_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~location
      ~attribute_access
      ~callees
    =
    add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_identifier:(ExpressionIdentifier.of_attribute_access ~location attribute_access)
      ~expression_for_logging:
        (Some
           (Node.create_with_default_location (Expression.Name (Name.Attribute attribute_access))))
      ~callees:(ExpressionCallees.from_attribute_access callees)


  let set_attribute_access_callees ~error_if_new ~location ~attribute_access ~callees =
    set_callees
      ~error_if_new
      ~expression_identifier:(ExpressionIdentifier.of_attribute_access ~location attribute_access)
      ~callees:(ExpressionCallees.from_attribute_access callees)


  let add_format_string_articifial_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~location
      ~format_string
      ~callees
    =
    add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_identifier:(ExpressionIdentifier.of_format_string_artificial ~location)
      ~expression_for_logging:(Some (Node.create_with_default_location format_string))
      ~callees:(ExpressionCallees.from_format_string_artificial callees)


  let add_format_string_stringify_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~location
      ~substring
      ~callees
    =
    add_callees
      ~debug
      ~caller
      ~on_existing_callees
      ~expression_identifier:(ExpressionIdentifier.of_format_string_stringify ~location)
      ~expression_for_logging:(Some (Node.create_with_default_location substring))
      ~callees:(ExpressionCallees.from_format_string_stringify callees)


  let set_define_callees ~error_if_new ~define_location ~callees =
    set_callees
      ~error_if_new
      ~expression_identifier:(ExpressionIdentifier.of_define_statement define_location)
      ~callees:(ExpressionCallees.from_define callees)


  let filter_empty_attribute_access =
    ExpressionIdentifier.Map.filter (fun _ callees ->
        not (ExpressionCallees.is_empty_attribute_access_callees callees))


  let update_expression_callees ~f = ExpressionIdentifier.Map.map f

  let map_call_target ~f ~map_call_if ~map_return_if =
    update_expression_callees ~f:(ExpressionCallees.map_call_target ~f ~map_call_if ~map_return_if)


  let map_target ~f = map_call_target ~f:(CallTarget.map_target ~f)

  let map_receiver_class ~f = map_call_target ~f:(CallTarget.map_receiver_class ~f)

  (* Ensure the taint analysis does not use these targets. *)
  let drop_decorated_targets = update_expression_callees ~f:ExpressionCallees.drop_decorated_targets

  let dedup_and_sort = update_expression_callees ~f:ExpressionCallees.dedup_and_sort

  let regenerate_call_indices ~indexer =
    (* The indices are relative to the locations. When location x is earlier than y, calls at
       location x have smaller indices than calls on location y. Hence here we rely on the
       assumption that the map is keyed on the locations. *)
    update_expression_callees ~f:(ExpressionCallees.regenerate_call_indices ~indexer)


  (** Return all callees of the call graph, depending on the use case. *)
  let all_targets ~use_case call_graph =
    call_graph
    |> ExpressionIdentifier.Map.data
    |> List.concat_map ~f:(ExpressionCallees.all_targets ~use_case)
    |> List.dedup_and_sort ~compare:Target.compare


  let resolve_expression call_graph ~expression_identifier =
    ExpressionIdentifier.Map.find_opt expression_identifier call_graph


  let resolve_call call_graph ~location ~call =
    resolve_expression
      call_graph
      ~expression_identifier:(ExpressionIdentifier.of_call ~location call)
    >>= function
    | ExpressionCallees.Call call -> Some call
    | _ -> None


  let resolve_attribute_access call_graph ~location ~attribute_access =
    resolve_expression
      call_graph
      ~expression_identifier:(ExpressionIdentifier.of_attribute_access ~location attribute_access)
    >>= function
    | ExpressionCallees.AttributeAccess attribute_access -> Some attribute_access
    | _ -> None


  let resolve_identifier call_graph ~location ~identifier =
    resolve_expression
      call_graph
      ~expression_identifier:(ExpressionIdentifier.of_identifier ~location identifier)
    >>= function
    | ExpressionCallees.Identifier identifier -> Some identifier
    | _ -> None


  let resolve_format_string_artificial call_graph ~location =
    resolve_expression
      call_graph
      ~expression_identifier:(ExpressionIdentifier.of_format_string_artificial ~location)
    >>= function
    | ExpressionCallees.FormatStringArtificial format_string_artificial ->
        Some format_string_artificial
    | _ -> None


  let resolve_format_string_stringify call_graph ~location =
    resolve_expression
      call_graph
      ~expression_identifier:(ExpressionIdentifier.of_format_string_stringify ~location)
    >>= function
    | ExpressionCallees.FormatStringStringify format_string_stringify ->
        Some format_string_stringify
    | _ -> None


  let resolve_define call_graph ~define_location =
    resolve_expression
      call_graph
      ~expression_identifier:(ExpressionIdentifier.of_define_statement define_location)
    >>= function
    | ExpressionCallees.Define call -> Some call
    | _ -> None


  let resolve_return call_graph ~statement_location =
    resolve_expression
      call_graph
      ~expression_identifier:(ExpressionIdentifier.of_return_statement statement_location)
    >>= function
    | ExpressionCallees.Return call -> Some call
    | _ -> None
end

(** Whole-program call graph, stored in the ocaml heap. This is a mapping from a callable to all its
    callees. *)
module WholeProgramCallGraph = struct
  type t = Target.t list Target.Map.Tree.t

  let empty = Target.Map.Tree.empty

  let is_empty = Target.Map.Tree.is_empty

  let of_alist_exn = Target.Map.Tree.of_alist_exn

  let add_or_exn ~callable ~callees call_graph =
    Target.Map.Tree.update call_graph callable ~f:(function
        | None -> callees
        | Some _ ->
            Format.asprintf "Program call graph already has callees for `%a`" Target.pp callable
            |> failwith)


  let fold graph ~init ~f =
    Target.Map.Tree.fold graph ~init ~f:(fun ~key:target ~data:callees -> f ~target ~callees)


  let merge_disjoint left right =
    Target.Map.Tree.merge_skewed
      ~combine:(fun ~key:_ _ _ -> failwith "call graphs are not disjoint")
      left
      right


  let to_target_graph graph = graph

  let number_edges graph =
    (* The number of edges should fit on an `int` (30 bits + 1 sign bit), but let's use Int64 and
       convert to an int at the end to catch overflows. This is just to be overly cautious. *)
    fold graph ~init:Int64.zero ~f:(fun ~target:_ ~callees count ->
        Int64.( + ) count (Int64.of_int (List.length callees)))
    |> Int64.to_int_exn
end

(** Call graphs of callables, stored in the shared memory. This is a mapping from a callable to its
    `DefineCallGraph.t`. *)
module SharedMemory = struct
  module T =
    SaveLoadSharedMemory.MakeKeyValue
      (Target.SharedMemoryKey)
      (struct
        type t = DefineCallGraph.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let handle_prefix = Hack_parallel.Std.Prefix.make ()

        let description = "call graphs of defines"
      end)

  include T

  type call_graphs = {
    whole_program_call_graph: WholeProgramCallGraph.t;
    define_call_graphs: T.t;
  }

  module ReadOnly = struct
    type t = T.ReadOnly.t

    let get handle ~cache ~callable = T.ReadOnly.get handle ~cache callable
  end

  let callables = T.keys

  let read_only = T.read_only

  let cleanup = T.cleanup

  let save_to_cache = T.save_to_cache

  let load_from_cache = T.load_from_cache
end
