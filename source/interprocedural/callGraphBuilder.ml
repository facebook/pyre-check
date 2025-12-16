(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* CallGraphBuilder: implements the logic to statically compute the call graph,
 * given a function definition.
 *
 * Note that the call graph is highly tuned for the taint analysis and might be
 * unsound for other analyses.
 *)

open Core
open Ast
open Statement
open Expression
open Pyre
open CallGraph
module TaintAccessPath = Analysis.TaintAccessPath
module PyrePysaLogic = Analysis.PyrePysaLogic
module AstResult = PyrePysaApi.AstResult

let log ~debug format =
  if debug then
    Log.dump format
  else
    Log.log ~section:`CallGraph format


module ReturnTypeBuilder = struct
  let from_annotation ~pyre_api annotation =
    PyrePysaApi.ReadOnly.Type.scalar_properties
      pyre_api
      (PyrePysaApi.PysaType.from_pyre1_type annotation)


  (* Try to infer the return type from the callable type, otherwise lazily fallback
   * to the resolved return type. *)
  let from_callable_with_fallback ~pyre_api ~callable_type ~return_type =
    let annotation =
      match callable_type with
      | Type.Callable { implementation = { annotation; _ }; overloads = []; _ }
        when Type.Variable.all_variables_are_resolved annotation ->
          annotation
      | _ -> Lazy.force return_type
    in
    PyrePysaApi.ReadOnly.Type.scalar_properties
      pyre_api
      (PyrePysaApi.PysaType.from_pyre1_type annotation)
end

let is_implicit_receiver ~is_static_method ~is_class_method ~explicit_receiver target =
  if is_static_method then
    false
  else if is_class_method then
    true
  else
    (not explicit_receiver) && Target.is_method_or_override target


module CallTargetBuilder = struct
  let receiver_class_from_type ~is_class_method annotation =
    annotation
    |> CallResolution.strip_optional
    |> CallResolution.strip_readonly
    |> CallResolution.unbind_type_variable
    |> Type.split
    |> fun (annotation, parameters) ->
    (Type.primitive_name annotation, parameters)
    |> function
    | Some "type", [Type.Record.Argument.Single parameter] when is_class_method -> (
        (* The receiver is the class itself. Technically, the receiver class type should be
           `Type[int]`. However, we strip away the `type` part since it is implied by the
           `is_class_method` flag. *)
        match parameter with
        | Type.Primitive class_name
        | Type.Parametric { name = class_name; _ } ->
            Some class_name
        | _ -> None)
    | Some "type", _ -> None
    | Some "super", _ -> None
    | name, _ -> name


  let create_with_default_index
      ~implicit_dunder_call
      ~return_type
      ?receiver_type
      ?(is_class_method = false)
      ?(is_static_method = false)
      ?(explicit_receiver = false)
      target
    =
    {
      CallTarget.target;
      implicit_receiver =
        is_implicit_receiver ~is_static_method ~is_class_method ~explicit_receiver target;
      implicit_dunder_call;
      index = 0;
      return_type;
      receiver_class = receiver_type >>= receiver_class_from_type ~is_class_method;
      is_class_method;
      is_static_method;
    }
end

let is_local identifier = String.is_prefix ~prefix:"$" identifier

let rec is_all_names = function
  | Expression.Name (Name.Identifier identifier) when not (is_local identifier) -> true
  | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
      is_all_names (Node.value base)
  | _ -> false


module CalleeKind = struct
  type t =
    | Method of { is_direct_call: bool }
    | Function

  let rec from_callee ~pyre_in_context ~callables_to_definitions_map callee callee_type =
    let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
    let is_super_call =
      let rec is_super callee =
        match Node.value callee with
        | Expression.Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
        | Call { callee; _ } -> is_super callee
        | Name (Name.Attribute { base; _ }) -> is_super base
        | _ -> false
      in
      is_super callee
    in
    match callee_type with
    | _ when is_super_call -> Method { is_direct_call = true }
    | Type.Parametric { name = "BoundMethod"; _ } ->
        Method { is_direct_call = is_all_names (Node.value callee) }
    | Type.Callable _ -> (
        match Node.value callee with
        | Expression.Name (Name.Attribute { base; _ }) ->
            let parent_type =
              CallResolution.resolve_ignoring_errors
                ~pyre_in_context
                ~callables_to_definitions_map
                base
            in
            let is_class () =
              let primitive, _ = Type.split parent_type in
              Type.primitive_name primitive
              >>| PyrePysaApi.ReadOnly.class_exists pyre_api
              |> Option.value ~default:false
            in
            if Type.is_class_type parent_type then
              Method { is_direct_call = true }
            else if is_class () then
              Method { is_direct_call = false }
            else
              Function
        | _ -> Function)
    | Type.Union (callee_type :: _) ->
        from_callee ~pyre_in_context ~callables_to_definitions_map callee callee_type
    | _ ->
        (* We must be dealing with a callable class. *)
        Method { is_direct_call = false }
end

let strip_optional annotation = Type.optional_value annotation |> Option.value ~default:annotation

let strip_meta annotation =
  if Type.is_class_type annotation then
    Type.single_argument annotation
  else
    annotation


(* Figure out what target to pick for an indirect call that resolves to implementation_target.
   E.g., if the receiver type is A, and A derives from Base, and the target is Base.method, then
   targeting the override tree of Base.method is wrong, as it would include all siblings for A.

 * Instead, we have the following cases:
 * a) receiver type matches implementation_target's declaring type -> override implementation_target
 * b) no implementation_target override entries are subclasses of A -> real implementation_target
 * c) some override entries are subclasses of A -> search upwards for actual implementation,
 *    and override all those where the override name is
 *  1) the override target if it exists in the override shared mem
 *  2) the real target otherwise
 *)
let compute_indirect_targets ~pyre_in_context ~override_graph ~receiver_type implementation_target =
  (* Target name must be the resolved implementation target *)
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
  let get_class_type = PyrePysaApi.ReadOnly.parse_reference pyre_api in
  let get_actual_target method_name =
    match override_graph with
    | Some override_graph
      when OverrideGraph.SharedMemory.ReadOnly.overrides_exist override_graph method_name ->
        method_name
        |> Target.as_regular_exn
        (* TODO(T204630385): Handle `Target.Parameterized` with `Override`. *)
        |> Target.Regular.get_corresponding_override_exn
        |> Target.from_regular
    | _ -> method_name
  in
  let receiver_type = receiver_type |> strip_meta |> strip_optional |> Type.weaken_literals in
  let declaring_type, method_name, kind =
    match Target.get_regular implementation_target with
    | Target.Regular.Method { class_name; method_name; kind } ->
        Reference.create class_name, method_name, kind
    | _ -> failwith "Unexpected target"
  in
  if Reference.equal declaring_type (Type.class_name receiver_type) then (* case a *)
    [get_actual_target implementation_target]
  else
    let overriding_types =
      match override_graph with
      | Some override_graph ->
          OverrideGraph.SharedMemory.ReadOnly.get_overriding_types
            override_graph
            ~member:implementation_target
      | None -> None
    in
    match overriding_types with
    | None ->
        (* case b *)
        [implementation_target]
    | Some overriding_types ->
        (* case c *)
        let keep_subtypes candidate =
          let candidate_type = get_class_type candidate in
          try
            PyrePysaApi.ReadOnly.less_or_equal pyre_api ~left:candidate_type ~right:receiver_type
          with
          | Analysis.ClassHierarchy.Untracked untracked_type ->
              Log.warning
                "Found untracked type `%s` when comparing `%a` and `%a`. The class `%a` will be \
                 considered a subclass of `%a`, which could lead to false positives."
                untracked_type
                Type.pp
                candidate_type
                Type.pp
                receiver_type
                Type.pp
                candidate_type
                Type.pp
                receiver_type;
              true
        in
        let override_targets =
          let create_override_target class_name =
            get_actual_target (Target.create_method ~kind class_name method_name)
          in
          List.filter overriding_types ~f:keep_subtypes
          |> fun subtypes -> List.map subtypes ~f:create_override_target
        in
        implementation_target :: override_targets


let rec resolve_callees_from_type
    ~debug
    ~callables_to_definitions_map
    ~pyre_in_context
    ~override_graph
    ?(dunder_call = false)
    ?receiver_type
    ~return_type
    ~callee_kind
    callable_type
  =
  let resolve_callees_from_type ?(dunder_call = dunder_call) =
    resolve_callees_from_type ~dunder_call
  in
  let pp_callable_type format callable_type =
    Format.fprintf
      format
      "callable type %a (i.e., %s)"
      Type.pp
      callable_type
      (Type.show_type_t callable_type)
  in
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
  match callable_type with
  | Type.Callable { kind = Named name; _ } -> (
      let return_type =
        ReturnTypeBuilder.from_callable_with_fallback ~pyre_api ~callable_type ~return_type
      in
      match receiver_type with
      | Some receiver_type ->
          let targets =
            match callee_kind with
            | CalleeKind.Method { is_direct_call = true; _ } ->
                [Target.create_method_from_reference name]
            | _ ->
                compute_indirect_targets
                  ~pyre_in_context
                  ~override_graph
                  ~receiver_type
                  (Target.create_method_from_reference name)
          in
          let targets =
            List.map
              ~f:(fun target ->
                let is_class_method, is_static_method =
                  CallablesSharedMemory.ReadOnly.get_method_kind callables_to_definitions_map target
                in
                CallTargetBuilder.create_with_default_index
                  ~implicit_dunder_call:dunder_call
                  ~return_type:(Some return_type)
                  ~receiver_type
                  ~is_class_method
                  ~is_static_method
                  target)
              targets
          in
          CallCallees.create ~call_targets:targets ()
      | None -> (
          let target =
            CallablesSharedMemory.ReadOnly.callable_from_reference callables_to_definitions_map name
          in
          match target with
          | Some target ->
              let is_class_method, is_static_method =
                CallablesSharedMemory.ReadOnly.get_method_kind callables_to_definitions_map target
              in
              CallCallees.create
                ~call_targets:
                  [
                    CallTargetBuilder.create_with_default_index
                      ~explicit_receiver:true
                      ~implicit_dunder_call:dunder_call
                      ~return_type:(Some return_type)
                      ~is_class_method
                      ~is_static_method
                      ?receiver_type
                      target;
                  ]
                ()
          | None ->
              CallCallees.unresolved
                ~debug
                ~message:
                  (lazy
                    (Format.asprintf
                       "type resolution returned unknown callable %a"
                       Reference.pp
                       name))
                ~reason:Unresolved.UnknownCallableFromType
                ()))
  | Type.Callable { kind = Anonymous; _ } ->
      CallCallees.unresolved
        ~debug
        ~message:(lazy (Format.asprintf "%a has kind `Anonymous`" pp_callable_type callable_type))
        ~reason:Unresolved.AnonymousCallableType
        ()
  | Type.Parametric { name = "BoundMethod"; arguments = [Single callable; Single receiver_type] } ->
      resolve_callees_from_type
        ~debug
        ~callables_to_definitions_map
        ~pyre_in_context
        ~override_graph
        ~receiver_type
        ~return_type
        ~callee_kind
        callable
  | Type.Union (element :: elements) ->
      let first_targets =
        resolve_callees_from_type
          ~debug
          ~callables_to_definitions_map
          ~pyre_in_context
          ~override_graph
          ~callee_kind
          ?receiver_type
          ~return_type
          element
      in
      List.fold elements ~init:first_targets ~f:(fun combined_targets new_target ->
          resolve_callees_from_type
            ~debug
            ~callables_to_definitions_map
            ~pyre_in_context
            ~override_graph
            ?receiver_type
            ~return_type
            ~callee_kind
            new_target
          |> CallCallees.join combined_targets)
  | Type.Parametric { name = "type"; arguments = [Single class_type] } ->
      resolve_constructor_callee
        ~debug
        ~callables_to_definitions_map
        ~pyre_in_context
        ~override_graph
        class_type
      |> CallCallees.default_to_unresolved
           ~debug
           ~message:
             (lazy
               (Format.asprintf
                  "Failed to resolve construct callees from %a"
                  pp_callable_type
                  callable_type))
           ~reason:Unresolved.UnknownConstructorCallable
  | callable_type -> (
      (* Handle callable classes. `typing.Type` interacts specially with __call__, so we choose to
         ignore it for now to make sure our constructor logic via `cls()` still works. *)
      match
        CallResolution.resolve_attribute_access_ignoring_untracked
          ~pyre_in_context
          ~base_type:callable_type
          ~attribute:"__call__"
      with
      | Type.Any
      | Type.Top ->
          CallCallees.unresolved
            ~debug
            ~message:
              (lazy
                (Format.asprintf
                   "Resolved `Any` or `Top` when treating %a as callable class"
                   pp_callable_type
                   callable_type))
            ~reason:Unresolved.AnyTopCallableClass
            ()
      (* Callable protocol. *)
      | Type.Callable { kind = Anonymous; _ } as resolved_dunder_call ->
          Type.primitive_name callable_type
          >>| (fun primitive_callable_name ->
                let return_type =
                  ReturnTypeBuilder.from_callable_with_fallback
                    ~pyre_api
                    ~callable_type:resolved_dunder_call
                    ~return_type
                in
                let target =
                  Target.create_method (Reference.create primitive_callable_name) "__call__"
                in
                let is_class_method, is_static_method =
                  CallablesSharedMemory.ReadOnly.get_method_kind callables_to_definitions_map target
                in
                CallCallees.create
                  ~call_targets:
                    [
                      CallTargetBuilder.create_with_default_index
                        ~implicit_dunder_call:true
                        ~return_type:(Some return_type)
                        ~is_class_method
                        ~is_static_method
                        ?receiver_type
                        target;
                    ]
                  ())
          |> CallCallees.default_to_unresolved
               ~debug
               ~message:
                 (lazy
                   (Format.asprintf
                      "Failed to resolve protocol from %a"
                      pp_callable_type
                      callable_type))
               ~reason:Unresolved.UnknownCallableProtocol
      | annotation ->
          if not dunder_call then
            resolve_callees_from_type
              ~debug
              ~callables_to_definitions_map
              ~pyre_in_context
              ~override_graph
              ~return_type
              ~dunder_call:true
              ~callee_kind
              annotation
          else
            CallCallees.unresolved
              ~debug
              ~message:
                (lazy
                  (Format.asprintf
                     "Failed to resolve %a as callable class, protocol, or a non dunder call."
                     pp_callable_type
                     callable_type))
              ~reason:Unresolved.UnknownCallableClass
              ())


and resolve_callees_from_type_external
    ~pyre_in_context
    ~callables_to_definitions_map
    ~override_graph
    ~return_type
    ?(dunder_call = false)
    callee
  =
  let callee_type =
    CallResolution.resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map callee
  in
  let callee_kind =
    CalleeKind.from_callee ~pyre_in_context ~callables_to_definitions_map callee callee_type
  in
  resolve_callees_from_type
    ~debug:false
    ~callables_to_definitions_map
    ~pyre_in_context
    ~override_graph
    ~dunder_call
    ~return_type
    ~callee_kind
    callee_type


and resolve_constructor_callee
    ~debug
    ~callables_to_definitions_map
    ~pyre_in_context
    ~override_graph
    class_type
  =
  let meta_type = Type.class_type class_type in
  match
    ( CallResolution.resolve_attribute_access_ignoring_untracked
        ~pyre_in_context
        ~base_type:meta_type
        ~attribute:"__new__",
      CallResolution.resolve_attribute_access_ignoring_untracked
        ~pyre_in_context
        ~base_type:meta_type
        ~attribute:"__init__" )
  with
  | Type.Any, _
  | Type.Top, _
  | _, Type.Any
  | _, Type.Top ->
      None
  | new_callable_type, init_callable_type ->
      let new_callees =
        resolve_callees_from_type
          ~debug
          ~callables_to_definitions_map
          ~pyre_in_context
          ~override_graph
          ~receiver_type:meta_type
          ~return_type:(lazy class_type)
          ~callee_kind:(Method { is_direct_call = true })
            (* __new__() is a static method. See
               https://docs.python.org/3/reference/datamodel.html#object.__new__ *)
          new_callable_type
      in
      let init_callees =
        resolve_callees_from_type
          ~debug
          ~callables_to_definitions_map
          ~pyre_in_context
          ~override_graph
          ~receiver_type:meta_type
          ~return_type:(lazy Type.none)
          ~callee_kind:(Method { is_direct_call = true })
          init_callable_type
      in
      (* Technically, `object.__new__` returns `object` and `C.__init__` returns None.
       * In practice, we actually want to use the class type. *)
      let return_type =
        let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
        ReturnTypeBuilder.from_annotation ~pyre_api class_type
      in
      let set_return_type call_target =
        { call_target with CallTarget.return_type = Some return_type }
      in
      let unset_receiver_class call_target =
        { call_target with CallTarget.receiver_class = None }
      in
      Some
        (CallCallees.create
           ~new_targets:
             (new_callees.call_targets
             |> List.map ~f:set_return_type
             |> List.map ~f:unset_receiver_class)
           ~init_targets:
             (init_callees.call_targets
             |> List.map ~f:set_return_type
             |> List.map ~f:unset_receiver_class)
           ~unresolved:(Unresolved.join new_callees.unresolved init_callees.unresolved)
           ())


let resolve_callee_from_defining_expression
    ~debug
    ~callables_to_definitions_map
    ~pyre_in_context
    ~override_graph
    ~callee:{ Node.value = callee; _ }
    ~return_type
    ~implementing_class
  =
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
  match implementing_class, callee with
  | Type.Top, Expression.Name name when is_all_names callee ->
      (* If implementing_class is unknown, this must be a function rather than a method. We can use
         global resolution on the callee. *)
      PyrePysaApi.ReadOnly.global pyre_api (Ast.Expression.name_to_reference_exn name)
      >>= PyrePysaLogic.undecorated_signature_of_global
      >>| fun undecorated_signature ->
      resolve_callees_from_type
        ~debug
        ~callables_to_definitions_map
        ~pyre_in_context
        ~override_graph
        ~return_type
        ~callee_kind:Function
        (Type.Callable undecorated_signature)
  | _ -> (
      let implementing_class_name =
        if Type.is_class_type implementing_class then
          Type.arguments implementing_class
          >>= fun parameters ->
          List.nth parameters 0
          >>= function
          | Single implementing_class -> Some implementing_class
          | _ -> None
        else
          Some implementing_class
      in
      match implementing_class_name with
      | Some implementing_class_name ->
          let class_primitive =
            match implementing_class_name with
            | Parametric { name; _ } -> Some name
            | Primitive name -> Some name
            | _ -> None
          in
          let method_name =
            match callee with
            | Expression.Name (Name.Attribute { attribute; _ }) -> Some attribute
            | _ -> None
          in
          method_name
          >>= (fun method_name ->
                class_primitive >>| fun class_name -> Format.sprintf "%s.%s" class_name method_name)
          >>| Reference.create
          (* Here, we blindly reconstruct the callable instead of going through the global
             resolution, as Pyre doesn't have an API to get the undecorated signature of methods. *)
          >>= fun name ->
          let callable_type =
            Type.Callable
              {
                Type.Callable.kind = Named name;
                implementation =
                  { annotation = Lazy.force return_type; parameters = Type.Callable.Defined [] };
                overloads = [];
              }
          in
          Some
            (resolve_callees_from_type
               ~debug
               ~callables_to_definitions_map
               ~pyre_in_context
               ~override_graph
               ~return_type
               ~receiver_type:implementing_class
               ~callee_kind:(Method { is_direct_call = false })
               callable_type)
      | _ -> None)


type resolved_stringify =
  | Str
  | Repr

let resolve_stringify_call ~pyre_in_context ~type_of_expression_shared_memory expression =
  let string_callee =
    Node.create
      ~location:(Node.location expression)
      (Expression.Name
         (Name.Attribute
            {
              base = expression;
              attribute = "__str__";
              origin =
                Some
                  (Origin.create
                     ?base:(Ast.Expression.origin expression)
                     ~location:(Node.location expression)
                     Origin.ResolveStrCall);
            }))
  in
  try
    match
      TypeOfExpressionSharedMemory.compute_or_retrieve_pyre_type
        type_of_expression_shared_memory
        ~pyre_in_context
        string_callee
      |> Type.callable_name
    with
    | Some name when Reference.equal name (Reference.create "object.__str__") ->
        (* Call resolved to object.__str__, fallback to calling __repr__ if it exists. *)
        Repr
    | _ -> Str
  with
  | Analysis.ClassHierarchy.Untracked _ -> Str


(* Rewrite certain calls for the interprocedural analysis (e.g, pysa).
 * These rewrites are done symbolically during the analysis.
 * These should be preferred over AST transformations (see `preprocess_special_calls`). *)
let apply_identified_shim_call ~identified_callee ~arguments =
  let open Shims.ShimArgumentMapping in
  match identified_callee, arguments with
  | Some Shims.IdentifiedCallee.FunctoolsPartial, _actual_callable :: actual_arguments ->
      Some
        {
          identifier = "functools.partial";
          callee = Target.Argument { index = 0 };
          arguments =
            List.mapi actual_arguments ~f:(fun index_minus_one { Call.Argument.name; _ } ->
                {
                  Argument.name = name >>| Node.value;
                  value = Target.Argument { index = index_minus_one + 1 };
                });
        }
  | ( Some Shims.IdentifiedCallee.MultiprocessingProcess,
      [
        { Call.Argument.name = Some { Node.value = target; _ }; _ };
        {
          Call.Argument.value = { Node.value = Expression.Tuple process_arguments; _ };
          name = Some { Node.value = args; _ };
        };
      ] )
    when target |> Analysis.TaintAccessPath.Root.chop_parameter_prefix |> String.equal "target"
         && args |> Analysis.TaintAccessPath.Root.chop_parameter_prefix |> String.equal "args" ->
      Some
        {
          identifier = "multiprocessing.Process";
          callee = Target.Argument { index = 0 };
          arguments =
            List.mapi process_arguments ~f:(fun index _ ->
                {
                  Argument.name = None;
                  value = Target.GetTupleElement { index; inner = Target.Argument { index = 1 } };
                });
        }
  | _ -> None


let shim_special_calls_for_pyre1 { Call.callee; arguments; origin = _ } =
  let identified_callee =
    match Node.value callee with
    | Expression.Name
        (Name.Attribute
          {
            base = { Node.value = Expression.Name (Name.Identifier "functools"); _ };
            attribute = "partial";
            _;
          }) ->
        Some Shims.IdentifiedCallee.FunctoolsPartial
    | Expression.Name
        (Name.Attribute
          {
            base = { Node.value = Expression.Name (Name.Identifier "multiprocessing"); _ };
            attribute = "Process";
            _;
          }) ->
        Some Shims.IdentifiedCallee.MultiprocessingProcess
    | _ -> None
  in
  apply_identified_shim_call ~identified_callee ~arguments


let shim_special_calls_for_pyrefly ~callees ~arguments =
  let define_name_equals ~name target =
    Target.get_regular target
    |> Target.Regular.define_name
    >>| Reference.show
    >>| String.equal name
    |> Option.value ~default:false
  in
  let identified_callee =
    if List.exists callees ~f:(define_name_equals ~name:"functools.partial.__new__") then
      Some Shims.IdentifiedCallee.FunctoolsPartial
    else if
      List.exists
        callees
        ~f:(define_name_equals ~name:"multiprocessing.process.BaseProcess.__init__")
    then
      Some Shims.IdentifiedCallee.MultiprocessingProcess
    else
      None
  in
  apply_identified_shim_call ~identified_callee ~arguments


(* Rewrite certain calls for the interprocedural analysis (e.g, pysa).
 * These rewrites are done as AST transformations. In general, this should be
 * avoided, hence this is only used for a few specific functions in the standard
 * library. One main difference between preprocessing and shimming is that shims
 * are considered additional calls, where preprocessing completely removes the
 * original call. This is preferred for things like `str`/`iter`/`next`. *)
let preprocess_special_calls
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~location:call_location
    {
      Call.callee = { Node.location = callee_location; _ } as callee;
      arguments;
      origin = call_origin;
    }
  =
  let attribute_access ~base ~method_name ~origin =
    {
      Node.value = Expression.Name (Name.Attribute { base; attribute = method_name; origin });
      location = callee_location;
    }
  in
  match Node.value callee, arguments with
  | Name (Name.Identifier "str"), [{ Call.Argument.value; _ }] ->
      (* str() takes an optional encoding and errors - if these are present, the call shouldn't be
         redirected: https://docs.python.org/3/library/stdtypes.html#str *)
      let method_name, origin_kind =
        match resolve_stringify_call ~pyre_in_context ~type_of_expression_shared_memory value with
        | Str -> "__str__", Origin.StrCallToDunderStr
        | Repr -> "__repr__", Origin.StrCallToDunderRepr
      in
      let origin = Some (Origin.create ?base:call_origin ~location:call_location origin_kind) in
      let callee = attribute_access ~base:value ~method_name ~origin in
      Some { Call.callee; arguments = []; origin }
  | Name (Name.Identifier "iter"), [{ Call.Argument.value; _ }] ->
      (* Only handle `iter` with a single argument here. *)
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.IterCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__iter__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "next"), [{ Call.Argument.value; _ }] ->
      (* Only handle `next` with a single argument here. *)
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.NextCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__next__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "anext"), [{ Call.Argument.value; _ }] ->
      (* Only handle `anext` with a single argument here. *)
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.NextCall) in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__anext__" ~origin;
          arguments = [];
          origin;
        }
  | _ -> None


let shim_for_call ~pyre_in_context ~callables_to_definitions_map call =
  match shim_special_calls_for_pyre1 call with
  | Some shim -> Some shim
  | None ->
      SpecialCallResolution.shim_calls_for_pyre1
        ~resolve_expression_to_type:
          (CallResolution.resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map)
        call


let shim_for_call_for_pyrefly ~callees ~nested_callees ~arguments =
  match shim_special_calls_for_pyrefly ~callees ~arguments with
  | Some identified_callee -> Some identified_callee
  | None -> SpecialCallResolution.shim_calls_for_pyrefly ~callees ~nested_callees ~arguments


let create_shim_callee_expression ~debug ~callable ~location ~call shim =
  log ~debug "Found shim for call: %a" Shims.ShimArgumentMapping.pp shim;
  Shims.ShimArgumentMapping.create_artificial_call ~call_location:location call shim
  |> function
  | Ok { Call.callee = shim_callee_expression; _ } -> Some shim_callee_expression
  | Error error ->
      let () =
        Log.warning
          "Error applying shim argument mapping: %s for expression `%a` in `%a` at %a"
          error
          Expression.pp
          (Node.create_with_default_location (Expression.Call call))
          Target.pp
          callable
          Location.pp
          location
      in
      None


let preprocess_call ~pyre_in_context ~type_of_expression_shared_memory ~location original_call =
  match
    preprocess_special_calls
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~location
      original_call
  with
  | Some call -> call
  | None -> (
      match
        Analysis.AnnotatedCall.preprocess_special_calls
          ~resolve_expression_to_type:
            (TypeOfExpressionSharedMemory.compute_or_retrieve_pyre_type
               type_of_expression_shared_memory
               ~pyre_in_context)
          ~location
          original_call
      with
      | Some call -> call
      | None -> original_call)


let rec preprocess_expression
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~callable
    expression
  =
  (* This uses `Expression.Mapper` to recursively rewrite the given expression.
   *
   * Each `map_XXX` function is responsible for calling `Mapper.map` on sub-expressions to
   * properly recurse down the AST. This is why we sometimes call `default_map_XXX`.
   *
   * The mapper will use the same `pyre_in_context` for all sub-expressions. If
   * we need to update the context (for instance, for generators), we need to call
   * `preprocess_expression ~pyre_in_context` with the new context instead of
   * calling `Mapper.map`. This is why this function is recursive.
   *)
  let map_binary_operator ~mapper ~location ({ BinaryOperator.left; _ } as binary_operator) =
    BinaryOperator.lower_to_call ~location ~callee_location:left.Node.location binary_operator
    |> Mapper.default_map_call_node ~mapper ~location
  in
  let map_comparison_operator
      ~mapper
      ~location
      ({ ComparisonOperator.left; _ } as comparison_operator)
    =
    match
      ComparisonOperator.lower_to_expression
        ~location
        ~callee_location:left.location
        comparison_operator
    with
    | Some { Node.value = Expression.Call call; _ } ->
        Mapper.default_map_call_node ~mapper ~location call
    | _ -> Mapper.default_map_comparison_operator_node ~mapper ~location comparison_operator
  in
  let map_slice ~mapper ~location slice =
    Slice.lower_to_call ~location slice |> Mapper.default_map_call_node ~mapper ~location
  in
  let map_subscript ~mapper ~location { Subscript.base; index; origin = subscript_origin } =
    let origin = Some (Origin.create ?base:subscript_origin ~location Origin.SubscriptGetItem) in
    Expression.Call
      {
        Call.callee =
          {
            Node.value =
              Expression.Name
                (Name.Attribute
                   { base = Mapper.map ~mapper base; attribute = "__getitem__"; origin });
            location = Node.location base;
          };
        arguments = [{ Call.Argument.value = Mapper.map ~mapper index; name = None }];
        origin;
      }
    |> Node.create ~location
  in
  let map_comprehension_generators generators =
    let fold_generator
        (generators, outer_pyre_context)
        ({ Comprehension.Generator.target; iterator; conditions; async } as generator)
      =
      let inner_pyre_context =
        Statement.generator_assignment generator
        |> PyrePysaApi.InContext.resolve_assignment outer_pyre_context
      in
      (* We need to preprocess conditions with the new pyre context. We need to call
         `preprocess_expression` instead of `Mapper.map` *)
      let conditions =
        List.map
          ~f:
            (preprocess_expression
               ~pyre_in_context:inner_pyre_context
               ~type_of_expression_shared_memory
               ~callable)
          conditions
      in
      (* We explicitly do NOT preprocess the target and iterator since we need to call
         `generator_assignment` + `resolve_assignment` during the taint fixpoint, using the original
         expressions. Updating the `target` and `iterator` here would lead to inconsistencies of the
         type context. *)
      let generator = { Comprehension.Generator.target; iterator; conditions; async } in
      generator :: generators, inner_pyre_context
    in
    let reversed_generators, pyre_in_context =
      List.fold ~f:fold_generator ~init:([], pyre_in_context) generators
    in
    List.rev reversed_generators, pyre_in_context
  in
  let map_comprehension ~mapper:_ ~location ~make_node { Comprehension.element; generators } =
    let generators, pyre_in_context = map_comprehension_generators generators in
    {
      Comprehension.element =
        preprocess_expression ~pyre_in_context ~type_of_expression_shared_memory ~callable element;
      generators;
    }
    |> make_node
    |> Node.create ~location
  in
  let map_generator = map_comprehension ~make_node:(fun e -> Expression.Generator e) in
  let map_list_comprehension =
    map_comprehension ~make_node:(fun e -> Expression.ListComprehension e)
  in
  let map_set_comprehension =
    map_comprehension ~make_node:(fun e -> Expression.SetComprehension e)
  in
  let map_dictionary_comprehension
      ~mapper:_
      ~location
      { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators }
    =
    let generators, pyre_in_context = map_comprehension_generators generators in
    Expression.DictionaryComprehension
      {
        Comprehension.element =
          {
            Dictionary.Entry.KeyValue.key =
              preprocess_expression ~pyre_in_context ~type_of_expression_shared_memory ~callable key;
            value =
              preprocess_expression
                ~pyre_in_context
                ~type_of_expression_shared_memory
                ~callable
                value;
          };
        generators;
      }
    |> Node.create ~location
  in
  let map_call ~mapper ~location call =
    preprocess_call ~pyre_in_context ~type_of_expression_shared_memory ~location call
    |> Mapper.default_map_call_node ~mapper ~location
  in
  Mapper.map
    ~mapper:
      (Mapper.create_default
         ~map_binary_operator
         ~map_comparison_operator
         ~map_slice
         ~map_subscript
         ~map_generator
         ~map_dictionary_comprehension
         ~map_list_comprehension
         ~map_set_comprehension
         ~map_call
         ())
    expression


let preprocess_assignments statement =
  let statement =
    (* Note that there are cases where we perform two consecutive redirects.
     * For instance, for `d[j] += x` *)
    match statement with
    | {
     Node.value = Statement.AugmentedAssign ({ AugmentedAssign.target; _ } as augmented_assignment);
     location;
    } ->
        let call =
          AugmentedAssign.lower_to_expression
            ~location
            ~callee_location:target.Node.location
            augmented_assignment
        in
        let target =
          map_origin
            ~f:(fun origin ->
              Some
                (Origin.create
                   ?base:origin
                   ~location:(Node.location target)
                   Origin.AugmentedAssignLHS))
            target
        in
        {
          Node.location;
          value =
            Statement.Assign
              {
                Assign.target;
                annotation = None;
                value = Some call;
                origin = Some (Origin.create ~location Origin.AugmentedAssignStatement);
              };
        }
    | _ -> statement
  in
  match statement with
  | {
   Node.value =
     Statement.Assign
       {
         Assign.target =
           { Node.value = Expression.Subscript { base; index; origin = subscript_origin; _ }; _ };
         value = Some value_expression;
         origin = assign_origin;
         _;
       };
   location;
  } ->
      (* TODO(T187636576): For now, we translate assignments such as `d[a] = b` into
         `d.__setitem__(a, b)`. Unfortunately, this won't work for multi-target assignments such as
         `x, y[a], z = w`. In the future, we should implement proper logic to handle those. *)
      let index_argument = { Call.Argument.value = index; name = None } in
      let value_argument = { Call.Argument.value = value_expression; name = None } in
      {
        Node.location;
        value =
          Statement.Expression
            {
              Node.location;
              value =
                Expression.Call
                  {
                    callee =
                      {
                        value =
                          Name
                            (Name.Attribute
                               {
                                 base;
                                 attribute = "__setitem__";
                                 origin =
                                   Some
                                     (Origin.create
                                        ?base:subscript_origin
                                        ~location
                                        Origin.SubscriptSetItem);
                               });
                        location;
                      };
                    arguments = [index_argument; value_argument];
                    origin =
                      Some (Origin.create ?base:assign_origin ~location Origin.SubscriptSetItem);
                  };
            };
      }
  | statement -> statement


let preprocess_parameter_default_value = preprocess_expression

(* This must be called *once* before analyzing a statement in a control flow graph. *)
let preprocess_statement ~pyre_in_context ~type_of_expression_shared_memory ~callable statement =
  (* First, preprocess assignments *)
  let { Node.location; value } = preprocess_assignments statement in
  (* Then, preprocess expressions nested witin the statement *)
  let preprocess_expression =
    preprocess_expression ~pyre_in_context ~type_of_expression_shared_memory ~callable
  in
  let value =
    match value with
    | Statement.Assign { target; value; annotation; origin } ->
        Statement.Assign
          {
            target = preprocess_expression target;
            value = Option.map ~f:preprocess_expression value;
            annotation;
            origin;
          }
    | Assert { test; message; origin } ->
        Statement.Assert { test = preprocess_expression test; message; origin }
    | Delete expressions -> Statement.Delete (List.map ~f:preprocess_expression expressions)
    | Expression expression -> Statement.Expression (preprocess_expression expression)
    | Raise { expression; from } ->
        Statement.Raise
          {
            expression = Option.map ~f:preprocess_expression expression;
            from = Option.map ~f:preprocess_expression from;
          }
    | Return { expression; is_implicit } ->
        Statement.Return
          { expression = Option.map ~f:preprocess_expression expression; is_implicit }
    | TypeAlias { name; type_params; value } ->
        Statement.TypeAlias
          { name = preprocess_expression name; type_params; value = preprocess_expression value }
    | Define _
    | Break
    | Class _
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        value
    | Try _ ->
        (* Try statements are lowered down in `Cfg.create`, but they are preserved in the final Cfg.
           They should be ignored. *)
        value
    | For _
    | If _
    | Match _
    | With _
    | While _ ->
        failwith "For/If/Match/With/While nodes should always be rewritten by `Cfg.create`"
    | AugmentedAssign _ ->
        failwith "AugmentedAssign nodes should always be rewritten by `preprocess_assignments`"
  in
  { Node.location; value }


(* This must be called *once* before analyzing a generator. *)
let preprocess_generator
    ~pyre_in_context:outer_pyre_context
    ~type_of_expression_shared_memory
    ~callable
    generator
  =
  let ({ Assign.target; value; annotation; origin } as assignment) =
    Statement.generator_assignment generator
  in
  (* Since generators create variables that Pyre sees as scoped within the generator, handle them by
     adding the generator's bindings to the resolution. This returns the type context inside the
     generator/conditions. *)
  let inner_pyre_context = PyrePysaApi.InContext.resolve_assignment outer_pyre_context assignment in
  let assignment =
    {
      Assign.target =
        preprocess_expression
          ~pyre_in_context:outer_pyre_context
          ~type_of_expression_shared_memory
          ~callable
          target;
      Assign.value =
        Option.map
          ~f:
            (preprocess_expression
               ~pyre_in_context:outer_pyre_context
               ~type_of_expression_shared_memory
               ~callable)
          value;
      annotation;
      origin;
    }
  in
  assignment, inner_pyre_context


let resolve_recognized_callees
    ~debug
    ~callables_to_definitions_map
    ~pyre_in_context
    ~override_graph
    ~callee
    ~return_type
    ~callee_type
  =
  (* Special treatment for a set of hardcoded decorators returning callable classes. *)
  (match Node.value callee, callee_type with
  | ( _,
      Type.Parametric
        {
          name = "BoundMethod";
          arguments = [Single (Parametric { name; _ }); Single implementing_class];
        } )
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      resolve_callee_from_defining_expression
        ~debug
        ~callables_to_definitions_map
        ~pyre_in_context
        ~override_graph
        ~callee
        ~return_type
        ~implementing_class
  | Expression.Name (Name.Attribute { base; _ }), Parametric { name; _ }
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      (* Because of the special class, we don't get a bound method & lose the self argument for
         non-classmethod LRU cache wrappers. Reconstruct self in this case. *)
      CallResolution.resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map base
      |> fun implementing_class ->
      resolve_callee_from_defining_expression
        ~debug
        ~callables_to_definitions_map
        ~pyre_in_context
        ~override_graph
        ~callee
        ~return_type
        ~implementing_class
  | _ -> None)
  >>| fun call_callees -> { call_callees with recognized_call = CallCallees.RecognizedCall.True }


let resolve_callee_ignoring_decorators
    ~debug
    ~pyre_in_context
    ~callables_to_definitions_map
    ~override_graph
    ~return_type
    callee
  =
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
  let return_type () = ReturnTypeBuilder.from_annotation ~pyre_api (Lazy.force return_type) in
  let contain_class_method signatures =
    signatures
    |> List.exists ~f:(fun signature ->
           List.exists
             CallablesSharedMemory.class_method_decorators
             ~f:(Define.Signature.has_decorator signature))
  in
  let log format =
    if debug then
      Log.dump format
    else
      Format.ifprintf Format.err_formatter format
  in
  let result =
    match Node.value callee with
    | Expression.Name name when is_all_names (Node.value callee) -> (
        (* Resolving expressions that do not reference local variables or parameters. *)
        let name = Ast.Expression.name_to_reference_exn name in
        match PyrePysaApi.ReadOnly.resolve_exports pyre_api name with
        | Some
            (PyrePysaLogic.ResolvedReference.ModuleAttribute
              {
                export =
                  PyrePysaLogic.ResolvedReference.Exported
                    (PyrePysaLogic.ModuleExport.Name.Define _);
                remaining = [];
                _;
              }) ->
            Result.Ok
              [
                CallTargetBuilder.create_with_default_index
                  ~implicit_dunder_call:false
                  ~return_type:(Some (return_type ()))
                  (Target.create_function name);
              ]
        | Some
            (PyrePysaLogic.ResolvedReference.ModuleAttribute
              {
                from;
                name;
                export =
                  PyrePysaLogic.ResolvedReference.Exported PyrePysaLogic.ModuleExport.Name.Class;
                remaining = [attribute];
                _;
              }) -> (
            let class_name = Reference.create ~prefix:from name in
            PyrePysaApi.ReadOnly.get_class_summary pyre_api (Reference.show class_name)
            >>= (fun class_summary ->
                  PyrePysaApi.PysaClassSummary.pyre1_find_attribute class_summary attribute)
            >>| Node.value
            |> function
            | Some { Analysis.ClassSummary.Attribute.kind = Method { static; signatures; _ }; _ } ->
                let is_class_method = contain_class_method signatures in
                Result.Ok
                  [
                    CallTargetBuilder.create_with_default_index
                      ~implicit_dunder_call:false
                      ~return_type:(Some (return_type ()))
                      ~is_class_method
                      ~is_static_method:static
                      (Target.create_method class_name attribute);
                  ]
            | Some attribute ->
                let () =
                  log
                    "Bypassing decorators - Non-method attribute `%s` for callee `%s`"
                    (Analysis.ClassSummary.Attribute.show_attribute attribute)
                    (Expression.show callee)
                in
                Result.Error Unresolved.NonMethodAttribute
            | None ->
                let () =
                  log
                    "Bypassing decorators - Failed to find attribute `%s` for callee `%s`"
                    attribute
                    (Expression.show callee)
                in
                Result.Error Unresolved.CannotFindAttribute)
        | _ ->
            let () =
              log
                "Bypassing decorators - Failed to resolve exports for callee `%s`"
                (Expression.show callee)
            in
            Result.Error Unresolved.CannotResolveExports)
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        (* Resolve `base.attribute` by looking up the type of `base` or the types of its parent
           classes in the Method Resolution Order. *)
        match
          CallResolution.resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map base
        with
        (* The base expression is a class. *)
        | Type.Parametric { name = "type"; arguments = [Single (Type.Primitive class_name)] }
        (* The base expression is a class that has type parameters, such as `class A(Generic[T])`
           where `T` is a type variable. *)
        | Type.Parametric
            { name = "type"; arguments = [Single (Type.Parametric { name = class_name; _ })] }
        (* The base expression is an object (but not a class, since classes are technically objects
           as well). *)
        | Type.Primitive class_name
        (* The base expression is an object that has type arguments. For example, the base
           expression is an instance of class `class. A(Generic[T])`. *)
        | Type.Parametric { name = class_name; arguments = _ } -> (
            let find_attribute element =
              match
                PyrePysaApi.ReadOnly.get_class_summary pyre_api element
                >>= (fun class_summary ->
                      PyrePysaApi.PysaClassSummary.pyre1_find_attribute class_summary attribute)
                >>| Node.value
              with
              | Some { Analysis.ClassSummary.Attribute.kind = Method { static; signatures; _ }; _ }
                ->
                  Some (element, contain_class_method signatures, static)
              | _ -> None
            in
            let parent_classes_in_mro = PyrePysaApi.ReadOnly.class_mro pyre_api class_name in
            match List.find_map (class_name :: parent_classes_in_mro) ~f:find_attribute with
            | Some (base_class, is_class_method, is_static_method) ->
                let receiver_type =
                  (* Discard the type parameters, assuming they do not affect finding the actual
                     callee. *)
                  Type.Parametric
                    { name = "type"; arguments = [Single (Type.Primitive class_name)] }
                in
                let targets =
                  Target.create_method (Reference.create base_class) attribute
                  (* Over-approximately consider that any overriding method might be called. We
                     prioritize reducing false negatives than reducing false positives. *)
                  |> compute_indirect_targets ~pyre_in_context ~override_graph ~receiver_type
                  |> List.map
                       ~f:
                         (CallTargetBuilder.create_with_default_index
                            ~implicit_dunder_call:false
                            ~return_type:(Some (return_type ()))
                            ~is_class_method
                            ~is_static_method)
                in
                Result.Ok targets
            | None ->
                let () =
                  log
                    "Bypassing decorators - Failed to find parent class of `%s` that defines \
                     method `%s`"
                    class_name
                    attribute
                in
                Result.Error Unresolved.CannotFindParentClass)
        | _type ->
            let () =
              log
                "Bypassing decorators - Unknown base type `%s` in callee `%a`"
                (Type.show_type_t _type)
                Expression.pp
                callee
            in
            Result.Error Unresolved.UnknownBaseType)
    | Expression.Name (Name.Identifier _) ->
        let () =
          log
            "Bypassing decorators - Unknown identifier callee `%a`"
            Expression.pp_expression
            callee.Node.value
        in
        Result.Error Unresolved.UnknownIdentifierCallee
    | Expression.Call _ ->
        let () =
          log
            "Bypassing decorators - Unknown call callee `%a`"
            Expression.pp_expression
            callee.Node.value
        in
        Result.Error Unresolved.UnknownCallCallee
    | _ ->
        let () =
          log
            "Bypassing decorators - Unknown callee AST `%a`"
            Expression.pp_expression
            callee.Node.value
        in
        Result.Error Unresolved.UnknownCalleeAST
  in
  let () =
    match result with
    | Result.Error reason when debug ->
        Log.dump
          "Bypassed decorators to resolve callees (using global resolution): Failed to resolve \
           callee `%a` due to `%a`"
          Expression.pp
          callee
          Unresolved.pp_bypassing_decorators
          reason
    | Result.Ok targets when debug ->
        Log.dump
          "Bypassed decorators to resolve callees (using global resolution): `%s`"
          (targets |> List.map ~f:CallTarget.show |> String.concat ~sep:",")
    | _ -> ()
  in
  result


let get_defining_attributes ~pyre_in_context ~base_type_info ~attribute =
  let rec get_defining_parents annotation =
    match annotation with
    | Type.Union annotations
    | Type.Variable
        {
          Type.Variable.TypeVar.constraints = Type.Record.TypeVarConstraints.Explicit annotations;
          _;
        } ->
        List.concat_map annotations ~f:get_defining_parents
    | _ -> [CallResolution.defining_attribute ~pyre_in_context annotation attribute]
  in
  base_type_info |> strip_meta |> strip_optional |> get_defining_parents


type attribute_access_properties = {
  property_targets: CallTarget.t list;
  is_attribute: bool;
}

let resolve_attribute_access_properties
    ~pyre_in_context
    ~override_graph
    ~base_type_info
    ~attribute
    ~setter
  =
  let property_targets_of_attribute property =
    let return_type =
      if setter then
        ReturnType.none
      else
        let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
        PyrePysaLogic.type_of_attribute property |> ReturnTypeBuilder.from_annotation ~pyre_api
    in
    let parent = PyrePysaLogic.AnnotatedAttribute.parent property |> Reference.create in
    let property_targets =
      let kind = if setter then Target.Pyre1PropertySetter else Target.Normal in
      if Type.is_class_type base_type_info then
        [Target.create_method ~kind parent attribute]
      else
        let callee = Target.create_method ~kind parent attribute in
        compute_indirect_targets
          ~pyre_in_context
          ~override_graph
          ~receiver_type:base_type_info
          callee
    in
    List.map
      ~f:
        (CallTargetBuilder.create_with_default_index
           ~implicit_dunder_call:false
           ~return_type:(Some return_type))
      property_targets
  in
  let attributes = get_defining_attributes ~pyre_in_context ~base_type_info ~attribute in
  let properties, non_properties =
    List.partition_map
      ~f:(function
        | Some property when PyrePysaLogic.AnnotatedAttribute.property property ->
            Either.First property
        | attribute -> Either.Second attribute)
      attributes
  in
  let property_targets = List.concat_map ~f:property_targets_of_attribute properties in
  let is_attribute = (not (List.is_empty non_properties)) || List.is_empty attributes in
  { property_targets; is_attribute }


let resolve_regular_callees
    ~debug
    ~callables_to_definitions_map
    ~pyre_in_context
    ~override_graph
    ~return_type
    ~callee
  =
  let maybe_callable_expression =
    match callee.Node.value with
    | Expression.Constant _
    | Expression.BinaryOperator _
    | Expression.BooleanOperator _
    | Expression.ComparisonOperator _
    | Expression.Dictionary _
    | Expression.DictionaryComprehension _
    | Expression.FormatString _
    | Expression.List _
    | Expression.ListComprehension _
    | Expression.Set _
    | Expression.SetComprehension _
    | Expression.Tuple _
    | Expression.UnaryOperator _ ->
        false
    | _ -> true
  in
  if not maybe_callable_expression then
    (* Performance optimization. None of these types can hold callables. *)
    CallCallees.create ()
  else
    let callee_type =
      CallResolution.resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map callee
    in
    let maybe_callable_type =
      match callee_type with
      | Type.Bottom
      | Type.Literal _
      | Type.NoneType
      | Type.TypeOperation _ ->
          false
      | _ -> true
    in
    if not maybe_callable_type then
      (* Performance optimization. None of these types can hold callables. *)
      CallCallees.create ()
    else
      let () =
        log
          ~debug
          "Checking if `%a` is a callable, resolved type is `%a`"
          Expression.pp
          callee
          Type.pp
          callee_type
      in
      let recognized_callees =
        resolve_recognized_callees
          ~debug
          ~callables_to_definitions_map
          ~pyre_in_context
          ~override_graph
          ~callee
          ~return_type
          ~callee_type
        |> CallCallees.default_to_unresolved
             ~reason:Unresolved.UnrecognizedCallee
             ~message:(lazy "Unrecognized callee")
      in
      if CallCallees.is_partially_resolved recognized_callees then
        let () = log ~debug "Recognized special callee:@,`%a`" CallCallees.pp recognized_callees in
        recognized_callees
      else
        let callee_kind =
          CalleeKind.from_callee ~pyre_in_context ~callables_to_definitions_map callee callee_type
        in
        let callees_from_type =
          resolve_callees_from_type
            ~debug
            ~callables_to_definitions_map
            ~pyre_in_context
            ~override_graph
            ~return_type
            ~callee_kind
            callee_type
        in
        if CallCallees.is_partially_resolved callees_from_type then
          let () =
            log
              ~debug
              "Resolved callee from its resolved type:@,`%a`"
              CallCallees.pp
              callees_from_type
          in
          callees_from_type
        else
          resolve_callee_ignoring_decorators
            ~debug
            ~pyre_in_context
            ~callables_to_definitions_map
            ~override_graph
            ~return_type
            callee
          |> function
          | Result.Ok call_targets -> CallCallees.create ~call_targets ()
          | Result.Error reason ->
              CallCallees.unresolved
                ~reason:(Unresolved.BypassingDecorators reason)
                ~message:(lazy "Bypassing decorators")
                ()


module IdentifierReference = struct
  type t =
    | Global of {
        reference: Reference.t;
        export_name: PyrePysaLogic.ModuleExport.Name.t option;
      }
    | Nonlocal of Reference.t
end

let as_identifier_reference ~define_name ~pyre_in_context expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) ->
      let reference = Reference.create identifier in
      if PyrePysaApi.InContext.is_global pyre_in_context ~reference then
        Some
          (IdentifierReference.Global
             { reference = Reference.delocalize reference; export_name = None })
      else
        define_name
        >>= fun define_name ->
        if CallResolution.is_nonlocal ~pyre_in_context ~define:define_name reference then
          Some (IdentifierReference.Nonlocal (Reference.delocalize reference))
        else
          None
  | Name name -> (
      name_to_reference name
      >>= fun reference ->
      PyrePysaApi.ReadOnly.resolve_exports
        (PyrePysaApi.InContext.pyre_api pyre_in_context)
        reference
      >>= function
      | PyrePysaLogic.ResolvedReference.ModuleAttribute
          {
            from;
            name;
            remaining = [];
            export = PyrePysaLogic.ResolvedReference.Exported export_name;
          } ->
          Some
            (IdentifierReference.Global
               {
                 reference = Reference.combine from (Reference.create name);
                 export_name = Some export_name;
               })
      | _ -> None)
  | _ -> None


let is_builtin_reference = function
  | IdentifierReference.Global { reference; _ } -> reference |> Reference.single |> Option.is_some
  | Nonlocal _ -> false


let resolve_attribute_access_global_targets
    ~define_name
    ~pyre_in_context
    ~base_type_info
    ~base
    ~attribute
    ~origin
  =
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
  let expression =
    Expression.Name (Name.Attribute { Name.Attribute.base; attribute; origin })
    |> Node.create_with_default_location
  in
  match as_identifier_reference ~define_name ~pyre_in_context expression with
  | Some (IdentifierReference.Global { reference; _ }) -> [reference]
  | Some (Nonlocal _) -> []
  | None ->
      let rec find_targets targets = function
        | Type.Union annotations -> List.fold ~init:targets ~f:find_targets annotations
        | Parametric { name = "type"; arguments = [Single annotation] } ->
            (* Access on a class, i.e `Foo.bar`, translated into `Foo.__class__.bar`. *)
            let parent =
              let attribute =
                Type.split annotation
                |> fst
                |> Type.primitive_name
                >>= PyrePysaApi.ReadOnly.attribute_from_class_name
                      pyre_api
                      ~transitive:true
                      ~name:attribute
                      ~type_for_lookup:annotation
              in
              match attribute with
              | Some attribute when PyrePysaLogic.AnnotatedAttribute.defined attribute ->
                  Type.Primitive (PyrePysaLogic.AnnotatedAttribute.parent attribute)
                  |> Type.class_name
              | _ -> Type.class_name annotation
            in
            let attribute = Format.sprintf "__class__.%s" attribute in
            let target = Reference.create ~prefix:parent attribute in
            target :: targets
        | Type.Primitive class_name ->
            (* Access on an instance, i.e `self.foo`. *)
            let parents =
              let successors = PyrePysaApi.ReadOnly.class_mro pyre_api class_name in
              class_name :: successors
            in
            let add_target targets parent =
              let parent = Reference.create parent in
              let target = Reference.create ~prefix:parent attribute in
              target :: targets
            in
            List.fold ~init:targets ~f:add_target parents
        | annotation ->
            let target = Reference.create ~prefix:(Type.class_name annotation) attribute in
            target :: targets
      in
      find_targets [] base_type_info


let return_type_for_call ~pyre_in_context ~callee =
  lazy
    (Expression.Call
       {
         callee;
         arguments = [];
         origin = Some (Origin.create ~location:Location.any Origin.ForTypeChecking);
       }
    |> Node.create_with_default_location
    |> CallResolution.resolve_ignoring_untracked ~pyre_in_context)


let resolve_callable_targets_from_global_identifiers ~define_name ~pyre_in_context expression =
  match as_identifier_reference ~define_name ~pyre_in_context expression with
  | Some
      (IdentifierReference.Global
        { reference; export_name = Some (PyrePysaLogic.ModuleExport.Name.Define _) }) ->
      let target = Target.create_function reference in
      let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
      let return_type =
        return_type_for_call ~pyre_in_context ~callee:expression
        |> Lazy.force
        |> ReturnTypeBuilder.from_annotation ~pyre_api
      in
      [
        CallTargetBuilder.create_with_default_index
          ~implicit_dunder_call:false
          ~return_type:(Some return_type)
          target;
      ]
  | _ -> []


let resolve_identifier ~define_name ~pyre_in_context ~identifier =
  let expression =
    Expression.Name (Name.Identifier identifier) |> Node.create_with_default_location
  in
  let global_targets, nonlocal_targets =
    expression
    |> as_identifier_reference ~define_name ~pyre_in_context
    |> Option.filter ~f:(Fn.non is_builtin_reference)
    >>| (function
          | IdentifierReference.Global
              { reference; export_name = Some PyrePysaLogic.ModuleExport.Name.GlobalVariable }
          | IdentifierReference.Global { reference; export_name = None } ->
              ( [
                  CallTargetBuilder.create_with_default_index
                    ~implicit_dunder_call:false
                    ~return_type:None
                    (Target.create_object reference);
                ],
                [] )
          | IdentifierReference.Global _ -> [], []
          | Nonlocal nonlocal ->
              ( [],
                [
                  CallTargetBuilder.create_with_default_index
                    ~implicit_dunder_call:false
                    ~return_type:None
                    (Target.create_object nonlocal);
                ] ))
    |> Option.value ~default:([], [])
  in
  let callable_targets =
    resolve_callable_targets_from_global_identifiers ~define_name ~pyre_in_context expression
  in
  match global_targets, nonlocal_targets, callable_targets with
  | [], [], [] -> None
  | _ ->
      (* Exist at least a non-empty list. *)
      Some
        {
          IdentifierCallees.global_targets;
          nonlocal_targets;
          if_called = CallCallees.create ~call_targets:callable_targets ();
        }


let resolve_callees
    ~debug
    ~pyre_in_context
    ~callables_to_definitions_map
    ~override_graph
    ~caller
    ~location
    ~call:({ Call.callee; arguments; origin = _ } as call)
  =
  log
    ~debug
    "Resolving function call `%a`"
    Expression.pp
    (Expression.Call call |> Node.create ~location);
  (* Resolving the return type can be costly, hence we prefer the annotation on the callee when
     possible. When that does not work, we fallback to a full resolution of the call expression
     (done lazily). *)
  let return_type =
    lazy
      (Expression.Call call
      |> Node.create_with_default_location
      |> CallResolution.resolve_ignoring_untracked ~pyre_in_context)
  in
  let regular_callees =
    resolve_regular_callees
      ~debug
      ~callables_to_definitions_map
      ~pyre_in_context
      ~override_graph
      ~return_type
      ~callee
  in
  let shim_target =
    shim_for_call ~pyre_in_context ~callables_to_definitions_map call
    >>= fun shim ->
    create_shim_callee_expression ~debug ~callable:caller ~location ~call shim
    >>= fun shim_callee_expression ->
    resolve_regular_callees
      ~debug
      ~callables_to_definitions_map
      ~pyre_in_context
      ~override_graph
      ~return_type
      ~callee:shim_callee_expression
    |> function
    | {
        CallCallees.call_targets = _ :: _ as call_targets;
        new_targets = [];
        init_targets = [];
        shim_target = None;
        unresolved = Unresolved.False;
        _;
      } ->
        Some { ShimTarget.call_targets; decorated_targets = []; argument_mapping = shim }
    | _ ->
        let () =
          log
            ~debug
            "Failed to resolve callees for shimmed callee %a"
            Expression.pp
            shim_callee_expression
        in
        None
  in
  let higher_order_parameters =
    let filter_implicit_dunder_calls ({ CallCallees.call_targets; _ } as callees) =
      (* Heuristic: if a parameter is an instance of a callable class (i.e, a class with a `__call__` method),
       * we only want to consider it a higher order parameter if we might miss a call to `__call__`.
       * For instance, if the callee has no body or is not (type) annotated. *)
      let rec loosely_less_equal_class class_name = function
        | Type.Primitive name -> String.equal name class_name
        | Type.Union list -> List.exists list ~f:(loosely_less_equal_class class_name)
        | _ -> false
      in
      let parameter_has_annotation class_name = function
        | { Node.value = { Parameter.annotation = Some annotation; _ }; _ }
        (* we don't want to perform a true "less equal" check, since it's expensive.
         * Let's use a cheap heuristic. *)
          when loosely_less_equal_class
                 class_name
                 (Type.create
                    ~variables:(fun _ -> None)
                    ~aliases:(fun ?replace_unbound_parameters_with_any:_ _ -> None)
                    annotation) ->
            true
        | _ -> false
      in
      let preserve_implicit_dunder_call ({ CallTarget.target; _ } as higher_order_callee) =
        let target =
          target |> Target.get_regular |> Target.Regular.override_to_method |> Target.from_regular
        in
        let higher_order_callee = { higher_order_callee with CallTarget.target } in
        match higher_order_callee with
        | {
         CallTarget.implicit_dunder_call = true;
         target =
           Target.Regular
             (Target.Regular.Method { class_name = callable_class; method_name = "__call__"; _ });
         _;
        } -> (
            match regular_callees.call_targets with
            | [callee] when Target.is_function_or_method callee.target ->
                CallablesSharedMemory.ReadOnly.get_signature
                  callables_to_definitions_map
                  callee.target
                >>| (fun { CallablesSharedMemory.CallableSignature.parameters; is_stub_like; _ } ->
                      is_stub_like
                      ||
                      match parameters with
                      | AstResult.Some parameters ->
                          not (List.exists parameters ~f:(parameter_has_annotation callable_class))
                      | _ -> true)
                |> Option.value ~default:true
            | _ -> true)
        | _ -> true
      in
      let call_targets = List.filter call_targets ~f:preserve_implicit_dunder_call in
      { callees with call_targets }
    in
    let get_higher_order_function_targets index { Call.Argument.value = argument; _ } =
      let callees =
        resolve_regular_callees
          ~debug
          ~pyre_in_context
          ~override_graph
          ~callables_to_definitions_map
          ~return_type:(return_type_for_call ~pyre_in_context ~callee:argument)
          ~callee:argument
        |> filter_implicit_dunder_calls
      in
      match callees, argument with
      | { CallCallees.call_targets = _ :: _ as regular_targets; unresolved; _ }, _ ->
          Some { HigherOrderParameter.index; call_targets = regular_targets; unresolved }
      | _, { Node.value = Expression.Lambda _; _ } ->
          Some
            {
              HigherOrderParameter.index;
              call_targets = [];
              unresolved = Unresolved.True LambdaArgument;
            }
      | _ -> None
    in
    if Option.is_none shim_target then
      List.filter_mapi arguments ~f:get_higher_order_function_targets
      |> HigherOrderParameterMap.from_list
    else (* disable higher order parameters if call is shimmed *)
      HigherOrderParameterMap.empty
  in
  { regular_callees with higher_order_parameters; shim_target }


let resolve_attribute_access
    ~pyre_in_context
    ~debug
    ~callables_to_definitions_map
    ~override_graph
    ~define_name
    ~attribute_targets
    ~base
    ~attribute
    ~origin
    ~setter
  =
  let base_type_info =
    CallResolution.resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map base
  in

  log
    ~debug
    "Checking if `%s` is an attribute, property or global variable. Resolved type for base `%a` is \
     `%a`"
    attribute
    Expression.pp
    base
    Type.pp
    base_type_info;

  let { property_targets; is_attribute } =
    resolve_attribute_access_properties
      ~pyre_in_context
      ~override_graph
      ~base_type_info
      ~attribute
      ~setter
  in

  let global_targets =
    resolve_attribute_access_global_targets
      ~define_name
      ~pyre_in_context
      ~base_type_info
      ~base
      ~attribute
      ~origin
    |> List.map ~f:Target.create_object
    (* Use a hashset here for faster lookups. *)
    |> List.filter ~f:(Hash_set.mem attribute_targets)
    |> List.map
         ~f:
           (CallTargetBuilder.create_with_default_index
              ~implicit_dunder_call:false
              ~return_type:None)
  in

  let callable_targets =
    Expression.Name (Name.Attribute { Name.Attribute.base; attribute; origin })
    |> Node.create_with_default_location
    |> resolve_callable_targets_from_global_identifiers ~define_name ~pyre_in_context
  in

  {
    AttributeAccessCallees.property_targets;
    global_targets;
    is_attribute;
    if_called = CallCallees.create ~call_targets:callable_targets ();
  }


module MissingFlowTypeAnalysis = struct
  type t = { caller: Target.t }

  (* For the missing flow analysis (`--find-missing-flows=type`), we turn unresolved
   * calls into sinks, so that we may find sources flowing into those calls. *)
  let add_unknown_callee
      ~missing_flow_type_analysis
      ~expression:{ Node.value; location }
      ({ CallCallees.unresolved; call_targets; _ } as callees)
    =
    match missing_flow_type_analysis with
    | Some { caller } when Unresolved.is_unresolved unresolved ->
        (* TODO(T117715045): Move the target creation in `taint/missingFlow.ml`. *)
        let callee =
          match value with
          | Expression.Call { callee = { Node.value = callee; _ }; _ } -> callee
          | _ -> value
        in
        let target =
          Format.asprintf
            "unknown-callee:%a:%a:%a"
            Target.pp_pretty
            (Target.strip_parameters caller)
            Location.pp
            location
            Expression.pp
            (callee
            |> Node.create_with_default_location
            |> Ast.Expression.delocalize ~create_origin:(fun ~expression:_ _ -> None))
        in
        let call_target =
          {
            CallTarget.target = Target.Regular.Object target |> Target.from_regular;
            implicit_receiver = false;
            implicit_dunder_call = false;
            index = 0;
            return_type = Some ReturnType.unknown;
            receiver_class = None;
            is_class_method = false;
            is_static_method = false;
          }
        in
        { callees with call_targets = call_target :: call_targets }
    | _ -> callees
end

(* Helper to check that expression identifiers are truly unique. *)
module ExpressionIdentifierInvariant = struct
  type t = Expression.t ExpressionIdentifier.Map.t ref

  let create () = ref ExpressionIdentifier.Map.empty

  let check_invariant map ~callable ~expression =
    map :=
      ExpressionIdentifier.Map.update
        (ExpressionIdentifier.of_expression expression)
        (function
          | None -> Some expression
          | Some existing when Expression.equal existing expression -> Some existing
          | Some existing ->
              Format.asprintf
                "During call graph building of `%a`, found two expressions with the same \
                 expression identifier:\n\
                 Identifier: %a\n\
                 First expression: %a\n\
                 Second expression: %a"
                Target.pp_external
                callable
                ExpressionIdentifier.pp
                (ExpressionIdentifier.of_expression expression)
                Expression.pp
                existing
                Expression.pp
                expression
              |> failwith)
        !map
end

module CallGraphBuilder = struct
  module Context = struct
    type t = {
      module_qualifier: Reference.t;
      define_name: Reference.t option; (* None if this is an artificial define. *)
      callable: Target.t;
      debug: bool;
      override_graph: OverrideGraph.SharedMemory.ReadOnly.t option;
      missing_flow_type_analysis: MissingFlowTypeAnalysis.t option;
      attribute_targets: Target.HashSet.t;
      callables_to_definitions_map: CallablesSharedMemory.ReadOnly.t;
      callables_to_decorators_map: CallableToDecoratorsMap.SharedMemory.ReadOnly.t;
      type_of_expression_shared_memory: TypeOfExpressionSharedMemory.t;
      expression_identifier_invariant: ExpressionIdentifierInvariant.t option;
    }
  end

  (* We use `Expression.Folder.fold` to visit all expressions recursively. Each `fold_XXX` is
     responsible for calling `Folder.fold` on all sub-expressions, with the right pyre context. *)

  (* Internal state of the folder. *)
  module State = struct
    type t = {
      context: Context.t;
      pyre_in_context: PyrePysaApi.InContext.t;
      assignment_target: ExpressionIdentifier.t option;
      callees_at_location: DefineCallGraph.t ref; (* This will be mutated. *)
    }
  end

  let resolve_callees
      ~state:
        {
          State.context =
            {
              Context.debug;
              callable;
              callables_to_definitions_map;
              override_graph;
              missing_flow_type_analysis;
              _;
            };
          pyre_in_context;
          _;
        }
      ~location
      ~call
    =
    resolve_callees
      ~debug
      ~pyre_in_context
      ~callables_to_definitions_map
      ~override_graph
      ~caller:callable
      ~location
      ~call
    |> MissingFlowTypeAnalysis.add_unknown_callee
         ~missing_flow_type_analysis
         ~expression:(Expression.Call call |> Node.create ~location)


  let resolve_attribute_access
      ~state:
        {
          State.context =
            {
              Context.debug;
              define_name;
              callables_to_definitions_map;
              override_graph;
              attribute_targets;
              _;
            };
          pyre_in_context;
          _;
        }
    =
    resolve_attribute_access
      ~pyre_in_context
      ~debug
      ~callables_to_definitions_map
      ~override_graph
      ~define_name
      ~attribute_targets


  let resolve_identifier ~state:{ State.context = { Context.define_name; _ }; pyre_in_context; _ } =
    resolve_identifier ~define_name ~pyre_in_context


  let build_for_inner_define ~context:{ Context.debug; callable; _ } ~callees_at_location = function
    | {
        Node.value =
          Statement.Define
            ({
               Define.signature =
                 { name; parent = (* Create targets only for inner functions. *) Function _; _ };
               _;
             } as define);
        location;
      } ->
        (* Pyre1 does not include decorators in the define location, but Pyrefly does. Let's be
           consistent. *)
        let define_location = Define.location_with_decorators { Node.location; value = define } in
        let delocalized_name = Reference.delocalize name in
        let target = Target.from_define ~define_name:delocalized_name ~define in
        callees_at_location :=
          DefineCallGraph.add_define_callees
            ~debug
            ~caller:callable
            ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
            ~define
            ~define_location
            ~callees:
              { DefineCallees.define_targets = [CallTarget.create target]; decorated_targets = [] }
            !callees_at_location
    | _ -> ()


  let check_expression_identifier_invariant
      ~state:{ State.context = { Context.callable; expression_identifier_invariant; _ }; _ }
      ~location
      expression
    =
    match expression_identifier_invariant with
    | Some expression_identifier_invariant ->
        ExpressionIdentifierInvariant.check_invariant
          expression_identifier_invariant
          ~callable
          ~expression:{ Node.location; value = expression }
    | None -> ()


  let fold_call
      ~folder
      ~state:({ State.callees_at_location; context = { Context.debug; callable; _ }; _ } as state)
      ~location
      ({ Call.callee = callee_expression; arguments; origin = call_origin } as call)
    =
    let () = check_expression_identifier_invariant ~state ~location (Expression.Call call) in
    let callees = resolve_callees ~state ~location ~call in
    callees_at_location :=
      DefineCallGraph.add_call_callees
        ~debug
        ~caller:callable
        ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
        ~location
        ~call
        ~callees
        !callees_at_location;
    (* Special-case `getattr()` and `setattr()` for the taint analysis. *)
    let () =
      match call with
      | {
       Call.callee = { Node.value = Name (Name.Identifier "getattr"); _ };
       arguments =
         [
           { Call.Argument.value = base; name = None };
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Constant (Constant.String { StringLiteral.value = attribute; _ });
                 _;
               };
             name = None;
           };
           { Call.Argument.value = _; name = None };
         ];
       _;
      } ->
          let origin =
            Some (Origin.create ?base:call_origin ~location Origin.GetAttrConstantLiteral)
          in
          let callees = resolve_attribute_access ~state ~base ~attribute ~origin ~setter:false in
          callees_at_location :=
            DefineCallGraph.add_attribute_access_callees
              ~debug
              ~caller:callable
              ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
              ~location
              ~attribute_access:{ Name.Attribute.base; attribute; origin }
              ~callees
              !callees_at_location
      | {
       Call.callee =
         {
           Node.value =
             Name
               (Name.Attribute
                 {
                   base = { Node.value = Name (Name.Identifier "object"); _ };
                   attribute = "__setattr__";
                   _;
                 });
           _;
         };
       arguments =
         [
           { Call.Argument.value = self; name = None };
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Constant (Constant.String { value = attribute; kind = String });
                 _;
               };
             name = None;
           };
           { Call.Argument.value = _; name = None };
         ];
       _;
      } ->
          let origin =
            Some (Origin.create ?base:call_origin ~location Origin.SetAttrConstantLiteral)
          in
          let callees =
            resolve_attribute_access ~state ~base:self ~attribute ~origin ~setter:true
          in
          callees_at_location :=
            DefineCallGraph.add_attribute_access_callees
              ~debug
              ~caller:callable
              ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
              ~location
              ~attribute_access:{ Name.Attribute.base = self; attribute; origin }
              ~callees
              !callees_at_location
      | _ -> ()
    in
    (* Fold sub-expressions *)
    let state =
      match Node.value callee_expression with
      (* Skip visiting the callee itself to avoid having duplicate call edges. We still visit
         sub-expressions of the callee. *)
      | Expression.Name (Name.Attribute { base; _ }) -> Folder.fold ~folder ~state base
      | Expression.Name (Name.Identifier _) -> state
      | _ -> Folder.fold ~folder ~state callee_expression
    in
    match call_origin, arguments with
    | ( Some
          {
            Origin.kind =
              Nested { head = Origin.SubscriptSetItem; tail = Origin.ChainedAssign { index } };
            _;
          },
        [{ Call.Argument.value = index_argument; _ }; _] )
      when index >= 1 ->
        (* `x = a[b] = c` was turned into `a.__setitem__(b, c)`. We should NOT revisit c, since it
           was already visited for `x = c`. *)
        Folder.fold ~folder ~state index_argument
    | _ ->
        List.fold
          ~init:state
          ~f:(fun state { Call.Argument.value; _ } -> Folder.fold ~folder ~state value)
          arguments


  let fold_name
      ~folder
      ~state:
        ({
           State.callees_at_location;
           assignment_target;
           context = { Context.debug; callable; _ };
           _;
         } as state)
      ~location
      name
    =
    let () = check_expression_identifier_invariant ~state ~location (Expression.Name name) in
    match name with
    | Name.Attribute ({ Name.Attribute.base; attribute; origin } as attribute_access) ->
        let setter =
          match assignment_target with
          | Some assignment_target ->
              ExpressionIdentifier.equal
                assignment_target
                (ExpressionIdentifier.of_attribute_access ~location attribute_access)
          | None -> false
        in
        let callees = resolve_attribute_access ~state ~base ~attribute ~origin ~setter in
        callees_at_location :=
          DefineCallGraph.add_attribute_access_callees
            ~debug
            ~caller:callable
            ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
            ~location
            ~attribute_access
            ~callees
            !callees_at_location;
        Folder.fold ~folder ~state base
    | Name.Identifier identifier ->
        resolve_identifier ~state ~identifier
        >>| (fun callees ->
              callees_at_location :=
                DefineCallGraph.add_identifier_callees
                  ~debug
                  ~caller:callable
                  ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
                  ~location
                  ~identifier
                  ~callees
                  !callees_at_location)
        |> Option.value ~default:();
        state


  let fold_format_string
      ~folder
      ~state:
        ({
           State.callees_at_location;
           pyre_in_context;
           context =
             {
               Context.debug;
               callable;
               callables_to_definitions_map;
               override_graph;
               type_of_expression_shared_memory;
               _;
             };
           _;
         } as state)
      ~location
      substrings
    =
    let () =
      check_expression_identifier_invariant ~state ~location (Expression.FormatString substrings)
    in
    let artificial_target =
      CallTargetBuilder.create_with_default_index
        ~implicit_dunder_call:false
        ~return_type:None
        Target.ArtificialTargets.format_string
    in
    callees_at_location :=
      DefineCallGraph.add_format_string_articifial_callees
        ~debug
        ~location
        ~caller:callable
        ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
        ~format_string:(Expression.FormatString substrings)
        ~callees:(FormatStringArtificialCallees.from_f_string_targets [artificial_target])
        !callees_at_location;
    List.iter substrings ~f:(function
        | Substring.Literal _ -> ()
        | Substring.Format { value; format_spec } -> (
            let register_stringify_call_targets
                ({ Node.location = expression_location; _ } as expression)
              =
              let { CallCallees.call_targets; _ } =
                let callee =
                  let method_name, origin_kind =
                    match
                      resolve_stringify_call
                        ~pyre_in_context
                        ~type_of_expression_shared_memory
                        expression
                    with
                    | Str -> "__str__", Origin.FormatStringImplicitStr
                    | Repr -> "__repr__", Origin.FormatStringImplicitRepr
                  in
                  {
                    Node.value =
                      Expression.Name
                        (Name.Attribute
                           {
                             base = expression;
                             attribute = method_name;
                             origin =
                               Some
                                 (Origin.create
                                    ?base:(Ast.Expression.origin expression)
                                    ~location:expression_location
                                    origin_kind);
                           });
                    location = expression_location;
                  }
                in
                resolve_regular_callees
                  ~debug
                  ~callables_to_definitions_map
                  ~pyre_in_context
                  ~override_graph
                  ~return_type:(lazy Type.string)
                  ~callee
              in
              if not (List.is_empty call_targets) then
                callees_at_location :=
                  DefineCallGraph.add_format_string_stringify_callees
                    ~debug
                    ~caller:callable
                    ~location:expression_location
                    ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
                    ~substring:(Node.value expression)
                    ~callees:(FormatStringStringifyCallees.from_stringify_targets call_targets)
                    !callees_at_location
            in
            register_stringify_call_targets value;
            match format_spec with
            | Some format_spec -> register_stringify_call_targets format_spec
            | None -> ()));
    Folder.default_fold_format_string ~folder ~state substrings


  let fold_comprehension_generator
      ~folder
      ~state:
        ({
           State.pyre_in_context = outer_pyre_context;
           context = { Context.callable; type_of_expression_shared_memory; _ };
           _;
         } as state)
      ({ Comprehension.Generator.conditions; _ } as generator)
    =
    let { Ast.Statement.Assign.target; value; _ }, inner_pyre_context =
      preprocess_generator
        ~pyre_in_context:outer_pyre_context
        ~type_of_expression_shared_memory
        ~callable
        generator
    in
    let state = Folder.fold ~folder ~state target in
    let state = Folder.fold ~folder ~state (Option.value_exn value) in
    (* conditions need to use the new pyre context *)
    let state = { state with pyre_in_context = inner_pyre_context } in
    let state =
      List.fold
        ~init:state
        ~f:(fun state condition -> Folder.fold ~folder ~state condition)
        conditions
    in
    state


  let fold_comprehension_generators ~folder ~state generators =
    List.fold
      ~init:state
      ~f:(fun state generator -> fold_comprehension_generator ~folder ~state generator)
      generators


  let fold_comprehension
      ~folder
      ~state:({ State.pyre_in_context = outer_pyre_context; _ } as state)
      ~location:_
      { Comprehension.element; generators }
    =
    let state = fold_comprehension_generators ~folder ~state generators in
    let state = Folder.fold ~folder ~state element in
    { state with pyre_in_context = outer_pyre_context }


  let fold_dictionary_comprehension
      ~folder
      ~state:({ State.pyre_in_context = outer_pyre_context; _ } as state)
      ~location:_
      { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators }
    =
    let state = fold_comprehension_generators ~folder ~state generators in
    let state = Folder.fold ~folder ~state key in
    let state = Folder.fold ~folder ~state value in
    { state with pyre_in_context = outer_pyre_context }


  (* Build the call graph for the given expression. This mutates `callees_at_location`. *)
  let build_for_expression
      ~pyre_in_context
      ~context
      ~assignment_target
      ~callees_at_location
      expression
    =
    let state = { State.context; pyre_in_context; assignment_target; callees_at_location } in
    let (_ : State.t) =
      Folder.fold
        ~folder:
          (Folder.create
             ~fold_call
             ~fold_name
             ~fold_format_string
             ~fold_generator:fold_comprehension
             ~fold_dictionary_comprehension
             ~fold_list_comprehension:fold_comprehension
             ~fold_set_comprehension:fold_comprehension
             ())
        ~state
        expression
    in
    ()
end

(* On return expressions, we may want to have additional callees to simulate the usage of the
 * returned values. For example, consider method
 * @graphql_root_field(...)
 * def foo():
 *   return LazyUserDict()
 * that returns a GraphQL query entrypoint object. Object `LazyUserDict` will be used for querying
 * but the queries are not explicit in the source code. Hence we explicate the queries by calling
 * the methods in the returned object, which can be realized by rewriting the return statement into
 *   LazyUserDict().async_phone_number()
 *   return LazyUserDict()
 * Here method `async_phone_number` in class `LazyUserDict` is decorated by `@graphql_field` to
 * declare it is a query. *)
let register_callees_on_return
    ~debug
    ~pyre_in_context
    ~callables_to_decorators_map
    ~callables_to_definitions_map
    ~callable
    ~return_expression
    ~statement_location
    callees_at_location
  =
  let get_decorator_names callable =
    callable
    |> CallableToDecoratorsMap.SharedMemory.ReadOnly.get_decorators callables_to_decorators_map
    >>| (fun decorators ->
          List.filter_map decorators ~f:(fun decorator ->
              match Ast.Statement.Decorator.from_expression decorator with
              | Some { Decorator.name = { Node.value = decorator_name; _ }; _ } ->
                  Some (Reference.show decorator_name)
              | _ -> None))
    |> Option.value ~default:[]
    |> Data_structures.SerializableStringSet.of_list
  in
  let decorator_names = get_decorator_names callable in
  let method_decorator =
    List.find_map Recognized.graphql_decorators ~f:(fun (callable_decorator, method_decorator) ->
        if Data_structures.SerializableStringSet.mem callable_decorator decorator_names then
          Some method_decorator
        else
          None)
  in
  match return_expression, method_decorator with
  | Some return_expression, Some method_decorator ->
      let return_expression_type =
        return_expression
        |> CallResolution.resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map
        |> CallResolution.strip_optional
      in
      let return_expression_inner_type, argument =
        return_expression_type
        |> Type.split
        |> fun (annotation, parameters) ->
        (Type.primitive_name annotation, parameters)
        |> function
        | Some "list", [Type.Record.Argument.Single parameter]
        | Some "set", [Type.Record.Argument.Single parameter] ->
            parameter, ReturnShimCallees.ReturnExpressionElement
        | Some _, _ (* Unable to identify the base type *)
        | None, _ ->
            return_expression_type, ReturnShimCallees.ReturnExpression
      in
      let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
      let has_decorator_with_name ~decorator_name:name callable =
        callable |> get_decorator_names |> Data_structures.SerializableStringSet.mem name
      in
      let methods_in_return_expression_type =
        return_expression_inner_type
        |> Type.primitive_name
        >>| fun return_expression_inner_class_name ->
        PyrePysaApi.ReadOnly.get_class_summary pyre_api return_expression_inner_class_name
        >>| PyrePysaApi.PysaClassSummary.pyre1_get_attributes
        |> Option.value ~default:[]
        |> List.filter_map
             ~f:(fun { Node.value = { Analysis.ClassSummary.Attribute.kind; name }; _ } ->
               match kind with
               | Analysis.ClassSummary.Attribute.Method _ ->
                   let target =
                     Target.create_method (Reference.create return_expression_inner_class_name) name
                   in
                   if has_decorator_with_name ~decorator_name:method_decorator target then
                     Some (CallTarget.create target)
                   else
                     None
               | _ -> None)
      in
      callees_at_location :=
        DefineCallGraph.add_return_callees
          ~debug
          ~caller:callable
          ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
          ~return_expression
          ~statement_location
          ~callees:
            {
              ReturnShimCallees.call_targets =
                Option.value ~default:[] methods_in_return_expression_type;
              arguments = [argument];
            }
          !callees_at_location
  | _ -> ()


(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraphFixpoint (Context : sig
  val builder_context : CallGraphBuilder.Context.t

  val pyre_api : PyrePysaApi.ReadOnly.t

  val callees_at_location : DefineCallGraph.t ref (* This can be mutated. *)

  val define : Ast.Statement.Define.t Node.t
end) =
struct
  let debug = Context.builder_context.debug

  let callable = Context.builder_context.callable

  let type_of_expression_shared_memory = Context.builder_context.type_of_expression_shared_memory

  include PyrePysaLogic.Fixpoint.Make (struct
    type t = unit [@@deriving show]

    let bottom = ()

    let less_or_equal ~left:_ ~right:_ = true

    let join _ _ = ()

    let widen ~previous:_ ~next:_ ~iteration:_ = ()

    let forward_statement ~pyre_in_context ~statement =
      log ~debug "Building call graph of statement: `%a`" Ast.Statement.pp statement;
      let statement =
        preprocess_statement ~pyre_in_context ~type_of_expression_shared_memory ~callable statement
      in
      let forward_expression =
        CallGraphBuilder.build_for_expression
          ~pyre_in_context
          ~context:Context.builder_context
          ~assignment_target:None
          ~callees_at_location:Context.callees_at_location
      in
      match Node.value statement with
      | Statement.Assign { target; value; annotation = _; origin } ->
          let () =
            match value, origin with
            | _, Some { Origin.kind = Origin.ChainedAssign { index }; _ } when index >= 1 ->
                (* We already visited the value expression for index = 0, so skip it this time. *)
                ()
            | Some value, _ -> forward_expression value
            | _ -> ()
          in
          CallGraphBuilder.build_for_expression
            ~pyre_in_context
            ~context:Context.builder_context
            ~callees_at_location:Context.callees_at_location
            ~assignment_target:(Some (ExpressionIdentifier.of_expression target))
            target
      | Assert { test; _ } -> forward_expression test
      | Delete expressions -> List.iter ~f:forward_expression expressions
      | Expression expression -> forward_expression expression
      | Raise { expression; from } ->
          let () = Option.iter ~f:forward_expression from in
          let () = Option.iter ~f:forward_expression expression in
          ()
      | Return { expression = return_expression; is_implicit = _ } ->
          let () = Option.iter ~f:forward_expression return_expression in
          let () =
            register_callees_on_return
              ~debug
              ~pyre_in_context
              ~callables_to_decorators_map:Context.builder_context.callables_to_decorators_map
              ~callables_to_definitions_map:Context.builder_context.callables_to_definitions_map
              ~callable
              ~return_expression
              ~statement_location:statement.Node.location
              Context.callees_at_location
          in
          ()
      | TypeAlias { name; type_params = _; value } ->
          let () = forward_expression name in
          let () = forward_expression value in
          ()
      | Define _ ->
          CallGraphBuilder.build_for_inner_define
            ~context:Context.builder_context
            ~callees_at_location:Context.callees_at_location
            statement
      | Break
      | Class _ ->
          ()
      | Continue
      | Global _
      | Import _
      | Nonlocal _
      | Pass ->
          ()
      | Try _ ->
          (* Try statements are lowered down in `Cfg.create`, but they are preserved in the final
             Cfg. They should be ignored. *)
          ()
      | For _
      | If _
      | Match _
      | With _
      | While _ ->
          failwith "For/If/Match/With/While nodes should always be rewritten by `Cfg.create`"
      | AugmentedAssign _ ->
          failwith "AugmentedAssign nodes should always be rewritten by `preprocess_assignments`"


    let forward ~statement_key _ ~statement =
      let pyre_in_context =
        PyrePysaApi.InContext.create_at_statement_scope
          Context.pyre_api
          ~module_qualifier:Context.builder_context.module_qualifier
          ~define_name:(Option.value_exn Context.builder_context.define_name)
          ~define:Context.define
          ~statement_key
      in
      forward_statement ~pyre_in_context ~statement


    let backward ~statement_key:_ _ ~statement:_ = ()
  end)
end

(* The result of finding higher order function callees inside a callable. *)
module HigherOrderCallGraph = struct
  type t = {
    returned_callables: CallTarget.Set.t;
    call_graph: DefineCallGraph.t;
        (* Higher order function callees (i.e., parameterized targets) and potentially regular
           callees. *)
  }
  [@@deriving equal, show]

  let empty = { returned_callables = CallTarget.Set.bottom; call_graph = DefineCallGraph.empty }

  let merge
      { returned_callables = left_returned_callables; call_graph = left_call_graph }
      { returned_callables = right_returned_callables; call_graph = right_call_graph }
    =
    {
      returned_callables = CallTarget.Set.join left_returned_callables right_returned_callables;
      call_graph = DefineCallGraph.merge left_call_graph right_call_graph;
    }


  let is_empty { returned_callables; call_graph } =
    CallTarget.Set.is_bottom returned_callables && DefineCallGraph.is_empty call_graph


  let to_json_alist { returned_callables; call_graph } =
    let returned_callables =
      returned_callables |> CallTarget.Set.elements |> List.map ~f:CallTarget.to_json
    in
    ["returned_callables", `List returned_callables; "calls", DefineCallGraph.to_json call_graph]


  include MakeSaveCallGraph (struct
    type nonrec t = t

    let name = "higher order call graphs"

    let is_empty = is_empty

    let to_json_alist = to_json_alist
  end)

  module State = struct
    include
      Abstract.MapDomain.Make
        (struct
          include TaintAccessPath.Root

          let name = "variables"

          let absence_implicitly_maps_to_bottom = true
        end)
        (CallTarget.Set)

    let empty = bottom

    let create_root_from_identifier identifier = TaintAccessPath.Root.Variable identifier

    let initialize_from_roots ~callables_to_definitions_map alist =
      alist
      |> List.map ~f:(fun (root, target) ->
             (* ASTs use `TaintAccessPath.parameter_prefix` to distinguish local variables from
                parameters, but using parameters from the define does not result in creating
                parameterized targets whose parameter names contain
                `TaintAccessPath.parameter_prefix`. To be consistent, we use the former. In
                addition, we treat formal arguments and local variables as the same variant under
                `TaintAccessPath.Root`, so that we can look up the value bound to an `Identifier`
                easily. *)
             let root =
               match TaintAccessPath.Root.parameter_name root with
               | Some name ->
                   name
                   |> TaintAccessPath.Root.prepend_parameter_prefix
                   |> create_root_from_identifier
               | None -> root
             in
             let is_class_method, is_static_method =
               CallablesSharedMemory.ReadOnly.get_method_kind callables_to_definitions_map target
             in
             ( root,
               target
               |> CallTarget.create
                    ~implicit_receiver:
                      (is_implicit_receiver
                         ~is_static_method
                         ~is_class_method
                         ~explicit_receiver:false
                         target)
                    ~is_class_method
                    ~is_static_method
               |> CallTarget.Set.singleton ))
      |> of_list


    let initialize_from_callable ~callables_to_definitions_map = function
      | Target.Regular _ -> bottom
      | Target.Parameterized { parameters; _ } ->
          parameters
          |> Target.ParameterMap.to_alist
          |> initialize_from_roots ~callables_to_definitions_map
  end

  module MakeTransferFunction (Context : sig
    (* Inputs. *)
    val pyre_api : PyrePysaApi.ReadOnly.t

    val get_callee_model : Target.t -> t option

    val debug : bool

    val module_qualifier : Reference.t

    val define : Ast.Statement.Define.t Node.t

    val define_name : Reference.t

    val callable : Target.t

    val callables_to_definitions_map : CallablesSharedMemory.ReadOnly.t

    val type_of_expression_shared_memory : TypeOfExpressionSharedMemory.t

    val skip_analysis_targets : Target.HashSet.t

    val called_when_parameter : Target.HashSet.t

    val profiler : CallGraphProfiler.t

    val maximum_target_depth : int

    val maximum_parameterized_targets_at_call_site : int option

    val maximum_parameterized_targets_when_analyzing_define : int option

    val input_define_call_graph : DefineCallGraph.t

    (* Outputs. *)
    val output_define_call_graph : DefineCallGraph.t ref
  end) =
  struct
    type t = State.t [@@deriving show]

    let bottom = State.bottom

    let less_or_equal = State.less_or_equal

    let join = State.join

    let widen ~previous ~next ~iteration = State.widen ~prev:previous ~next ~iteration

    let log format =
      if Context.debug then
        Log.dump format
      else
        Log.log ~section:`CallGraph format


    let store_callees ~weak ~root ~callees state =
      State.update state root ~f:(function
          | None -> callees
          | Some existing_callees ->
              if weak then CallTarget.Set.join existing_callees callees else callees)


    let returned_callables call_targets =
      call_targets
      |> List.map ~f:(fun { CallTarget.target; _ } ->
             log "Fetching returned callables for `%a`" Target.pp_pretty target;
             match Context.get_callee_model target with
             | Some { returned_callables; _ } -> returned_callables
             | None -> CallTarget.Set.bottom)
      |> Algorithms.fold_balanced ~f:CallTarget.Set.join ~init:CallTarget.Set.bottom


    let self_variable =
      if Ast.Statement.Define.is_method (Node.value Context.define) then
        let { Ast.Statement.Define.signature = { parameters; _ }; _ } = Node.value Context.define in
        match TaintAccessPath.normalize_parameters parameters with
        | { root = TaintAccessPath.Root.PositionalParameter { position = 0; _ }; qualified_name; _ }
          :: _ ->
            Some (TaintAccessPath.Root.Variable qualified_name)
        | _ -> None
      else
        None


    let is_decorated_target = Target.is_decorated Context.callable

    let get_returned_callables state =
      self_variable
      >>| (fun self_variable ->
            (* For `__init__`, any functions stored in `self` would be returned, in order to
               propagate them. *)
            match Target.get_regular Context.callable with
            | Target.Regular.Method { method_name = "__init__"; _ } -> State.get self_variable state
            | _ -> CallTarget.Set.bottom)
      |> Option.value ~default:CallTarget.Set.bottom
      |> CallTarget.Set.join (State.get TaintAccessPath.Root.LocalResult state)


    let cartesian_product_with_limit ~limit ~message_when_exceeding_limit list =
      match limit, list with
      | Some limit, head :: tail when limit > 0 ->
          let number_of_combinations =
            List.fold
              ~init:(List.length head)
              ~f:(fun so_far element -> element |> List.length |> Int.( * ) so_far)
              tail
          in
          if number_of_combinations > limit then (
            log
              "%s due to `%d` exceeding limit `%d`"
              message_when_exceeding_limit
              number_of_combinations
              limit;
            None)
          else
            Some (Algorithms.cartesian_product list)
      | _, _ -> Some (Algorithms.cartesian_product list)


    module AnalyzeDecoratedTargetsResult = struct
      type t = {
        (* A subset of the input call targets that are of `kind=Decorated`. *)
        decorated_targets: CallTarget.t list;
        (* A subset of the input call targets that are not of `kind=Decorated`. *)
        non_decorated_targets: CallTarget.t list;
        (* The result of evaluating the input call targets. *)
        result_targets: CallTarget.t list;
      }
    end

    (* Analyze any list of `CallTarget.t`s, which may contain decorated targets -- we would get
       their returned callable. The reason is that, the original call graph contains call edges to
       `foo@decorated` targets, which are symbolic target representing the decorated callables.
       Those need to be replaced by the actual function being called, which will usually be some
       kind of `decorator.inner[captured(f):foo]` target. *)
    let resolve_decorated_targets call_targets =
      let decorated_targets, non_decorated_targets =
        List.partition_tf
          ~f:(fun { CallTarget.target; _ } -> Target.is_decorated target)
          call_targets
      in
      log
        "Decorated targets: `%a`"
        Target.List.pp_pretty_with_kind
        (List.map ~f:CallTarget.target decorated_targets);
      {
        AnalyzeDecoratedTargetsResult.decorated_targets;
        non_decorated_targets;
        result_targets =
          non_decorated_targets
          |> CallTarget.Set.of_list
          |> CallTarget.Set.join (returned_callables decorated_targets)
          |> CallTarget.Set.elements;
      }


    let validate_target target =
      let exceed_depth = Target.depth target > Context.maximum_target_depth in
      let contain_recursive_target = Target.contain_recursive_target target in
      let skip_analysis =
        Target.should_skip_analysis ~skip_analysis_targets:Context.skip_analysis_targets target
      in
      if contain_recursive_target || exceed_depth || skip_analysis then
        let () =
          log
            "Invalid target: `%a` (contain_recursive_target: `%b`. exceed_depth: `%b`. \
             skip_analysis: `%b`)"
            Target.pp_pretty_with_kind
            target
            contain_recursive_target
            exceed_depth
            skip_analysis
        in
        None
      else
        Some target


    (* Results of analyzing a certain kind of call targets (e.g., `call_targets` or `init_targets`)
       on a callee expression. *)
    module AnalyzeCalleeResult = struct
      type t = {
        (* Transforming the input call targets by providing parameter targets. *)
        parameterized_targets: CallTarget.t list;
        (* The sublist of the input call target list that are of `kind=Decorated`. *)
        decorated_targets: CallTarget.t list;
        (* The sublist of the input call target list that are not transformed above. We create these
           regular targets when (1) no parameter targets exist or (2) we cannot find function bodies
           of the callee, so that the taint analysis can still use `higher_order_parameters`. *)
        non_parameterized_targets: CallTarget.t list;
        (* The sublist of the input call target list that are stubs. *)
        stub_targets: Target.t list;
      }
    end

    let analyze_callee_targets
        ~location
        ~call
        ~arguments
        ~unresolved
        ~override_implicit_receiver
        ~argument_callees
        ~track_apply_call_step_name
        callee_targets
      =
      let track_apply_call_step step f =
        CallGraphProfiler.track_apply_call_step
          ~profiler:Context.profiler
          ~analysis:Forward
          ~step
          ~call_target:
            (* A hack to distinguish the profiling of different calls to
               `analyze_callee_targets`. *)
            (Some (Target.Regular.Object track_apply_call_step_name |> Target.from_regular))
          ~location
          ~argument:None
          ~f
      in
      let formal_arguments_if_non_stub target =
        if Target.is_override target || Target.is_object target then
          (* TODO(T204630385): It is possible for a target to be an `Override`, which we do not
             handle for now, or an `Object`, such as a function-typed variable that cannot be
             resolved by the original call graph building. *)
          None
        else
          match
            CallablesSharedMemory.ReadOnly.get_signature Context.callables_to_definitions_map target
          with
          | Some { CallablesSharedMemory.CallableSignature.is_stub_like; parameters; _ } ->
              if is_stub_like then
                let () = log "Callable `%a` is a stub" Target.pp_pretty_with_kind target in
                None
              else
                parameters
                |> AstResult.to_option
                >>| TaintAccessPath.normalize_parameters
                >>| List.map ~f:(fun { TaintAccessPath.NormalizedParameter.root; _ } -> root)
          | None ->
              log "Cannot find define for callable `%a`" Target.pp_pretty_with_kind target;
              None
      in
      let create_parameter_target_excluding_args_kwargs (parameter_target, (_, argument_matches)) =
        match argument_matches, parameter_target with
        | { TaintAccessPath.root = TaintAccessPath.Root.StarParameter _; _ } :: _, _
        | { TaintAccessPath.root = TaintAccessPath.Root.StarStarParameter _; _ } :: _, _ ->
            (* TODO(T215864108): Since we do not distinguish paths under the same `Root`, we may run
               into conflicts in `of_alist_exn` below, which is avoided by excluding those cases,
               such as kwargs and args. *)
            None
        | { TaintAccessPath.root; _ } :: _, Some parameter_target ->
            Some (root, parameter_target.CallTarget.target)
        | _ -> (* TODO: Consider the remaining `argument_matches`. *) None
      in
      let non_parameterized_targets ~parameterized_targets call_targets =
        let regular_targets_from_parameterized =
          List.filter_map parameterized_targets ~f:(function
              | { CallTarget.target = Target.Parameterized { regular; _ }; _ } -> Some regular
              | { CallTarget.target = Target.Regular _; _ } -> None)
        in
        let is_parameterized { CallTarget.target; _ } =
          let regular = Target.get_regular target in
          List.exists regular_targets_from_parameterized ~f:(Target.Regular.equal regular)
        in
        List.filter call_targets ~f:(fun call_target -> call_target |> is_parameterized |> not)
      in
      let {
        AnalyzeDecoratedTargetsResult.decorated_targets;
        non_decorated_targets = _;
        result_targets = call_targets_from_callee;
      }
        =
        track_apply_call_step ComputeCalleeTargets (fun () ->
            resolve_decorated_targets callee_targets)
      in
      let recompute_implicit_receiver ({ CallTarget.implicit_receiver; _ } as callee_target) =
        if not override_implicit_receiver then
          implicit_receiver
        else
          match unresolved with
          | Unresolved.True _ ->
              (* Since the original call graphs cannot find the callees, the callee must be
                 discovered by higher order call graph building. Since it is unclear whether the
                 callee is always a function or a method under any context, the arguments must be
                 explicit, unless the callee is a bound method, which is not yet handled. *)
              log
                "Setting `implicit_receiver` to false for callee target `%a`"
                CallTarget.pp
                callee_target;
              false
          | Unresolved.False -> implicit_receiver
      in
      let create_call_target = function
        | Some callee_target :: parameter_targets ->
            let callee_regular, closure =
              match CallTarget.target callee_target with
              | Target.Regular regular -> regular, Target.ParameterMap.empty
              | Target.Parameterized { regular; parameters } -> regular, parameters
            in
            let formal_arguments =
              callee_regular |> Target.from_regular |> formal_arguments_if_non_stub
            in
            let parameter_targets, arguments =
              match ImplicitArgument.implicit_argument ~is_implicit_new:false callee_target with
              | ImplicitArgument.Callee ->
                  ( None :: parameter_targets,
                    { Call.Argument.name = None; value = call.Call.callee } :: arguments )
              | ImplicitArgument.CalleeBase ->
                  let { Node.value = call_expression; location } = call.Call.callee in
                  let self =
                    match call_expression with
                    | Expression.Name (Name.Attribute { base; _ }) -> base
                    | _ ->
                        (* Default to a placeholder self if we don't understand/retain information
                           of what self is. *)
                        Expression.Constant Constant.NoneLiteral |> Node.create ~location
                  in
                  ( None :: parameter_targets,
                    { Call.Argument.name = None; value = self } :: arguments )
              | ImplicitArgument.None -> parameter_targets, arguments
            in
            log
              "Formal arguments of callee regular `%a`: `%a`"
              Target.Regular.pp
              callee_regular
              TaintAccessPath.Root.List.pp
              (Option.value ~default:[] formal_arguments);
            let parameters =
              formal_arguments
              >>| TaintAccessPath.match_actuals_to_formals arguments
              >>| List.zip_exn parameter_targets
              >>| List.filter_map ~f:create_parameter_target_excluding_args_kwargs
              >>| Target.ParameterMap.of_alist_exn
              |> Option.value ~default:Target.ParameterMap.empty
              |> Target.ParameterMap.union
                   (fun _ _ right ->
                     (* The formal argument should shadow variables from the closure that share the
                        same name. *)
                     Some right)
                   closure
            in
            log "Parameter targets: %a" (Target.ParameterMap.pp Target.pp_pretty) parameters;
            if Target.ParameterMap.is_empty parameters then
              None
            else
              Target.Parameterized { regular = callee_regular; parameters }
              |> validate_target
              >>| fun target ->
              {
                callee_target with
                CallTarget.target;
                implicit_receiver = recompute_implicit_receiver callee_target;
              }
        | _ -> None
      in
      (* Treat an empty list as a single element list so that in each result of the cartesian
         product, there is still one element for the empty list, which preserves the indices of
         arguments. *)
      let to_option_list = function
        | [] -> [None]
        | list -> List.map ~f:(fun target -> Some target) list
      in
      let parameterized_targets =
        track_apply_call_step CreateParameterizedTargets (fun () ->
            argument_callees
            |> List.map ~f:(fun call_targets ->
                   call_targets |> CallTarget.Set.elements |> to_option_list)
            |> List.cons (to_option_list call_targets_from_callee)
            |> cartesian_product_with_limit
                 ~limit:Context.maximum_parameterized_targets_at_call_site
                 ~message_when_exceeding_limit:
                   "Avoid generating parameterized targets when analyzing call"
            |> Option.value ~default:[]
            |> List.filter_map ~f:create_call_target
            |> List.dedup_and_sort ~compare:CallTarget.compare)
      in
      let non_parameterized_targets =
        track_apply_call_step FindNonParameterizedTargets (fun () ->
            call_targets_from_callee
            |> non_parameterized_targets ~parameterized_targets
            |> List.map ~f:(fun call_target ->
                   {
                     call_target with
                     CallTarget.implicit_receiver = recompute_implicit_receiver call_target;
                   }))
      in
      List.iter parameterized_targets ~f:(fun { CallTarget.target; _ } ->
          log "Created parameterized target: `%a`" Target.pp_pretty target);
      let stub_targets =
        List.filter_map call_targets_from_callee ~f:(fun { CallTarget.target; _ } ->
            let is_stub =
              CallablesSharedMemory.ReadOnly.is_stub_like
                Context.callables_to_definitions_map
                target
              |> Option.value ~default:false
            in
            if is_stub then Some target else None)
      in
      {
        AnalyzeCalleeResult.parameterized_targets;
        decorated_targets;
        non_parameterized_targets;
        stub_targets;
      }


    let rec analyze_call ~pyre_in_context ~location ~call ~arguments ~state =
      let track_apply_call_step step f =
        CallGraphProfiler.track_apply_call_step
          ~profiler:Context.profiler
          ~analysis:Forward
          ~step
          ~call_target:None
          ~location
          ~argument:None
          ~f
      in
      let analyze_argument
          ~higher_order_parameters
          index
          (state_so_far, additional_higher_order_parameters, decorated_targets)
          { Call.Argument.value = argument; _ }
        =
        let callees, new_state =
          analyze_expression ~pyre_in_context ~state:state_so_far ~expression:argument
        in
        let call_targets_from_higher_order_parameters =
          match HigherOrderParameterMap.find_opt higher_order_parameters index with
          | Some { HigherOrderParameter.call_targets; _ } -> call_targets
          | None -> []
        in
        let partition_called_when_parameter =
          CallTarget.Set.fold
            CallTarget.Set.Element
            ~init:(CallTarget.Set.bottom, CallTarget.Set.bottom)
            ~f:(fun call_target (called_when_parameter, not_called_when_parameter) ->
              let is_called_when_parameter =
                call_target
                |> CallTarget.target
                |> Target.strip_parameters
                |> Core.Hash_set.mem Context.called_when_parameter
              in
              if is_called_when_parameter then
                CallTarget.Set.add call_target called_when_parameter, not_called_when_parameter
              else
                called_when_parameter, CallTarget.Set.add call_target not_called_when_parameter)
        in
        let {
          AnalyzeDecoratedTargetsResult.decorated_targets = new_decorated_targets;
          non_decorated_targets = _;
          result_targets;
        }
          =
          call_targets_from_higher_order_parameters
          |> CallTarget.Set.of_list
          |> CallTarget.Set.join callees
          |> CallTarget.Set.elements
          |> resolve_decorated_targets
        in
        let called_when_parameter, not_called_when_parameter =
          (* Partition on the targets after resolving decorated targets, not before resolving.
             Resolving decorated targets may result in targets that needs to be treated as
             `called_when_parameter`. *)
          result_targets |> CallTarget.Set.of_list |> partition_called_when_parameter
        in
        log
          "Finished analyzing argument `%a` -- called_when_parameter: %a. \
           not_called_when_parameter: %a"
          Expression.pp
          argument
          CallTarget.Set.pp
          called_when_parameter
          CallTarget.Set.pp
          not_called_when_parameter;
        let additional_higher_order_parameters =
          if CallTarget.Set.is_bottom called_when_parameter then
            additional_higher_order_parameters
          else
            HigherOrderParameterMap.add
              additional_higher_order_parameters
              {
                HigherOrderParameter.call_targets = CallTarget.Set.elements called_when_parameter;
                index;
                unresolved = Unresolved.False;
              }
        in
        ( ( new_state,
            additional_higher_order_parameters,
            List.rev_append new_decorated_targets decorated_targets ),
          not_called_when_parameter )
      in
      let ({
             CallCallees.call_targets = original_call_targets;
             higher_order_parameters = original_higher_order_parameters;
             unresolved;
             init_targets = original_init_targets;
             (* TODO(T243083593): Resolve decorated targets for __new__ *)
             new_targets = _;
             shim_target = original_shim_target;
             _;
           } as original_call_callees)
        =
        track_apply_call_step ResolveCall (fun () ->
            match DefineCallGraph.resolve_call ~location ~call Context.input_define_call_graph with
            | Some callees -> callees
            | None ->
                failwith
                  (Format.asprintf
                     "Could not find callees for `%a` in `%a` at `%a` in the call graph: `%a`"
                     Ast.Expression.Call.pp
                     call
                     Target.pp_pretty
                     Context.callable
                     Location.pp
                     location
                     DefineCallGraph.pp
                     Context.input_define_call_graph))
      in
      (* The analysis of the callee AST handles the redirection to artifically created decorator
         defines. *)
      let callee_return_values, state =
        analyze_expression ~pyre_in_context ~state ~expression:call.Call.callee
      in
      let ( (state, additional_higher_order_parameters, decorated_targets_from_arguments),
            argument_callees_not_called_when_parameter )
        =
        track_apply_call_step AnalyzeArguments (fun () ->
            List.fold_mapi
              arguments
              ~f:(analyze_argument ~higher_order_parameters:original_higher_order_parameters)
              ~init:(state, HigherOrderParameterMap.empty, []))
      in
      let ( parameterized_call_targets,
            decorated_call_targets,
            non_parameterized_call_targets,
            stub_call_targets )
        =
        let {
          AnalyzeCalleeResult.parameterized_targets = parameterized_callee_return_targets;
          decorated_targets = decorated_callee_return_targets;
          non_parameterized_targets = non_parameterized_callee_return_targets;
          stub_targets = stub_callee_return_targets;
        }
          =
          callee_return_values
          |> CallTarget.Set.elements
          |> analyze_callee_targets
               ~location
               ~call
               ~arguments
               ~unresolved
               ~override_implicit_receiver:true
               ~argument_callees:argument_callees_not_called_when_parameter
               ~track_apply_call_step_name:"callee_return_targets"
        in
        let {
          AnalyzeCalleeResult.parameterized_targets = parameterized_call_targets;
          decorated_targets = decorated_call_targets;
          non_parameterized_targets = non_parameterized_call_targets;
          stub_targets = stub_call_targets;
        }
          =
          analyze_callee_targets
            ~location
            ~call
            ~arguments
            ~unresolved
            ~override_implicit_receiver:false
            ~argument_callees:argument_callees_not_called_when_parameter
            ~track_apply_call_step_name:"call_targets"
            original_call_targets
        in
        ( List.rev_append parameterized_callee_return_targets parameterized_call_targets,
          List.rev_append decorated_callee_return_targets decorated_call_targets,
          List.rev_append non_parameterized_callee_return_targets non_parameterized_call_targets,
          List.rev_append stub_callee_return_targets stub_call_targets )
      in
      let {
        AnalyzeCalleeResult.parameterized_targets = parameterized_init_targets;
        decorated_targets = decorated_init_targets;
        non_parameterized_targets = non_parameterized_init_targets;
        stub_targets = stub_init_targets;
      }
        =
        analyze_callee_targets
          ~location
          ~call
          ~arguments
          ~unresolved
          ~override_implicit_receiver:false
          ~argument_callees:argument_callees_not_called_when_parameter
          ~track_apply_call_step_name:"init_targets"
          original_init_targets
      in
      (* Discard higher order parameters only if each original target is parameterized, except for
         the targets that must be treated as being called. *)
      let new_higher_order_parameters =
        if
          List.is_empty non_parameterized_call_targets
          && List.is_empty non_parameterized_init_targets
        then
          additional_higher_order_parameters
        else
          HigherOrderParameterMap.join
            original_higher_order_parameters
            additional_higher_order_parameters
      in
      let new_shim_target =
        match original_shim_target with
        | Some { ShimTarget.call_targets = shim_call_targets; argument_mapping; _ } ->
            let { AnalyzeDecoratedTargetsResult.result_targets; decorated_targets; _ } =
              resolve_decorated_targets shim_call_targets
            in
            Some { ShimTarget.call_targets = result_targets; decorated_targets; argument_mapping }
        | None -> None
      in
      let new_call_targets =
        parameterized_call_targets
        |> List.rev_append non_parameterized_call_targets
        |> List.dedup_and_sort ~compare:CallTarget.compare
      in
      let new_init_targets =
        parameterized_init_targets
        |> List.rev_append non_parameterized_init_targets
        |> List.dedup_and_sort ~compare:CallTarget.compare
      in
      (* Unset `unresolved` when the original call graph building cannot resolve callees under cases
         like `f()` or `f`. *)
      let new_unresolved =
        match unresolved with
        | Unresolved.True (BypassingDecorators UnknownIdentifierCallee)
        | Unresolved.True (BypassingDecorators UnknownCallCallee)
          when not (CallTarget.Set.is_bottom callee_return_values) ->
            Unresolved.False
        | _ -> unresolved
      in
      track_apply_call_step StoreCallCallees (fun () ->
          Context.output_define_call_graph :=
            DefineCallGraph.set_call_callees
              ~error_if_new:true
              ~location
              ~call
              ~callees:
                {
                  original_call_callees with
                  call_targets = new_call_targets;
                  decorated_targets =
                    decorated_call_targets
                    |> List.rev_append decorated_init_targets
                    |> List.rev_append decorated_targets_from_arguments
                    |> List.dedup_and_sort ~compare:CallTarget.compare;
                  init_targets = new_init_targets;
                  higher_order_parameters = new_higher_order_parameters;
                  shim_target = new_shim_target;
                  unresolved = new_unresolved;
                }
              !Context.output_define_call_graph);
      track_apply_call_step FetchReturnedCallables (fun () ->
          let returned_callables_from_call =
            new_call_targets |> List.rev_append new_init_targets |> returned_callables
          in
          (* To avoid false negatives when analyzing targets with `kind=Decorated`, sometimes
           * we allow all function-typed arguments to be passed directly to the return values.
           * - Case 1 is when calls might be unresolved.
           * - Case 2 is when there exists a stub `__init__` target. The stub target's summary
           * is considered as passing through.
           * - Case 3 is when there exists a stub call target. The stub target's summary
           * is considered as passing through.*)
          let pass_through_arguments =
            let exist_stub_init_targets = not (List.is_empty stub_init_targets) in
            let exist_stub_call_targets = not (List.is_empty stub_call_targets) in
            let should_pass_through =
              is_decorated_target
              && (exist_stub_init_targets
                 || Unresolved.is_unresolved unresolved
                 || exist_stub_call_targets)
            in
            if should_pass_through then
              let () =
                log
                  "Passing through arguments due to `is_decorated_target`: %b, \
                   `exist_stub_init_targets`: %b, `exist_stub_call_targets`: %b, `unresolved`: %a"
                  is_decorated_target
                  exist_stub_init_targets
                  exist_stub_call_targets
                  Unresolved.pp
                  unresolved
              in
              Algorithms.fold_balanced
                ~f:CallTarget.Set.join
                ~init:CallTarget.Set.bottom
                argument_callees_not_called_when_parameter
            else
              CallTarget.Set.bottom
          in
          CallTarget.Set.join pass_through_arguments returned_callables_from_call, state)


    and analyze_comprehension_generators ~pyre_in_context ~state generators =
      let add_binding
          (state, pyre_in_context)
          ({ Comprehension.Generator.conditions; _ } as generator)
        =
        let { Assign.target; value; _ }, inner_pyre_context =
          preprocess_generator
            ~pyre_in_context
            ~type_of_expression_shared_memory:Context.type_of_expression_shared_memory
            ~callable:Context.callable
            generator
        in
        let state =
          match value with
          | Some value -> analyze_expression ~pyre_in_context ~state ~expression:value |> snd
          | None -> state
        in
        (* TODO: assign value to target *)
        let _ = target in
        (* Analyzing the conditions might have side effects. *)
        let analyze_condition state condiiton =
          analyze_expression ~pyre_in_context:inner_pyre_context ~state ~expression:condiiton |> snd
        in
        let state = List.fold conditions ~init:state ~f:analyze_condition in
        state, inner_pyre_context
      in
      List.fold ~f:add_binding generators ~init:(state, pyre_in_context)


    and analyze_dictionary_comprehension
        ~pyre_in_context
        ~state
        { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators; _ }
      =
      let state, pyre_in_context =
        analyze_comprehension_generators ~pyre_in_context ~state generators
      in
      let _, state = analyze_expression ~pyre_in_context ~state ~expression:value in
      let _, state = analyze_expression ~pyre_in_context ~state ~expression:key in
      CallTarget.Set.bottom, state


    and analyze_comprehension ~pyre_in_context ~state { Comprehension.element; generators; _ } =
      let bound_state, pyre_in_context =
        analyze_comprehension_generators ~pyre_in_context ~state generators
      in
      let _, state = analyze_expression ~pyre_in_context ~state:bound_state ~expression:element in
      CallTarget.Set.bottom, state


    (* Return possible callees and the new state. *)
    and analyze_expression
        ~pyre_in_context
        ~state
        ~expression:({ Node.value; location } as expression)
      =
      log
        "Analyzing expression `%a` at `%a` with state `%a`"
        Expression.pp_expression
        expression.Node.value
        Location.pp
        location
        State.pp
        state;
      let analyze_expression_inner () =
        match value with
        | Expression.Await { Await.operand = expression; origin = _ } ->
            analyze_expression ~pyre_in_context ~state ~expression
        | BooleanOperator { left; right; _ } ->
            let _, state = analyze_expression ~pyre_in_context ~state ~expression:left in
            let _, state = analyze_expression ~pyre_in_context ~state ~expression:right in
            CallTarget.Set.bottom, state
        | ComparisonOperator { left; operator = _; right; origin = _ } ->
            let _, state = analyze_expression ~pyre_in_context ~state ~expression:left in
            let _, state = analyze_expression ~pyre_in_context ~state ~expression:right in
            CallTarget.Set.bottom, state
        | Call ({ callee = _; arguments; origin = _ } as call) ->
            analyze_call ~pyre_in_context ~location ~call ~arguments ~state
        | Constant _ -> CallTarget.Set.bottom, state
        | Dictionary entries ->
            let analyze_dictionary_entry state = function
              | Dictionary.Entry.KeyValue { key; value } ->
                  let _, state = analyze_expression ~pyre_in_context ~state ~expression:key in
                  let _, state = analyze_expression ~pyre_in_context ~state ~expression:value in
                  state
              | Splat s -> analyze_expression ~pyre_in_context ~state ~expression:s |> snd
            in
            let state = List.fold entries ~f:analyze_dictionary_entry ~init:state in
            CallTarget.Set.bottom, state
        | DictionaryComprehension comprehension ->
            analyze_dictionary_comprehension ~pyre_in_context ~state comprehension
        | Generator comprehension -> analyze_comprehension ~pyre_in_context ~state comprehension
        | Lambda { parameters = _; body } ->
            let _, state = analyze_expression ~pyre_in_context ~state ~expression:body in
            CallTarget.Set.bottom, state
        | List list ->
            let analyze_list_element state expression =
              analyze_expression ~pyre_in_context ~state ~expression |> snd
            in
            let state = List.fold list ~f:analyze_list_element ~init:state in
            CallTarget.Set.bottom, state
        | ListComprehension comprehension ->
            analyze_comprehension ~pyre_in_context ~state comprehension
        | Set set ->
            let analyze_set_element state expression =
              analyze_expression ~pyre_in_context ~state ~expression |> snd
            in
            let state = List.fold ~f:analyze_set_element set ~init:state in
            CallTarget.Set.bottom, state
        | SetComprehension comprehension ->
            analyze_comprehension ~pyre_in_context ~state comprehension
        | Name (Name.Identifier identifier) ->
            let global_callables =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_identifier ~location ~identifier
              >>| (fun ({
                          IdentifierCallees.if_called =
                            {
                              CallCallees.call_targets = original_call_targets;
                              init_targets = original_init_targets;
                              (* TODO(T243083593): Resolve decorated targets for __new__ *)
                              new_targets = _;
                              _;
                            } as if_called;
                          _;
                        } as identifier_callees) ->
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_call_targets;
                      non_decorated_targets = non_decorated_call_targets;
                      result_targets = result_call_targets;
                    }
                      =
                      resolve_decorated_targets original_call_targets
                    in
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_init_targets;
                      non_decorated_targets = non_decorated_init_targets;
                      result_targets = _;
                    }
                      =
                      resolve_decorated_targets original_init_targets
                    in
                    Context.output_define_call_graph :=
                      DefineCallGraph.set_identifier_callees
                        ~error_if_new:true
                        ~identifier
                        ~location
                        ~identifier_callees:
                          {
                            identifier_callees with
                            if_called =
                              {
                                if_called with
                                CallGraph.CallCallees.call_targets = non_decorated_call_targets;
                                init_targets = non_decorated_init_targets;
                                decorated_targets =
                                  decorated_call_targets
                                  |> List.rev_append decorated_init_targets
                                  |> List.dedup_and_sort ~compare:CallTarget.compare;
                              };
                          }
                        !Context.output_define_call_graph;
                    CallTarget.Set.of_list result_call_targets)
              |> Option.value ~default:CallTarget.Set.bottom
            in
            let callables_from_variable =
              State.get (State.create_root_from_identifier identifier) state
            in
            CallTarget.Set.join global_callables callables_from_variable, state
        | Name (Name.Attribute attribute_access) ->
            let callables =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_attribute_access ~location ~attribute_access
              >>| (fun ({
                          AttributeAccessCallees.property_targets;
                          if_called =
                            {
                              CallCallees.call_targets = original_call_targets;
                              init_targets = original_init_targets;
                              (* TODO(T243083593): Resolve decorated targets for __new__ *)
                              new_targets = _;
                              _;
                            } as if_called;
                          is_attribute =
                            _
                            (* This is irrelevant. Regardless of whether this could potentially be
                               an attribute access, we still need to treat `property_targets` in the
                               same way as `callable_targets`. *);
                          _;
                        } as attribute_access_callees) ->
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_call_targets;
                      non_decorated_targets = non_decorated_call_targets;
                      result_targets = result_call_targets;
                    }
                      =
                      resolve_decorated_targets original_call_targets
                    in
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_init_targets;
                      non_decorated_targets = non_decorated_init_targets;
                      result_targets = _;
                    }
                      =
                      resolve_decorated_targets original_init_targets
                    in
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets = decorated_property_targets;
                      non_decorated_targets = _;
                      result_targets = result_property_targets;
                    }
                      =
                      (* Since properties can be decorated, we need to get the "inlined"
                         properties. *)
                      resolve_decorated_targets property_targets
                    in
                    Context.output_define_call_graph :=
                      DefineCallGraph.set_attribute_access_callees
                        ~error_if_new:false (* empty attribute accesses are stripped *)
                        ~location
                        ~attribute_access
                        ~callees:
                          {
                            attribute_access_callees with
                            property_targets = result_property_targets;
                            if_called =
                              {
                                if_called with
                                CallGraph.CallCallees.call_targets = non_decorated_call_targets;
                                init_targets = non_decorated_init_targets;
                                decorated_targets =
                                  decorated_property_targets
                                  |> List.rev_append decorated_call_targets
                                  |> List.rev_append decorated_init_targets
                                  |> List.dedup_and_sort ~compare:CallTarget.compare;
                              };
                          }
                        !Context.output_define_call_graph;
                    (* TODO(T222400916): We need to simulate the call to the property targets (by
                       calling `analyze_callee_targets`), which can return callables. *)
                    (* We should NOT return the property targets here. If method `A.foo` is a
                       property, then accessing the property `A().foo` means calling the getter, but
                       the result of the access is not the getter itself. *)
                    CallTarget.Set.of_list result_call_targets)
              |> Option.value ~default:CallTarget.Set.bottom
            in
            callables, state
        | Starred (Starred.Once expression)
        | Starred (Starred.Twice expression) ->
            let _, state = analyze_expression ~pyre_in_context ~state ~expression in
            CallTarget.Set.bottom, state
        | FormatString substrings ->
            let analyze_substring state = function
              | Substring.Literal _ -> state
              | Substring.Format { value; format_spec } ->
                  (* TODO: redirect decorators in the stringify target *)
                  let _, state = analyze_expression ~pyre_in_context ~state ~expression:value in
                  let state =
                    match format_spec with
                    | Some format_spec ->
                        analyze_expression ~pyre_in_context ~state ~expression:format_spec |> snd
                    | None -> state
                  in
                  state
            in
            let state = List.fold substrings ~init:state ~f:analyze_substring in
            CallTarget.Set.bottom, state
        | Ternary { target; test; alternative } ->
            let _, state = analyze_expression ~pyre_in_context ~state ~expression:test in
            let value_then, state_then =
              analyze_expression ~pyre_in_context ~state ~expression:target
            in
            let value_else, state_else =
              analyze_expression ~pyre_in_context ~state ~expression:alternative
            in
            CallTarget.Set.join value_then value_else, join state_then state_else
        | Tuple expressions ->
            let analyze_tuple_element state expression =
              analyze_expression ~pyre_in_context ~state ~expression |> snd
            in
            let state = List.fold ~f:analyze_tuple_element ~init:state expressions in
            CallTarget.Set.bottom, state
        | UnaryOperator { operand; _ } ->
            let _, state = analyze_expression ~pyre_in_context ~state ~expression:operand in
            CallTarget.Set.bottom, state
        | WalrusOperator { target = _; value; origin = _ } ->
            analyze_expression ~pyre_in_context ~state ~expression:value
        | Yield None -> CallTarget.Set.bottom, state
        | Yield (Some expression)
        | YieldFrom expression ->
            let callees, state = analyze_expression ~pyre_in_context ~state ~expression in
            callees, store_callees ~weak:true ~root:TaintAccessPath.Root.LocalResult ~callees state
        | Slice _ ->
            failwith "Slice nodes should always be rewritten by `CallGraph.redirect_expressions`"
        | Subscript _ ->
            failwith
              "Subscripts nodes should always be rewritten by `CallGraph.redirect_expressions`"
        | BinaryOperator _ ->
            failwith
              "BinaryOperator nodes should always be rewritten by `CallGraph.redirect_expressions`"
      in
      let call_targets, state =
        CallGraphProfiler.track_expression_analysis
          ~profiler:Context.profiler
          ~analysis:Forward
          ~expression
          ~f:analyze_expression_inner
      in
      log
        "Finished analyzing expression `%a`: `%a`"
        Expression.pp
        expression
        CallTarget.Set.pp
        call_targets;
      call_targets, state


    let analyze_parameter_default_value ~pyre_in_context ~parameter_name ~state = function
      | None -> state
      | Some default_value ->
          let default_value =
            preprocess_parameter_default_value
              ~pyre_in_context
              ~type_of_expression_shared_memory:Context.type_of_expression_shared_memory
              ~callable:Context.callable
              default_value
          in
          let callees, state =
            analyze_expression ~pyre_in_context ~state ~expression:default_value
          in
          let root = parameter_name |> State.create_root_from_identifier in
          store_callees ~weak:true ~root ~callees state


    let analyze_statement ~pyre_in_context ~state ~statement =
      log "Analyzing statement `%a` with state `%a`" Statement.pp statement State.pp state;
      let state =
        let statement =
          preprocess_statement
            ~pyre_in_context
            ~type_of_expression_shared_memory:Context.type_of_expression_shared_memory
            ~callable:Context.callable
            statement
        in
        match Node.value statement with
        | Statement.Assign { Assign.target; value = Some value; _ } -> (
            match TaintAccessPath.of_expression ~self_variable target with
            | None -> state
            | Some { root; path } ->
                let callees, state = analyze_expression ~pyre_in_context ~state ~expression:value in
                (* For now, we ignore the path entirely. Thus, we should only perform strong updates
                   when writing to an empty path. E.g, `x = foo` should be strong update, `x.foo =
                   bar` should be a weak update. *)
                let strong_update = TaintAccessPath.Path.is_empty path in
                store_callees ~weak:(not strong_update) ~root ~callees state)
        | Assign { Assign.target; value = None; _ } -> (
            match TaintAccessPath.of_expression ~self_variable target with
            | None -> state
            | Some { root; path } ->
                let strong_update = TaintAccessPath.Path.is_empty path in
                store_callees ~weak:(not strong_update) ~root ~callees:CallTarget.Set.bottom state)
        | Assert { test; _ } -> analyze_expression ~pyre_in_context ~state ~expression:test |> snd
        | Define ({ Define.signature = { name; _ }; _ } as define) ->
            let define_location =
              Define.location_with_decorators
                { Node.location = Node.location statement; value = define }
            in
            let callees_without_captures, captures =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_define ~define_location
              >>| (fun { DefineCallees.define_targets; _ } ->
                    let {
                      AnalyzeDecoratedTargetsResult.decorated_targets;
                      non_decorated_targets = _ (* Not useful to taint analysis. *);
                      result_targets;
                    }
                      =
                      resolve_decorated_targets define_targets
                    in
                    let () =
                      Context.output_define_call_graph :=
                        DefineCallGraph.set_define_callees
                          ~error_if_new:true
                          ~define_location
                          ~callees:
                            { DefineCallees.define_targets = result_targets; decorated_targets }
                          !Context.output_define_call_graph
                    in
                    let captures =
                      match define_targets with
                      | [define_target] ->
                          define_target
                          |> CallTarget.target
                          |> (* Since `Define` statements inside another `Define` are stripped out
                                (to avoid bloat), use this API to query the definition. *)
                          CallablesSharedMemory.ReadOnly.get_captures
                            Context.callables_to_definitions_map
                      | _ ->
                          Format.asprintf
                            "Expect a single `define_target` but got `[%s]`"
                            (define_targets |> List.map ~f:CallTarget.show |> String.concat ~sep:";")
                          |> failwith
                    in
                    result_targets, captures)
              |> Option.value ~default:([], None)
            in
            let callees =
              match captures with
              | Some captures ->
                  let parameters_roots, parameters_targets =
                    captures
                    |> List.filter_map ~f:(fun name ->
                           let captured = State.create_root_from_identifier name in
                           log "Inner function captures `%a`" TaintAccessPath.Root.pp captured;
                           let parameter_targets =
                             state
                             |> State.get captured
                             |> CallTarget.Set.elements
                             |> List.map ~f:CallTarget.target
                           in
                           (* Sometimes a captured variable does not have a record in `state`, but
                              we still want to create a callee with the captured variables that have
                              records in `state`. *)
                           if List.is_empty parameter_targets then
                             None
                           else
                             Some (captured, parameter_targets))
                    |> List.unzip
                  in
                  if List.is_empty parameters_targets then
                    callees_without_captures
                  else
                    parameters_targets
                    |> cartesian_product_with_limit
                         ~limit:Context.maximum_parameterized_targets_when_analyzing_define
                         ~message_when_exceeding_limit:
                           "Avoid generating parameterized targets when analyzing `Define` \
                            statement"
                    |> Option.value ~default:[]
                    |> List.concat_map ~f:(fun parameters_targets ->
                           List.map callees_without_captures ~f:(fun call_target ->
                               match
                                 validate_target
                                   (Target.Parameterized
                                      {
                                        regular =
                                          call_target |> CallTarget.target |> Target.get_regular;
                                        parameters =
                                          parameters_targets
                                          |> List.zip_exn parameters_roots
                                          |> Target.ParameterMap.of_alist_exn;
                                      })
                               with
                               | Some parameterized -> { call_target with target = parameterized }
                               | None -> call_target))
              | None -> callees_without_captures
            in
            store_callees
              ~weak:false
              ~root:(name |> Reference.show |> State.create_root_from_identifier)
              ~callees:(CallTarget.Set.of_list callees)
              state
        | Delete expressions ->
            let analyze_delete state expression =
              analyze_expression ~pyre_in_context ~state ~expression |> snd
            in
            List.fold ~f:analyze_delete ~init:state expressions
        | Expression expression ->
            analyze_expression ~pyre_in_context ~state ~expression |> Core.snd
        | Global _
        | Import _
        | Nonlocal _
        | Pass
        | Raise { expression = None; _ } ->
            state
        | Raise { expression = Some expression; _ } ->
            analyze_expression ~pyre_in_context ~state ~expression |> snd
        | Return { expression = Some expression; _ } ->
            (* No need to propagate `ReturnShimCallees`, since the taint analysis only need to
               analyze them once. TODO(T231956685): Resolve decorated targets. *)
            let callees, state = analyze_expression ~pyre_in_context ~state ~expression in
            let () =
              Context.input_define_call_graph
              |> DefineCallGraph.resolve_return ~statement_location:statement.Node.location
              >>| (fun callees ->
                    Context.output_define_call_graph :=
                      DefineCallGraph.add_return_callees
                        ~debug:Context.debug
                        ~caller:Context.callable
                        ~on_existing_callees:DefineCallGraph.OnExistingCallees.WarnThenJoin
                        ~return_expression:expression
                        ~statement_location:statement.Node.location
                        ~callees
                        !Context.output_define_call_graph)
              |> Option.value ~default:()
            in
            store_callees ~weak:true ~root:TaintAccessPath.Root.LocalResult ~callees state
        | Return { expression = None; _ }
        | Try _ ->
            (* Try statements are lowered down in `Cfg.create`, but they are preserved in the final
               Cfg. They should be ignored. *)
            state
        | Break
        | Class _
        | Continue
        | TypeAlias _ ->
            state
        | For _
        | If _
        | Match _
        | With _
        | While _ ->
            failwith "For/If/Match/With/While nodes should always be rewritten by `Cfg.create`"
        | AugmentedAssign _ ->
            failwith
              "AugmentedAssign nodes should always be rewritten by `CallGraph.preprocess_statement`"
      in
      log "Finished analyzing statement `%a`: `%a`" Statement.pp statement State.pp state;
      state


    let forward ~statement_key state ~statement =
      CallGraphProfiler.track_statement_analysis
        ~profiler:Context.profiler
        ~analysis:Forward
        ~statement
        ~f:(fun () ->
          let pyre_in_context =
            PyrePysaApi.InContext.create_at_statement_scope
              Context.pyre_api
              ~module_qualifier:Context.module_qualifier
              ~define_name:Context.define_name
              ~define:Context.define
              ~statement_key
          in
          analyze_statement ~pyre_in_context ~state ~statement)


    let backward ~statement_key:_ _ ~statement:_ = failwith "unused"
  end
end

let debug_higher_order_call_graph define =
  Ast.Statement.Define.dump define
  || Ast.Statement.Define.dump_call_graph define
  || Ast.Statement.Define.dump_higher_order_call_graph define


let higher_order_call_graph_of_define
    ~define_call_graph
    ~pyre_api
    ~callables_to_definitions_map
    ~type_of_expression_shared_memory
    ~skip_analysis_targets
    ~called_when_parameter
    ~qualifier
    ~callable
    ~define
    ~initial_state
    ~get_callee_model
    ~profiler
    ~maximum_target_depth
    ~maximum_parameterized_targets_at_call_site
  =
  let module Context = struct
    let input_define_call_graph = define_call_graph

    let output_define_call_graph = ref (DefineCallGraph.copy define_call_graph)

    let pyre_api = pyre_api

    let get_callee_model = get_callee_model

    let debug = debug_higher_order_call_graph (Node.value define)

    let module_qualifier = qualifier

    let define = define

    let define_name = Target.define_name_exn callable

    let callable = callable

    let callables_to_definitions_map = callables_to_definitions_map

    let type_of_expression_shared_memory = type_of_expression_shared_memory

    let skip_analysis_targets = skip_analysis_targets

    let called_when_parameter = called_when_parameter

    let profiler = profiler

    let maximum_target_depth = maximum_target_depth

    let maximum_parameterized_targets_at_call_site = maximum_parameterized_targets_at_call_site

    let maximum_parameterized_targets_when_analyzing_define =
      maximum_parameterized_targets_at_call_site
  end
  in
  log
    ~debug:Context.debug
    "Building higher order call graph of `%a` with initial state `%a`. Define call graph: `%a`"
    Target.pp
    callable
    HigherOrderCallGraph.State.pp
    initial_state
    DefineCallGraph.pp
    define_call_graph;
  let module TransferFunction = HigherOrderCallGraph.MakeTransferFunction (Context) in
  let module Fixpoint = PyrePysaLogic.Fixpoint.Make (TransferFunction) in
  (* Handle parameters. *)
  let initial_state =
    let pyre_in_context =
      PyrePysaApi.InContext.create_at_function_scope
        pyre_api
        ~module_qualifier:qualifier
        ~define_name:(Target.define_name_exn callable)
    in
    List.fold
      define.Ast.Node.value.Ast.Statement.Define.signature.parameters
      ~init:initial_state
      ~f:(fun state { Node.value = { Parameter.name; value = default_value; _ }; _ } ->
        TransferFunction.analyze_parameter_default_value
          ~pyre_in_context
          ~state
          ~parameter_name:name
          default_value)
  in
  let returned_callables =
    let cfg =
      PyrePysaLogic.Cfg.create
        ~normalize_asserts:(PyrePysaApi.ReadOnly.is_pyre1 pyre_api)
        (Node.value define)
    in
    Fixpoint.forward ~cfg ~initial:initial_state
    |> Fixpoint.exit
    >>| TransferFunction.get_returned_callables
    |> Option.value ~default:CallTarget.Set.bottom
  in
  let call_indexer = CallGraph.Indexer.create () in
  {
    HigherOrderCallGraph.returned_callables;
    call_graph =
      !Context.output_define_call_graph
      |> DefineCallGraph.filter_empty_attribute_access
      |> DefineCallGraph.dedup_and_sort
      |> DefineCallGraph.regenerate_call_indices ~indexer:call_indexer;
  }


let call_graph_of_define
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.find_missing_flows; _ }
    ~pyre_api
    ~override_graph
    ~attribute_targets
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~type_of_expression_shared_memory
    ~check_invariants
    ~qualifier
    ~callable
    ~define
  =
  let timer = Timer.start () in
  let is_missing_flow_type_analysis =
    Option.equal
      Configuration.MissingFlowKind.equal
      find_missing_flows
      (Some Configuration.MissingFlowKind.Type)
  in
  let define_name = Target.define_name_exn callable in
  let callees_at_location = ref DefineCallGraph.empty in
  let context =
    {
      CallGraphBuilder.Context.module_qualifier = qualifier;
      define_name = Some define_name;
      callable;
      missing_flow_type_analysis =
        (if is_missing_flow_type_analysis then
           Some { MissingFlowTypeAnalysis.caller = callable }
        else
          None);
      debug =
        Ast.Statement.Define.dump (Node.value define)
        || Ast.Statement.Define.dump_call_graph (Node.value define);
      override_graph;
      attribute_targets;
      callables_to_definitions_map;
      callables_to_decorators_map;
      type_of_expression_shared_memory;
      expression_identifier_invariant =
        (if check_invariants then Some (ExpressionIdentifierInvariant.create ()) else None);
    }
  in
  let module DefineFixpoint = DefineCallGraphFixpoint (struct
    let builder_context = context

    let pyre_api = pyre_api

    let callees_at_location = callees_at_location

    let define = define
  end)
  in
  let () = log ~debug:context.debug "Building call graph of `%a`" Target.pp_pretty callable in
  (* Handle parameters. *)
  let () =
    let pyre_in_context =
      PyrePysaApi.InContext.create_at_function_scope
        pyre_api
        ~module_qualifier:qualifier
        ~define_name
    in
    List.iter
      define.Ast.Node.value.Ast.Statement.Define.signature.parameters
      ~f:(fun { Node.value = { Parameter.value; _ }; _ } ->
        Option.iter value ~f:(fun value ->
            let value =
              preprocess_parameter_default_value
                ~pyre_in_context
                ~type_of_expression_shared_memory
                ~callable
                value
            in
            CallGraphBuilder.build_for_expression
              ~pyre_in_context
              ~assignment_target:None
              ~context
              ~callees_at_location
              value))
  in

  let cfg =
    PyrePysaLogic.Cfg.create
      ~normalize_asserts:(PyrePysaApi.ReadOnly.is_pyre1 pyre_api)
      (Node.value define)
  in
  log ~debug:context.debug "Processing CFG:@.%a" PyrePysaLogic.Cfg.pp cfg;
  DefineFixpoint.forward ~cfg ~initial:() |> ignore;
  let call_indexer = CallGraph.Indexer.create () in
  let call_graph =
    !callees_at_location
    |> DefineCallGraph.filter_empty_attribute_access
    |> DefineCallGraph.map_target
         ~f:(CallableToDecoratorsMap.SharedMemory.redirect_to_decorated callables_to_decorators_map)
         ~map_call_if:CallCallees.should_redirect_to_decorated
         ~map_return_if:(fun _ -> false)
    |> DefineCallGraph.dedup_and_sort
    |> DefineCallGraph.regenerate_call_indices ~indexer:call_indexer
  in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Call graph built"
    ~section:`DependencyGraph
    ~normals:["callable", Target.show_pretty callable]
    ~timer
    ();
  call_graph


let call_graph_of_callable
    ~static_analysis_configuration
    ~pyre_api
    ~override_graph
    ~attribute_targets
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~type_of_expression_shared_memory
    ~check_invariants
    ~callable
  =
  match CallablesSharedMemory.ReadOnly.get_define callables_to_definitions_map callable with
  | Some { CallablesSharedMemory.DefineAndQualifier.qualifier; define } ->
      call_graph_of_define
        ~static_analysis_configuration
        ~pyre_api
        ~override_graph
        ~attribute_targets
        ~callables_to_decorators_map
        ~callables_to_definitions_map
        ~type_of_expression_shared_memory
        ~check_invariants
        ~qualifier
        ~callable
        ~define
  | _ -> Format.asprintf "Found no definition for `%a`" Target.pp_pretty callable |> failwith


let call_graph_of_decorated_callable
    ~debug
    ~pyre_api
    ~override_graph
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~type_of_expression_shared_memory
    ~callable
    ~body:
      {
        CallableToDecoratorsMap.DecoratedDefineBody.return_expression;
        original_function_name;
        original_function_name_location;
        module_qualifier;
        define_name;
        decorated_callable;
        _;
      }
  =
  let pyre_in_context =
    PyrePysaApi.InContext.create_at_function_scope pyre_api ~module_qualifier ~define_name
  in
  let resolve_callees ~call_graph expression =
    let context =
      {
        CallGraphBuilder.Context.module_qualifier;
        define_name = Some define_name;
        callable = decorated_callable;
        missing_flow_type_analysis = None;
        debug;
        override_graph;
        attribute_targets = Target.HashSet.create ();
        callables_to_definitions_map;
        callables_to_decorators_map;
        type_of_expression_shared_memory;
        expression_identifier_invariant = None;
      }
    in
    let expression =
      preprocess_expression
        ~pyre_in_context
        ~type_of_expression_shared_memory
        ~callable:decorated_callable
        expression
    in
    CallGraphBuilder.build_for_expression
      ~pyre_in_context
      ~assignment_target:None
      ~context
      ~callees_at_location:call_graph
      expression
  in
  (* If Pyre cannot resolve the callable on the callable expression, we hardcode the callable. *)
  let add_callees_for_original_function_name_if_unresolved
      ~callee
      ~original_function_name
      ~original_function_name_location
      ~call_graph
    =
    let original_function_name =
      match original_function_name with
      | Name.Attribute attribute -> attribute
      | original_function_name ->
          Format.asprintf
            "Expect the decorated callable to be an attribute but got `%a`"
            Name.pp
            original_function_name
          |> failwith
    in
    let should_add_callable =
      !call_graph
      |> DefineCallGraph.resolve_attribute_access
           ~location:original_function_name_location
           ~attribute_access:original_function_name
      >>| (fun {
                 AttributeAccessCallees.if_called =
                   { CallCallees.call_targets = callable_targets; _ };
                 property_targets;
                 _;
               } -> List.is_empty callable_targets && List.is_empty property_targets)
      |> Option.value ~default:true
    in
    if should_add_callable then
      let is_class_method, is_static_method =
        CallablesSharedMemory.ReadOnly.get_method_kind callables_to_definitions_map callee
      in
      call_graph :=
        DefineCallGraph.set_attribute_access_callees
          ~error_if_new:false (* empty attribute accesses are stripped *)
          ~location:original_function_name_location
          ~attribute_access:original_function_name
          ~callees:
            (AttributeAccessCallees.create
               ~if_called:
                 (CallCallees.create
                    ~call_targets:
                      [
                        CallTarget.create
                          ~implicit_receiver:
                            (is_implicit_receiver
                               ~is_static_method
                               ~is_class_method
                               ~explicit_receiver:false
                               callee)
                          ~is_class_method
                          ~is_static_method
                          callee;
                      ]
                    ())
               ())
          !call_graph
  in
  let call_graph = ref DefineCallGraph.empty in
  resolve_callees ~call_graph return_expression;
  add_callees_for_original_function_name_if_unresolved
    ~callee:callable
    ~original_function_name
    ~original_function_name_location
    ~call_graph;
  DefineCallGraph.filter_empty_attribute_access !call_graph


let default_scheduler_policy =
  Scheduler.Policy.fixed_chunk_size
    ~minimum_chunks_per_worker:1
    ~minimum_chunk_size:2
    ~preferred_chunk_size:5000
    ()


(** Build the whole call graph of the program.

    The overrides must be computed first because we depend on a global shared memory graph to
    include overrides in the call graph. Without it, we'll underanalyze and have an inconsistent
    fixpoint. *)
let build_whole_program_call_graph_for_pyre1
    ~scheduler
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.scheduler_policies; _ } as static_analysis_configuration)
    ~pyre_api
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~type_of_expression_shared_memory
    ~override_graph
    ~store_shared_memory
    ~attribute_targets
    ~skip_analysis_targets
    ~check_invariants
    ~definitions
    ~create_dependency_for
  =
  let reduce
      (left_define_call_graphs, left_whole_program_call_graph)
      (right_define_call_graphs, right_whole_program_call_graph)
    =
    (* We should check the keys in two define call graphs are disjoint. If not disjoint, we should
     * fail the analysis. But we don't perform such check due to performance reasons.
     * Additionally, since this `reduce` is used in `Scheduler.map_reduce`, the right parameter
     * is accumulated, so we must select left as smaller and right as larger for O(n) merging. *)
    ( CallGraph.SharedMemory.AddOnly.merge_same_handle_disjoint_keys
        ~smaller:left_define_call_graphs
        ~larger:right_define_call_graphs,
      WholeProgramCallGraph.merge_disjoint
        left_whole_program_call_graph
        right_whole_program_call_graph )
  in
  let attribute_targets = attribute_targets |> Target.Set.elements |> Target.HashSet.of_list in
  let define_call_graphs = CallGraph.SharedMemory.create () |> CallGraph.SharedMemory.add_only in
  let empty_define_call_graphs = CallGraph.SharedMemory.AddOnly.create_empty define_call_graphs in
  let define_call_graphs, whole_program_call_graph =
    let build_call_graph ((define_call_graphs, whole_program_call_graph) as so_far) callable =
      if Target.should_skip_analysis ~skip_analysis_targets callable then
        so_far
      else
        let callable_call_graph =
          Alarm.with_alarm
            ~max_time_in_seconds:60
            ~event_name:"call graph building"
            ~callable:(Target.show_pretty callable)
            (fun () ->
              call_graph_of_callable
                ~static_analysis_configuration
                ~pyre_api
                ~override_graph
                ~attribute_targets
                ~callables_to_decorators_map
                ~callables_to_definitions_map
                ~type_of_expression_shared_memory
                ~check_invariants
                ~callable)
            ()
        in
        let define_call_graphs =
          if store_shared_memory then
            CallGraph.SharedMemory.AddOnly.add define_call_graphs callable callable_call_graph
          else
            define_call_graphs
        in
        let whole_program_call_graph =
          WholeProgramCallGraph.add_or_exn
            whole_program_call_graph
            ~callable
            ~callees:
              (DefineCallGraph.all_targets ~use_case:create_dependency_for callable_call_graph)
        in
        define_call_graphs, whole_program_call_graph
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.CallGraph
        ~default:default_scheduler_policy
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:(empty_define_call_graphs, WholeProgramCallGraph.empty)
      ~map:(fun definitions ->
        List.fold
          definitions
          ~init:(empty_define_call_graphs, WholeProgramCallGraph.empty)
          ~f:build_call_graph)
      ~reduce
      ~inputs:definitions
      ()
  in
  let define_call_graphs, whole_program_call_graph =
    let build_call_graph_for_decorated_target
        ((define_call_graphs, whole_program_call_graph) as so_far)
        callable
      =
      match
        CallableToDecoratorsMap.SharedMemory.decorated_callable_body
          callables_to_decorators_map
          callable
      with
      | None -> so_far
      | Some decorated_callable_body ->
          let decorated_callable =
            CallableToDecoratorsMap.SharedMemory.redirect_to_decorated
              callables_to_decorators_map
              callable
          in
          let callable_call_graph =
            Alarm.with_alarm
              ~max_time_in_seconds:60
              ~event_name:"call graph building"
              ~callable:(Target.show_pretty decorated_callable)
              (fun () ->
                call_graph_of_decorated_callable
                  ~debug:false
                  ~pyre_api
                  ~override_graph
                  ~callables_to_definitions_map
                  ~callables_to_decorators_map
                  ~type_of_expression_shared_memory
                  ~callable (* original callable *)
                  ~body:decorated_callable_body)
              ()
          in
          let define_call_graphs =
            if store_shared_memory then
              CallGraph.SharedMemory.AddOnly.add
                define_call_graphs
                decorated_callable
                callable_call_graph
            else
              define_call_graphs
          in
          let whole_program_call_graph =
            WholeProgramCallGraph.add_or_exn
              whole_program_call_graph
              ~callable:decorated_callable
              ~callees:
                (DefineCallGraph.all_targets ~use_case:create_dependency_for callable_call_graph)
          in
          define_call_graphs, whole_program_call_graph
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.DecoratorResolution
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:(define_call_graphs, whole_program_call_graph)
      ~map:(fun definitions ->
        List.fold
          definitions
          ~init:(empty_define_call_graphs, WholeProgramCallGraph.empty)
          ~f:build_call_graph_for_decorated_target)
      ~reduce
      ~inputs:definitions
      ()
  in
  let define_call_graphs = CallGraph.SharedMemory.from_add_only define_call_graphs in
  { CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs }


let build_whole_program_call_graph_for_pyrefly
    ~scheduler
    ~scheduler_policies
    ~pyrefly_api
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~override_graph
    ~store_shared_memory
    ~attribute_targets
    ~skip_analysis_targets
    ~definitions
    ~create_dependency_for
  =
  let transform_redirected_call_graph decorated_target call_graph =
    (* For call graph of decorated targets, add a call graph edge for the decorated function itself,
       in the return expression `decorator1(decorator2(original_function))` *)
    let original_callable = Target.set_kind Target.Normal decorated_target in
    let {
      CallableToDecoratorsMap.DecoratedDefineBody.original_function_name;
      original_function_name_location;
      _;
    }
      =
      CallableToDecoratorsMap.SharedMemory.decorated_callable_body
        callables_to_decorators_map
        original_callable
      |> Option.value_exn ~message:"Unexpected decorated target without a decorated body"
    in
    let original_function_name =
      match original_function_name with
      | Name.Identifier name -> name
      | original_function_name ->
          Format.asprintf
            "Expect the decorated callable to be an identifier, but got `%a`"
            Name.pp
            original_function_name
          |> failwith
    in
    let {
      PyreflyApi.CallableMetadata.is_staticmethod = is_static_method;
      is_classmethod = is_class_method;
      _;
    }
      =
      PyreflyApi.ReadOnly.get_callable_metadata
        pyrefly_api
        (Target.define_name_exn original_callable)
    in
    DefineCallGraph.set_identifier_callees
      ~error_if_new:false
      ~location:original_function_name_location
      ~identifier:original_function_name
      ~identifier_callees:
        (IdentifierCallees.create
           ~if_called:
             (CallCallees.create
                ~call_targets:
                  [
                    CallTarget.create
                      ~implicit_receiver:
                        (is_implicit_receiver
                           ~is_static_method
                           ~is_class_method
                           ~explicit_receiver:false
                           original_callable)
                      ~is_class_method
                      ~is_static_method
                      original_callable;
                  ]
                ())
           ())
      call_graph
  in
  let add_targets callable call_graph =
    match CallablesSharedMemory.ReadOnly.get_define callables_to_definitions_map callable with
    | AstResult.Some
        {
          CallablesSharedMemory.DefineAndQualifier.define = { Node.location; value = define };
          qualifier;
        } ->
        let allow_modifier = function
          | PyrePysaApi.TypeModifier.Optional
          | PyrePysaApi.TypeModifier.Coroutine
          | PyrePysaApi.TypeModifier.Awaitable
          | PyrePysaApi.TypeModifier.ReadOnly
          | PyrePysaApi.TypeModifier.TypeVariableBound ->
              true
          | PyrePysaApi.TypeModifier.Type -> false
        in
        let is_class_instance modifiers = List.for_all ~f:allow_modifier modifiers in
        let is_class_type modifiers =
          match List.rev modifiers with
          | PyrePysaApi.TypeModifier.Type :: rest -> List.for_all ~f:allow_modifier rest
          | _ -> false
        in
        let map_attribute_access
            ~location:attribute_access_location
            ~attribute_access:
              ({ Ast.Expression.Name.Attribute.base; attribute; _ } as attribute_access)
            call_graph
          =
          let targets =
            PyreflyApi.ReadOnly.get_type_of_expression
              pyrefly_api
              ~qualifier
              ~location:(Ast.Node.location base)
            >>| PyreflyApi.ReadOnly.Type.get_class_names pyrefly_api
            >>| (fun { PyrePysaApi.ClassNamesFromType.classes; _ } -> classes)
            >>| List.map ~f:(fun { PyrePysaApi.ClassWithModifiers.modifiers; class_name } ->
                    let parents =
                      class_name :: PyreflyApi.ReadOnly.class_mro pyrefly_api class_name
                    in
                    if is_class_instance modifiers then
                      List.map
                        ~f:(fun class_name -> Format.sprintf "%s.%s" class_name attribute)
                        parents
                    else if is_class_type modifiers then
                      List.map
                        ~f:(fun class_name -> Format.sprintf "%s.__class__.%s" class_name attribute)
                        parents
                    else
                      [])
            >>| List.concat
            >>| List.map ~f:Reference.create
            >>| List.map ~f:Target.create_object
            >>| List.filter ~f:(fun target -> Target.Set.mem target attribute_targets)
            >>| List.map ~f:(fun target -> CallGraph.CallTarget.create target)
            |> Option.value ~default:[]
          in
          if not (List.is_empty targets) then
            DefineCallGraph.add_attribute_access_callees
              ~debug:false
              ~caller:callable
              ~on_existing_callees:DefineCallGraph.OnExistingCallees.Join
              ~location:attribute_access_location
              ~attribute_access
              ~callees:(AttributeAccessCallees.create ~global_targets:targets ~is_attribute:true ())
              call_graph
          else
            call_graph
        in
        let add_shim_target ~debug ~expression_location ~call ~arguments call_graph =
          let fetch_regular_targets call_callees =
            call_callees.CallCallees.call_targets
            |> List.rev_append call_callees.CallCallees.init_targets
            |> List.rev_append call_callees.CallCallees.new_targets
            |> List.map ~f:CallTarget.target
          in
          DefineCallGraph.resolve_call ~location:expression_location ~call call_graph
          >>= fun original_call_callees ->
          let { Node.value = callee_expression; location = callee_location } = call.Call.callee in
          (* This is the call inside `call`, if any *)
          let nested_callees =
            match callee_expression with
            | Expression.Call nested_call ->
                DefineCallGraph.resolve_call ~location:callee_location ~call:nested_call call_graph
                >>| fetch_regular_targets
            | _ -> None
          in
          let () =
            log
              ~debug
              "Shimming call: `%a`. Original callees: `%a`"
              Call.pp
              call
              CallCallees.pp
              original_call_callees
          in
          shim_for_call_for_pyrefly
            ~callees:(fetch_regular_targets original_call_callees)
            ~nested_callees:(Option.value ~default:[] nested_callees)
            ~arguments
          >>= fun shim ->
          create_shim_callee_expression ~debug ~callable ~location ~call shim
          >>= fun ({ Node.value = shim_callee; location = shim_callee_location } as
                  shim_callee_expression) ->
          let () =
            log
              ~debug
              "Shimmed callee: `%a` at `%a`"
              Expression.pp
              shim_callee_expression
              Location.pp
              shim_callee_location
          in
          let set_shim_target ~call_targets call_graph =
            if List.is_empty call_targets then
              let () =
                log
                  ~debug
                  "Failed to resolve callees for shimmed callee %a"
                  Expression.pp
                  shim_callee_expression
              in
              call_graph
            else
              let shim_target =
                Some { ShimTarget.call_targets; decorated_targets = []; argument_mapping = shim }
              in
              DefineCallGraph.set_call_callees
                ~error_if_new:false
                ~location:expression_location
                ~call
                ~callees:{ original_call_callees with CallCallees.shim_target }
                call_graph
          in
          match shim_callee with
          | Expression.Name (Name.Identifier identifier) ->
              DefineCallGraph.resolve_identifier
                ~location:shim_callee_location
                ~identifier
                call_graph
              >>| fun { IdentifierCallees.if_called = { CallCallees.call_targets; _ }; _ } ->
              set_shim_target ~call_targets call_graph
          | Expression.Name (Name.Attribute attribute_access) ->
              DefineCallGraph.resolve_attribute_access
                ~location:shim_callee_location
                ~attribute_access
                call_graph
              >>| fun { AttributeAccessCallees.if_called = { CallCallees.call_targets; _ }; _ } ->
              set_shim_target ~call_targets call_graph
          | shim_callee ->
              (* If `shim_callee` does not refer to an existing AST node (i.e., it is a made-up
                 one), then we give up adding a shim target. Otherwise we fetch callees on them and
                 use those as shim targets -- see the above cases. *)
              let () =
                log
                  ~debug
                  "Unknown shim callee (probably a made-up AST node): `%s`"
                  (Expression.show_expression shim_callee)
              in
              Some call_graph
        in
        let module Visitor = Ast.Visit.Make (struct
          type t = DefineCallGraph.t

          let debug =
            Ast.Statement.Define.dump define || Ast.Statement.Define.dump_call_graph define


          let expression call_graph { Node.value = expression; location = expression_location } =
            match expression with
            | Expression.Name (Ast.Expression.Name.Attribute attribute_access) ->
                (* For each attribute access, check the base and determine whether the attribute has
                   a user-provided model. *)
                map_attribute_access ~location:expression_location ~attribute_access call_graph
            | Expression.Call ({ Call.arguments; _ } as call) ->
                add_shim_target ~debug ~expression_location ~call ~arguments call_graph
                |> Option.value ~default:call_graph
            | _ -> call_graph


          let statement call_graph _ = call_graph
        end)
        in
        Visitor.visit call_graph (Source.create [Node.create ~location (Statement.Define define)])
    | _ -> call_graph
  in
  let transform_call_graph _ callable call_graph =
    let call_indexer = CallGraph.Indexer.create () in
    let call_graph =
      if Target.is_decorated callable then
        transform_redirected_call_graph callable call_graph
      else
        call_graph
        |> DefineCallGraph.map_target
             ~f:
               (CallableToDecoratorsMap.SharedMemory.redirect_to_decorated
                  callables_to_decorators_map)
             ~map_call_if:CallCallees.should_redirect_to_decorated
             ~map_return_if:(fun _ -> false)
        |> add_targets callable
    in
    call_graph
    |> DefineCallGraph.dedup_and_sort
    |> DefineCallGraph.filter_empty_attribute_access
    |> DefineCallGraph.filter_empty_identifier
    |> DefineCallGraph.filter_empty_format_string_stringify
    |> DefineCallGraph.regenerate_call_indices ~indexer:call_indexer
  in
  let method_has_overrides method_name =
    match override_graph with
    | Some override_graph ->
        OverrideGraph.SharedMemory.ReadOnly.overrides_exist
          override_graph
          (Target.Regular (Target.Regular.Method method_name))
    | None -> false
  in
  PyreflyApi.ReadOnly.parse_call_graphs
    pyrefly_api
    ~scheduler
    ~scheduler_policies
    ~method_has_overrides
    ~store_shared_memory
    ~attribute_targets
    ~skip_analysis_targets
    ~definitions
    ~create_dependency_for
    ~redirect_to_decorated:
      (CallableToDecoratorsMap.SharedMemory.redirect_to_decorated_opt callables_to_decorators_map)
    ~transform_call_graph


let build_whole_program_call_graph
    ~scheduler
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.scheduler_policies; _ } as static_analysis_configuration)
    ~pyre_api
    ~resolve_module_path
    ~callables_to_definitions_map
    ~callables_to_decorators_map
    ~type_of_expression_shared_memory
    ~override_graph
    ~store_shared_memory
    ~attribute_targets
    ~skip_analysis_targets
    ~check_invariants
    ~definitions
    ~create_dependency_for
  =
  let { CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs } =
    match pyre_api with
    | PyrePysaApi.ReadOnly.Pyre1 _ ->
        build_whole_program_call_graph_for_pyre1
          ~scheduler
          ~static_analysis_configuration
          ~pyre_api
          ~callables_to_definitions_map
          ~callables_to_decorators_map
          ~type_of_expression_shared_memory
          ~override_graph
          ~store_shared_memory
          ~attribute_targets
          ~skip_analysis_targets
          ~check_invariants
          ~definitions
          ~create_dependency_for
    | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
        build_whole_program_call_graph_for_pyrefly
          ~scheduler
          ~scheduler_policies
          ~pyrefly_api
          ~callables_to_definitions_map
          ~callables_to_decorators_map
          ~override_graph
          ~attribute_targets
          ~store_shared_memory
          ~skip_analysis_targets
          ~definitions
          ~create_dependency_for
  in
  let () =
    let define_call_graphs_read_only = CallGraph.SharedMemory.read_only define_call_graphs in
    DefineCallGraph.save_to_directory
      ~scheduler
      ~static_analysis_configuration
      ~resolve_qualifier:(CallablesSharedMemory.ReadOnly.get_qualifier callables_to_definitions_map)
      ~resolve_module_path
      ~get_call_graph:(fun callable ->
        CallGraph.SharedMemory.ReadOnly.get define_call_graphs_read_only ~cache:false ~callable)
      ~json_kind:NewlineDelimitedJson.Kind.CallGraph
      ~filename_prefix:"call-graph"
      ~callables:definitions
  in
  { CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs }
