(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* CallGraph: defines the call graph of a callable (function or method), which
 * stores the set of calles for each call site.
 *
 * This also implements the logic to statically compute the call graph, given a
 * function definition.
 *
 * Note that the call graph is highly tuned for the taint analysis and might be
 * unsound for other analyses.
 *)

open Core
open Data_structures
open Ast
open Statement
open Expression
open Pyre
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment
module PyrePysaLogic = Analysis.PyrePysaLogic

module JsonHelper = struct
  let add_optional name value to_json bindings =
    match value with
    | Some value -> (name, to_json value) :: bindings
    | None -> bindings


  let add_flag_if name value condition bindings =
    if condition then
      (name, value) :: bindings
    else
      bindings


  let add_list name elements to_json bindings =
    match elements with
    | [] -> bindings
    | _ -> (name, `List (List.map ~f:to_json elements)) :: bindings
end

(** Represents type information about the return type of a call. *)
module ReturnType = struct
  type t = {
    is_boolean: bool;
    is_integer: bool;
    is_float: bool;
    is_enumeration: bool;
  }
  [@@deriving compare, eq, sexp, hash]

  let pp formatter { is_boolean; is_integer; is_float; is_enumeration } =
    let add_if condition tag tags =
      if condition then
        tag :: tags
      else
        tags
    in
    []
    |> add_if is_enumeration "enum"
    |> add_if is_float "float"
    |> add_if is_integer "int"
    |> add_if is_boolean "bool"
    |> String.concat ~sep:"|"
    |> Format.fprintf formatter "{%s}"


  let show = Format.asprintf "%a" pp

  let none = { is_boolean = false; is_integer = false; is_float = false; is_enumeration = false }

  let any = none

  let bool = { is_boolean = true; is_integer = true; is_float = true; is_enumeration = false }

  let integer = { is_boolean = false; is_integer = true; is_float = true; is_enumeration = false }

  let from_annotation ~pyre_api annotation =
    let matches_at_leaves ~f annotation =
      let rec matches_at_leaves ~f annotation =
        match annotation with
        | Type.Any
        | Type.Bottom ->
            false
        | Type.Union [Type.NoneType; annotation]
        | Type.Union [annotation; Type.NoneType]
        | Type.Parametric { name = "typing.Awaitable"; arguments = [Single annotation] } ->
            matches_at_leaves ~f annotation
        | Type.Tuple (Concatenation concatenation) ->
            Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
            >>| (fun element -> matches_at_leaves ~f element)
            |> Option.value ~default:(f annotation)
        | Type.Tuple (Type.OrderedTypes.Concrete annotations) ->
            List.for_all annotations ~f:(matches_at_leaves ~f)
        | annotation -> f annotation
      in
      matches_at_leaves ~f annotation
    in
    try
      let is_boolean =
        matches_at_leaves annotation ~f:(fun left ->
            PyrePysaEnvironment.ReadOnly.less_or_equal pyre_api ~left ~right:Type.bool)
      in
      let is_integer =
        matches_at_leaves annotation ~f:(fun left ->
            PyrePysaEnvironment.ReadOnly.less_or_equal pyre_api ~left ~right:Type.integer)
      in
      let is_float =
        matches_at_leaves annotation ~f:(fun left ->
            PyrePysaEnvironment.ReadOnly.less_or_equal pyre_api ~left ~right:Type.float)
      in
      let is_enumeration =
        matches_at_leaves annotation ~f:(fun left ->
            PyrePysaEnvironment.ReadOnly.less_or_equal pyre_api ~left ~right:Type.enumeration)
      in
      { is_boolean; is_integer; is_float; is_enumeration }
    with
    | PyrePysaLogic.UntrackedClass untracked_type ->
        Log.warning
          "Found untracked type `%s` when checking the return type `%a` of a call. The return type \
           will NOT be considered a scalar, which could lead to missing breadcrumbs."
          untracked_type
          Type.pp
          annotation;
        none


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
    from_annotation ~pyre_api annotation


  let to_json { is_boolean; is_integer; is_float; is_enumeration } =
    let add_string_if name condition elements =
      if condition then `String name :: elements else elements
    in
    []
    |> add_string_if "boolean" is_boolean
    |> add_string_if "integer" is_integer
    |> add_string_if "float" is_float
    |> add_string_if "enum" is_enumeration
    |> fun elements -> `List elements
end

module CallableToDecoratorsMap = struct
  type decorators = {
    decorators: Expression.t list;
    define_location: Location.t;
  }

  let ignored_decorator_prefixes_for_higher_order =
    ["sqlalchemy.testing"; "pytest"] |> SerializableStringSet.of_list


  let ignored_decorators_for_higher_order =
    [
      "property";
      "dataclass";
      "functools.cache";
      "functools.cached_property";
      "functools.lru_cache";
      "functools.total_ordering";
      "functools.singledispatch";
      "functools.wraps";
      "enum.verify";
      "enum.unique";
      "enum.property";
      "enum.member";
      "enum.nonmember";
      "enum.global_enum";
      "typing.final";
      "typing.runtime_checkable";
      "typing.dataclass_transform";
      "typing.overload";
      "typing.no_type_check";
      "typing.no_type_check_decorator";
      "typing.override";
      "typing.type_check_only";
      "atexit.register";
      "contextlib.contextmanager";
      "contextlib.asynccontextmanager";
      "abc.abstractmethod";
      "abc.abstractproperty";
      "pyre_extensions.override";
      "typing_extensions.deprecated";
      "typing_extensions.final";
      "typing_extensions.overload";
      "typing_extensions.override";
      "typing_extensions.runtime_checkable";
      "click.command";
      "click.group";
      "click.argument";
      "click.option";
      "click.password_option";
      "click.confirmation_option";
      "click.version_option";
      "click.help_option";
      "click.pass_context";
      "click.pass_obj";
      "click.make_pass_decorator";
      "click.decorators.pass_meta_key";
      "overrides.override";
      "overrides.overrides";
    ]
    @ Target.class_method_decorators
    @ Target.static_method_decorators
    @ Recognized.ignored_decorators_for_higher_order
    |> SerializableStringSet.of_list


  let filter_decorator decorator =
    if
      Analysis.DecoratorPreprocessing.has_any_decorator_action
        ~actions:(Analysis.DecoratorPreprocessing.Action.Set.of_list [DoNotInline; Discard])
        decorator
    then
      false
    else
      match Ast.Statement.Decorator.from_expression decorator with
      | Some { Decorator.name = { Node.value = decorator_name; _ }; _ } ->
          let decorator_name = Reference.show decorator_name in
          (not (SerializableStringSet.mem decorator_name ignored_decorators_for_higher_order))
          && not
               (SerializableStringSet.exists
                  (fun prefix -> String.is_prefix ~prefix decorator_name)
                  ignored_decorator_prefixes_for_higher_order)
      | None -> true


  let collect_decorators ~callables_to_definitions_map callable =
    callable
    |> Target.CallablesSharedMemory.ReadOnly.get_signature callables_to_definitions_map
    >>= fun { Target.CallablesSharedMemory.Signature.decorators; location = define_location; _ } ->
    let decorators = decorators |> List.filter ~f:filter_decorator |> List.rev in
    if List.is_empty decorators then
      None
    else
      Some { decorators; define_location }


  module SharedMemory = struct
    module T =
      Hack_parallel.Std.SharedMemory.FirstClassWithKeys.Make
        (Target.SharedMemoryKey)
        (struct
          type t = decorators

          let prefix = Hack_parallel.Std.Prefix.make ()

          let description = "callables to decorators"
        end)

    type t = T.t

    module ReadOnly = struct
      type t = T.ReadOnly.t

      let get = T.ReadOnly.get ~cache:true

      let get_decorators readonly callable =
        callable |> get readonly >>| fun { decorators; _ } -> decorators


      let mem = T.ReadOnly.mem
    end

    let empty = T.create

    let read_only = T.read_only

    let cleanup = T.cleanup ~clean_old:true

    let save_decorator_counts_to_directory
        ~static_analysis_configuration:
          {
            Configuration.StaticAnalysis.save_results_to;
            output_format;
            configuration = { local_root; _ };
            _;
          }
        ~scheduler
        shared_memory
      =
      let module DecoratorCount = struct
        type t = {
          count: int;
          decorator: string;
        }
        [@@deriving compare]

        let to_json { count; decorator } =
          [
            {
              NewlineDelimitedJson.Line.kind = DecoratorCount;
              data = `Assoc ["count", `Int count; "decorator", `String decorator];
            };
          ]
      end
      in
      let create_decorator_count = function
        | [] -> None
        | decorator :: _ as decorators ->
            Some { DecoratorCount.count = List.length decorators; decorator }
      in
      let show_decorator decorator =
        match decorator.Node.value with
        | Expression.Call { Call.callee; _ } -> (* Decorator factory. *) Expression.show callee
        | _ -> Expression.show decorator
      in
      let decorator_counts =
        shared_memory
        |> T.to_alist
        |> List.map ~f:(fun (_, { decorators; _ }) -> List.map ~f:show_decorator decorators)
        |> List.concat
        |> List.sort_and_group ~compare:String.compare
        |> List.filter_map ~f:create_decorator_count
        |> List.sort ~compare:DecoratorCount.compare
        |> List.rev
      in
      let filename_prefix = "decorator-counts" in
      match save_results_to with
      | Some directory -> (
          Log.info "Writing the decorator counts to `%s`" (PyrePath.absolute directory);
          match output_format with
          | Configuration.TaintOutputFormat.Json ->
              NewlineDelimitedJson.write_file
                ~path:
                  (PyrePath.append directory ~element:(Format.asprintf "%s.json" filename_prefix))
                ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
                ~to_json_lines:DecoratorCount.to_json
                decorator_counts
          | Configuration.TaintOutputFormat.ShardedJson ->
              NewlineDelimitedJson.remove_sharded_files ~directory ~filename_prefix;
              NewlineDelimitedJson.write_sharded_files
                ~scheduler
                ~directory
                ~filename_prefix
                ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
                ~to_json_lines:DecoratorCount.to_json
                decorator_counts)
      | None -> ()


    (* We assume `DecoratorPreprocessing.setup_preprocessing` is called before since we use its
       shared memory here. *)
    let create ~callables_to_definitions_map ~scheduler ~scheduler_policy callables =
      let shared_memory = T.create () in
      let shared_memory_add_only = T.add_only shared_memory in
      let empty_shared_memory = T.AddOnly.create_empty shared_memory_add_only in
      let map =
        List.fold ~init:empty_shared_memory ~f:(fun shared_memory target ->
            match collect_decorators ~callables_to_definitions_map target with
            | Some value -> T.AddOnly.add shared_memory target value
            | None -> shared_memory)
      in
      let shared_memory_add_only =
        Scheduler.map_reduce
          scheduler
          ~policy:scheduler_policy
          ~initial:shared_memory_add_only
          ~map
          ~reduce:(fun left right ->
            T.AddOnly.merge_same_handle_disjoint_keys ~smaller:left ~larger:right)
          ~inputs:callables
          ()
      in
      T.from_add_only shared_memory_add_only
  end

  (* Redirect any call to callable `foo` to its decorated version, if any. *)
  let redirect_to_decorated ~callable decorators =
    if Target.is_normal callable && SharedMemory.ReadOnly.mem decorators callable then
      Target.set_kind Target.Decorated callable
    else
      callable
end

let is_implicit_receiver ~is_static_method ~is_class_method ~explicit_receiver target =
  if is_static_method then
    false
  else if is_class_method then
    true
  else
    (not explicit_receiver) && Target.is_method_or_override target


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
    [@@deriving compare, eq, show { with_path = false }, sexp, hash]
  end

  include T

  module Set = struct
    type call_target = T.t

    include Abstract.SetDomain.MakeWithSet (struct
      module Element = T
      (* TODO: Use `CallCallees` instead of `CallTarget` to distinguish new and init targets. *)

      include Data_structures.SerializableSet.Make (Element)

      type element = Element.t

      let show_element = Element.show

      let element_name = "target"
    end)
  end

  let target { target; _ } = target

  let equal_ignoring_indices left right = equal left { right with index = left.index }

  let dedup_and_sort targets =
    targets
    |> List.sort ~compare
    |> List.remove_consecutive_duplicates ~which_to_keep:`First ~equal:equal_ignoring_indices


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


  let default =
    {
      target = "<default_call_target>" |> Reference.create |> Target.create_object;
      implicit_receiver = false;
      implicit_dunder_call = false;
      index = 0;
      return_type = Some ReturnType.any;
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
      target;
      implicit_receiver =
        is_implicit_receiver ~is_static_method ~is_class_method ~explicit_receiver target;
      implicit_dunder_call;
      index = 0;
      return_type;
      receiver_class = receiver_type >>= receiver_class_from_type ~is_class_method;
      is_class_method;
      is_static_method;
    }


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


  let redirect_to_decorated ~decorators ({ target; _ } as call_target) =
    {
      call_target with
      target = CallableToDecoratorsMap.redirect_to_decorated ~callable:target decorators;
    }


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


    let regenerate_index ~indexer ({ target; _ } as call_target) =
      { call_target with index = get_index ~indexer target }
  end
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
  [@@deriving eq, show, to_yojson]

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
  [@@deriving eq, show, to_yojson]

  type t =
    | True of reason
    | False
  [@@deriving eq, show]

  let is_unresolved = function
    | True _ -> true
    | False -> false


  let join left right =
    match left, right with
    | True _, True _ -> left
    | True _, False -> left
    | False, True _ -> right
    | False, False -> False


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
  [@@deriving eq, show { with_path = false }]

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


  let deduplicate { index; call_targets; unresolved } =
    { index; call_targets = CallTarget.dedup_and_sort call_targets; unresolved }


  let to_json { index; call_targets; unresolved } =
    ["parameter_index", `Int index; "calls", `List (List.map ~f:CallTarget.to_json call_targets)]
    |> JsonHelper.add_flag_if
         "unresolved"
         (Unresolved.to_json unresolved)
         (Unresolved.is_unresolved unresolved)
    |> fun bindings -> `Assoc bindings


  let redirect_to_decorated ~decorators ({ call_targets; _ } as higher_order_parameter) =
    {
      higher_order_parameter with
      call_targets = List.map ~f:(CallTarget.redirect_to_decorated ~decorators) call_targets;
    }


  let regenerate_call_indices ~indexer ({ call_targets; _ } as higher_order_parameter) =
    {
      higher_order_parameter with
      call_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) call_targets;
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

  let join left right =
    Map.union (fun _ left right -> Some (HigherOrderParameter.join left right)) left right


  let deduplicate map = Map.map HigherOrderParameter.deduplicate map

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


  let redirect_to_decorated ~decorators =
    Map.map (HigherOrderParameter.redirect_to_decorated ~decorators)


  let regenerate_call_indices ~indexer =
    Map.map (HigherOrderParameter.regenerate_call_indices ~indexer)
end

(** An aggregate of all possible callees at a call site. *)
module CallCallees = struct
  module RecognizedCall = struct
    type t =
      | True
      | False
      | Unknown
    [@@deriving eq, show { with_path = false }, to_yojson]

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
  [@@deriving eq, show { with_path = false }]

  let create
      ?(call_targets = [])
      ?(new_targets = [])
      ?(init_targets = [])
      ?(decorated_targets = [])
      ?(higher_order_parameters = HigherOrderParameterMap.empty)
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
        unresolved = left_unresolved;
        recognized_call = left_recognized_call;
      }
      {
        call_targets = right_call_targets;
        new_targets = right_new_targets;
        init_targets = right_init_targets;
        decorated_targets = right_decorator_targets;
        higher_order_parameters = right_higher_order_parameters;
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
    let unresolved = Unresolved.join left_unresolved right_unresolved in
    let recognized_call = RecognizedCall.join left_recognized_call right_recognized_call in
    {
      call_targets;
      new_targets;
      init_targets;
      decorated_targets;
      higher_order_parameters;
      unresolved;
      recognized_call;
    }


  let deduplicate
      {
        call_targets;
        new_targets;
        init_targets;
        decorated_targets;
        higher_order_parameters;
        unresolved;
        recognized_call;
      }
    =
    let call_targets = CallTarget.dedup_and_sort call_targets in
    let new_targets = CallTarget.dedup_and_sort new_targets in
    let init_targets = CallTarget.dedup_and_sort init_targets in
    let decorated_targets = CallTarget.dedup_and_sort decorated_targets in
    let higher_order_parameters = HigherOrderParameterMap.deduplicate higher_order_parameters in
    {
      call_targets;
      new_targets;
      init_targets;
      decorated_targets;
      higher_order_parameters;
      unresolved;
      recognized_call;
    }


  let all_targets
      ~use_case
      { call_targets; new_targets; init_targets; higher_order_parameters; decorated_targets; _ }
    =
    call_targets
    |> List.rev_append new_targets
    |> List.rev_append init_targets
    |> List.rev_append decorated_targets
    |> List.map ~f:CallTarget.target
    |> List.rev_append (HigherOrderParameterMap.all_targets ~use_case higher_order_parameters)


  let equal_ignoring_types
      {
        call_targets = call_targets_left;
        new_targets = new_targets_left;
        init_targets = init_targets_left;
        decorated_targets = decorator_targets_left;
        higher_order_parameters = higher_order_parameter_lefts;
        unresolved = unresolved_left;
        recognized_call = recognized_call_left;
      }
      {
        call_targets = call_targets_right;
        new_targets = new_targets_right;
        init_targets = init_targets_right;
        decorated_targets = decorator_targets_right;
        higher_order_parameters = higher_order_parameter_rights;
        unresolved = unresolved_right;
        recognized_call = recognized_call_right;
      }
    =
    List.equal CallTarget.equal_ignoring_types call_targets_left call_targets_right
    && List.equal CallTarget.equal_ignoring_types new_targets_left new_targets_right
    && List.equal CallTarget.equal_ignoring_types init_targets_left init_targets_right
    && List.equal CallTarget.equal_ignoring_types decorator_targets_left decorator_targets_right
    && HigherOrderParameterMap.equal_ignoring_types
         higher_order_parameter_lefts
         higher_order_parameter_rights
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
      | "typing.Mapping"
      | "typing.MutableMapping"
      | "TypedDictionary"
      | "NonTotalTypedDictionary"
      | "collections.OrderedDict"
      | "collections.defaultdict" ->
          true
      | _ -> false
    in
    is_method_of_class ~is_class_name callees


  let is_sequence_method callees =
    let is_class_name = function
      | "list"
      | "typing.Sequence"
      | "typing.MutableSequence"
      | "collections.deque"
      | "tuple" ->
          true
      | _ -> false
    in
    is_method_of_class ~is_class_name callees


  let is_string_method callees =
    let is_class_name = function
      | "str" -> true
      | _ -> false
    in
    is_method_of_class ~is_class_name callees


  let is_object_new = function
    | [] -> (* Unresolved call, assume it's object.__new__ *) true
    | [call_target] -> (
        match Target.get_regular call_target.CallTarget.target with
        | Target.Regular.Method { class_name = "object"; method_name = "__new__"; kind = Normal } ->
            true
        | _ -> false)
    | _ -> false


  let is_object_init = function
    | [] -> (* Unresolved call, assume it's object.__init__ *) true
    | [call_target] -> (
        match Target.get_regular call_target.CallTarget.target with
        | Target.Regular.Method { class_name = "object"; method_name = "__init__"; kind = Normal }
          ->
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


  let redirect_to_decorated
      ~decorators
      ({ call_targets; higher_order_parameters; new_targets; init_targets; recognized_call; _ } as
      call_callees)
    =
    if RecognizedCall.redirect_to_decorated recognized_call then
      {
        call_callees with
        call_targets = List.map ~f:(CallTarget.redirect_to_decorated ~decorators) call_targets;
        higher_order_parameters =
          HigherOrderParameterMap.redirect_to_decorated ~decorators higher_order_parameters;
        new_targets = List.map ~f:(CallTarget.redirect_to_decorated ~decorators) new_targets;
        init_targets = List.map ~f:(CallTarget.redirect_to_decorated ~decorators) init_targets;
      }
    else
      call_callees


  let drop_decorated_targets call_callees = { call_callees with decorated_targets = [] }

  let regenerate_call_indices
      ~indexer
      ({
         call_targets;
         new_targets;
         init_targets;
         higher_order_parameters;
         decorated_targets =
           _ (* No need to regenerate because they will not be used in taint analysis. *);
         _;
       } as call_callees)
    =
    {
      call_callees with
      call_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) call_targets;
      new_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) new_targets;
      init_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) init_targets;
      higher_order_parameters =
        HigherOrderParameterMap.regenerate_call_indices ~indexer higher_order_parameters;
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
    callable_targets: CallTarget.t list;
    (* Call targets for the calls to artificially created callables that call the decorators. Only
       used by call graph building. *)
    decorated_targets: CallTarget.t list;
  }
  [@@deriving eq, show { with_path = false }]

  let empty =
    {
      property_targets = [];
      global_targets = [];
      is_attribute = true;
      callable_targets = [];
      decorated_targets = [];
    }


  let is_empty attribute_access_callees = equal attribute_access_callees empty

  let create
      ?(property_targets = empty.property_targets)
      ?(global_targets = empty.global_targets)
      ?(callable_targets = empty.callable_targets)
      ?(is_attribute = empty.is_attribute)
      ?(decorated_targets = empty.decorated_targets)
      ()
    =
    { property_targets; global_targets; is_attribute; callable_targets; decorated_targets }


  let deduplicate
      { property_targets; global_targets; is_attribute; callable_targets; decorated_targets }
    =
    {
      property_targets = CallTarget.dedup_and_sort property_targets;
      global_targets = CallTarget.dedup_and_sort global_targets;
      is_attribute;
      callable_targets = CallTarget.dedup_and_sort callable_targets;
      decorated_targets = CallTarget.dedup_and_sort decorated_targets;
    }


  let join
      {
        property_targets = left_property_targets;
        global_targets = left_global_targets;
        is_attribute = left_is_attribute;
        callable_targets = left_callable_targets;
        decorated_targets = left_decorated_targets;
      }
      {
        property_targets = right_property_targets;
        global_targets = right_global_targets;
        is_attribute = right_is_attribute;
        callable_targets = right_callable_targets;
        decorated_targets = right_decorated_targets;
      }
    =
    {
      property_targets = List.rev_append left_property_targets right_property_targets;
      global_targets = List.rev_append left_global_targets right_global_targets;
      is_attribute = left_is_attribute || right_is_attribute;
      callable_targets = List.rev_append left_callable_targets right_callable_targets;
      decorated_targets = List.rev_append left_decorated_targets right_decorated_targets;
    }


  let all_targets
      ~use_case
      { property_targets; global_targets; callable_targets; is_attribute = _; decorated_targets }
    =
    (match use_case with
    | AllTargetsUseCase.CallGraphDependency ->
        (* A property could (in theory) return a callable. *)
        List.rev_append property_targets decorated_targets
    | AllTargetsUseCase.TaintAnalysisDependency -> List.rev_append property_targets global_targets
    | AllTargetsUseCase.Everything ->
        global_targets
        |> List.rev_append property_targets
        |> List.rev_append callable_targets
        |> List.rev_append decorated_targets)
    |> List.map ~f:CallTarget.target


  let equal_ignoring_types
      {
        property_targets = property_targets_left;
        global_targets = global_targets_left;
        is_attribute = is_attribute_left;
        callable_targets = callable_targets_left;
        decorated_targets = decorated_targets_left;
      }
      {
        property_targets = property_targets_right;
        global_targets = global_targets_right;
        is_attribute = is_attribute_right;
        callable_targets = callable_targets_right;
        decorated_targets = decorated_targets_right;
      }
    =
    List.equal CallTarget.equal_ignoring_types property_targets_left property_targets_right
    && List.equal CallTarget.equal_ignoring_types global_targets_left global_targets_right
    && Bool.equal is_attribute_left is_attribute_right
    && List.equal CallTarget.equal_ignoring_types callable_targets_left callable_targets_right
    && List.equal CallTarget.equal_ignoring_types decorated_targets_left decorated_targets_right


  let to_json
      { property_targets; global_targets; is_attribute; callable_targets; decorated_targets }
    =
    []
    |> JsonHelper.add_list "properties" property_targets CallTarget.to_json
    |> JsonHelper.add_list "globals" global_targets CallTarget.to_json
    |> JsonHelper.add_list "callables" callable_targets CallTarget.to_json
    |> JsonHelper.add_list "decorated_targets" decorated_targets CallTarget.to_json
    |> JsonHelper.add_flag_if "is_attribute" (`Bool true) is_attribute
    |> fun bindings -> `Assoc (List.rev bindings)


  let redirect_to_decorated ~decorators ({ callable_targets; _ } as attribute_callees) =
    {
      attribute_callees with
      callable_targets = List.map ~f:(CallTarget.redirect_to_decorated ~decorators) callable_targets;
    }


  let drop_decorated_targets attribute_access_callees =
    { attribute_access_callees with decorated_targets = [] }


  let regenerate_call_indices
      ~indexer
      ({
         property_targets;
         global_targets;
         callable_targets =
           _ (* No need to regenerate because they will not be used in taint analysis. *);
         _;
       } as attribute_callees)
    =
    {
      attribute_callees with
      property_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) property_targets;
      global_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) global_targets;
    }
end

(** An aggregate of all possible callees for a given identifier expression, i.e `foo`. *)
module IdentifierCallees = struct
  type t = {
    global_targets: CallTarget.t list;
    nonlocal_targets: CallTarget.t list;
    (* Function-typed runtime values that the identifier may evaluate into. *)
    callable_targets: CallTarget.t list;
    (* Call targets for the calls to artificially created callables that call the decorators. Only
       used by call graph building. *)
    decorated_targets: CallTarget.t list;
  }
  [@@deriving eq, show { with_path = false }]

  module Reference = struct
    type t =
      | Global of {
          reference: Reference.t;
          export_name: PyrePysaLogic.ModuleExport.Name.t option;
        }
      | Nonlocal of Reference.t
  end

  let create
      ?(global_targets = [])
      ?(nonlocal_targets = [])
      ?(callable_targets = [])
      ?(decorated_targets = [])
      ()
    =
    { global_targets; nonlocal_targets; callable_targets; decorated_targets }


  let deduplicate { global_targets; nonlocal_targets; callable_targets; decorated_targets } =
    {
      global_targets = CallTarget.dedup_and_sort global_targets;
      nonlocal_targets = CallTarget.dedup_and_sort nonlocal_targets;
      callable_targets = CallTarget.dedup_and_sort callable_targets;
      decorated_targets = CallTarget.dedup_and_sort decorated_targets;
    }


  let join
      {
        global_targets = left_global_targets;
        nonlocal_targets = left_nonlocal_targets;
        callable_targets = left_callable_targets;
        decorated_targets = left_decorated_targets;
      }
      {
        global_targets = right_global_targets;
        nonlocal_targets = right_nonlocal_targets;
        callable_targets = right_callable_targets;
        decorated_targets = right_decorated_targets;
      }
    =
    {
      global_targets = List.rev_append left_global_targets right_global_targets;
      nonlocal_targets = List.rev_append left_nonlocal_targets right_nonlocal_targets;
      callable_targets = List.rev_append left_callable_targets right_callable_targets;
      decorated_targets = List.rev_append left_decorated_targets right_decorated_targets;
    }


  let all_targets
      ~use_case
      { global_targets; nonlocal_targets; callable_targets; decorated_targets }
    =
    (match use_case with
    | AllTargetsUseCase.CallGraphDependency -> decorated_targets
    | AllTargetsUseCase.TaintAnalysisDependency -> []
    | AllTargetsUseCase.Everything ->
        nonlocal_targets
        |> List.rev_append global_targets
        |> List.rev_append callable_targets
        |> List.rev_append decorated_targets)
    |> List.map ~f:CallTarget.target


  let to_json { global_targets; nonlocal_targets; callable_targets; decorated_targets } =
    []
    |> JsonHelper.add_list "globals" global_targets CallTarget.to_json
    |> JsonHelper.add_list "nonlocals" nonlocal_targets CallTarget.to_json
    |> JsonHelper.add_list "callables" callable_targets CallTarget.to_json
    |> JsonHelper.add_list "decorated_targets" decorated_targets CallTarget.to_json
    |> fun bindings -> `Assoc (List.rev bindings)


  let redirect_to_decorated ~decorators ({ callable_targets; _ } as identifier_callees) =
    {
      identifier_callees with
      callable_targets = List.map ~f:(CallTarget.redirect_to_decorated ~decorators) callable_targets;
    }


  let drop_decorated_targets identifier_callees = { identifier_callees with decorated_targets = [] }

  let regenerate_call_indices
      ~indexer
      ({ global_targets; nonlocal_targets; callable_targets = _; _ } as identifier_callees)
    =
    {
      identifier_callees with
      global_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) global_targets;
      nonlocal_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) nonlocal_targets;
    }
end

(** An aggregate of callees for formatting strings. *)
module StringFormatCallees = struct
  type t = {
    (* Implicit callees for any expression that is stringified. *)
    stringify_targets: CallTarget.t list;
    (* Artificial callees for distinguishing f-strings within a function. *)
    f_string_targets: CallTarget.t list;
  }
  [@@deriving eq, show { with_path = false }]

  let deduplicate { stringify_targets; f_string_targets } =
    {
      stringify_targets = CallTarget.dedup_and_sort stringify_targets;
      f_string_targets = CallTarget.dedup_and_sort f_string_targets;
    }


  let join
      { stringify_targets = left_stringify_targets; f_string_targets = left_f_string_targets }
      { stringify_targets = right_stringify_targets; f_string_targets = right_f_string_targets }
    =
    {
      stringify_targets = List.rev_append left_stringify_targets right_stringify_targets;
      f_string_targets = List.rev_append left_f_string_targets right_f_string_targets;
    }


  let all_targets ~use_case { stringify_targets; f_string_targets } =
    (match use_case with
    | AllTargetsUseCase.CallGraphDependency -> []
    | AllTargetsUseCase.TaintAnalysisDependency -> stringify_targets
    | AllTargetsUseCase.Everything -> List.rev_append f_string_targets stringify_targets)
    |> List.map ~f:CallTarget.target


  let from_stringify_targets stringify_targets = { stringify_targets; f_string_targets = [] }

  let from_f_string_targets f_string_targets = { stringify_targets = []; f_string_targets }

  let to_json { stringify_targets; f_string_targets } =
    []
    |> JsonHelper.add_list "stringify" stringify_targets CallTarget.to_json
    |> JsonHelper.add_list "f-string" f_string_targets CallTarget.to_json
    |> fun bindings -> `Assoc bindings


  let redirect_to_decorated ~decorators:_ = Fn.id

  let drop_decorated_targets = Fn.id

  let regenerate_call_indices ~indexer { stringify_targets; f_string_targets } =
    {
      stringify_targets =
        List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) stringify_targets;
      f_string_targets = List.map ~f:(CallTarget.Indexer.regenerate_index ~indexer) f_string_targets;
    }
end

(** An aggregate of all possible callees for an arbitrary expression. *)
module ExpressionCallees = struct
  type t = {
    call: CallCallees.t option;
    attribute_access: AttributeAccessCallees.t option;
    identifier: IdentifierCallees.t option;
    string_format: StringFormatCallees.t option;
  }
  [@@deriving eq, show { with_path = false }]

  let from_call callees =
    { call = Some callees; attribute_access = None; identifier = None; string_format = None }


  let from_attribute_access properties =
    { call = None; attribute_access = Some properties; identifier = None; string_format = None }


  let from_identifier identifier =
    { call = None; attribute_access = None; identifier = Some identifier; string_format = None }


  let from_string_format string_format =
    { call = None; attribute_access = None; identifier = None; string_format = Some string_format }


  let join
      {
        call = left_call;
        attribute_access = left_attribute_access;
        identifier = left_identifier;
        string_format = left_string_format;
      }
      {
        call = right_call;
        attribute_access = right_attribute_access;
        identifier = right_identifier;
        string_format = right_string_format;
      }
    =
    {
      call = Option.merge ~f:CallCallees.join left_call right_call;
      attribute_access =
        Option.merge ~f:AttributeAccessCallees.join left_attribute_access right_attribute_access;
      identifier = Option.merge ~f:IdentifierCallees.join left_identifier right_identifier;
      string_format =
        Option.merge ~f:StringFormatCallees.join left_string_format right_string_format;
    }


  let deduplicate { call; attribute_access; identifier; string_format } =
    {
      call = call >>| CallCallees.deduplicate;
      attribute_access = attribute_access >>| AttributeAccessCallees.deduplicate;
      identifier = identifier >>| IdentifierCallees.deduplicate;
      string_format = string_format >>| StringFormatCallees.deduplicate;
    }


  let all_targets ~use_case { call; attribute_access; identifier; string_format } =
    let call_targets = call >>| CallCallees.all_targets ~use_case |> Option.value ~default:[] in
    let attribute_access_targets =
      attribute_access >>| AttributeAccessCallees.all_targets ~use_case |> Option.value ~default:[]
    in
    let identifier_targets =
      identifier >>| IdentifierCallees.all_targets ~use_case |> Option.value ~default:[]
    in
    let string_format_targets =
      string_format >>| StringFormatCallees.all_targets ~use_case |> Option.value ~default:[]
    in
    call_targets
    |> List.rev_append attribute_access_targets
    |> List.rev_append identifier_targets
    |> List.rev_append string_format_targets


  let is_empty_attribute_access_callees = function
    | {
        call = None;
        attribute_access = Some some_attribute_access;
        identifier = None;
        string_format = None;
      } ->
        AttributeAccessCallees.is_empty some_attribute_access
    | _ -> false


  let equal_ignoring_types
      {
        call = call_left;
        attribute_access = attribute_access_left;
        identifier = identifier_left;
        string_format = string_format_left;
      }
      {
        call = call_right;
        attribute_access = attribute_access_right;
        identifier = identifier_right;
        string_format = string_format_right;
      }
    =
    Option.equal CallCallees.equal_ignoring_types call_left call_right
    && Option.equal
         AttributeAccessCallees.equal_ignoring_types
         attribute_access_left
         attribute_access_right
    && Option.equal IdentifierCallees.equal identifier_left identifier_right
    && Option.equal StringFormatCallees.equal string_format_left string_format_right


  let to_json { call; attribute_access; identifier; string_format } =
    []
    |> JsonHelper.add_optional "call" call CallCallees.to_json
    |> JsonHelper.add_optional "attribute_access" attribute_access AttributeAccessCallees.to_json
    |> JsonHelper.add_optional "identifier" identifier IdentifierCallees.to_json
    |> JsonHelper.add_optional "string_format" string_format StringFormatCallees.to_json
    |> fun bindings -> `Assoc (List.rev bindings)


  let redirect_to_decorated ~decorators { call; attribute_access; identifier; string_format } =
    {
      call = call >>| CallCallees.redirect_to_decorated ~decorators;
      attribute_access =
        attribute_access >>| AttributeAccessCallees.redirect_to_decorated ~decorators;
      identifier = identifier >>| IdentifierCallees.redirect_to_decorated ~decorators;
      string_format = string_format >>| StringFormatCallees.redirect_to_decorated ~decorators;
    }


  let drop_decorated_targets { call; attribute_access; identifier; string_format } =
    {
      call = call >>| CallCallees.drop_decorated_targets;
      attribute_access = attribute_access >>| AttributeAccessCallees.drop_decorated_targets;
      identifier = identifier >>| IdentifierCallees.drop_decorated_targets;
      string_format = string_format >>| StringFormatCallees.drop_decorated_targets;
    }


  let regenerate_call_indices ~indexer { call; attribute_access; identifier; string_format } =
    CallTarget.Indexer.generate_fresh_indices indexer;
    {
      call = call >>| CallCallees.regenerate_call_indices ~indexer;
      attribute_access =
        attribute_access >>| AttributeAccessCallees.regenerate_call_indices ~indexer;
      identifier = identifier >>| IdentifierCallees.regenerate_call_indices ~indexer;
      string_format = string_format >>| StringFormatCallees.regenerate_call_indices ~indexer;
    }
end

(** An aggregate of all possible callees for an arbitrary location.

    Note that multiple expressions might have the same location. *)
module LocationCallees = struct
  module Map = struct
    include SerializableStringMap

    type t = ExpressionCallees.t SerializableStringMap.t

    let custom_equal = equal

    let equal = equal ExpressionCallees.equal

    let singleton ~expression_identifier ~callees = singleton expression_identifier callees

    let add map ~expression_identifier ~callees =
      update
        expression_identifier
        (function
          | Some existing_callees -> Some (ExpressionCallees.join existing_callees callees)
          | None -> Some callees)
        map


    let to_json map =
      let bindings =
        fold (fun key value sofar -> (key, ExpressionCallees.to_json value) :: sofar) map []
      in
      `Assoc bindings
  end

  type t =
    | Singleton of ExpressionCallees.t
    | Compound of Map.t
  [@@deriving eq]

  let pp formatter = function
    | Singleton callees -> Format.fprintf formatter "%a" ExpressionCallees.pp callees
    | Compound map ->
        Map.to_alist map
        |> List.map ~f:(fun (key, value) -> Format.asprintf "%s: %a" key ExpressionCallees.pp value)
        |> String.concat ~sep:", "
        |> Format.fprintf formatter "%s"


  let show callees = Format.asprintf "%a" pp callees

  let equal_ignoring_types location_callees_left location_callees_right =
    match location_callees_left, location_callees_right with
    | Singleton callees_left, Singleton callees_right ->
        ExpressionCallees.equal_ignoring_types callees_left callees_right
    | Compound map_left, Compound map_right ->
        Map.custom_equal ExpressionCallees.equal_ignoring_types map_left map_right
    | _ -> false


  let to_json = function
    | Singleton callees -> `Assoc ["singleton", ExpressionCallees.to_json callees]
    | Compound map -> `Assoc ["compound", Map.to_json map]
end

let log ~debug format =
  if debug then
    Log.dump format
  else
    Log.log ~section:`CallGraph format


let call_identifier { Call.callee; _ } =
  match Node.value callee with
  | Name (Name.Attribute { attribute; _ }) -> attribute
  | Name (Name.Identifier name) -> name
  | _ ->
      (* Fall back to something that hopefully identifies the call well. *)
      Expression.show callee


let expression_identifier = function
  | Expression.Call call -> Some (call_identifier call)
  | Expression.Name (Name.Attribute { attribute; _ }) -> Some attribute
  | _ -> (* not a valid call site. *) None


(** The call graph of a function or method definition. This is for testing purpose only. *)
module DefineCallGraphForTest = struct
  type t = LocationCallees.t Location.Map.Tree.t [@@deriving eq]

  let pp formatter call_graph =
    let pp_pair formatter (key, value) =
      Format.fprintf formatter "@,%a -> %a" Location.pp key LocationCallees.pp value
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    call_graph |> Location.Map.Tree.to_alist |> Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs


  let show = Format.asprintf "%a" pp

  let empty = Location.Map.Tree.empty

  let add call_graph ~location ~callees =
    Location.Map.Tree.set call_graph ~key:location ~data:callees


  let equal_ignoring_types call_graph_left call_graph_right =
    Location.Map.Tree.equal LocationCallees.equal_ignoring_types call_graph_left call_graph_right
end

module MakeSaveCallGraph (CallGraph : sig
  type t

  val name : string

  val is_empty : t -> bool

  val to_json_alist : t -> (string * Yojson.Safe.t) list
end) =
struct
  let filename_and_path ~callables_to_definitions_map ~resolve_module_path callable
      : (string * Yojson.Safe.t) list
    =
    let bindings = ["callable", `String (Target.external_name callable)] in
    let resolve_module_path = Option.value ~default:(fun _ -> None) resolve_module_path in
    callable
    |> Target.CallablesSharedMemory.ReadOnly.get_qualifier callables_to_definitions_map
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
      ~callables_to_definitions_map
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
                  (filename_and_path ~callables_to_definitions_map ~resolve_module_path callable
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
  type t = LocationCallees.Map.t Location.SerializableMap.t [@@deriving eq]

  let to_json call_graph =
    let bindings =
      Location.SerializableMap.fold
        (fun key data sofar -> (Location.show key, LocationCallees.Map.to_json data) :: sofar)
        call_graph
        []
    in
    `Assoc bindings


  let pp formatter call_graph =
    let pp_pair formatter (key, value) =
      Format.fprintf
        formatter
        "@,%a -> %a"
        Location.pp
        key
        (LocationCallees.Map.pp ExpressionCallees.pp)
        value
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    call_graph
    |> Location.SerializableMap.to_alist
    |> Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs


  let show = Format.asprintf "%a" pp

  let empty = Location.SerializableMap.empty

  let is_empty = Location.SerializableMap.is_empty

  include MakeSaveCallGraph (struct
    type nonrec t = t

    let name = "call graphs"

    let is_empty = is_empty

    let to_json_alist call_graph = ["calls", to_json call_graph]
  end)

  let copy = Fn.id

  let for_test map =
    map
    |> Location.SerializableMap.to_alist
    |> List.map ~f:(fun (location, callees) ->
           match SerializableStringMap.to_alist callees with
           | [] -> failwith "unreachable"
           | [(_, callees)] ->
               location, LocationCallees.Singleton (ExpressionCallees.deduplicate callees)
           | _ ->
               ( location,
                 LocationCallees.Compound
                   (SerializableStringMap.map ExpressionCallees.deduplicate callees) ))
    |> Location.Map.Tree.of_alist_exn


  let merge =
    let merge_location_callees_map =
      LocationCallees.Map.merge (fun _ left right ->
          match left, right with
          | Some left, Some right -> Some (ExpressionCallees.join left right)
          | Some left, None -> Some left
          | None, Some right -> Some right
          | None, None -> None)
    in
    Location.SerializableMap.merge (fun _ left right ->
        match left, right with
        | Some left, Some right -> Some (merge_location_callees_map left right)
        | Some left, None -> Some left
        | None, Some right -> Some right
        | None, None -> None)


  let add_callees ~debug ~expression_identifier ~location ~statement_for_logging ~callees =
    let () =
      log
        ~debug
        "Resolved callees at `%a` for expression `%a`:@,%a "
        Location.pp
        location
        Statement.pp
        statement_for_logging
        ExpressionCallees.pp
        callees
    in
    Location.SerializableMap.update location (function
        | None -> Some (LocationCallees.Map.singleton ~expression_identifier ~callees)
        | Some existing_callees ->
            Some (LocationCallees.Map.add existing_callees ~expression_identifier ~callees))


  let set_call_callees ~call ~location ~call_callees =
    let expression_identifier = call_identifier call in
    Location.SerializableMap.update location (function
        | None ->
            Some
              (LocationCallees.Map.singleton
                 ~expression_identifier
                 ~callees:(ExpressionCallees.from_call call_callees))
        | Some callees ->
            Some
              (LocationCallees.Map.update
                 expression_identifier
                 (function
                   | Some callees ->
                       Some { callees with ExpressionCallees.call = Some call_callees }
                   | None -> Some (ExpressionCallees.from_call call_callees))
                 callees))


  let set_identifier_callees ~identifier ~location ~identifier_callees =
    Location.SerializableMap.update location (function
        | None ->
            Some
              (LocationCallees.Map.singleton
                 ~expression_identifier:identifier
                 ~callees:(ExpressionCallees.from_identifier identifier_callees))
        | Some callees ->
            Some
              (LocationCallees.Map.update
                 identifier
                 (function
                   | Some callees ->
                       Some { callees with ExpressionCallees.identifier = Some identifier_callees }
                   | None -> Some (ExpressionCallees.from_identifier identifier_callees))
                 callees))


  let set_attribute_access_callees ~attribute ~location ~attribute_access_callees =
    Location.SerializableMap.update location (function
        | None ->
            Some
              (LocationCallees.Map.singleton
                 ~expression_identifier:attribute
                 ~callees:(ExpressionCallees.from_attribute_access attribute_access_callees))
        | Some callees ->
            Some
              (LocationCallees.Map.update
                 attribute
                 (function
                   | Some callees ->
                       Some
                         {
                           callees with
                           ExpressionCallees.attribute_access = Some attribute_access_callees;
                         }
                   | None -> Some (ExpressionCallees.from_attribute_access attribute_access_callees))
                 callees))


  let filter_empty_attribute_access =
    let exist_non_empty_attribute_access =
      SerializableStringMap.exists (fun _ callees ->
          not (ExpressionCallees.is_empty_attribute_access_callees callees))
    in
    Location.SerializableMap.filter (fun _ -> exist_non_empty_attribute_access)


  let update_expression_callees ~f = Location.SerializableMap.map (LocationCallees.Map.map f)

  let redirect_to_decorated ~decorators =
    update_expression_callees ~f:(ExpressionCallees.redirect_to_decorated ~decorators)


  (* Ensure the taint analysis does not use these targets. *)
  let drop_decorated_targets = update_expression_callees ~f:ExpressionCallees.drop_decorated_targets

  let regenerate_call_indices ~indexer =
    (* The indices are relative to the locations. When location x is earlier than y, calls at
       location x have smaller indices than calls on location y. Hence here we rely on the
       assumption that the map is keyed on the locations. *)
    update_expression_callees ~f:(ExpressionCallees.regenerate_call_indices ~indexer)


  (** Return all callees of the call graph, depending on the use case. *)
  let all_targets ~use_case call_graph =
    call_graph
    |> Location.SerializableMap.data
    |> List.concat_map ~f:(fun map ->
           map
           |> LocationCallees.Map.data
           |> List.concat_map ~f:(ExpressionCallees.all_targets ~use_case))
    |> List.dedup_and_sort ~compare:Target.compare


  let resolve_expression call_graph ~location ~expression_identifier =
    match Location.SerializableMap.find_opt location call_graph with
    | Some callees -> (
        match SerializableStringMap.data callees with
        | [] -> None
        | [callee] -> Some callee
        | _ -> SerializableStringMap.find_opt expression_identifier callees)
    | None -> None


  let resolve_call call_graph ~location ~call =
    expression_identifier (Expression.Call call)
    >>= fun expression_identifier ->
    resolve_expression call_graph ~location ~expression_identifier
    >>= fun { ExpressionCallees.call; _ } -> call


  let resolve_attribute_access call_graph ~location ~attribute =
    resolve_expression call_graph ~location ~expression_identifier:attribute
    >>= fun { ExpressionCallees.attribute_access; _ } -> attribute_access


  let resolve_identifier call_graph ~location ~identifier =
    resolve_expression call_graph ~location ~expression_identifier:identifier
    >>= fun { ExpressionCallees.identifier; _ } -> identifier


  let string_format_expression_identifier = "$__str__$"

  let resolve_string_format call_graph ~location =
    resolve_expression
      call_graph
      ~location
      ~expression_identifier:string_format_expression_identifier
    >>= fun { ExpressionCallees.string_format; _ } -> string_format
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
    let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
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
              >>= PyrePysaEnvironment.ReadOnly.get_class_summary pyre_api
              |> Option.is_some
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
  let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
  let get_class_type = PyrePysaEnvironment.ReadOnly.parse_reference pyre_api in
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
            PyrePysaEnvironment.ReadOnly.less_or_equal
              pyre_api
              ~left:candidate_type
              ~right:receiver_type
          with
          | PyrePysaLogic.UntrackedClass untracked_type ->
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
            get_actual_target
              (Target.Regular.Method { class_name = Reference.show class_name; method_name; kind }
              |> Target.from_regular)
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
  let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
  match callable_type with
  | Type.Callable { kind = Named name; _ } -> (
      let return_type =
        ReturnType.from_callable_with_fallback ~pyre_api ~callable_type ~return_type
      in
      match receiver_type with
      | Some receiver_type ->
          let targets =
            match callee_kind with
            | CalleeKind.Method { is_direct_call = true; _ } -> [Target.create_method name]
            | _ ->
                compute_indirect_targets
                  ~pyre_in_context
                  ~override_graph
                  ~receiver_type
                  (Target.create_method name)
          in
          let targets =
            List.map
              ~f:(fun target ->
                let is_class_method, is_static_method =
                  Target.CallablesSharedMemory.ReadOnly.get_method_kind
                    callables_to_definitions_map
                    target
                in
                CallTarget.create_with_default_index
                  ~implicit_dunder_call:dunder_call
                  ~return_type:(Some return_type)
                  ~receiver_type
                  ~is_class_method
                  ~is_static_method
                  target)
              targets
          in
          CallCallees.create ~call_targets:targets ()
      | None ->
          let target =
            match callee_kind with
            | Method _ -> Target.create_method name
            | _ -> Target.create_function name
          in
          let is_class_method, is_static_method =
            Target.CallablesSharedMemory.ReadOnly.get_method_kind
              callables_to_definitions_map
              target
          in
          CallCallees.create
            ~call_targets:
              [
                CallTarget.create_with_default_index
                  ~explicit_receiver:true
                  ~implicit_dunder_call:dunder_call
                  ~return_type:(Some return_type)
                  ~is_class_method
                  ~is_static_method
                  ?receiver_type
                  target;
              ]
            ())
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
                  ReturnType.from_callable_with_fallback
                    ~pyre_api
                    ~callable_type:resolved_dunder_call
                    ~return_type
                in
                let target =
                  Target.Regular.Method
                    {
                      Target.class_name = primitive_callable_name;
                      method_name = "__call__";
                      kind = Normal;
                    }
                  |> Target.from_regular
                in
                let is_class_method, is_static_method =
                  Target.CallablesSharedMemory.ReadOnly.get_method_kind
                    callables_to_definitions_map
                    target
                in
                CallCallees.create
                  ~call_targets:
                    [
                      CallTarget.create_with_default_index
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
        let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
        ReturnType.from_annotation ~pyre_api class_type
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
  let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
  match implementing_class, callee with
  | Type.Top, Expression.Name name when is_all_names callee ->
      (* If implementing_class is unknown, this must be a function rather than a method. We can use
         global resolution on the callee. *)
      PyrePysaEnvironment.ReadOnly.global pyre_api (Ast.Expression.name_to_reference_exn name)
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


let resolve_stringify_call ~pyre_in_context ~callables_to_definitions_map expression =
  let string_callee =
    Node.create_with_default_location
      (Expression.Name
         (Name.Attribute
            {
              base = expression;
              attribute = "__str__";
              origin =
                Some { Node.location = Node.location expression; value = Origin.ForTypeChecking };
            }))
  in
  try
    match
      CallResolution.resolve_ignoring_errors
        ~pyre_in_context
        ~callables_to_definitions_map
        string_callee
      |> Type.callable_name
    with
    | Some name when Reference.equal name (Reference.create "object.__str__") ->
        (* Call resolved to object.__str__, fallback to calling __repr__ if it exists. *)
        "__repr__"
    | _ -> "__str__"
  with
  | PyrePysaLogic.UntrackedClass _ -> "__str__"


(* Rewrite certain calls for the interprocedural analysis (e.g, pysa).
 * This may or may not be sound depending on the analysis performed. *)
let transform_special_calls
    ~pyre_in_context
    ~callables_to_definitions_map
    ~location:call_location
    ({ Call.callee = { Node.location = callee_location; _ } as callee; arguments; origin = _ } as
    original_call)
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
      let origin = Some { Node.location = call_location; value = Origin.StrCall } in
      let callee =
        attribute_access
          ~base:value
          ~method_name:(resolve_stringify_call ~pyre_in_context ~callables_to_definitions_map value)
          ~origin
      in
      Some { Call.callee; arguments = []; origin }
  | Name (Name.Identifier "iter"), [{ Call.Argument.value; _ }] ->
      (* Only handle `iter` with a single argument here. *)
      let origin = Some { Node.location = call_location; value = Origin.IterCall } in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__iter__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "next"), [{ Call.Argument.value; _ }] ->
      (* Only handle `next` with a single argument here. *)
      let origin = Some { Node.location = call_location; value = Origin.NextCall } in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__next__" ~origin;
          arguments = [];
          origin;
        }
  | Name (Name.Identifier "anext"), [{ Call.Argument.value; _ }] ->
      (* Only handle `anext` with a single argument here. *)
      let origin = Some { Node.location = call_location; value = Origin.NextCall } in
      Some
        {
          Call.callee = attribute_access ~base:value ~method_name:"__anext__" ~origin;
          arguments = [];
          origin;
        }
  | ( Expression.Name
        (Name.Attribute
          {
            base = { Node.value = Expression.Name (Name.Identifier "functools"); _ };
            attribute = "partial";
            _;
          }),
      { Call.Argument.value = actual_callable; _ } :: actual_arguments ) ->
      let origin =
        Some { Node.location = call_location; value = Origin.PysaCallRedirect "functools.partial" }
      in
      Some { Call.callee = actual_callable; arguments = actual_arguments; origin }
  | ( Expression.Name
        (Name.Attribute
          {
            base = { Node.value = Expression.Name (Name.Identifier "multiprocessing"); _ };
            attribute = "Process";
            _;
          }),
      [
        { Call.Argument.value = process_callee; name = Some { Node.value = "$parameter$target"; _ } };
        {
          Call.Argument.value = { Node.value = Expression.Tuple process_arguments; _ };
          name = Some { Node.value = "$parameter$args"; _ };
        };
      ] ) ->
      let origin =
        Some
          {
            Node.location = call_location;
            value = Origin.PysaCallRedirect "multiprocessing.Process";
          }
      in
      Some
        {
          Call.callee = process_callee;
          arguments =
            List.map process_arguments ~f:(fun value -> { Call.Argument.value; name = None });
          origin;
        }
  | _ -> SpecialCallResolution.redirect ~pyre_in_context ~location:call_location original_call


let redirect_special_calls ~pyre_in_context ~callables_to_definitions_map ~location call =
  match transform_special_calls ~pyre_in_context ~callables_to_definitions_map ~location call with
  | Some call -> call
  | None ->
      (* Rewrite certain calls using the same logic used in the type checker.
       * This should be sound for most analyses. *)
      PyrePysaEnvironment.InContext.redirect_special_calls pyre_in_context ~location call


let redirect_expressions ~pyre_in_context ~callables_to_definitions_map ~location = function
  | Expression.Subscript { Subscript.base; index } ->
      let origin = Some { Node.location; value = Origin.SubscriptGetItem } in
      Expression.Call
        {
          callee =
            {
              Node.value =
                Expression.Name (Name.Attribute { base; attribute = "__getitem__"; origin });
              location = Node.location base;
            };
          arguments = [{ Call.Argument.value = index; name = None }];
          origin;
        }
  | Expression.Call call ->
      let call =
        redirect_special_calls ~pyre_in_context ~callables_to_definitions_map ~location call
      in
      Expression.Call call
  | Expression.Slice slice -> Slice.lowered ~location slice |> Node.value
  | expression -> expression


let redirect_assignments = function
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
      {
        Node.location;
        value = Statement.Assign { Assign.target; annotation = None; value = Some call };
      }
  | {
      Node.value =
        Statement.Assign
          {
            Assign.target = { Node.value = Expression.Subscript { base; index }; _ };
            value = Some value_expression;
            _;
          };
      location;
    } ->
      (* TODO(T187636576): For now, we translate assignments such as `d[a] = b` into
         `d.__setitem__(a, b)`. Unfortunately, this won't work for multi-target assignments such as
         `x, y[a], z = w`. In the future, we should implement proper logic to handle those. *)
      let index_argument = { Call.Argument.value = index; name = None } in
      let value_argument = { Call.Argument.value = value_expression; name = None } in
      let origin = Some { Node.location; value = Origin.SubscriptSetItem } in
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
                        value = Name (Name.Attribute { base; attribute = "__setitem__"; origin });
                        location;
                      };
                    arguments = [index_argument; value_argument];
                    origin;
                  };
            };
      }
  | statement -> statement


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
  | Expression.Name name, _
    when is_all_names (Node.value callee)
         && Set.mem SpecialCallResolution.recognized_callable_target_types callee_type ->
      Ast.Expression.name_to_reference name
      >>| Reference.show
      >>| fun name ->
      let return_type =
        let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
        ReturnType.from_annotation ~pyre_api (Lazy.force return_type)
      in
      CallCallees.create
        ~call_targets:
          [
            CallTarget.create_with_default_index
              ~implicit_dunder_call:false
              ~return_type:(Some return_type)
              (Target.Regular.Function { name; kind = Normal } |> Target.from_regular);
          ]
        ()
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
  let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
  let return_type () = ReturnType.from_annotation ~pyre_api (Lazy.force return_type) in
  let contain_class_method signatures =
    signatures
    |> List.exists ~f:(fun signature ->
           List.exists Target.class_method_decorators ~f:(Define.Signature.has_decorator signature))
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
        match PyrePysaEnvironment.ReadOnly.resolve_exports pyre_api name with
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
                CallTarget.create_with_default_index
                  ~implicit_dunder_call:false
                  ~return_type:(Some (return_type ()))
                  (Target.Regular.Function { name = Reference.show name; kind = Normal }
                  |> Target.from_regular);
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
            let class_name = Reference.create ~prefix:from name |> Reference.show in
            PyrePysaEnvironment.ReadOnly.get_class_summary pyre_api class_name
            >>| Node.value
            >>| PyrePysaLogic.ClassSummary.attributes
            >>= Identifier.SerializableMap.find_opt attribute
            >>| Node.value
            |> function
            | Some { kind = Method { static; signatures; _ }; _ } ->
                let is_class_method = contain_class_method signatures in
                Result.Ok
                  [
                    CallTarget.create_with_default_index
                      ~implicit_dunder_call:false
                      ~return_type:(Some (return_type ()))
                      ~is_class_method
                      ~is_static_method:static
                      (Target.Regular.Method
                         { Target.class_name; method_name = attribute; kind = Normal }
                      |> Target.from_regular);
                  ]
            | Some attribute ->
                let () =
                  log
                    "Bypassing decorators - Non-method attribute `%s` for callee `%s`"
                    (PyrePysaLogic.ClassSummary.Attribute.show_attribute attribute)
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
                PyrePysaEnvironment.ReadOnly.get_class_summary pyre_api element
                >>| Node.value
                >>| PyrePysaLogic.ClassSummary.attributes
                >>= Identifier.SerializableMap.find_opt attribute
                >>| Node.value
              with
              | Some
                  {
                    PyrePysaLogic.ClassSummary.Attribute.kind = Method { static; signatures; _ };
                    _;
                  } ->
                  Some (element, contain_class_method signatures, static)
              | _ -> None
            in
            let parent_classes_in_mro =
              PyrePysaEnvironment.ReadOnly.successors pyre_api class_name
            in
            match List.find_map (class_name :: parent_classes_in_mro) ~f:find_attribute with
            | Some (base_class, is_class_method, is_static_method) ->
                let receiver_type =
                  (* Discard the type parameters, assuming they do not affect finding the actual
                     callee. *)
                  Type.Parametric
                    { name = "type"; arguments = [Single (Type.Primitive class_name)] }
                in
                let targets =
                  Target.Regular.Method
                    { Target.class_name = base_class; method_name = attribute; kind = Normal }
                  |> Target.from_regular
                  (* Over-approximately consider that any overriding method might be called. We
                     prioritize reducing false negatives than reducing false positives. *)
                  |> compute_indirect_targets ~pyre_in_context ~override_graph ~receiver_type
                  |> List.map
                       ~f:
                         (CallTarget.create_with_default_index
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
        let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
        PyrePysaLogic.type_of_attribute property |> ReturnType.from_annotation ~pyre_api
    in
    let parent = PyrePysaLogic.AnnotatedAttribute.parent property |> Reference.create in
    let property_targets =
      let kind = if setter then Target.PropertySetter else Target.Normal in
      if Type.is_class_type base_type_info then
        [Target.create_method ~kind (Reference.create ~prefix:parent attribute)]
      else
        let callee = Target.create_method ~kind (Reference.create ~prefix:parent attribute) in
        compute_indirect_targets
          ~pyre_in_context
          ~override_graph
          ~receiver_type:base_type_info
          callee
    in
    List.map
      ~f:
        (CallTarget.create_with_default_index
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


let as_identifier_reference ~define ~pyre_in_context expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) ->
      let reference = Reference.create identifier in
      if PyrePysaEnvironment.InContext.is_global pyre_in_context ~reference then
        Some
          (IdentifierCallees.Reference.Global
             { reference = Reference.delocalize reference; export_name = None })
      else
        define
        >>= fun define ->
        if CallResolution.is_nonlocal ~pyre_in_context ~define reference then
          Some (IdentifierCallees.Reference.Nonlocal (Reference.delocalize reference))
        else
          None
  | Name name -> (
      name_to_reference name
      >>= fun reference ->
      PyrePysaEnvironment.ReadOnly.resolve_exports
        (PyrePysaEnvironment.InContext.pyre_api pyre_in_context)
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
            (IdentifierCallees.Reference.Global
               {
                 reference = Reference.combine from (Reference.create name);
                 export_name = Some export_name;
               })
      | _ -> None)
  | _ -> None


let is_builtin_reference = function
  | IdentifierCallees.Reference.Global { reference; _ } ->
      reference |> Reference.single |> Option.is_some
  | Nonlocal _ -> false


let resolve_attribute_access_global_targets
    ~define
    ~pyre_in_context
    ~base_type_info
    ~base
    ~attribute
    ~origin
  =
  let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
  let expression =
    Expression.Name (Name.Attribute { Name.Attribute.base; attribute; origin })
    |> Node.create_with_default_location
  in
  match as_identifier_reference ~define ~pyre_in_context expression with
  | Some (IdentifierCallees.Reference.Global { reference; _ }) -> [reference]
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
                >>= PyrePysaEnvironment.ReadOnly.attribute_from_class_name
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
              let successors = PyrePysaEnvironment.ReadOnly.successors pyre_api class_name in
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
         origin = Some { Node.location = Location.any; value = Origin.ForTypeChecking };
       }
    |> Node.create_with_default_location
    |> CallResolution.resolve_ignoring_untracked ~pyre_in_context)


let resolve_callable_targets_from_global_identifiers ~define ~pyre_in_context expression =
  match as_identifier_reference ~define ~pyre_in_context expression with
  | Some
      (IdentifierCallees.Reference.Global
        { reference; export_name = Some (PyrePysaLogic.ModuleExport.Name.Define _) }) ->
      let target = Target.create_function reference in
      let pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context in
      let return_type =
        return_type_for_call ~pyre_in_context ~callee:expression
        |> Lazy.force
        |> ReturnType.from_annotation ~pyre_api
      in
      [
        CallTarget.create_with_default_index
          ~implicit_dunder_call:false
          ~return_type:(Some return_type)
          target;
      ]
  | _ -> []


let resolve_identifier ~define ~pyre_in_context ~identifier =
  let expression =
    Expression.Name (Name.Identifier identifier) |> Node.create_with_default_location
  in
  let global_targets, nonlocal_targets =
    expression
    |> as_identifier_reference ~define ~pyre_in_context
    |> Option.filter ~f:(Fn.non is_builtin_reference)
    >>| (function
          | IdentifierCallees.Reference.Global
              { reference; export_name = Some PyrePysaLogic.ModuleExport.Name.GlobalVariable }
          | IdentifierCallees.Reference.Global { reference; export_name = None } ->
              ( [
                  CallTarget.create_with_default_index
                    ~implicit_dunder_call:false
                    ~return_type:None
                    (Target.create_object reference);
                ],
                [] )
          | IdentifierCallees.Reference.Global _ -> [], []
          | Nonlocal nonlocal ->
              ( [],
                [
                  CallTarget.create_with_default_index
                    ~implicit_dunder_call:false
                    ~return_type:None
                    (Target.create_object nonlocal);
                ] ))
    |> Option.value ~default:([], [])
  in
  let callable_targets =
    resolve_callable_targets_from_global_identifiers ~define ~pyre_in_context expression
  in
  match global_targets, nonlocal_targets, callable_targets with
  | [], [], [] -> None
  | _ ->
      (* Exist at least a non-empty list. *)
      Some
        {
          IdentifierCallees.global_targets;
          nonlocal_targets;
          callable_targets;
          decorated_targets = [];
        }


let resolve_callees
    ~debug
    ~pyre_in_context
    ~callables_to_definitions_map
    ~override_graph
    ~call:({ Call.callee; arguments; origin = _ } as call)
  =
  log
    ~debug
    "Resolving function call `%a`"
    Expression.pp
    (Expression.Call call |> Node.create_with_default_location);
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
                Target.CallablesSharedMemory.ReadOnly.get_signature
                  callables_to_definitions_map
                  callee.target
                >>| (fun { Target.CallablesSharedMemory.Signature.parameters; is_stub; _ } ->
                      is_stub
                      || not (List.exists parameters ~f:(parameter_has_annotation callable_class)))
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
    List.filter_mapi arguments ~f:get_higher_order_function_targets
    |> HigherOrderParameterMap.from_list
  in
  { regular_callees with higher_order_parameters }


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
      ~define:define_name
      ~pyre_in_context
      ~base_type_info
      ~base
      ~attribute
      ~origin
    |> List.map ~f:Target.create_object
    (* Use a hashset here for faster lookups. *)
    |> List.filter ~f:(Hash_set.mem attribute_targets)
    |> List.map
         ~f:(CallTarget.create_with_default_index ~implicit_dunder_call:false ~return_type:None)
  in

  let callable_targets =
    Expression.Name (Name.Attribute { Name.Attribute.base; attribute; origin })
    |> Node.create_with_default_location
    |> resolve_callable_targets_from_global_identifiers ~define:define_name ~pyre_in_context
  in

  {
    AttributeAccessCallees.property_targets;
    global_targets;
    is_attribute;
    callable_targets;
    decorated_targets = [];
  }


module AssignmentTarget = struct
  type t = { location: Location.t }
end

module MissingFlowTypeAnalysis = struct
  type t = { qualifier: Reference.t }

  (* For the missing flow analysis (`--find-missing-flows=type`), we turn unresolved
   * calls into sinks, so that we may find sources flowing into those calls. *)
  let add_unknown_callee
      ~missing_flow_type_analysis
      ~expression:{ Node.value; location }
      ({ CallCallees.unresolved; call_targets; _ } as callees)
    =
    match missing_flow_type_analysis with
    | Some { qualifier } when Unresolved.is_unresolved unresolved ->
        (* TODO(T117715045): Move the target creation in `taint/missingFlow.ml`. *)
        let callee =
          match value with
          | Expression.Call { callee = { Node.value = callee; _ }; _ } -> callee
          | _ -> value
        in
        let target =
          Format.asprintf
            "unknown-callee:%a:%a:%a"
            Reference.pp
            qualifier
            Location.pp
            location
            Expression.pp
            (callee
            |> Node.create_with_default_location
            |> Ast.Expression.delocalize ~create_origin:(fun _ -> None))
        in
        let call_target =
          {
            CallTarget.target = Target.Regular.Object target |> Target.from_regular;
            implicit_receiver = false;
            implicit_dunder_call = false;
            index = 0;
            return_type = Some ReturnType.any;
            receiver_class = None;
            is_class_method = false;
            is_static_method = false;
          }
        in
        { callees with call_targets = call_target :: call_targets }
    | _ -> callees
end

module NodeVisitorContext = struct
  type t = {
    pyre_api: PyrePysaEnvironment.ReadOnly.t;
    define_name: Reference.t option;
    debug: bool;
    override_graph: OverrideGraph.SharedMemory.ReadOnly.t option;
    missing_flow_type_analysis: MissingFlowTypeAnalysis.t option;
    attribute_targets: Target.HashSet.t;
    callables_to_definitions_map: Target.CallablesSharedMemory.ReadOnly.t;
  }
end

module CalleeVisitor = struct
  module NodeVisitor = struct
    type t = {
      pyre_in_context: PyrePysaEnvironment.InContext.t;
      assignment_target: AssignmentTarget.t option;
      context: NodeVisitorContext.t;
      callees_at_location: DefineCallGraph.t ref; (* This can be mutated. *)
    }

    let expression_visitor
        ({
           pyre_in_context;
           assignment_target;
           context =
             {
               NodeVisitorContext.debug;
               override_graph;
               missing_flow_type_analysis;
               define_name;
               attribute_targets;
               callables_to_definitions_map;
               _;
             };
           callees_at_location;
         } as state)
        ({ Node.value; location } as expression)
      =
      let resolve_callees =
        resolve_callees ~debug ~pyre_in_context ~callables_to_definitions_map ~override_graph
      in
      let register_targets ~expression_identifier ?(location = location) callees =
        callees_at_location :=
          DefineCallGraph.add_callees
            ~debug
            ~expression_identifier
            ~location
            ~statement_for_logging:(Statement.Expression expression |> Node.create ~location)
            ~callees
            !callees_at_location
      in
      let value =
        redirect_expressions ~pyre_in_context ~callables_to_definitions_map ~location value
      in
      let () =
        match value with
        | Expression.Call call ->
            resolve_callees ~call
            |> MissingFlowTypeAnalysis.add_unknown_callee ~missing_flow_type_analysis ~expression
            |> ExpressionCallees.from_call
            |> register_targets ~expression_identifier:(call_identifier call)
        | Expression.Name (Name.Attribute { Name.Attribute.base; attribute; origin }) ->
            let setter =
              match assignment_target with
              | Some { AssignmentTarget.location = assignment_target_location } ->
                  Location.equal assignment_target_location location
              | None -> false
            in
            resolve_attribute_access
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
            |> ExpressionCallees.from_attribute_access
            |> register_targets ~expression_identifier:attribute
        | Expression.Name (Name.Identifier identifier) ->
            resolve_identifier ~define:define_name ~pyre_in_context ~identifier
            >>| ExpressionCallees.from_identifier
            >>| register_targets ~expression_identifier:identifier
            |> ignore
        | Expression.BinaryOperator ({ left; _ } as operator) ->
            let implicit_call =
              BinaryOperator.lower_to_call ~location ~callee_location:left.Node.location operator
            in
            resolve_callees ~call:implicit_call
            |> MissingFlowTypeAnalysis.add_unknown_callee ~missing_flow_type_analysis ~expression
            |> ExpressionCallees.from_call
            |> register_targets ~expression_identifier:(call_identifier implicit_call)
        | Expression.ComparisonOperator ({ left; _ } as comparison) -> (
            match
              ComparisonOperator.lower_to_expression
                ~location
                ~callee_location:left.location
                comparison
            with
            | Some { Node.value = Expression.Call call; _ } ->
                let call =
                  redirect_special_calls
                    ~pyre_in_context
                    ~callables_to_definitions_map
                    ~location
                    call
                in
                resolve_callees ~call
                |> MissingFlowTypeAnalysis.add_unknown_callee
                     ~missing_flow_type_analysis
                     ~expression
                |> ExpressionCallees.from_call
                |> register_targets ~expression_identifier:(call_identifier call)
            | _ -> ())
        | Expression.FormatString substrings ->
            let artificial_target =
              CallTarget.create_with_default_index
                ~implicit_dunder_call:false
                ~return_type:None
                Target.ArtificialTargets.format_string
            in
            let callees =
              ExpressionCallees.from_string_format
                (StringFormatCallees.from_f_string_targets [artificial_target])
            in
            (* Use indexed artificial targets to distinguish format strings at different
               locations. *)
            register_targets
              ~expression_identifier:DefineCallGraph.string_format_expression_identifier
              ~location
              callees;
            List.iter substrings ~f:(function
                | Substring.Literal _ -> ()
                | Substring.Format { value; format_spec } -> (
                    let register_call_targets
                        ({ Node.location = expression_location; _ } as expression)
                      =
                      let { CallCallees.call_targets; _ } =
                        let callee =
                          let method_name =
                            resolve_stringify_call
                              ~pyre_in_context
                              ~callables_to_definitions_map
                              expression
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
                                         {
                                           Node.location = expression_location;
                                           value = Origin.FormatStringImplicitStr;
                                         };
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
                        let callees =
                          ExpressionCallees.from_string_format
                            (StringFormatCallees.from_stringify_targets call_targets)
                        in
                        register_targets
                          ~expression_identifier:DefineCallGraph.string_format_expression_identifier
                          ~location:expression_location
                          callees
                    in
                    register_call_targets value;
                    match format_spec with
                    | Some format_spec -> register_call_targets format_spec
                    | None -> ()))
        | _ -> ()
      in
      (* Special-case `getattr()` and `setattr()` for the taint analysis. *)
      let () =
        match value with
        | Expression.Call
            {
              callee = { Node.value = Name (Name.Identifier "getattr"); _ };
              arguments =
                [
                  { Call.Argument.value = base; _ };
                  {
                    Call.Argument.value =
                      {
                        Node.value =
                          Expression.Constant
                            (Constant.String { StringLiteral.value = attribute; _ });
                        _;
                      };
                    _;
                  };
                  { Call.Argument.value = _; _ };
                ];
              origin = _;
            } ->
            resolve_attribute_access
              ~pyre_in_context
              ~debug
              ~callables_to_definitions_map
              ~override_graph
              ~define_name
              ~attribute_targets
              ~base
              ~attribute
              ~origin:(Some { Node.location; value = Origin.GetAttrConstantLiteral })
              ~setter:false
            |> ExpressionCallees.from_attribute_access
            |> register_targets ~expression_identifier:attribute
        | Expression.Call
            {
              callee =
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
              origin = _;
            } ->
            resolve_attribute_access
              ~pyre_in_context
              ~debug
              ~callables_to_definitions_map
              ~override_graph
              ~define_name
              ~attribute_targets
              ~base:self
              ~attribute
              ~origin:(Some { Node.location; value = Origin.SetAttrConstantLiteral })
              ~setter:true
            |> ExpressionCallees.from_attribute_access
            |> register_targets ~expression_identifier:attribute
        | _ -> ()
      in
      state


    let statement_visitor state _ = state

    let generator_visitor ({ pyre_in_context; _ } as state) generator =
      (* Since generators create variables that Pyre sees as scoped within the generator, handle
         them by adding the generator's bindings to the resolution. *)
      let ({ Ast.Statement.Assign.target = _; value; _ } as assignment) =
        Ast.Statement.Statement.generator_assignment generator
      in
      (* Since the analysis views the generator as an assignment, we need to also register (extra)
         calls that (are generated above and) appear within the right-hand-side of the assignment*)
      let iter, iter_next, location =
        match value with
        | Some
            {
              value =
                Expression.Await
                  {
                    Node.value =
                      Expression.Call
                        {
                          callee =
                            {
                              Node.value =
                                Name
                                  (Name.Attribute
                                    {
                                      base =
                                        {
                                          Node.value =
                                            Expression.Call
                                              {
                                                callee =
                                                  {
                                                    Node.value =
                                                      Name
                                                        (Name.Attribute
                                                          { attribute = "__aiter__"; _ });
                                                    _;
                                                  };
                                                _;
                                              } as aiter;
                                          _;
                                        };
                                      attribute = "__anext__";
                                      _;
                                    });
                              _;
                            };
                          _;
                        } as aiter_anext;
                    _;
                  };
              location;
            } ->
            (* E.g., x async for x in y *) aiter, aiter_anext, location
        | Some
            {
              value =
                Expression.Call
                  {
                    callee =
                      {
                        Node.value =
                          Name
                            (Name.Attribute
                              {
                                base =
                                  {
                                    Node.value =
                                      Expression.Call
                                        {
                                          callee =
                                            {
                                              Node.value =
                                                Name (Name.Attribute { attribute = "__iter__"; _ });
                                              _;
                                            };
                                          _;
                                        } as iter;
                                    _;
                                  };
                                attribute = "__next__";
                                _;
                              });
                        _;
                      };
                    _;
                  } as iter_next;
              location;
            } ->
            (* E.g., x for x in y *) iter, iter_next, location
        | _ -> failwith "Expect generators to be treated as e.__iter__().__next__()"
      in
      let state = expression_visitor state { Node.value = iter; location } in
      let state = expression_visitor state { Node.value = iter_next; location } in
      {
        state with
        pyre_in_context =
          PyrePysaEnvironment.InContext.resolve_assignment pyre_in_context assignment;
      }


    let node state = function
      | Visit.Expression expression -> expression_visitor state expression
      | Visit.Statement statement -> statement_visitor state statement
      | Visit.Generator generator -> generator_visitor state generator
      | _ -> state


    let visit_statement_children _ statement =
      match Node.value statement with
      | Statement.Assign _
      | Statement.Define _
      | Statement.Class _ ->
          false
      | _ -> true


    let visit_expression_children _ _ = true

    let visit_format_string_children _ _ = true

    let visit_expression_based_on_parent ~parent_expression expression =
      (* Only skip visiting the callee. *)
      match parent_expression.Node.value, expression.Node.value with
      | Expression.Call { callee; _ }, Expression.Name (Name.Identifier _)
      | Expression.Call { callee; _ }, Expression.Name (Name.Attribute _) ->
          not (Expression.equal callee expression)
      | _ -> true
  end

  module T = Visit.MakeNodeVisitor (NodeVisitor)

  (* Visit the given expression and register the resolved callees into `callees_at_location`. This
     function has side effects due to updating `callees_at_location`. *)
  let visit_expression ~pyre_in_context ~assignment_target ~context ~callees_at_location =
    T.visit_expression
      ~parent_expression:None
      ~state:(ref { NodeVisitor.pyre_in_context; assignment_target; context; callees_at_location })


  (* Visit the given statement and register the resolved callees into `callees_at_location`. This
     function has side effects due to updating `callees_at_location`. *)
  let visit_statement ~pyre_in_context ~assignment_target ~context ~callees_at_location =
    T.visit_statement
      ~state:(ref { NodeVisitor.pyre_in_context; assignment_target; context; callees_at_location })
end

(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraphFixpoint (Context : sig
  val node_visitor_context : NodeVisitorContext.t

  val callees_at_location : DefineCallGraph.t ref (* This can be mutated. *)

  val define : Ast.Statement.Define.t
end) =
struct
  include PyrePysaLogic.Fixpoint.Make (struct
    type t = unit [@@deriving show]

    let bottom = ()

    let less_or_equal ~left:_ ~right:_ = true

    let join _ _ = ()

    let widen ~previous:_ ~next:_ ~iteration:_ = ()

    let forward_statement ~pyre_in_context ~statement =
      log
        ~debug:Context.node_visitor_context.debug
        "Building call graph of statement: `%a`"
        Ast.Statement.pp
        statement;
      let statement = redirect_assignments statement in
      match Node.value statement with
      | Statement.Assign { Assign.target; value = Some value; _ } ->
          CalleeVisitor.visit_expression
            ~pyre_in_context
            ~assignment_target:None
            ~context:Context.node_visitor_context
            ~callees_at_location:Context.callees_at_location
            value;
          CalleeVisitor.visit_expression
            ~pyre_in_context
            ~assignment_target:(Some { location = Node.location target })
            ~context:Context.node_visitor_context
            ~callees_at_location:Context.callees_at_location
            target
      | Statement.Assign { Assign.target; value = None; _ } ->
          CalleeVisitor.visit_expression
            ~pyre_in_context
            ~assignment_target:(Some { location = Node.location target })
            ~context:Context.node_visitor_context
            ~callees_at_location:Context.callees_at_location
            target
      | Statement.AugmentedAssign _ ->
          failwith "statement should be lowered using redirect_assignments"
      (* Control flow statements should NOT be visited, since they are lowered down during the
         control flow graph building. *)
      | Statement.If _
      | Statement.Class _
      | Statement.Define _
      | Statement.For _
      | Statement.Match _
      | Statement.While _
      | Statement.With _
      | Statement.Try _ ->
          ()
      | _ ->
          CalleeVisitor.visit_statement
            ~pyre_in_context
            ~assignment_target:None
            ~context:Context.node_visitor_context
            ~callees_at_location:Context.callees_at_location
            statement


    let forward ~statement_key _ ~statement =
      let pyre_in_context =
        PyrePysaEnvironment.InContext.create_at_statement_key
          Context.node_visitor_context.pyre_api
          ~define_name:(Option.value_exn Context.node_visitor_context.define_name)
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
  [@@deriving eq, show]

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
               Target.CallablesSharedMemory.ReadOnly.get_method_kind
                 callables_to_definitions_map
                 target
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

  module MakeFixpoint (Context : sig
    (* Inputs. *)
    val qualifier : Reference.t

    val pyre_api : PyrePysaEnvironment.ReadOnly.t

    val get_callee_model : Target.t -> t option

    val debug : bool

    val define : Ast.Statement.Define.t

    val define_name : Reference.t

    val callable : Target.t option

    val callables_to_definitions_map : Target.CallablesSharedMemory.ReadOnly.t

    val skip_analysis_targets : Target.HashSet.t

    val profiler : CallGraphProfiler.t

    val maximum_target_depth : int

    val maximum_parameterized_targets_at_call_site : int option

    val maximum_parameterized_targets_when_analyzing_define : int option

    val input_define_call_graph : DefineCallGraph.t

    (* Outputs. *)
    val output_define_call_graph : DefineCallGraph.t ref
  end) =
  struct
    let self_variable =
      if Ast.Statement.Define.is_method Context.define then
        let { Ast.Statement.Define.signature = { parameters; _ }; _ } = Context.define in
        match TaintAccessPath.normalize_parameters parameters with
        | { root = TaintAccessPath.Root.PositionalParameter { position = 0; _ }; qualified_name; _ }
          :: _ ->
            Some (TaintAccessPath.Root.Variable qualified_name)
        | _ -> None
      else
        None


    let is_decorated_target =
      Context.callable >>| Target.is_decorated |> Option.value ~default:false


    let get_returned_callables state =
      self_variable
      >>| (fun self_variable ->
            (* For `__init__`, any functions stored in `self` would be returned, in order to
               propagate them. *)
            if Reference.is_suffix ~suffix:(Reference.create "__init__") Context.define_name then
              State.get self_variable state
            else
              CallTarget.Set.bottom)
      |> Option.value ~default:CallTarget.Set.bottom
      |> CallTarget.Set.join (State.get TaintAccessPath.Root.LocalResult state)


    let log format =
      if Context.debug then
        Log.dump format
      else
        Log.log ~section:`CallGraph format


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


    module Fixpoint = Analysis.Fixpoint.Make (struct
      type t = State.t [@@deriving show]

      let bottom = State.bottom

      let less_or_equal = State.less_or_equal

      let join = State.join

      let widen ~previous ~next ~iteration = State.widen ~prev:previous ~next ~iteration

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


      (* Results of analyzing a certain kind of call targets (e.g., `call_targets` or
         `init_targets`) on a callee expression. *)
      module AnalyzeCalleeResult = struct
        type t = {
          (* Transforming the input call targets by providing parameter targets. *)
          parameterized_targets: CallTarget.t list;
          (* The sublist of the input call target list that are of `kind=Decorated`. *)
          decorated_targets: CallTarget.t list;
          (* The sublist of the input call target list that are not transformed above. We create
             these regular targets when (1) no parameter targets exist or (2) we cannot find
             function bodies of the callee, so that the taint analysis can still use
             `higher_order_parameters`. *)
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
              Target.CallablesSharedMemory.ReadOnly.get_signature
                Context.callables_to_definitions_map
                target
            with
            | None ->
                log "Cannot find define for callable `%a`" Target.pp_pretty_with_kind target;
                None
            | Some { Target.CallablesSharedMemory.Signature.is_stub; parameters; _ } ->
                if is_stub then
                  let () = log "Callable `%a` is a stub" Target.pp_pretty_with_kind target in
                  None
                else
                  Some
                    (parameters
                    |> TaintAccessPath.normalize_parameters
                    |> List.map ~f:(fun { TaintAccessPath.NormalizedParameter.root; _ } -> root))
        in
        let create_parameter_target_excluding_args_kwargs (parameter_target, (_, argument_matches)) =
          match argument_matches, parameter_target with
          | { TaintAccessPath.root = TaintAccessPath.Root.StarParameter _; _ } :: _, _
          | { TaintAccessPath.root = TaintAccessPath.Root.StarStarParameter _; _ } :: _, _ ->
              (* TODO(T215864108): Since we do not distinguish paths under the same `Root`, we may
                 run into conflicts in `of_alist_exn` below, which is avoided by excluding those
                 cases, such as kwargs and args. *)
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
                       (* The formal argument should shadow variables from the closure that share
                          the same name. *)
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
                Target.CallablesSharedMemory.ReadOnly.is_stub
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
        let analyze_arguments
            ~higher_order_parameters
            index
            state_so_far
            { Call.Argument.value = argument; _ }
          =
          let callees, new_state =
            analyze_expression ~pyre_in_context ~state:state_so_far ~expression:argument
          in
          let call_targets_from_higher_order_parameters =
            match HigherOrderParameterMap.Map.find_opt index higher_order_parameters with
            | Some { HigherOrderParameter.call_targets; _ } -> call_targets
            | None -> []
          in
          let callees =
            call_targets_from_higher_order_parameters
            |> CallTarget.Set.of_list
            |> CallTarget.Set.join callees
          in
          log
            "Finished analyzing argument `%a`: %a"
            Expression.pp
            argument
            CallTarget.Set.pp
            callees;
          new_state, callees
        in
        let ({
               CallCallees.call_targets = original_call_targets;
               higher_order_parameters;
               unresolved;
               init_targets = original_init_targets;
               _;
             } as original_call_callees)
          =
          track_apply_call_step ResolveCall (fun () ->
              match
                DefineCallGraph.resolve_call ~location ~call Context.input_define_call_graph
              with
              | Some callees -> callees
              | None ->
                  failwith
                    (Format.asprintf
                       "Could not find callees for `%a` in `%a` at `%a` in the call graph: `%a`"
                       Ast.Expression.Call.pp
                       call
                       Reference.pp
                       Context.qualifier
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
        let state, argument_callees =
          track_apply_call_step AnalyzeArguments (fun () ->
              List.fold_mapi arguments ~f:(analyze_arguments ~higher_order_parameters) ~init:state)
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
                 ~argument_callees
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
              ~argument_callees
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
            ~argument_callees
            ~track_apply_call_step_name:"init_targets"
            original_init_targets
        in
        (* Discard higher order parameters only if each original target is parameterized. *)
        let higher_order_parameters =
          if
            List.is_empty non_parameterized_call_targets
            && List.is_empty non_parameterized_init_targets
          then
            HigherOrderParameterMap.empty
          else
            higher_order_parameters
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
        (* Unset `unresolved` when the original call graph building cannot resolve callees under
           cases like `f()` or `f`. *)
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
                ~location
                ~call
                ~call_callees:
                  {
                    original_call_callees with
                    call_targets = new_call_targets;
                    decorated_targets =
                      decorated_call_targets
                      |> List.rev_append decorated_init_targets
                      |> List.dedup_and_sort ~compare:CallTarget.compare;
                    init_targets = new_init_targets;
                    higher_order_parameters;
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
                     `exist_stub_init_targets`: %b, `exist_stub_call_targets`: %b, `unresolved`: \
                     %a"
                    is_decorated_target
                    exist_stub_init_targets
                    exist_stub_call_targets
                    Unresolved.pp
                    unresolved
                in
                Algorithms.fold_balanced
                  ~f:CallTarget.Set.join
                  ~init:CallTarget.Set.bottom
                  argument_callees
              else
                CallTarget.Set.bottom
            in
            CallTarget.Set.join pass_through_arguments returned_callables_from_call, state)


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
          match
            redirect_expressions
              ~pyre_in_context
              ~callables_to_definitions_map:Context.callables_to_definitions_map
              ~location
              value
          with
          | Expression.Await expression -> analyze_expression ~pyre_in_context ~state ~expression
          | BinaryOperator { left; right; _ } ->
              let _, state = analyze_expression ~pyre_in_context ~state ~expression:left in
              let _, state = analyze_expression ~pyre_in_context ~state ~expression:right in
              CallTarget.Set.bottom, state
          | BooleanOperator _ -> CallTarget.Set.bottom, state
          | ComparisonOperator _ -> CallTarget.Set.bottom, state
          | Call ({ callee = _; arguments; origin = _ } as call) ->
              analyze_call ~pyre_in_context ~location ~call ~arguments ~state
          | Constant _ -> CallTarget.Set.bottom, state
          | Dictionary _ -> CallTarget.Set.bottom, state
          | DictionaryComprehension _ -> CallTarget.Set.bottom, state
          | Generator _ -> CallTarget.Set.bottom, state
          | Lambda { parameters = _; body = _ } -> CallTarget.Set.bottom, state
          | List _ -> CallTarget.Set.bottom, state
          | ListComprehension _ -> CallTarget.Set.bottom, state
          | Name (Name.Identifier identifier) ->
              let global_callables =
                Context.input_define_call_graph
                |> DefineCallGraph.resolve_identifier ~location ~identifier
                >>| (fun ({ IdentifierCallees.callable_targets; _ } as identifier_callees) ->
                      let {
                        AnalyzeDecoratedTargetsResult.decorated_targets;
                        non_decorated_targets;
                        result_targets;
                      }
                        =
                        resolve_decorated_targets callable_targets
                      in
                      Context.output_define_call_graph :=
                        DefineCallGraph.set_identifier_callees
                          ~identifier
                          ~location
                          ~identifier_callees:
                            {
                              identifier_callees with
                              callable_targets = non_decorated_targets;
                              decorated_targets;
                            }
                          !Context.output_define_call_graph;
                      CallTarget.Set.of_list result_targets)
                |> Option.value ~default:CallTarget.Set.bottom
              in
              let callables_from_variable =
                State.get (State.create_root_from_identifier identifier) state
              in
              CallTarget.Set.join global_callables callables_from_variable, state
          | Name (Name.Attribute { base = _; attribute; origin = _ }) ->
              let callables =
                Context.input_define_call_graph
                |> DefineCallGraph.resolve_attribute_access ~location ~attribute
                >>| (fun ({
                            AttributeAccessCallees.callable_targets;
                            property_targets;
                            is_attribute =
                              _
                              (* This is irrelevant. Regardless of whether this could potentially be
                                 an attribute access, we still need to treat `property_targets` in
                                 the same way as `callable_targets`. *);
                            _;
                          } as attribute_access_callees) ->
                      let {
                        AnalyzeDecoratedTargetsResult.decorated_targets = decorated_callable_targets;
                        non_decorated_targets = non_decorated_callable_targets;
                        result_targets = result_callable_targets;
                      }
                        =
                        resolve_decorated_targets callable_targets
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
                          ~attribute
                          ~location
                          ~attribute_access_callees:
                            {
                              attribute_access_callees with
                              callable_targets = non_decorated_callable_targets;
                              property_targets = result_property_targets;
                              decorated_targets =
                                List.rev_append
                                  decorated_callable_targets
                                  decorated_property_targets;
                            }
                          !Context.output_define_call_graph;
                      (* TODO(T222400916): We need to simulate the call to the property targets (by
                         calling `analyze_callee_targets`), which can return callables. *)
                      (* We should NOT return the property targets here. If method `A.foo` is a
                         property, then accessing the property `A().foo` means calling the getter,
                         but the result of the access is not the getter itself. *)
                      CallTarget.Set.of_list result_callable_targets)
                |> Option.value ~default:CallTarget.Set.bottom
              in
              callables, state
          | Set _ -> CallTarget.Set.bottom, state
          | SetComprehension _ -> CallTarget.Set.bottom, state
          | Starred (Starred.Once _)
          | Starred (Starred.Twice _) ->
              CallTarget.Set.bottom, state
          | Slice _ -> CallTarget.Set.bottom, state
          | Subscript _ -> CallTarget.Set.bottom, state
          | FormatString _ -> CallTarget.Set.bottom, state
          | Ternary { target = _; test = _; alternative = _ } -> CallTarget.Set.bottom, state
          | Tuple _ -> CallTarget.Set.bottom, state
          | UnaryOperator _ -> CallTarget.Set.bottom, state
          | WalrusOperator { target = _; value = _ } -> CallTarget.Set.bottom, state
          | Yield None -> CallTarget.Set.bottom, state
          | Yield (Some expression)
          | YieldFrom expression ->
              let callees, state = analyze_expression ~pyre_in_context ~state ~expression in
              ( callees,
                store_callees ~weak:true ~root:TaintAccessPath.Root.LocalResult ~callees state )
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


      let analyze_statement ~pyre_in_context ~state ~statement =
        log "Analyzing statement `%a` with state `%a`" Statement.pp statement State.pp state;
        let state =
          let statement = redirect_assignments statement in
          match Node.value statement with
          | Statement.Assign { Assign.target; value = Some value; _ } -> (
              match TaintAccessPath.of_expression ~self_variable target with
              | None -> state
              | Some { root; path } ->
                  let callees, state =
                    analyze_expression ~pyre_in_context ~state ~expression:value
                  in
                  (* For now, we ignore the path entirely. Thus, we should only perform strong
                     updates when writing to an empty path. E.g, `x = foo` should be strong update,
                     `x.foo = bar` should be a weak update. *)
                  let strong_update = TaintAccessPath.Path.is_empty path in
                  store_callees ~weak:(not strong_update) ~root ~callees state)
          | Assign { Assign.target; value = None; _ } -> (
              match TaintAccessPath.of_expression ~self_variable target with
              | None -> state
              | Some { root; path } ->
                  let strong_update = TaintAccessPath.Path.is_empty path in
                  store_callees ~weak:(not strong_update) ~root ~callees:CallTarget.Set.bottom state
              )
          | Assert _ -> state
          | Break
          | Class _
          | Continue ->
              state
          | Define ({ Define.signature = { name; _ }; _ } as define) ->
              let delocalized_name = Reference.delocalize name in
              let regular_target =
                define |> Target.create delocalized_name |> Target.as_regular_exn
              in
              let callees =
                (* Since `Define` statements inside another `Define` are stripped out (to avoid
                   bloat), use this API to query the definition. *)
                match
                  regular_target
                  |> Target.from_regular
                  |> Target.CallablesSharedMemory.ReadOnly.get_captures
                       Context.callables_to_definitions_map
                with
                | Some captures ->
                    let parameters_roots, parameters_targets =
                      captures
                      |> List.filter_map ~f:(fun { Define.Capture.name; _ } ->
                             let captured = State.create_root_from_identifier name in
                             log
                               "Inner function `%a` captures `%a`"
                               Reference.pp
                               delocalized_name
                               TaintAccessPath.Root.pp
                               captured;
                             let parameter_targets =
                               state
                               |> State.get captured
                               |> CallTarget.Set.elements
                               |> List.map ~f:CallTarget.target
                             in
                             (* Sometimes a captured variable does not have a record in `state`, but
                                we still want to create a callee with the captured variables that
                                have records in `state`. *)
                             if List.is_empty parameter_targets then
                               None
                             else
                               Some (captured, parameter_targets))
                      |> List.unzip
                    in
                    if List.is_empty parameters_targets then
                      regular_target
                      |> Target.from_regular
                      |> CallTarget.create
                      |> CallTarget.Set.singleton
                    else
                      parameters_targets
                      |> cartesian_product_with_limit
                           ~limit:Context.maximum_parameterized_targets_when_analyzing_define
                           ~message_when_exceeding_limit:
                             "Avoid generating parameterized targets when analyzing `Define` \
                              statement"
                      |> Option.value ~default:[]
                      |> List.map ~f:(fun parameters_targets ->
                             match
                               validate_target
                                 (Target.Parameterized
                                    {
                                      regular = regular_target;
                                      parameters =
                                        parameters_targets
                                        |> List.zip_exn parameters_roots
                                        |> Target.ParameterMap.of_alist_exn;
                                    })
                             with
                             | Some parameterized -> CallTarget.create parameterized
                             | None -> CallTarget.create_regular regular_target)
                      |> CallTarget.Set.of_list
                | _ ->
                    regular_target
                    |> Target.from_regular
                    |> CallTarget.create
                    |> CallTarget.Set.singleton
              in
              store_callees
                ~weak:false
                ~root:(name |> Reference.show |> State.create_root_from_identifier)
                ~callees
                state
          | Delete _ -> state
          | Expression expression ->
              analyze_expression ~pyre_in_context ~state ~expression |> Core.snd
          | For _
          | Global _
          | If _
          | Import _
          | Match _
          | Nonlocal _
          | Pass
          | Raise { expression = None; _ } ->
              state
          | Raise { expression = Some _; _ } -> state
          | Return { expression = Some expression; _ } ->
              let callees, state = analyze_expression ~pyre_in_context ~state ~expression in
              store_callees ~weak:true ~root:TaintAccessPath.Root.LocalResult ~callees state
          | Return { expression = None; _ }
          | Try _
          | TypeAlias _
          | With _
          | While _ ->
              state
          | Statement.AugmentedAssign _ ->
              failwith "statement should be lowered using redirect_assignments"
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
              PyrePysaEnvironment.InContext.create_at_statement_key
                Context.pyre_api
                ~define_name:Context.define_name
                ~define:Context.define
                ~statement_key
            in
            analyze_statement ~pyre_in_context ~state ~statement)


      let backward ~statement_key:_ _ ~statement:_ = failwith "unused"
    end)
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
    ~skip_analysis_targets
    ~callable
    ~qualifier
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

    let qualifier = qualifier

    let pyre_api = pyre_api

    let get_callee_model = get_callee_model

    let debug = debug_higher_order_call_graph define

    let define = define

    let define_name = PyrePysaLogic.qualified_name_of_define ~module_name:qualifier define

    let callable = callable

    let callables_to_definitions_map = callables_to_definitions_map

    let skip_analysis_targets = skip_analysis_targets

    let profiler = profiler

    let maximum_target_depth = maximum_target_depth

    let maximum_parameterized_targets_at_call_site = maximum_parameterized_targets_at_call_site

    (* TODO(T219483466): Make this configurable from command line. *)
    let maximum_parameterized_targets_when_analyzing_define =
      maximum_parameterized_targets_at_call_site
  end
  in
  log
    ~debug:Context.debug
    "Building higher order call graph of `%a` with initial state `%a`. Define call graph: `%a`"
    Reference.pp
    (PyrePysaLogic.qualified_name_of_define ~module_name:qualifier define)
    HigherOrderCallGraph.State.pp
    initial_state
    DefineCallGraph.pp
    define_call_graph;
  let module Fixpoint = HigherOrderCallGraph.MakeFixpoint (Context) in
  let returned_callables =
    Fixpoint.Fixpoint.forward ~cfg:(PyrePysaLogic.Cfg.create define) ~initial:initial_state
    |> Fixpoint.Fixpoint.exit
    >>| Fixpoint.get_returned_callables
    |> Option.value ~default:CallTarget.Set.bottom
  in
  let call_indexer = CallTarget.Indexer.create () in
  {
    HigherOrderCallGraph.returned_callables;
    call_graph =
      !Context.output_define_call_graph
      |> DefineCallGraph.filter_empty_attribute_access
      |> DefineCallGraph.regenerate_call_indices ~indexer:call_indexer;
  }


let call_graph_of_define
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.find_missing_flows; _ }
    ~pyre_api
    ~override_graph
    ~attribute_targets
    ~decorators
    ~callables_to_definitions_map
    ~qualifier
    ~define
  =
  let timer = Timer.start () in
  let is_missing_flow_type_analysis =
    Option.equal
      Configuration.MissingFlowKind.equal
      find_missing_flows
      (Some Configuration.MissingFlowKind.Type)
  in
  let define_name = PyrePysaLogic.qualified_name_of_define ~module_name:qualifier define in
  let callees_at_location = ref DefineCallGraph.empty in
  let context =
    {
      NodeVisitorContext.pyre_api;
      define_name = Some define_name;
      missing_flow_type_analysis =
        (if is_missing_flow_type_analysis then
           Some { MissingFlowTypeAnalysis.qualifier }
        else
          None);
      debug = Ast.Statement.Define.dump define || Ast.Statement.Define.dump_call_graph define;
      override_graph;
      attribute_targets;
      callables_to_definitions_map;
    }
  in
  let module DefineFixpoint = DefineCallGraphFixpoint (struct
    let node_visitor_context = context

    let callees_at_location = callees_at_location

    let define = define
  end)
  in
  let () = log ~debug:context.debug "Building call graph of `%a`" Reference.pp define_name in
  (* Handle parameters. *)
  let () =
    let pyre_in_context = PyrePysaEnvironment.InContext.create_at_global_scope pyre_api in
    List.iter
      define.Ast.Statement.Define.signature.parameters
      ~f:(fun { Node.value = { Parameter.value; _ }; _ } ->
        Option.iter value ~f:(fun value ->
            CalleeVisitor.visit_expression
              ~pyre_in_context
              ~assignment_target:None
              ~context
              ~callees_at_location
              value))
  in

  DefineFixpoint.forward ~cfg:(PyrePysaLogic.Cfg.create define) ~initial:() |> ignore;
  let call_indexer = CallTarget.Indexer.create () in
  let call_graph =
    !callees_at_location
    |> DefineCallGraph.filter_empty_attribute_access
    |> DefineCallGraph.redirect_to_decorated ~decorators
    |> DefineCallGraph.regenerate_call_indices ~indexer:call_indexer
  in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Call graph built"
    ~section:`DependencyGraph
    ~normals:["callable", Reference.show define_name]
    ~timer
    ();
  call_graph


let call_graph_of_callable
    ~static_analysis_configuration
    ~pyre_api
    ~override_graph
    ~attribute_targets
    ~decorators
    ~callables_to_definitions_map
    ~callable
  =
  match Target.CallablesSharedMemory.ReadOnly.get_define callables_to_definitions_map callable with
  | Some { Target.CallablesSharedMemory.DefineAndQualifier.qualifier; define } ->
      call_graph_of_define
        ~static_analysis_configuration
        ~pyre_api
        ~override_graph
        ~attribute_targets
        ~decorators
        ~callables_to_definitions_map
        ~qualifier
        ~define:(Node.value define)
  | _ -> Format.asprintf "Found no definition for `%a`" Target.pp_pretty callable |> failwith


module DecoratorDefine = struct
  type t = {
    define: Define.t;
    (* An artificial define that returns the call to the decorators. *)
    callable: Target.t;
    (* `Target` representation of the above define. *)
    call_graph: DefineCallGraph.t; (* Call graph of the above define. *)
  }
  [@@deriving show, eq]
end

module DecoratorResolution = struct
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
  let resolve_exn
      ?(debug = false)
      ~pyre_in_context
      ~override_graph
      ~callables_to_definitions_map
      ~decorators:decorators_map
      callable
    =
    let log format =
      if debug then
        Log.dump format
      else
        Format.ifprintf Format.err_formatter format
    in
    let resolve_callees ~call_graph =
      let context =
        {
          NodeVisitorContext.pyre_api = PyrePysaEnvironment.InContext.pyre_api pyre_in_context;
          define_name = None;
          missing_flow_type_analysis = None;
          debug;
          override_graph;
          attribute_targets = Target.HashSet.create ();
          callables_to_definitions_map;
        }
      in
      CalleeVisitor.visit_expression
        ~pyre_in_context
        ~assignment_target:None
        ~context
        ~callees_at_location:call_graph
    in
    let create_decorator_call previous_argument decorator =
      Node.create
        ~location:decorator.Node.location
          (* Avoid later registering all callees to the same location. *)
        (Expression.Call
           {
             Call.callee =
               Ast.Expression.delocalize
                 ~create_origin:(fun attributes ->
                   Some (Origin.ForDecoratedTargetCallee attributes))
                 decorator
               (* TODO: `decorator` might be a local variable whose definition should be included
                  here, in order to resolve its callee. *);
             arguments = [{ Call.Argument.name = None; value = previous_argument }];
             origin =
               Some { Node.location = decorator.Node.location; value = Origin.ForDecoratedTarget };
           })
    in
    match
      ( callable |> Target.get_regular |> Target.Regular.kind,
        CallableToDecoratorsMap.SharedMemory.ReadOnly.get decorators_map callable )
    with
    | None, _ -> Format.asprintf "Do not support `Override` or `Object` targets." |> failwith
    | Some Target.PropertySetter, _ -> PropertySetterUnsupported
    | Some Target.Decorated, _ -> failwith "unexpected"
    | _, None
    | Some Target.Normal, Some { CallableToDecoratorsMap.decorators = []; _ } ->
        Undecorated
    | ( Some Target.Normal,
        Some { CallableToDecoratorsMap.decorators = _ :: _ as decorators; define_location } ) ->
        let define_name = Target.define_name_exn callable in
        let callable_name =
          Ast.Expression.create_name_from_reference
            ~location:define_location
            ~create_origin:(fun attributes -> Some (Origin.ForDecoratedTargetCallee attributes))
            define_name
        in
        log "Decorators: [%s]" (decorators |> List.map ~f:Expression.show |> String.concat ~sep:";");
        let expression =
          List.fold
            decorators
            ~init:(Expression.Name callable_name |> Node.create ~location:define_location)
            ~f:create_decorator_call
        in
        let call_graph = ref DefineCallGraph.empty in
        resolve_callees ~call_graph expression;
        let define =
          {
            Define.signature =
              {
                Define.Signature.name = Reference.create ~prefix:define_name "@decorated";
                parameters = [];
                decorators = [];
                return_annotation = None;
                async = false;
                generator = false;
                parent = NestingContext.create_toplevel ();
                (* The class owning the method *)
                legacy_parent = None;
                type_params = [];
              };
            captures = [];
            unbound_names = [];
            body =
              [
                Statement.Return { Return.is_implicit = false; expression = Some expression }
                |> Node.create_with_default_location;
              ];
          }
        in
        Decorators
          {
            define;
            callable = CallableToDecoratorsMap.redirect_to_decorated ~callable decorators_map;
            call_graph = DefineCallGraph.filter_empty_attribute_access !call_graph;
          }


  module Results = struct
    (* A map from `kind=Decorated` targets to their defines. *)
    type t = DecoratorDefine.t Target.Map.t

    let empty = Target.Map.empty

    let resolve_batch_exn
        ~debug
        ~pyre_api
        ~scheduler
        ~scheduler_policy
        ~override_graph
        ~callables_to_definitions_map
        ~decorators
        callables
      =
      let pyre_in_context = PyrePysaEnvironment.InContext.create_at_global_scope pyre_api in
      let resolve callable =
        match
          resolve_exn
            ~debug
            ~pyre_in_context
            ~override_graph:(Some (OverrideGraph.SharedMemory.read_only override_graph))
            ~callables_to_definitions_map
            ~decorators
            callable
        with
        | Decorators resolved -> Some resolved
        | PropertySetterUnsupported
        | Undecorated ->
            None
      in
      let map callables =
        callables
        |> List.filter_map ~f:resolve
        |> List.map ~f:(fun ({ DecoratorDefine.callable; _ } as decorator_define) ->
               callable, decorator_define)
        |> Target.Map.of_alist_exn
      in
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:empty
        ~map
        ~reduce:
          (Target.Map.union (fun callable _ _ ->
               failwithf "Unexpected: %s" (Target.show_pretty_with_kind callable) ()))
        ~inputs:callables
        ()


    let register_decorator_defines ~decorator_resolution callables_to_definitions_map =
      let artificial_decorator_defines = Reference.create "artificial_decorator_defines" in
      decorator_resolution
      |> Target.Map.to_alist
      |> List.map ~f:(fun (callable, { DecoratorDefine.define; _ }) ->
             ( callable,
               {
                 Target.CallablesSharedMemory.DefineAndQualifier.qualifier =
                   artificial_decorator_defines;
                 define = Node.create_with_default_location define;
               } ))
      |> Target.CallablesSharedMemory.add_alist_sequential callables_to_definitions_map


    let decorated_targets = Target.Map.keys
  end
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

  let default_scheduler_policy =
    Scheduler.Policy.fixed_chunk_size
      ~minimum_chunks_per_worker:1
      ~minimum_chunk_size:2
      ~preferred_chunk_size:5000
      ()


  let register_decorator_call_graphs
      ~decorator_resolution
      ~use_case
      { whole_program_call_graph; define_call_graphs }
    =
    let call_graphs =
      decorator_resolution
      |> Target.Map.to_alist
      |> List.map ~f:(fun (callable, { DecoratorDefine.call_graph; _ }) -> callable, call_graph)
    in
    {
      whole_program_call_graph =
        List.fold
          call_graphs
          ~f:(fun whole_program_call_graph (callable, call_graph) ->
            WholeProgramCallGraph.add_or_exn
              whole_program_call_graph
              ~callable
              ~callees:(DefineCallGraph.all_targets ~use_case call_graph))
          ~init:whole_program_call_graph;
      define_call_graphs = add_alist_sequential define_call_graphs call_graphs;
    }


  (** Build the whole call graph of the program.

      The overrides must be computed first because we depend on a global shared memory graph to
      include overrides in the call graph. Without it, we'll underanalyze and have an inconsistent
      fixpoint. *)
  let build_whole_program_call_graph
      ~scheduler
      ~static_analysis_configuration:
        ({ Configuration.StaticAnalysis.scheduler_policies; _ } as static_analysis_configuration)
      ~pyre_api
      ~resolve_module_path
      ~callables_to_definitions_map
      ~override_graph
      ~store_shared_memory
      ~attribute_targets
      ~decorators
      ~decorator_resolution
      ~skip_analysis_targets
      ~definitions
      ~create_dependency_for
    =
    let attribute_targets = attribute_targets |> Target.Set.elements |> Target.HashSet.of_list in
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
                  ~decorators
                  ~callables_to_definitions_map
                  ~callable)
              ()
          in
          let define_call_graphs =
            if store_shared_memory then
              T.AddOnly.add define_call_graphs callable callable_call_graph
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
      let reduce
          (left_define_call_graphs, left_whole_program_call_graph)
          (right_define_call_graphs, right_whole_program_call_graph)
        =
        (* We should check the keys in two define call graphs are disjoint. If not disjoint, we should
         * fail the analysis. But we don't perform such check due to performance reasons.
         * Additionally, since this `reduce` is used in `Scheduler.map_reduce`, the right parameter
         * is accumulated, so we must select left as smaller and right as larger for O(n) merging. *)
        ( T.AddOnly.merge_same_handle_disjoint_keys
            ~smaller:left_define_call_graphs
            ~larger:right_define_call_graphs,
          WholeProgramCallGraph.merge_disjoint
            left_whole_program_call_graph
            right_whole_program_call_graph )
      in
      let define_call_graphs = T.create () |> T.add_only in
      let empty_define_call_graphs = T.AddOnly.create_empty define_call_graphs in
      let scheduler_policy =
        Scheduler.Policy.from_configuration_or_default
          scheduler_policies
          Configuration.ScheduleIdentifier.CallGraph
          ~default:default_scheduler_policy
      in
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:(define_call_graphs, WholeProgramCallGraph.empty)
        ~map:(fun definitions ->
          List.fold
            definitions
            ~init:(empty_define_call_graphs, WholeProgramCallGraph.empty)
            ~f:build_call_graph)
        ~reduce
        ~inputs:definitions
        ()
    in
    let define_call_graphs = T.from_add_only define_call_graphs in
    let define_call_graphs_read_only = T.read_only define_call_graphs in
    let () =
      DefineCallGraph.save_to_directory
        ~scheduler
        ~static_analysis_configuration
        ~callables_to_definitions_map
        ~resolve_module_path
        ~get_call_graph:(fun callable ->
          ReadOnly.get define_call_graphs_read_only ~cache:false ~callable)
        ~json_kind:NewlineDelimitedJson.Kind.CallGraph
        ~filename_prefix:"call-graph"
        ~callables:definitions
    in
    { whole_program_call_graph; define_call_graphs }
    |> register_decorator_call_graphs ~use_case:create_dependency_for ~decorator_resolution
end
