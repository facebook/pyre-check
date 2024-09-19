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
module PyrePysaApi = Analysis.PyrePysaApi
module Cfg = Analysis.Cfg
module ResolvedReference = Analysis.ResolvedReference
module ClassSummary = Analysis.ClassSummary
module AnnotatedAttribute = Analysis.AnnotatedAttribute
module TypeInfo = Analysis.TypeInfo

module JsonHelper = struct
  let add_optional name value to_json bindings =
    match value with
    | Some value -> (name, to_json value) :: bindings
    | None -> bindings


  let add_flag_if name condition bindings =
    if condition then
      (name, `Bool true) :: bindings
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
  [@@deriving compare, eq]

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
            PyrePysaApi.ReadOnly.less_or_equal pyre_api ~left ~right:Type.bool)
      in
      let is_integer =
        matches_at_leaves annotation ~f:(fun left ->
            PyrePysaApi.ReadOnly.less_or_equal pyre_api ~left ~right:Type.integer)
      in
      let is_float =
        matches_at_leaves annotation ~f:(fun left ->
            PyrePysaApi.ReadOnly.less_or_equal pyre_api ~left ~right:Type.float)
      in
      let is_enumeration =
        matches_at_leaves annotation ~f:(fun left ->
            PyrePysaApi.ReadOnly.less_or_equal pyre_api ~left ~right:Type.enumeration)
      in
      { is_boolean; is_integer; is_float; is_enumeration }
    with
    | Analysis.ClassHierarchy.Untracked untracked_type ->
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

(** A specific target of a given call, with extra information. *)
module CallTarget = struct
  type t = {
    target: Target.t;
    (* True if the call has an implicit receiver, such as calling an instance or a class method. For
       instance, `x.foo(0)` should be treated as `C.foo(x, 0)`. As another example, `C.foo(0)`
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
  [@@deriving compare, eq, show { with_path = false }]

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
    | Some "type", [Type.Record.Argument.Single parameter] when is_class_method ->
        (* The receiver is the class itself. Technically, the receiver class type should be
           `Type[int]`. However, we strip away the `type` part since it is implied by the
           `is_class_method` flag. *)
        Type.primitive_name parameter
    | Some "type", _ -> None
    | Some "super", _ -> None
    | name, _ -> name


  let create
      ?(implicit_receiver = false)
      ?(implicit_dunder_call = false)
      ?(index = 0)
      ?(return_type = Some ReturnType.any)
      ?receiver_class
      ?(is_class_method = false)
      ?(is_static_method = false)
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
    |> JsonHelper.add_flag_if "implicit_receiver" implicit_receiver
    |> JsonHelper.add_flag_if "implicit_dunder_call" implicit_dunder_call
    |> JsonHelper.add_optional "return_type" return_type ReturnType.to_json
    |> JsonHelper.add_optional "receiver_class" receiver_class (fun name -> `String name)
    |> JsonHelper.add_flag_if "is_class_method" is_class_method
    |> JsonHelper.add_flag_if "is_static_method" is_static_method
    |> fun bindings -> `Assoc (List.rev bindings)
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

(** Information about an argument being a callable. *)
module HigherOrderParameter = struct
  type t = {
    index: int;
    call_targets: CallTarget.t list;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: bool;
  }
  [@@deriving eq, show { with_path = false }]

  let all_targets { call_targets; _ } = List.map ~f:CallTarget.target call_targets

  let equal_ignoring_types
      { index = index_left; call_targets = call_targets_left; unresolved = unresolved_left }
      { index = index_right; call_targets = call_targets_right; unresolved = unresolved_right }
    =
    Int.equal index_left index_right
    && List.equal CallTarget.equal_ignoring_types call_targets_left call_targets_right
    && Bool.equal unresolved_left unresolved_right


  let join
      { index; call_targets = call_targets_left; unresolved = unresolved_left }
      { index = _; call_targets = call_targets_right; unresolved = unresolved_right }
    =
    {
      index;
      call_targets = List.rev_append call_targets_left call_targets_right;
      unresolved = unresolved_left || unresolved_right;
    }


  let deduplicate { index; call_targets; unresolved } =
    { index; call_targets = CallTarget.dedup_and_sort call_targets; unresolved }


  let to_json { index; call_targets; unresolved } =
    ["parameter_index", `Int index; "calls", `List (List.map ~f:CallTarget.to_json call_targets)]
    |> JsonHelper.add_flag_if "unresolved" unresolved
    |> fun bindings -> `Assoc bindings
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

  let all_targets map =
    Map.fold
      (fun _ higher_order_parameter targets ->
        List.rev_append targets (HigherOrderParameter.all_targets higher_order_parameter))
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
end

(** An aggregate of all possible callees at a call site. *)
module CallCallees = struct
  type t = {
    (* Normal call targets. *)
    call_targets: CallTarget.t list;
    (* Call targets for calls to the `__new__` class method. *)
    new_targets: CallTarget.t list;
    (* Call targets for calls to the `__init__` instance method. *)
    init_targets: CallTarget.t list;
    (* Information about arguments that are callables, and possibly called. *)
    higher_order_parameters: HigherOrderParameterMap.t;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: bool;
  }
  [@@deriving eq, show { with_path = false }]

  let create
      ?(call_targets = [])
      ?(new_targets = [])
      ?(init_targets = [])
      ?(higher_order_parameters = HigherOrderParameterMap.empty)
      ?(unresolved = false)
      ()
    =
    { call_targets; new_targets; init_targets; higher_order_parameters; unresolved }


  (* When `debug` is true, log the reason for creating `unresolved`. *)
  let unresolved ?(debug = false) ?reason () =
    let () =
      match reason with
      | Some reason when debug ->
          (* Use `dump` so that the log can also be printed when testing. *)
          Log.dump "Unresolved call: %s" reason
      | _ -> ()
    in
    {
      call_targets = [];
      new_targets = [];
      init_targets = [];
      higher_order_parameters = HigherOrderParameterMap.empty;
      unresolved = true;
    }


  let default_to_unresolved ?(debug = false) ?reason = function
    | Some value -> value
    | None -> unresolved ~debug ?reason ()


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
        higher_order_parameters = left_higher_order_parameters;
        unresolved = left_unresolved;
      }
      {
        call_targets = right_call_targets;
        new_targets = right_new_targets;
        init_targets = right_init_targets;
        higher_order_parameters = right_higher_order_parameters;
        unresolved = right_unresolved;
      }
    =
    let call_targets = List.rev_append left_call_targets right_call_targets in
    let new_targets = List.rev_append left_new_targets right_new_targets in
    let init_targets = List.rev_append left_init_targets right_init_targets in
    let higher_order_parameters =
      HigherOrderParameterMap.join left_higher_order_parameters right_higher_order_parameters
    in
    let unresolved = left_unresolved || right_unresolved in
    { call_targets; new_targets; init_targets; higher_order_parameters; unresolved }


  let deduplicate { call_targets; new_targets; init_targets; higher_order_parameters; unresolved } =
    let call_targets = CallTarget.dedup_and_sort call_targets in
    let new_targets = CallTarget.dedup_and_sort new_targets in
    let init_targets = CallTarget.dedup_and_sort init_targets in
    let higher_order_parameters = HigherOrderParameterMap.deduplicate higher_order_parameters in
    { call_targets; new_targets; init_targets; higher_order_parameters; unresolved }


  let all_targets { call_targets; new_targets; init_targets; higher_order_parameters; _ } =
    call_targets
    |> List.rev_append new_targets
    |> List.rev_append init_targets
    |> List.map ~f:CallTarget.target
    |> List.rev_append (HigherOrderParameterMap.all_targets higher_order_parameters)


  let equal_ignoring_types
      {
        call_targets = call_targets_left;
        new_targets = new_targets_left;
        init_targets = init_targets_left;
        higher_order_parameters = higher_order_parameter_lefts;
        unresolved = unresolved_left;
      }
      {
        call_targets = call_targets_right;
        new_targets = new_targets_right;
        init_targets = init_targets_right;
        higher_order_parameters = higher_order_parameter_rights;
        unresolved = unresolved_right;
      }
    =
    List.equal CallTarget.equal_ignoring_types call_targets_left call_targets_right
    && List.equal CallTarget.equal_ignoring_types new_targets_left new_targets_right
    && List.equal CallTarget.equal_ignoring_types init_targets_left init_targets_right
    && HigherOrderParameterMap.equal_ignoring_types
         higher_order_parameter_lefts
         higher_order_parameter_rights
    && Bool.equal unresolved_left unresolved_right


  let is_method_of_class ~is_class_name callees =
    let is_call_target = function
      | { CallTarget.target = Method { class_name; _ }; receiver_class = Some receiver_class; _ }
      | { target = Override { class_name; _ }; receiver_class = Some receiver_class; _ } ->
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
    | [
        {
          CallTarget.target =
            Target.Method { class_name = "object"; method_name = "__new__"; kind = Normal };
          _;
        };
      ] ->
        true
    | _ -> false


  let is_object_init = function
    | [] -> (* Unresolved call, assume it's object.__init__ *) true
    | [
        {
          CallTarget.target =
            Target.Method { class_name = "object"; method_name = "__init__"; kind = Normal };
          _;
        };
      ] ->
        true
    | _ -> false


  let to_json { call_targets; new_targets; init_targets; higher_order_parameters; unresolved } =
    let bindings =
      []
      |> JsonHelper.add_list "calls" call_targets CallTarget.to_json
      |> JsonHelper.add_list "new_calls" new_targets CallTarget.to_json
      |> JsonHelper.add_list "init_calls" init_targets CallTarget.to_json
    in
    let bindings =
      if not (HigherOrderParameterMap.is_empty higher_order_parameters) then
        ("higher_order_parameters", HigherOrderParameterMap.to_json higher_order_parameters)
        :: bindings
      else
        bindings
    in
    let bindings = JsonHelper.add_flag_if "unresolved" unresolved bindings in
    `Assoc (List.rev bindings)
end

(** An aggregrate of all possible callees for a given attribute access. *)
module AttributeAccessCallees = struct
  type t = {
    property_targets: CallTarget.t list;
    global_targets: CallTarget.t list;
    (* True if the attribute access should also be considered a regular attribute.
     * For instance, if the object has type `Union[A, B]` where only `A` defines a property. *)
    is_attribute: bool;
  }
  [@@deriving eq, show { with_path = false }]

  let deduplicate { property_targets; global_targets; is_attribute } =
    {
      property_targets = CallTarget.dedup_and_sort property_targets;
      global_targets = CallTarget.dedup_and_sort global_targets;
      is_attribute;
    }


  let join
      {
        property_targets = left_property_targets;
        global_targets = left_global_targets;
        is_attribute = left_is_attribute;
      }
      {
        property_targets = right_property_targets;
        global_targets = right_global_targets;
        is_attribute = right_is_attribute;
      }
    =
    {
      property_targets = List.rev_append left_property_targets right_property_targets;
      global_targets = List.rev_append left_global_targets right_global_targets;
      is_attribute = left_is_attribute || right_is_attribute;
    }


  let all_targets { property_targets; global_targets; _ } =
    List.rev_append property_targets global_targets |> List.map ~f:CallTarget.target


  let equal_ignoring_types
      {
        property_targets = property_targets_left;
        global_targets = global_targets_left;
        is_attribute = is_attribute_left;
      }
      {
        property_targets = property_targets_right;
        global_targets = global_targets_right;
        is_attribute = is_attribute_right;
      }
    =
    List.equal CallTarget.equal_ignoring_types property_targets_left property_targets_right
    && List.equal CallTarget.equal_ignoring_types global_targets_left global_targets_right
    && Bool.equal is_attribute_left is_attribute_right


  let empty = { property_targets = []; global_targets = []; is_attribute = true }

  let is_empty attribute_access_callees = equal attribute_access_callees empty

  let to_json { property_targets; global_targets; is_attribute } =
    []
    |> JsonHelper.add_list "properties" property_targets CallTarget.to_json
    |> JsonHelper.add_list "globals" global_targets CallTarget.to_json
    |> JsonHelper.add_flag_if "is_attribute" is_attribute
    |> fun bindings -> `Assoc (List.rev bindings)
end

(** An aggregate of all possible callees for a given identifier expression, i.e `foo`. *)
module IdentifierCallees = struct
  type t = {
    global_targets: CallTarget.t list;
    nonlocal_targets: CallTarget.t list;
  }
  [@@deriving eq, show { with_path = false }]

  type identifier_reference =
    | Global of Reference.t
    | Nonlocal of Reference.t

  let deduplicate { global_targets; nonlocal_targets } =
    {
      global_targets = CallTarget.dedup_and_sort global_targets;
      nonlocal_targets = CallTarget.dedup_and_sort nonlocal_targets;
    }


  let join
      { global_targets = left_global_targets; nonlocal_targets = left_nonlocal_targets }
      { global_targets = right_global_targets; nonlocal_targets = right_nonlocal_targets }
    =
    {
      global_targets = List.rev_append left_global_targets right_global_targets;
      nonlocal_targets = List.rev_append left_nonlocal_targets right_nonlocal_targets;
    }


  let all_targets { global_targets; nonlocal_targets } =
    List.map ~f:CallTarget.target (global_targets @ nonlocal_targets)


  let to_json { global_targets; nonlocal_targets } =
    `Assoc
      [
        "globals", `List (List.map ~f:CallTarget.to_json global_targets);
        "nonlocals", `List (List.map ~f:CallTarget.to_json nonlocal_targets);
      ]
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


  let all_targets { stringify_targets; f_string_targets } =
    List.rev_append stringify_targets f_string_targets |> List.map ~f:CallTarget.target


  let from_stringify_targets stringify_targets = { stringify_targets; f_string_targets = [] }

  let from_f_string_targets f_string_targets = { stringify_targets = []; f_string_targets }

  let to_json { stringify_targets; f_string_targets } =
    []
    |> JsonHelper.add_list "stringify" stringify_targets CallTarget.to_json
    |> JsonHelper.add_list "f-string" f_string_targets CallTarget.to_json
    |> fun bindings -> `Assoc bindings
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


  let from_call_with_empty_attribute callees =
    {
      call = Some callees;
      attribute_access = Some AttributeAccessCallees.empty;
      identifier = None;
      string_format = None;
    }


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


  let all_targets { call; attribute_access; identifier; string_format } =
    let call_targets = call >>| CallCallees.all_targets |> Option.value ~default:[] in
    let attribute_access_targets =
      attribute_access >>| AttributeAccessCallees.all_targets |> Option.value ~default:[]
    in
    let identifier_targets =
      identifier >>| IdentifierCallees.all_targets |> Option.value ~default:[]
    in
    let string_format_targets =
      string_format >>| StringFormatCallees.all_targets |> Option.value ~default:[]
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
end

(** An aggregate of all possible callees for an arbitrary location.

    Note that multiple expressions might have the same location. *)
module LocationCallees = struct
  type t =
    | Singleton of ExpressionCallees.t
    | Compound of ExpressionCallees.t SerializableStringMap.t
  [@@deriving eq]

  let pp formatter = function
    | Singleton callees -> Format.fprintf formatter "%a" ExpressionCallees.pp callees
    | Compound map ->
        SerializableStringMap.to_alist map
        |> List.map ~f:(fun (key, value) -> Format.asprintf "%s: %a" key ExpressionCallees.pp value)
        |> String.concat ~sep:", "
        |> Format.fprintf formatter "%s"


  let show callees = Format.asprintf "%a" pp callees

  let all_targets = function
    | Singleton raw_callees -> ExpressionCallees.all_targets raw_callees
    | Compound map ->
        SerializableStringMap.data map |> List.concat_map ~f:ExpressionCallees.all_targets


  let equal_ignoring_types location_callees_left location_callees_right =
    match location_callees_left, location_callees_right with
    | Singleton callees_left, Singleton callees_right ->
        ExpressionCallees.equal_ignoring_types callees_left callees_right
    | Compound map_left, Compound map_right ->
        SerializableStringMap.equal ExpressionCallees.equal_ignoring_types map_left map_right
    | _ -> false


  let to_json = function
    | Singleton callees -> `Assoc ["singleton", ExpressionCallees.to_json callees]
    | Compound map ->
        let bindings =
          SerializableStringMap.fold
            (fun key value sofar -> (key, ExpressionCallees.to_json value) :: sofar)
            map
            []
        in
        `Assoc ["compound", `Assoc bindings]
end

module UnprocessedLocationCallees = struct
  type t = ExpressionCallees.t SerializableStringMap.t

  let singleton ~expression_identifier ~callees =
    SerializableStringMap.singleton expression_identifier callees


  let add map ~expression_identifier ~callees =
    SerializableStringMap.update
      expression_identifier
      (function
        | Some existing_callees -> Some (ExpressionCallees.join existing_callees callees)
        | None -> Some callees)
      map
end

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


(** The call graph of a function or method definition. *)
module DefineCallGraph = struct
  type t = LocationCallees.t Location.Map.Tree.t [@@deriving eq]

  let pp formatter call_graph =
    let pp_pair formatter (key, value) =
      Format.fprintf formatter "@,%a -> %a" Location.pp key LocationCallees.pp value
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    call_graph |> Location.Map.Tree.to_alist |> Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs


  let show = Format.asprintf "%a" pp

  let empty = Location.Map.Tree.empty

  let is_empty = Location.Map.Tree.is_empty

  let add call_graph ~location ~callees =
    Location.Map.Tree.set call_graph ~key:location ~data:callees


  let resolve_expression call_graph ~location ~expression_identifier =
    match Location.Map.Tree.find call_graph location with
    | Some (LocationCallees.Singleton callees) -> Some callees
    | Some (LocationCallees.Compound name_to_callees) ->
        SerializableStringMap.find_opt expression_identifier name_to_callees
    | None -> None


  let resolve_call call_graph ~location ~call =
    expression_identifier (Expression.Call call)
    >>= fun expression_identifier ->
    resolve_expression call_graph ~location ~expression_identifier >>= fun { call; _ } -> call


  let resolve_attribute_access call_graph ~location ~attribute =
    resolve_expression call_graph ~location ~expression_identifier:attribute
    >>= fun { attribute_access; _ } -> attribute_access


  let resolve_identifier call_graph ~location ~identifier =
    resolve_expression call_graph ~location ~expression_identifier:identifier
    >>= fun { identifier; _ } -> identifier


  let string_format_expression_identifier = "$__str__$"

  let resolve_string_format call_graph ~location =
    resolve_expression
      call_graph
      ~location
      ~expression_identifier:string_format_expression_identifier
    >>= fun { string_format; _ } -> string_format


  let equal_ignoring_types call_graph_left call_graph_right =
    Location.Map.Tree.equal LocationCallees.equal_ignoring_types call_graph_left call_graph_right


  (** Return all callees of the call graph, as a sorted list. *)
  let all_targets call_graph =
    Location.Map.Tree.data call_graph
    |> List.concat_map ~f:LocationCallees.all_targets
    |> List.dedup_and_sort ~compare:Target.compare


  let to_json ~pyre_api ~resolve_module_path ~callable call_graph =
    let bindings = ["callable", `String (Target.external_name callable)] in
    let bindings =
      let resolve_module_path = Option.value ~default:(fun _ -> None) resolve_module_path in
      Target.get_module_and_definition ~pyre_api callable
      >>| fst
      >>= resolve_module_path
      >>| (function
            | { RepositoryPath.filename = Some filename; _ } ->
                ("filename", `String filename) :: bindings
            | { path; _ } ->
                ("filename", `String "*") :: ("path", `String (PyrePath.absolute path)) :: bindings)
      |> Option.value ~default:bindings
    in
    let bindings =
      let edges =
        Location.Map.Tree.fold call_graph ~init:[] ~f:(fun ~key ~data sofar ->
            (Location.show key, LocationCallees.to_json data) :: sofar)
      in
      ("calls", `Assoc edges) :: bindings
    in
    `Assoc (List.rev bindings)
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
module CallTargetIndexer = struct
  type t = {
    indices: int Target.HashMap.t;
    mutable seen_targets: Target.Set.t;
  }

  let create () = { indices = Target.HashMap.create (); seen_targets = Target.Set.empty }

  let generate_fresh_indices indexer =
    Target.Set.iter (Hashtbl.incr indexer.indices) indexer.seen_targets;
    indexer.seen_targets <- Target.Set.empty


  let create_target
      indexer
      ~implicit_dunder_call
      ~return_type
      ?receiver_type
      ?(is_class_method = false)
      ?(is_static_method = false)
      ?(explicit_receiver = false)
      original_target
    =
    let target_for_index = Target.override_to_method original_target in
    let index = Hashtbl.find indexer.indices target_for_index |> Option.value ~default:0 in
    indexer.seen_targets <- Target.Set.add target_for_index indexer.seen_targets;
    let implicit_receiver =
      if is_static_method then
        false
      else if is_class_method then
        true
      else
        (not explicit_receiver) && Target.is_method_or_override target_for_index
    in
    {
      CallTarget.target = original_target;
      implicit_receiver;
      implicit_dunder_call;
      index;
      return_type;
      receiver_class = receiver_type >>= CallTarget.receiver_class_from_type ~is_class_method;
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


let class_method_decorators = ["classmethod"; "abstractclassmethod"; "abc.abstractclassmethod"]

let static_method_decorators = ["staticmethod"; "abstractstaticmethod"; "abc.abstractstaticmethod"]

module CalleeKind = struct
  type t =
    | Method of {
        is_direct_call: bool;
        is_class_method: bool;
        is_static_method: bool;
      }
    | Function

  let rec from_callee ~pyre_in_context callee callee_type =
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
    let is_static_method, is_class_method =
      (* TODO(T171340051): A better implementation that uses `Annotation`. *)
      match
        PyrePysaApi.ReadOnly.get_define_body pyre_api (callee |> Expression.show |> Reference.create)
      with
      | Some define_body ->
          let define = Node.value define_body in
          ( List.exists static_method_decorators ~f:(Ast.Statement.Define.has_decorator define),
            List.exists class_method_decorators ~f:(Ast.Statement.Define.has_decorator define) )
      | None -> false, false
    in
    match callee_type with
    | _ when is_super_call -> Method { is_direct_call = true; is_static_method; is_class_method }
    | Type.Parametric { name = "BoundMethod"; arguments = [Single _; Single receiver_type] } ->
        let is_class_method =
          (* Sometimes the define body of the callee is unavailable, in which case we identify calls
             to class methods via the receiver type. The callee is a class method, if the callee
             type is `BoundMethod` and the receiver type is the type of a class type.

             TODO(T171340051): We can delete this pattern matching case if using a better
             implementation. *)
          let type_, parameters = Type.split receiver_type in
          match Type.primitive_name type_, parameters with
          | Some "type", [Type.Record.Argument.Single (Type.Primitive _)] -> true
          | _ -> false
        in
        Method
          { is_direct_call = is_all_names (Node.value callee); is_static_method; is_class_method }
    | Type.Parametric { name = "BoundMethod"; _ } ->
        Method
          { is_direct_call = is_all_names (Node.value callee); is_static_method; is_class_method }
    | Type.Callable _ -> (
        match Node.value callee with
        | Expression.Name (Name.Attribute { base; _ }) ->
            let parent_type = CallResolution.resolve_ignoring_errors ~pyre_in_context base in
            let is_class () =
              let primitive, _ = Type.split parent_type in
              Type.primitive_name primitive
              >>= PyrePysaApi.ReadOnly.get_class_summary pyre_api
              |> Option.is_some
            in
            if Type.is_builtins_type parent_type then
              Method { is_direct_call = true; is_static_method; is_class_method }
            else if is_class () then
              Method { is_direct_call = false; is_static_method; is_class_method }
            else
              Function
        | _ -> Function)
    | Type.Union (callee_type :: _) -> from_callee ~pyre_in_context callee callee_type
    | _ ->
        (* We must be dealing with a callable class. *)
        Method { is_direct_call = false; is_static_method; is_class_method }


  let is_class_method = function
    | Method { is_class_method; _ } -> is_class_method
    | _ -> false


  let is_static_method = function
    | Method { is_static_method; _ } -> is_static_method
    | _ -> false
end

let strip_optional annotation = Type.optional_value annotation |> Option.value ~default:annotation

let strip_meta annotation =
  if Type.is_builtins_type annotation then
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
        Target.get_corresponding_override method_name
    | _ -> method_name
  in
  let receiver_type = receiver_type |> strip_meta |> strip_optional |> Type.weaken_literals in
  let declaring_type, method_name, kind =
    match implementation_target with
    | Target.Method { class_name; method_name; kind } ->
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
            get_actual_target
              (Target.Method { class_name = Reference.show class_name; method_name; kind })
          in
          List.filter overriding_types ~f:keep_subtypes
          |> fun subtypes -> List.map subtypes ~f:create_override_target
        in
        implementation_target :: override_targets


let rec resolve_callees_from_type
    ~debug
    ~pyre_in_context
    ~override_graph
    ~call_indexer
    ?(dunder_call = false)
    ?receiver_type
    ~return_type
    ~callee_kind
    callable_type
  =
  let resolve_callees_from_type ?(dunder_call = dunder_call) =
    resolve_callees_from_type ~dunder_call
  in
  let callable_type_string =
    Format.asprintf
      "callable type %a (i.e., %s)"
      Type.pp
      callable_type
      (Type.show_type_t callable_type)
  in
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
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
                CallTargetIndexer.create_target
                  call_indexer
                  ~implicit_dunder_call:dunder_call
                  ~return_type:(Some return_type)
                  ~receiver_type
                  ~is_class_method:(CalleeKind.is_class_method callee_kind)
                  ~is_static_method:(CalleeKind.is_static_method callee_kind)
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
          CallCallees.create
            ~call_targets:
              [
                CallTargetIndexer.create_target
                  call_indexer
                  ~explicit_receiver:true
                  ~implicit_dunder_call:dunder_call
                  ~return_type:(Some return_type)
                  ~is_class_method:(CalleeKind.is_class_method callee_kind)
                  ~is_static_method:(CalleeKind.is_static_method callee_kind)
                  ?receiver_type
                  target;
              ]
            ())
  | Type.Callable { kind = Anonymous; _ } ->
      CallCallees.unresolved
        ~debug
        ~reason:(Format.asprintf "%s has kind `Anonymous`" callable_type_string)
        ()
  | Type.Parametric { name = "BoundMethod"; arguments = [Single callable; Single receiver_type] } ->
      resolve_callees_from_type
        ~debug
        ~pyre_in_context
        ~override_graph
        ~call_indexer
        ~receiver_type
        ~return_type
        ~callee_kind
        callable
  | Type.Union (element :: elements) ->
      let first_targets =
        resolve_callees_from_type
          ~debug
          ~pyre_in_context
          ~override_graph
          ~call_indexer
          ~callee_kind
          ?receiver_type
          ~return_type
          element
      in
      List.fold elements ~init:first_targets ~f:(fun combined_targets new_target ->
          resolve_callees_from_type
            ~debug
            ~pyre_in_context
            ~override_graph
            ~call_indexer
            ?receiver_type
            ~return_type
            ~callee_kind
            new_target
          |> CallCallees.join combined_targets)
  | Type.Parametric { name = "type"; arguments = [Single class_type] } ->
      resolve_constructor_callee ~debug ~pyre_in_context ~override_graph ~call_indexer class_type
      |> CallCallees.default_to_unresolved
           ~debug
           ~reason:
             (Format.asprintf "Failed to resolve construct callees from %s" callable_type_string)
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
            ~reason:
              (Format.asprintf
                 "Resolved `Any` or `Top` when treating %s as callable class"
                 callable_type_string)
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
                  Target.Method
                    {
                      Target.class_name = primitive_callable_name;
                      method_name = "__call__";
                      kind = Normal;
                    }
                in
                CallCallees.create
                  ~call_targets:
                    [
                      CallTargetIndexer.create_target
                        call_indexer
                        ~implicit_dunder_call:true
                        ~return_type:(Some return_type)
                        ~is_class_method:(CalleeKind.is_class_method callee_kind)
                        ~is_static_method:(CalleeKind.is_static_method callee_kind)
                        ?receiver_type
                        target;
                    ]
                  ())
          |> CallCallees.default_to_unresolved
               ~debug
               ~reason:(Format.asprintf "Failed to resolve protocol from %s" callable_type_string)
      | annotation ->
          if not dunder_call then
            resolve_callees_from_type
              ~debug
              ~pyre_in_context
              ~override_graph
              ~call_indexer
              ~return_type
              ~dunder_call:true
              ~callee_kind
              annotation
          else
            CallCallees.unresolved
              ~debug
              ~reason:
                (Format.asprintf
                   "Failed to resolve %s as callable class, protocol, or a non dunder call."
                   callable_type_string)
              ())


and resolve_callees_from_type_external
    ~pyre_in_context
    ~override_graph
    ~return_type
    ?(dunder_call = false)
    callee
  =
  let callee_type = CallResolution.resolve_ignoring_errors ~pyre_in_context callee in
  let callee_kind = CalleeKind.from_callee ~pyre_in_context callee callee_type in
  resolve_callees_from_type
    ~debug:false
    ~pyre_in_context
    ~override_graph
    ~call_indexer:(CallTargetIndexer.create ()) (* Don't care about indexing the callees. *)
    ~dunder_call
    ~return_type
    ~callee_kind
    callee_type


and resolve_constructor_callee ~debug ~pyre_in_context ~override_graph ~call_indexer class_type =
  let meta_type = Type.builtins_type class_type in
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
          ~pyre_in_context
          ~override_graph
          ~call_indexer
          ~receiver_type:meta_type
          ~return_type:(lazy class_type)
          ~callee_kind:
            (Method { is_direct_call = true; is_static_method = true; is_class_method = false })
            (* __new__() is a static method. See
               https://docs.python.org/3/reference/datamodel.html#object.__new__ *)
          new_callable_type
      in
      let init_callees =
        resolve_callees_from_type
          ~debug
          ~pyre_in_context
          ~override_graph
          ~call_indexer
          ~receiver_type:meta_type
          ~return_type:(lazy Type.none)
          ~callee_kind:
            (Method { is_direct_call = true; is_static_method = false; is_class_method = false })
          init_callable_type
      in
      (* Technically, `object.__new__` returns `object` and `C.__init__` returns None.
       * In practice, we actually want to use the class type. *)
      let return_type =
        let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
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
           ~unresolved:(new_callees.unresolved || init_callees.unresolved)
           ())


let resolve_callee_from_defining_expression
    ~debug
    ~pyre_in_context
    ~override_graph
    ~call_indexer
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
      >>= fun { Analysis.AttributeResolution.Global.undecorated_signature; _ } ->
      undecorated_signature
      >>| fun undecorated_signature ->
      resolve_callees_from_type
        ~debug
        ~pyre_in_context
        ~override_graph
        ~call_indexer
        ~return_type
        ~callee_kind:Function
        (Type.Callable undecorated_signature)
  | _ -> (
      let implementing_class_name =
        if Type.is_builtins_type implementing_class then
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
               ~pyre_in_context
               ~override_graph
               ~call_indexer
               ~return_type
               ~receiver_type:implementing_class
               ~callee_kind:
                 (Method
                    { is_direct_call = false; is_class_method = false; is_static_method = false })
               callable_type)
      | _ -> None)


let resolve_stringify_call ~pyre_in_context expression =
  let string_callee =
    Node.create_with_default_location
      (Expression.Name (Name.Attribute { base = expression; attribute = "__str__"; special = true }))
  in
  try
    match
      CallResolution.resolve_ignoring_errors ~pyre_in_context string_callee |> Type.callable_name
    with
    | Some name when Reference.equal name (Reference.create "object.__str__") ->
        (* Call resolved to object.__str__, fallback to calling __repr__ if it exists. *)
        "__repr__"
    | _ -> "__str__"
  with
  | Analysis.ClassHierarchy.Untracked _ -> "__str__"


(* Rewrite certain calls for the interprocedural analysis (e.g, pysa).
 * This may or may not be sound depending on the analysis performed. *)
let transform_special_calls ~pyre_in_context { Call.callee; arguments } =
  let attribute_access base method_name =
    {
      Node.value =
        Expression.Name (Name.Attribute { base; attribute = method_name; special = true });
      location = Node.location callee;
    }
  in
  match Node.value callee, arguments with
  | Name (Name.Identifier "str"), [{ Call.Argument.value; _ }] ->
      (* str() takes an optional encoding and errors - if these are present, the call shouldn't be
         redirected: https://docs.python.org/3/library/stdtypes.html#str *)
      let callee = attribute_access value (resolve_stringify_call ~pyre_in_context value) in
      Some { Call.callee; arguments = [] }
  | Name (Name.Identifier "iter"), [{ Call.Argument.value; _ }] ->
      (* Only handle `iter` with a single argument here. *)
      Some { Call.callee = attribute_access value "__iter__"; arguments = [] }
  | Name (Name.Identifier "next"), [{ Call.Argument.value; _ }] ->
      (* Only handle `next` with a single argument here. *)
      Some { Call.callee = attribute_access value "__next__"; arguments = [] }
  | Name (Name.Identifier "anext"), [{ Call.Argument.value; _ }] ->
      (* Only handle `anext` with a single argument here. *)
      Some { Call.callee = attribute_access value "__anext__"; arguments = [] }
  | ( Expression.Name
        (Name.Attribute
          {
            base = { Node.value = Expression.Name (Name.Identifier "functools"); _ };
            attribute = "partial";
            _;
          }),
      { Call.Argument.value = actual_callable; _ } :: actual_arguments ) ->
      Some { Call.callee = actual_callable; arguments = actual_arguments }
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
      Some
        {
          Call.callee = process_callee;
          arguments =
            List.map process_arguments ~f:(fun value -> { Call.Argument.value; name = None });
        }
  | _ -> SpecialCallResolution.redirect ~pyre_in_context { Call.callee; arguments }


let redirect_special_calls ~pyre_in_context call =
  match transform_special_calls ~pyre_in_context call with
  | Some call -> call
  | None ->
      (* Rewrite certain calls using the same logic used in the type checker.
       * This should be sound for most analyses. *)
      PyrePysaApi.InContext.redirect_special_calls pyre_in_context call


let redirect_expressions ~pyre_in_context ~location = function
  | Expression.BinaryOperator { BinaryOperator.operator; left; right } ->
      Expression.Call
        {
          Call.callee =
            {
              Node.location = Node.location left;
              value =
                Expression.Name
                  (Name.Attribute
                     {
                       Name.Attribute.base = left;
                       attribute = BinaryOperator.binary_operator_method operator;
                       special = true;
                     });
            };
          arguments = [{ Call.Argument.name = None; value = right }];
        }
  | Expression.Subscript { Subscript.base; index } ->
      Expression.Call
        {
          callee =
            {
              Node.value =
                Expression.Name (Name.Attribute { base; attribute = "__getitem__"; special = true });
              location = Node.location base;
            };
          arguments = [{ Call.Argument.value = index; name = None }];
        }
  | Expression.Call call ->
      let call = redirect_special_calls ~pyre_in_context call in
      Expression.Call call
  | Expression.Slice slice -> Slice.lowered ~location slice |> Node.value
  | expression -> expression


let redirect_assignments = function
  | {
      Node.value = Statement.AugmentedAssign ({ AugmentedAssign.target; _ } as augmented_assignment);
      location;
    } ->
      let call = AugmentedAssign.lower ~location augmented_assignment in
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
                          Name (Name.Attribute { base; attribute = "__setitem__"; special = true });
                        location;
                      };
                    arguments = [index_argument; value_argument];
                  };
            };
      }
  | statement -> statement


let resolve_recognized_callees
    ~debug
    ~pyre_in_context
    ~override_graph
    ~call_indexer
    ~callee
    ~return_type
    ~callee_type
  =
  (* Special treatment for a set of hardcoded decorators returning callable classes. *)
  match Node.value callee, callee_type with
  | ( _,
      Type.Parametric
        {
          name = "BoundMethod";
          arguments = [Single (Parametric { name; _ }); Single implementing_class];
        } )
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      resolve_callee_from_defining_expression
        ~debug
        ~pyre_in_context
        ~override_graph
        ~call_indexer
        ~callee
        ~return_type
        ~implementing_class
  | Expression.Name (Name.Attribute { base; _ }), Parametric { name; _ }
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      (* Because of the special class, we don't get a bound method & lose the self argument for
         non-classmethod LRU cache wrappers. Reconstruct self in this case. *)
      CallResolution.resolve_ignoring_errors ~pyre_in_context base
      |> fun implementing_class ->
      resolve_callee_from_defining_expression
        ~debug
        ~pyre_in_context
        ~override_graph
        ~call_indexer
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
        let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
        ReturnType.from_annotation ~pyre_api (Lazy.force return_type)
      in
      CallCallees.create
        ~call_targets:
          [
            CallTargetIndexer.create_target
              call_indexer
              ~implicit_dunder_call:false
              ~return_type:(Some return_type)
              (Target.Function { name; kind = Normal });
          ]
        ()
  | _ -> None


let resolve_callee_ignoring_decorators
    ~debug
    ~pyre_in_context
    ~call_indexer
    ~override_graph
    ~return_type
    callee
  =
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
  let return_type () = ReturnType.from_annotation ~pyre_api (Lazy.force return_type) in
  let contain_class_method signatures =
    signatures
    |> List.exists ~f:(fun signature ->
           List.exists class_method_decorators ~f:(Define.Signature.has_decorator signature))
  in
  let log format =
    if debug then
      Log.dump format
    else
      Format.ifprintf Format.err_formatter format
  in
  let targets =
    match Node.value callee with
    | Expression.Name name when is_all_names (Node.value callee) -> (
        (* Resolving expressions that do not reference local variables or parameters. *)
        let name = Ast.Expression.name_to_reference_exn name in
        match PyrePysaApi.ReadOnly.resolve_exports pyre_api name with
        | Some
            (ResolvedReference.ModuleAttribute
              {
                export = ResolvedReference.Exported (Analysis.Module.Export.Name.Define _);
                remaining = [];
                _;
              }) ->
            [
              CallTargetIndexer.create_target
                call_indexer
                ~implicit_dunder_call:false
                ~return_type:(Some (return_type ()))
                (Target.Function { name = Reference.show name; kind = Normal });
            ]
        | Some
            (ResolvedReference.ModuleAttribute
              {
                from;
                name;
                export = ResolvedReference.Exported Analysis.Module.Export.Name.Class;
                remaining = [attribute];
                _;
              }) -> (
            let class_name = Reference.create ~prefix:from name |> Reference.show in
            PyrePysaApi.ReadOnly.get_class_summary pyre_api class_name
            >>| Node.value
            >>| ClassSummary.attributes
            >>= Identifier.SerializableMap.find_opt attribute
            >>| Node.value
            |> function
            | Some { kind = Method { static; signatures; _ }; _ } ->
                let is_class_method = contain_class_method signatures in
                [
                  CallTargetIndexer.create_target
                    call_indexer
                    ~implicit_dunder_call:false
                    ~return_type:(Some (return_type ()))
                    ~is_class_method
                    ~is_static_method:static
                    (Target.Method { Target.class_name; method_name = attribute; kind = Normal });
                ]
            | Some attribute ->
                let () =
                  log
                    "Bypassing decorators - Non-method attribute `%s` for callee `%s`"
                    (ClassSummary.Attribute.show_attribute attribute)
                    (Expression.show callee)
                in
                []
            | None ->
                let () =
                  log
                    "Bypassing decorators - Failed to find attribute `%s` for callee `%s`"
                    attribute
                    (Expression.show callee)
                in
                [])
        | _ ->
            let () =
              log
                "Bypassing decorators - Failed to resolve exports for callee `%s`"
                (Expression.show callee)
            in
            [])
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        (* Resolve `base.attribute` by looking up the type of `base` or the types of its parent
           classes in the Method Resolution Order. *)
        match CallResolution.resolve_ignoring_errors ~pyre_in_context base with
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
                >>| Node.value
                >>| ClassSummary.attributes
                >>= Identifier.SerializableMap.find_opt attribute
                >>| Node.value
              with
              | Some { ClassSummary.Attribute.kind = Method { static; signatures; _ }; _ } ->
                  Some (element, contain_class_method signatures, static)
              | _ -> None
            in
            let parent_classes_in_mro = PyrePysaApi.ReadOnly.successors pyre_api class_name in
            match List.find_map (class_name :: parent_classes_in_mro) ~f:find_attribute with
            | Some (base_class, is_class_method, is_static_method) ->
                let receiver_type =
                  (* Discard the type parameters, assuming they do not affect finding the actual
                     callee. *)
                  Type.Parametric
                    { name = "type"; arguments = [Single (Type.Primitive class_name)] }
                in
                Target.Method
                  { Target.class_name = base_class; method_name = attribute; kind = Normal }
                (* Over-approximately consider that any overriding method might be called. We
                   prioritize reducing false negatives than reducing false positives. *)
                |> compute_indirect_targets ~pyre_in_context ~override_graph ~receiver_type
                |> List.map
                     ~f:
                       (CallTargetIndexer.create_target
                          call_indexer
                          ~implicit_dunder_call:false
                          ~return_type:(Some (return_type ()))
                          ~is_class_method
                          ~is_static_method)
            | None ->
                let () =
                  log
                    "Bypassing decorators - Failed to find parent class of `%s` that defines \
                     method `%s`"
                    class_name
                    attribute
                in
                [])
        | _type ->
            let () =
              log
                "Bypassing decorators - Unknown base type `%s` in callee `%a`"
                (Type.show_type_t _type)
                Expression.pp
                callee
            in
            [])
    | _ ->
        let () = log "Bypassing decorators - Unknown callee `%a`" Expression.pp callee in
        []
  in
  let () =
    match targets with
    | [] when debug ->
        Log.dump
          "Bypassed decorators to resolve callees (using global resolution): Failed to resolve \
           callee %a"
          Expression.pp
          callee
    | targets when debug ->
        Log.dump
          "Bypassed decorators to resolve callees (using global resolution): `%s`"
          (targets |> List.map ~f:CallTarget.show |> String.concat ~sep:",")
    | _ -> ()
  in
  targets


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
    ~call_indexer
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
        AnnotatedAttribute.annotation property
        |> TypeInfo.Unit.annotation
        |> ReturnType.from_annotation ~pyre_api
    in
    let parent = AnnotatedAttribute.parent property |> Reference.create in
    let property_targets =
      let kind = if setter then Target.PropertySetter else Target.Normal in
      if Type.is_builtins_type base_type_info then
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
        (CallTargetIndexer.create_target
           call_indexer
           ~implicit_dunder_call:false
           ~return_type:(Some return_type))
      property_targets
  in
  let attributes = get_defining_attributes ~pyre_in_context ~base_type_info ~attribute in
  let properties, non_properties =
    List.partition_map
      ~f:(function
        | Some property when AnnotatedAttribute.property property -> Either.First property
        | attribute -> Either.Second attribute)
      attributes
  in
  let property_targets = List.concat_map ~f:property_targets_of_attribute properties in
  let is_attribute = (not (List.is_empty non_properties)) || List.is_empty attributes in
  { property_targets; is_attribute }


let as_identifier_reference ~define ~pyre_in_context expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) ->
      let reference = Reference.create identifier in
      if PyrePysaApi.InContext.is_global pyre_in_context ~reference then
        Some (IdentifierCallees.Global (Reference.delocalize reference))
      else if CallResolution.is_nonlocal ~pyre_in_context ~define reference then
        Some (IdentifierCallees.Nonlocal (Reference.delocalize reference))
      else
        None
  | Name name -> (
      name_to_reference name
      >>= fun reference ->
      PyrePysaApi.ReadOnly.resolve_exports
        (PyrePysaApi.InContext.pyre_api pyre_in_context)
        reference
      >>= function
      | ResolvedReference.ModuleAttribute { from; name; remaining = []; _ } ->
          Some (IdentifierCallees.Global (Reference.combine from (Reference.create name)))
      | _ -> None)
  | _ -> None


let is_builtin_reference = function
  | IdentifierCallees.Global reference -> reference |> Reference.single |> Option.is_some
  | Nonlocal _ -> false


let resolve_attribute_access_global_targets
    ~define
    ~pyre_in_context
    ~base_type_info
    ~base
    ~attribute
    ~special
  =
  let pyre_api = PyrePysaApi.InContext.pyre_api pyre_in_context in
  let expression =
    Expression.Name (Name.Attribute { Name.Attribute.base; attribute; special })
    |> Node.create_with_default_location
  in
  match as_identifier_reference ~define ~pyre_in_context expression with
  | Some (IdentifierCallees.Global global) -> [global]
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
              | Some attribute when AnnotatedAttribute.defined attribute ->
                  Type.Primitive (AnnotatedAttribute.parent attribute) |> Type.class_name
              | _ -> Type.class_name annotation
            in
            let attribute = Format.sprintf "__class__.%s" attribute in
            let target = Reference.create ~prefix:parent attribute in
            target :: targets
        | Type.Primitive class_name ->
            (* Access on an instance, i.e `self.foo`. *)
            let parents =
              let successors =
                match PyrePysaApi.ReadOnly.get_class_metadata pyre_api class_name with
                | Some
                    { Analysis.ClassSuccessorMetadataEnvironment.successors = Some successors; _ }
                  ->
                    successors
                | _ -> []
              in
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


let resolve_identifier ~define ~pyre_in_context ~call_indexer ~identifier =
  Expression.Name (Name.Identifier identifier)
  |> Node.create_with_default_location
  |> as_identifier_reference ~define ~pyre_in_context
  |> Option.filter ~f:(Fn.non is_builtin_reference)
  >>| function
  | IdentifierCallees.Global global ->
      {
        IdentifierCallees.global_targets =
          [
            CallTargetIndexer.create_target
              call_indexer
              ~implicit_dunder_call:false
              ~return_type:None
              (Target.create_object global);
          ];
        nonlocal_targets = [];
      }
  | Nonlocal nonlocal ->
      {
        IdentifierCallees.nonlocal_targets =
          [
            CallTargetIndexer.create_target
              call_indexer
              ~implicit_dunder_call:false
              ~return_type:None
              (Target.create_object nonlocal);
          ];
        global_targets = [];
      }


(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraphFixpoint (Context : sig
  val pyre_api : PyrePysaApi.ReadOnly.t

  val define_name : Reference.t

  val define : Ast.Statement.Define.t

  val qualifier : Reference.t

  val debug : bool

  val callees_at_location : UnprocessedLocationCallees.t Location.Table.t

  val override_graph : OverrideGraph.SharedMemory.ReadOnly.t option

  val call_indexer : CallTargetIndexer.t

  val is_missing_flow_type_analysis : bool

  val attribute_targets : Target.HashSet.t
end) =
struct
  type assignment_target = { location: Location.t }

  type visitor_t = {
    pyre_in_context: PyrePysaApi.InContext.t;
    assignment_target: assignment_target option;
  }

  let log format =
    if Context.debug then
      Log.dump format
    else
      Log.log ~section:`CallGraph format


  let override_graph = Context.override_graph

  let call_indexer = Context.call_indexer

  let attribute_targets = Context.attribute_targets

  let resolve_regular_callees ~pyre_in_context ~override_graph ~call_indexer ~return_type ~callee =
    let callee_type = CallResolution.resolve_ignoring_errors ~pyre_in_context callee in
    log
      "Checking if `%a` is a callable, resolved type is `%a`"
      Expression.pp
      callee
      Type.pp
      callee_type;
    let recognized_callees =
      resolve_recognized_callees
        ~debug:Context.debug
        ~pyre_in_context
        ~override_graph
        ~call_indexer
        ~callee
        ~return_type
        ~callee_type
      |> CallCallees.default_to_unresolved
    in
    if CallCallees.is_partially_resolved recognized_callees then
      let () = log "Recognized special callee:@,`%a`" CallCallees.pp recognized_callees in
      recognized_callees
    else
      let callee_kind = CalleeKind.from_callee ~pyre_in_context callee callee_type in
      let callees_from_type =
        resolve_callees_from_type
          ~debug:Context.debug
          ~pyre_in_context
          ~override_graph
          ~call_indexer
          ~return_type
          ~callee_kind
          callee_type
      in
      if CallCallees.is_partially_resolved callees_from_type then
        let () =
          log "Resolved callee from its resolved type:@,`%a`" CallCallees.pp callees_from_type
        in
        callees_from_type
      else
        resolve_callee_ignoring_decorators
          ~debug:Context.debug
          ~pyre_in_context
          ~call_indexer
          ~override_graph
          ~return_type
          callee
        |> function
        | [] -> CallCallees.unresolved ()
        | call_targets -> CallCallees.create ~call_targets ()


  let resolve_callees
      ~pyre_in_context
      ~override_graph
      ~call_indexer
      ~call:({ Call.callee; arguments } as call)
    =
    log
      "Resolving function call `%a`"
      Expression.pp
      (Expression.Call call |> Node.create_with_default_location);
    let higher_order_parameters =
      let get_higher_order_function_targets index { Call.Argument.value = argument; _ } =
        let return_type =
          lazy
            (Expression.Call { callee = argument; arguments = [] }
            |> Node.create_with_default_location
            |> CallResolution.resolve_ignoring_untracked ~pyre_in_context)
        in
        match
          ( resolve_regular_callees
              ~pyre_in_context
              ~override_graph
              ~call_indexer
              ~return_type
              ~callee:argument,
            argument )
        with
        | { CallCallees.call_targets = _ :: _ as regular_targets; unresolved; _ }, _ ->
            Some { HigherOrderParameter.index; call_targets = regular_targets; unresolved }
        | _, { Node.value = Expression.Lambda _; _ } ->
            Some { HigherOrderParameter.index; call_targets = []; unresolved = true }
        | _ -> None
      in
      List.filter_mapi arguments ~f:get_higher_order_function_targets
      |> HigherOrderParameterMap.from_list
    in
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
      resolve_regular_callees ~pyre_in_context ~override_graph ~call_indexer ~return_type ~callee
    in
    { regular_callees with higher_order_parameters }


  let resolve_attribute_access
      ~pyre_in_context
      ~override_graph
      ~call_indexer
      ~attribute_targets
      ~base
      ~attribute
      ~special
      ~setter
    =
    let base_type_info = CallResolution.resolve_ignoring_errors ~pyre_in_context base in

    log
      "Checking if `%s` is an attribute, property or global variable. Resolved type for base `%a` \
       is `%a`"
      attribute
      Expression.pp
      base
      Type.pp
      base_type_info;

    let { property_targets; is_attribute } =
      resolve_attribute_access_properties
        ~pyre_in_context
        ~override_graph
        ~call_indexer
        ~base_type_info
        ~attribute
        ~setter
    in

    let global_targets =
      resolve_attribute_access_global_targets
        ~define:Context.define_name
        ~pyre_in_context
        ~base_type_info
        ~base
        ~attribute
        ~special
      |> List.map ~f:Target.create_object
      (* Use a hashset here for faster lookups. *)
      |> List.filter ~f:(Hash_set.mem attribute_targets)
      |> List.map
           ~f:
             (CallTargetIndexer.create_target
                call_indexer
                ~implicit_dunder_call:false
                ~return_type:None)
    in

    { AttributeAccessCallees.property_targets; global_targets; is_attribute }


  (* For the missing flow analysis (`--find-missing-flows=type`), we turn unresolved
   * calls into sinks, so that we may find sources flowing into those calls. *)
  let add_unknown_callee
      ~expression:{ Node.value; location }
      ({ CallCallees.unresolved; call_targets; _ } as callees)
    =
    if unresolved && Context.is_missing_flow_type_analysis then
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
          Context.qualifier
          Location.pp
          location
          Expression.pp
          (callee |> Node.create_with_default_location |> Ast.Expression.delocalize)
      in
      let call_target =
        {
          CallTarget.target = Target.Object target;
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
    else
      callees


  module NodeVisitor = struct
    type nonrec t = visitor_t

    let expression_visitor
        ({ pyre_in_context; assignment_target } as state)
        ({ Node.value; location } as expression)
      =
      CallTargetIndexer.generate_fresh_indices call_indexer;
      let register_targets ~expression_identifier ?(location = location) callees =
        log
          "Resolved callees for expression `%a`:@,%a"
          Expression.pp
          expression
          ExpressionCallees.pp
          callees;
        Hashtbl.update Context.callees_at_location location ~f:(function
            | None -> UnprocessedLocationCallees.singleton ~expression_identifier ~callees
            | Some existing_callees ->
                UnprocessedLocationCallees.add existing_callees ~expression_identifier ~callees)
      in
      let value = redirect_expressions ~pyre_in_context ~location value in
      let () =
        match value with
        | Expression.Call call ->
            resolve_callees ~pyre_in_context ~override_graph ~call_indexer ~call
            |> add_unknown_callee ~expression
            |> ExpressionCallees.from_call
            |> register_targets ~expression_identifier:(call_identifier call)
        | Expression.Name (Name.Attribute { Name.Attribute.base; attribute; special }) ->
            let setter =
              match assignment_target with
              | Some { location = assignment_target_location } ->
                  Location.equal assignment_target_location location
              | None -> false
            in
            resolve_attribute_access
              ~pyre_in_context
              ~override_graph
              ~call_indexer
              ~attribute_targets
              ~base
              ~attribute
              ~special
              ~setter
            |> ExpressionCallees.from_attribute_access
            |> register_targets ~expression_identifier:attribute
        | Expression.Name (Name.Identifier identifier) ->
            resolve_identifier
              ~define:Context.define_name
              ~pyre_in_context
              ~call_indexer
              ~identifier
            >>| ExpressionCallees.from_identifier
            >>| register_targets ~expression_identifier:identifier
            |> ignore
        | Expression.BinaryOperator _ -> failwith "T191035448"
        | Expression.ComparisonOperator comparison -> (
            match ComparisonOperator.override ~location comparison with
            | Some { Node.value = Expression.Call call; _ } ->
                let call = redirect_special_calls ~pyre_in_context call in
                resolve_callees ~pyre_in_context ~override_graph ~call_indexer ~call
                |> add_unknown_callee ~expression
                |> ExpressionCallees.from_call
                |> register_targets ~expression_identifier:(call_identifier call)
            | _ -> ())
        | Expression.FormatString substrings ->
            let artificial_target =
              CallTargetIndexer.create_target
                call_indexer
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
                          let method_name = resolve_stringify_call ~pyre_in_context expression in
                          {
                            Node.value =
                              Expression.Name
                                (Name.Attribute
                                   { base = expression; attribute = method_name; special = false });
                            location = expression_location;
                          }
                        in
                        CallTargetIndexer.generate_fresh_indices call_indexer;
                        resolve_regular_callees
                          ~pyre_in_context
                          ~override_graph
                          ~call_indexer
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
            } ->
            resolve_attribute_access
              ~pyre_in_context
              ~override_graph
              ~call_indexer
              ~attribute_targets
              ~base
              ~attribute
              ~special:false
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
            } ->
            resolve_attribute_access
              ~pyre_in_context
              ~override_graph
              ~call_indexer
              ~attribute_targets
              ~base:self
              ~attribute
              ~special:true
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
        pyre_in_context = PyrePysaApi.InContext.resolve_assignment pyre_in_context assignment;
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
  end

  module CalleeVisitor = Visit.MakeNodeVisitor (NodeVisitor)

  include Analysis.Fixpoint.Make (struct
    type t = unit [@@deriving show]

    let bottom = ()

    let less_or_equal ~left:_ ~right:_ = true

    let join _ _ = ()

    let widen ~previous:_ ~next:_ ~iteration:_ = ()

    let forward_statement ~pyre_in_context ~statement =
      log "Building call graph of statement: `%a`" Ast.Statement.pp statement;
      let statement = redirect_assignments statement in
      match Node.value statement with
      | Statement.Assign { Assign.target; value = Some value; _ } ->
          CalleeVisitor.visit_expression
            ~state:
              (ref
                 { pyre_in_context; assignment_target = Some { location = Node.location target } })
            target;
          CalleeVisitor.visit_expression
            ~state:(ref { pyre_in_context; assignment_target = None })
            value
      | Statement.Assign { Assign.target; value = None; _ } ->
          CalleeVisitor.visit_expression
            ~state:
              (ref
                 { pyre_in_context; assignment_target = Some { location = Node.location target } })
            target
      | _ ->
          CalleeVisitor.visit_statement
            ~state:(ref { pyre_in_context; assignment_target = None })
            statement


    let forward ~statement_key _ ~statement =
      let pyre_in_context =
        PyrePysaApi.InContext.create_at_statement_key
          Context.pyre_api
          ~define_name:Context.define_name
          ~define:Context.define
          ~statement_key
      in
      forward_statement ~pyre_in_context ~statement


    let backward ~statement_key:_ _ ~statement:_ = ()
  end)
end

let call_graph_of_define
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.find_missing_flows; _ }
    ~pyre_api
    ~override_graph
    ~attribute_targets
    ~qualifier
    ~define
  =
  let name = Analysis.FunctionDefinition.qualified_name_of_define ~module_name:qualifier define in
  let timer = Timer.start () in
  let callees_at_location = Location.Table.create () in
  let module DefineFixpoint = DefineCallGraphFixpoint (struct
    let pyre_api = pyre_api

    let define_name = name

    let define = define

    let qualifier = qualifier

    let debug = Ast.Statement.Define.dump define || Ast.Statement.Define.dump_call_graph define

    let callees_at_location = callees_at_location

    let override_graph = override_graph

    let call_indexer = CallTargetIndexer.create ()

    let attribute_targets = attribute_targets

    let is_missing_flow_type_analysis =
      Option.equal
        Configuration.MissingFlowKind.equal
        find_missing_flows
        (Some Configuration.MissingFlowKind.Type)
  end)
  in
  let () = DefineFixpoint.log "Building call graph of `%a`" Reference.pp name in
  (* Handle parameters. *)
  let () =
    let pyre_in_context = PyrePysaApi.InContext.create_at_global_scope pyre_api in
    List.iter
      define.Ast.Statement.Define.signature.parameters
      ~f:(fun { Node.value = { Parameter.value; _ }; _ } ->
        Option.iter value ~f:(fun value ->
            DefineFixpoint.CalleeVisitor.visit_expression
              ~state:(ref { DefineFixpoint.pyre_in_context; assignment_target = None })
              value))
  in

  DefineFixpoint.forward ~cfg:(Cfg.create define) ~initial:() |> ignore;
  let call_graph =
    Hashtbl.to_alist callees_at_location
    |> List.map ~f:(fun (location, unprocessed_callees) ->
           match SerializableStringMap.to_alist unprocessed_callees with
           | [] -> failwith "unreachable"
           | [(_, callees)] ->
               location, LocationCallees.Singleton (ExpressionCallees.deduplicate callees)
           | _ ->
               ( location,
                 LocationCallees.Compound
                   (SerializableStringMap.map ExpressionCallees.deduplicate unprocessed_callees) ))
    |> List.filter ~f:(fun (_, callees) ->
           match callees with
           | LocationCallees.Singleton singleton ->
               not (ExpressionCallees.is_empty_attribute_access_callees singleton)
           | LocationCallees.Compound compound ->
               SerializableStringMap.exists
                 (fun _ callees ->
                   not (ExpressionCallees.is_empty_attribute_access_callees callees))
                 compound)
    |> Location.Map.Tree.of_alist_exn
  in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Call graph built"
    ~section:`DependencyGraph
    ~normals:["callable", Reference.show name]
    ~timer
    ();
  call_graph


let call_graph_of_callable
    ~static_analysis_configuration
    ~pyre_api
    ~override_graph
    ~attribute_targets
    ~callable
  =
  match Target.get_module_and_definition callable ~pyre_api with
  | None -> Format.asprintf "Found no definition for `%a`" Target.pp_pretty callable |> failwith
  | Some (qualifier, define) ->
      call_graph_of_define
        ~static_analysis_configuration
        ~pyre_api
        ~override_graph
        ~attribute_targets
        ~qualifier
        ~define:(Node.value define)


(** Call graphs of callables, stored in the shared memory. This is a mapping from a callable to its
    `DefineCallGraph.t`. *)
module DefineCallGraphSharedMemory = struct
  module T =
    SaveLoadSharedMemory.MakeKeyValue
      (Target.SharedMemoryKey)
      (struct
        type t = LocationCallees.t Location.Map.Tree.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let handle_prefix = Hack_parallel.Std.Prefix.make ()

        let description = "call graphs of defines"
      end)

  type t = T.t

  let add handle ~callable ~call_graph = T.add handle callable call_graph

  let create = T.create

  let merge_same_handle = T.merge_same_handle

  module ReadOnly = struct
    type t = T.ReadOnly.t

    let get handle ~callable = T.ReadOnly.get handle callable
  end

  let read_only = T.read_only

  let cleanup = T.cleanup

  let save_to_cache = T.save_to_cache

  let load_from_cache = T.load_from_cache
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

type call_graphs = {
  whole_program_call_graph: WholeProgramCallGraph.t;
  define_call_graphs: DefineCallGraphSharedMemory.t;
}

(** Build the whole call graph of the program.

    The overrides must be computed first because we depend on a global shared memory graph to
    include overrides in the call graph. Without it, we'll underanalyze and have an inconsistent
    fixpoint. *)
let build_whole_program_call_graph
    ~scheduler
    ~static_analysis_configuration:
      ({
         Configuration.StaticAnalysis.save_results_to;
         output_format;
         dump_call_graph;
         scheduler_policies;
         configuration = { local_root; _ };
         _;
       } as static_analysis_configuration)
    ~pyre_api
    ~resolve_module_path
    ~override_graph
    ~store_shared_memory
    ~attribute_targets
    ~skip_analysis_targets
    ~definitions
  =
  let attribute_targets = attribute_targets |> Target.Set.elements |> Target.HashSet.of_list in
  let define_call_graphs, whole_program_call_graph =
    let build_call_graph ((define_call_graphs, whole_program_call_graph) as so_far) callable =
      if Target.Set.mem callable skip_analysis_targets then
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
                ~callable)
            ()
        in
        let define_call_graphs =
          if store_shared_memory then
            DefineCallGraphSharedMemory.add
              define_call_graphs
              ~callable
              ~call_graph:callable_call_graph
          else
            define_call_graphs
        in
        let whole_program_call_graph =
          WholeProgramCallGraph.add_or_exn
            whole_program_call_graph
            ~callable
            ~callees:(DefineCallGraph.all_targets callable_call_graph)
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
      ( DefineCallGraphSharedMemory.merge_same_handle
          ~smaller:left_define_call_graphs
          ~larger:right_define_call_graphs,
        WholeProgramCallGraph.merge_disjoint
          left_whole_program_call_graph
          right_whole_program_call_graph )
    in
    let define_call_graphs = DefineCallGraphSharedMemory.create () in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.CallGraph
        ~default:
          (Scheduler.Policy.fixed_chunk_size
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunk_size:6000
             ())
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:(define_call_graphs, WholeProgramCallGraph.empty)
      ~map:(fun definitions ->
        List.fold
          definitions
          ~init:(define_call_graphs, WholeProgramCallGraph.empty)
          ~f:build_call_graph)
      ~reduce
      ~inputs:definitions
      ()
  in
  let define_call_graphs_read_only = DefineCallGraphSharedMemory.read_only define_call_graphs in
  let call_graph_to_json callable =
    match DefineCallGraphSharedMemory.ReadOnly.get define_call_graphs_read_only ~callable with
    | Some call_graph when not (DefineCallGraph.is_empty call_graph) ->
        [
          {
            NewlineDelimitedJson.Line.kind = CallGraph;
            data = DefineCallGraph.to_json ~pyre_api ~resolve_module_path ~callable call_graph;
          };
        ]
    | _ -> []
  in
  let () =
    match save_results_to with
    | Some directory ->
        Log.info "Writing the call graph to `%s`" (PyrePath.absolute directory);
        let () =
          match output_format with
          | Configuration.TaintOutputFormat.Json ->
              NewlineDelimitedJson.write_file
                ~path:(PyrePath.append directory ~element:"call-graph.json")
                ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
                ~to_json_lines:call_graph_to_json
                definitions
          | Configuration.TaintOutputFormat.ShardedJson ->
              NewlineDelimitedJson.remove_sharded_files ~directory ~filename_prefix:"call-graph";
              NewlineDelimitedJson.write_sharded_files
                ~scheduler
                ~directory
                ~filename_prefix:"call-graph"
                ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
                ~to_json_lines:call_graph_to_json
                definitions
        in
        ()
    | None -> ()
  in
  let () =
    match dump_call_graph with
    | Some path ->
        Log.warning "Emitting the contents of the call graph to `%s`" (PyrePath.absolute path);
        NewlineDelimitedJson.write_file
          ~path
          ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
          ~to_json_lines:call_graph_to_json
          definitions
    | None -> ()
  in
  { whole_program_call_graph; define_call_graphs }
