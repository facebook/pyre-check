(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast
open Statement
open Expression
open Pyre

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

  let bool = { is_boolean = true; is_integer = false; is_float = false; is_enumeration = false }

  let integer = { is_boolean = false; is_integer = true; is_float = true; is_enumeration = false }

  let from_annotation ~resolution annotation =
    let matches_at_leaves ~f annotation =
      let rec matches_at_leaves ~f annotation =
        match annotation with
        | Type.Any
        | Type.Bottom ->
            false
        | Type.Union [Type.NoneType; annotation]
        | Type.Union [annotation; Type.NoneType]
        | Type.Parametric { name = "typing.Awaitable"; parameters = [Single annotation] } ->
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
            GlobalResolution.less_or_equal resolution ~left ~right:Type.bool)
      in
      let is_integer =
        matches_at_leaves annotation ~f:(fun left ->
            GlobalResolution.less_or_equal resolution ~left ~right:Type.integer)
      in
      let is_float =
        matches_at_leaves annotation ~f:(fun left ->
            GlobalResolution.less_or_equal resolution ~left ~right:Type.float)
      in
      let is_enumeration =
        matches_at_leaves annotation ~f:(fun left ->
            GlobalResolution.less_or_equal resolution ~left ~right:Type.enumeration)
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
  let from_callable_with_fallback ~resolution ~callable_type ~return_type =
    let annotation =
      match callable_type with
      | Type.Callable { implementation = { annotation; _ }; overloads = []; _ }
        when Type.Variable.all_variables_are_resolved annotation ->
          annotation
      | _ -> Lazy.force return_type
    in
    from_annotation ~resolution:(Resolution.global_resolution resolution) annotation
end

module CallTarget = struct
  type t = {
    target: Target.t;
    implicit_self: bool;
    implicit_dunder_call: bool;
    collapse_tito: bool;
    index: int;
    return_type: ReturnType.t option;
    receiver_type: Type.t option;
  }
  [@@deriving compare, eq, show { with_path = false }]

  let target { target; _ } = target

  let equal_ignoring_indices left right = equal left { right with index = left.index }

  let dedup_and_sort targets =
    targets
    |> List.sort ~compare
    |> List.remove_consecutive_duplicates ~which_to_keep:`First ~equal:equal_ignoring_indices


  let create
      ?(implicit_self = false)
      ?(implicit_dunder_call = false)
      ?(collapse_tito = true)
      ?(index = 0)
      ?(return_type = Some ReturnType.any)
      ?receiver_type
      target
    =
    {
      target;
      implicit_self;
      implicit_dunder_call;
      collapse_tito;
      index;
      return_type;
      receiver_type;
    }


  let equal_ignoring_types
      {
        target = target_left;
        implicit_self = implicit_self_left;
        implicit_dunder_call = implicit_dunder_call_left;
        collapse_tito = collapse_tito_left;
        index = index_left;
        return_type = _;
        receiver_type = _;
      }
      {
        target = target_right;
        implicit_self = implicit_self_right;
        implicit_dunder_call = implicit_dunder_call_right;
        collapse_tito = collapse_tito_right;
        index = index_right;
        return_type = _;
        receiver_type = _;
      }
    =
    Target.equal target_left target_right
    && implicit_self_left == implicit_self_right
    && implicit_dunder_call_left == implicit_dunder_call_right
    && collapse_tito_left == collapse_tito_right
    && index_left == index_right
end

module HigherOrderParameter = struct
  type t = {
    index: int;
    call_targets: CallTarget.t list;
  }
  [@@deriving eq, show { with_path = false }]

  let join left right =
    match left, right with
    | Some { index = left_index; _ }, Some { index = right_index; _ } ->
        if left_index <= right_index then
          left
        else
          right
    | Some _, None -> left
    | None, Some _ -> right
    | None, None -> None


  let all_targets { call_targets; _ } = List.map ~f:CallTarget.target call_targets

  let equal_ignoring_types
      { index = index_left; call_targets = call_targets_left }
      { index = index_right; call_targets = call_targets_right }
    =
    index_left == index_right
    && List.equal CallTarget.equal_ignoring_types call_targets_left call_targets_right
end

module CallCallees = struct
  type t = {
    call_targets: CallTarget.t list;
    new_targets: CallTarget.t list;
    init_targets: CallTarget.t list;
    higher_order_parameter: HigherOrderParameter.t option;
    unresolved: bool;
  }
  [@@deriving eq, show { with_path = false }]

  let create
      ?(call_targets = [])
      ?(new_targets = [])
      ?(init_targets = [])
      ?higher_order_parameter
      ?(unresolved = false)
      ()
    =
    { call_targets; new_targets; init_targets; higher_order_parameter; unresolved }


  let unresolved =
    {
      call_targets = [];
      new_targets = [];
      init_targets = [];
      higher_order_parameter = None;
      unresolved = true;
    }


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
        higher_order_parameter = left_higher_order_parameter;
        unresolved = left_unresolved;
      }
      {
        call_targets = right_call_targets;
        new_targets = right_new_targets;
        init_targets = right_init_targets;
        higher_order_parameter = right_higher_order_parameter;
        unresolved = right_unresolved;
      }
    =
    let call_targets = List.rev_append left_call_targets right_call_targets in
    let new_targets = List.rev_append left_new_targets right_new_targets in
    let init_targets = List.rev_append left_init_targets right_init_targets in
    let higher_order_parameter =
      HigherOrderParameter.join left_higher_order_parameter right_higher_order_parameter
    in
    let unresolved = left_unresolved || right_unresolved in
    { call_targets; new_targets; init_targets; higher_order_parameter; unresolved }


  let deduplicate { call_targets; new_targets; init_targets; higher_order_parameter; unresolved } =
    let call_targets = CallTarget.dedup_and_sort call_targets in
    let new_targets = CallTarget.dedup_and_sort new_targets in
    let init_targets = CallTarget.dedup_and_sort init_targets in
    let higher_order_parameter =
      match higher_order_parameter with
      | Some { HigherOrderParameter.index; call_targets } ->
          Some { HigherOrderParameter.index; call_targets = CallTarget.dedup_and_sort call_targets }
      | None -> None
    in
    { call_targets; new_targets; init_targets; higher_order_parameter; unresolved }


  let all_targets { call_targets; new_targets; init_targets; higher_order_parameter; _ } =
    call_targets
    |> List.rev_append new_targets
    |> List.rev_append init_targets
    |> List.map ~f:CallTarget.target
    |> List.rev_append
         (higher_order_parameter >>| HigherOrderParameter.all_targets |> Option.value ~default:[])


  let equal_ignoring_types
      {
        call_targets = call_targets_left;
        new_targets = new_targets_left;
        init_targets = init_targets_left;
        higher_order_parameter = higher_order_parameter_left;
        unresolved = unresolved_left;
      }
      {
        call_targets = call_targets_right;
        new_targets = new_targets_right;
        init_targets = init_targets_right;
        higher_order_parameter = higher_order_parameter_right;
        unresolved = unresolved_right;
      }
    =
    List.equal CallTarget.equal_ignoring_types call_targets_left call_targets_right
    && List.equal CallTarget.equal_ignoring_types new_targets_left new_targets_right
    && List.equal CallTarget.equal_ignoring_types init_targets_left init_targets_right
    && Option.equal
         HigherOrderParameter.equal_ignoring_types
         higher_order_parameter_left
         higher_order_parameter_right
    && unresolved_left == unresolved_right
end

module AttributeAccessCallees = struct
  type t = {
    property_targets: CallTarget.t list;
    global_targets: CallTarget.t list;
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
    && is_attribute_left == is_attribute_right


  let empty = { property_targets = []; global_targets = []; is_attribute = true }

  let is_empty attribute_access_callees = equal attribute_access_callees empty
end

module IdentifierCallees = struct
  type t = { global_targets: CallTarget.t list } [@@deriving eq, show { with_path = false }]

  let deduplicate { global_targets } = { global_targets = CallTarget.dedup_and_sort global_targets }

  let join { global_targets = left_global_targets } { global_targets = right_global_targets } =
    { global_targets = List.rev_append left_global_targets right_global_targets }


  let all_targets { global_targets } = List.map ~f:CallTarget.target global_targets
end

module FormatStringCallees = struct
  type t = { call_targets: CallTarget.t list } [@@deriving eq, show { with_path = false }]

  let deduplicate { call_targets } =
    { call_targets = List.dedup_and_sort ~compare:CallTarget.compare call_targets }


  let join { call_targets = left_call_targets } { call_targets = right_call_targets } =
    { call_targets = List.rev_append left_call_targets right_call_targets }


  let all_targets { call_targets } = List.map ~f:CallTarget.target call_targets
end

module ExpressionCallees = struct
  type t = {
    call: CallCallees.t option;
    attribute_access: AttributeAccessCallees.t option;
    identifier: IdentifierCallees.t option;
    format_string: FormatStringCallees.t option;
  }
  [@@deriving eq, show { with_path = false }]

  let from_call callees =
    { call = Some callees; attribute_access = None; identifier = None; format_string = None }


  let from_call_with_empty_attribute callees =
    {
      call = Some callees;
      attribute_access = Some AttributeAccessCallees.empty;
      identifier = None;
      format_string = None;
    }


  let from_attribute_access properties =
    { call = None; attribute_access = Some properties; identifier = None; format_string = None }


  let from_identifier identifier =
    { call = None; attribute_access = None; identifier = Some identifier; format_string = None }


  let from_format_string format_string =
    { call = None; attribute_access = None; identifier = None; format_string = Some format_string }


  let join
      {
        call = left_call;
        attribute_access = left_attribute_access;
        identifier = left_identifier;
        format_string = left_format_string;
      }
      {
        call = right_call;
        attribute_access = right_attribute_access;
        identifier = right_identifier;
        format_string = right_format_string;
      }
    =
    {
      call = Option.merge ~f:CallCallees.join left_call right_call;
      attribute_access =
        Option.merge ~f:AttributeAccessCallees.join left_attribute_access right_attribute_access;
      identifier = Option.merge ~f:IdentifierCallees.join left_identifier right_identifier;
      format_string =
        Option.merge ~f:FormatStringCallees.join left_format_string right_format_string;
    }


  let deduplicate { call; attribute_access; identifier; format_string } =
    {
      call = call >>| CallCallees.deduplicate;
      attribute_access = attribute_access >>| AttributeAccessCallees.deduplicate;
      identifier = identifier >>| IdentifierCallees.deduplicate;
      format_string = format_string >>| FormatStringCallees.deduplicate;
    }


  let all_targets { call; attribute_access; identifier; format_string } =
    let call_targets = call >>| CallCallees.all_targets |> Option.value ~default:[] in
    let attribute_access_targets =
      attribute_access >>| AttributeAccessCallees.all_targets |> Option.value ~default:[]
    in
    let identifier_targets =
      identifier >>| IdentifierCallees.all_targets |> Option.value ~default:[]
    in
    let format_string_targets =
      format_string >>| FormatStringCallees.all_targets |> Option.value ~default:[]
    in
    call_targets
    |> List.rev_append attribute_access_targets
    |> List.rev_append identifier_targets
    |> List.rev_append format_string_targets


  let is_empty_attribute_access_callees = function
    | {
        call = None;
        attribute_access = Some some_attribute_access;
        identifier = None;
        format_string = None;
      } ->
        AttributeAccessCallees.is_empty some_attribute_access
    | _ -> false


  let equal_ignoring_types
      {
        call = call_left;
        attribute_access = attribute_access_left;
        identifier = identifier_left;
        format_string = format_string_left;
      }
      {
        call = call_right;
        attribute_access = attribute_access_right;
        identifier = identifier_right;
        format_string = format_string_right;
      }
    =
    Option.equal CallCallees.equal_ignoring_types call_left call_right
    && Option.equal
         AttributeAccessCallees.equal_ignoring_types
         attribute_access_left
         attribute_access_right
    && Option.equal IdentifierCallees.equal identifier_left identifier_right
    && Option.equal FormatStringCallees.equal format_string_left format_string_right
end

module LocationCallees = struct
  type t =
    | Singleton of ExpressionCallees.t
    | Compound of ExpressionCallees.t String.Map.Tree.t
  [@@deriving eq]

  let pp formatter = function
    | Singleton callees -> Format.fprintf formatter "%a" ExpressionCallees.pp callees
    | Compound map ->
        String.Map.Tree.to_alist map
        |> List.map ~f:(fun (key, value) -> Format.asprintf "%s: %a" key ExpressionCallees.pp value)
        |> String.concat ~sep:", "
        |> Format.fprintf formatter "%s"


  let show callees = Format.asprintf "%a" pp callees

  let all_targets = function
    | Singleton raw_callees -> ExpressionCallees.all_targets raw_callees
    | Compound map -> String.Map.Tree.data map |> List.concat_map ~f:ExpressionCallees.all_targets


  let equal_ignoring_types location_callees_left location_callees_right =
    match location_callees_left, location_callees_right with
    | Singleton callees_left, Singleton callees_right ->
        ExpressionCallees.equal_ignoring_types callees_left callees_right
    | Compound map_left, Compound map_right ->
        String.Map.Tree.equal ExpressionCallees.equal_ignoring_types map_left map_right
    | _ -> false
end

module UnprocessedLocationCallees = struct
  type t = ExpressionCallees.t String.Map.Tree.t

  let singleton ~expression_identifier ~callees =
    String.Map.Tree.singleton expression_identifier callees


  let add map ~expression_identifier ~callees =
    String.Map.Tree.update map expression_identifier ~f:(function
        | Some existing_callees -> ExpressionCallees.join existing_callees callees
        | None -> callees)
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


module DefineCallGraph = struct
  type t = LocationCallees.t Location.Map.t [@@deriving eq]

  let pp formatter call_graph =
    let pp_pair formatter (key, value) =
      Format.fprintf formatter "@,%a -> %a" Location.pp key LocationCallees.pp value
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    call_graph |> Location.Map.to_alist |> Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs


  let show = Format.asprintf "%a" pp

  let empty = Location.Map.empty

  let add call_graph ~location ~callees = Location.Map.set call_graph ~key:location ~data:callees

  let resolve_expression call_graph ~location ~expression_identifier =
    match Location.Map.find call_graph location with
    | Some (LocationCallees.Singleton callees) -> Some callees
    | Some (LocationCallees.Compound name_to_callees) ->
        String.Map.Tree.find name_to_callees expression_identifier
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


  let format_string_expression_identifier = "$__str__$"

  let resolve_format_string call_graph ~location =
    resolve_expression
      call_graph
      ~location
      ~expression_identifier:format_string_expression_identifier
    >>= fun { format_string; _ } -> format_string


  let equal_ignoring_types call_graph_left call_graph_right =
    Location.Map.equal LocationCallees.equal_ignoring_types call_graph_left call_graph_right
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
    Target.Set.iter (Target.HashMap.incr indexer.indices) indexer.seen_targets;
    indexer.seen_targets <- Target.Set.empty


  let create_target
      indexer
      ~implicit_self
      ~implicit_dunder_call
      ~collapse_tito
      ~return_type
      ?receiver_type
      original_target
    =
    let target_for_index = Target.override_to_method original_target in
    let index = Target.HashMap.find indexer.indices target_for_index |> Option.value ~default:0 in
    indexer.seen_targets <- Target.Set.add target_for_index indexer.seen_targets;
    {
      CallTarget.target = original_target;
      implicit_self;
      implicit_dunder_call;
      collapse_tito;
      index;
      return_type;
      receiver_type;
    }
end

type callee_kind =
  | Method of { is_direct_call: bool }
  | Function

let is_local identifier = String.is_prefix ~prefix:"$" identifier

let rec is_all_names = function
  | Expression.Name (Name.Identifier identifier) when not (is_local identifier) -> true
  | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
      is_all_names (Node.value base)
  | _ -> false


let rec callee_kind ~resolution callee callee_type =
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
          let parent_type = CallResolution.resolve_ignoring_optional ~resolution base in
          let is_class () =
            parent_type
            |> GlobalResolution.class_summary (Resolution.global_resolution resolution)
            |> Option.is_some
          in
          if Type.is_meta parent_type then
            Method { is_direct_call = true }
          else if is_class () then
            Method { is_direct_call = false }
          else
            Function
      | _ -> Function)
  | Type.Union (callee_type :: _) -> callee_kind ~resolution callee callee_type
  | _ ->
      (* We must be dealing with a callable class. *)
      Method { is_direct_call = false }


let strip_optional annotation = Type.optional_value annotation |> Option.value ~default:annotation

let strip_meta annotation =
  if Type.is_meta annotation then
    Type.single_parameter annotation
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
let compute_indirect_targets ~resolution ~receiver_type implementation_target =
  (* Target name must be the resolved implementation target *)
  let global_resolution = Resolution.global_resolution resolution in
  let get_class_type = GlobalResolution.parse_reference global_resolution in
  let get_actual_target method_name =
    if DependencyGraphSharedMemory.overrides_exist method_name then
      Target.create_override method_name
    else
      Target.create_method method_name
  in
  let receiver_type = receiver_type |> strip_meta |> strip_optional |> Type.weaken_literals in
  let declaring_type = Reference.prefix implementation_target in
  if
    declaring_type
    >>| Reference.equal (Type.class_name receiver_type)
    |> Option.value ~default:false
  then (* case a *)
    [get_actual_target implementation_target]
  else
    let target_callable = Target.create_method implementation_target in
    match DependencyGraphSharedMemory.get_overriding_types ~member:implementation_target with
    | None ->
        (* case b *)
        [target_callable]
    | Some overriding_types ->
        (* case c *)
        let keep_subtypes candidate =
          let candidate_type = get_class_type candidate in
          try
            GlobalResolution.less_or_equal
              global_resolution
              ~left:candidate_type
              ~right:receiver_type
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
            let method_name = Reference.last implementation_target in
            Reference.create ~prefix:class_name method_name |> get_actual_target
          in
          List.filter overriding_types ~f:keep_subtypes
          |> fun subtypes -> List.map subtypes ~f:create_override_target
        in
        target_callable :: override_targets


let collapse_tito ~resolution ~callee ~callable_type =
  (* For most cases, it is simply incorrect to not collapse tito, as it will lead to incorrect
   * mapping from input to output taint. However, the collapsing of tito adversely affects our
   * analysis in the case of the builder pattern, i.e.
   *
   * class C:
   *  def set_field(self, field) -> "C":
   *    self.field = field
   *    return self
   *
   * In this case, collapsing tito leads to field
   * tainting the entire `self` for chained call. To prevent this problem, we special case
   * builders to preserve the tito structure. *)
  match callable_type with
  | Type.Parametric { name = "BoundMethod"; parameters = [_; Type.Parameter.Single implicit] } ->
      let return_annotation =
        (* To properly substitute type variables, we simulate `callee.__call__` for the bound
           method. *)
        let to_simulate =
          Node.create_with_default_location
            (Expression.Name
               (Name.Attribute { base = callee; attribute = "__call__"; special = true }))
        in
        match CallResolution.resolve_ignoring_untracked ~resolution to_simulate with
        | Type.Callable { Type.Callable.implementation; _ } ->
            Type.Callable.Overload.return_annotation implementation
        | _ -> Type.Top
      in
      not (Type.equal implicit return_annotation)
  | _ -> true


let rec resolve_callees_from_type
    ~resolution
    ~call_indexer
    ?(dunder_call = false)
    ?receiver_type
    ~return_type
    ~callee_kind
    ~collapse_tito
    callable_type
  =
  let resolve_callees_from_type ?(dunder_call = dunder_call) =
    resolve_callees_from_type ~dunder_call
  in
  match callable_type with
  | Type.Callable { kind = Named name; _ } -> (
      let return_type =
        ReturnType.from_callable_with_fallback ~resolution ~callable_type ~return_type
      in
      match receiver_type with
      | Some receiver_type ->
          let targets =
            match callee_kind with
            | Method { is_direct_call = true } -> [Target.create_method name]
            | _ -> compute_indirect_targets ~resolution ~receiver_type name
          in
          let targets =
            List.map
              ~f:(fun target ->
                CallTargetIndexer.create_target
                  call_indexer
                  ~implicit_self:true
                  ~implicit_dunder_call:dunder_call
                  ~collapse_tito
                  ~return_type:(Some return_type)
                  ~receiver_type
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
                  ~implicit_self:false
                  ~implicit_dunder_call:dunder_call
                  ~collapse_tito
                  ~return_type:(Some return_type)
                  ?receiver_type
                  target;
              ]
            ())
  | Type.Callable { kind = Anonymous; _ } -> CallCallees.unresolved
  | Type.Parametric { name = "BoundMethod"; parameters = [Single callable; Single receiver_type] }
    ->
      resolve_callees_from_type
        ~resolution
        ~call_indexer
        ~receiver_type
        ~return_type
        ~callee_kind
        ~collapse_tito
        callable
  | Type.Union (element :: elements) ->
      let first_targets =
        resolve_callees_from_type
          ~resolution
          ~call_indexer
          ~callee_kind
          ?receiver_type
          ~return_type
          ~collapse_tito
          element
      in
      List.fold elements ~init:first_targets ~f:(fun combined_targets new_target ->
          resolve_callees_from_type
            ~resolution
            ~call_indexer
            ?receiver_type
            ~return_type
            ~callee_kind
            ~collapse_tito
            new_target
          |> CallCallees.join combined_targets)
  | Type.Parametric { name = "type"; parameters = [Single class_type] } ->
      resolve_constructor_callee ~resolution ~call_indexer class_type
      |> Option.value ~default:CallCallees.unresolved
  | callable_type -> (
      (* Handle callable classes. `typing.Type` interacts specially with __call__, so we choose to
         ignore it for now to make sure our constructor logic via `cls()` still works. *)
      match
        CallResolution.resolve_attribute_access_ignoring_untracked
          ~resolution
          ~base_type:callable_type
          ~attribute:"__call__"
      with
      | Type.Any
      | Type.Top ->
          CallCallees.unresolved
      (* Callable protocol. *)
      | Type.Callable { kind = Anonymous; _ } as resolved_dunder_call ->
          Type.primitive_name callable_type
          >>| (fun primitive_callable_name ->
                let return_type =
                  ReturnType.from_callable_with_fallback
                    ~resolution
                    ~callable_type:resolved_dunder_call
                    ~return_type
                in
                let target =
                  Target.Method
                    { Target.class_name = primitive_callable_name; method_name = "__call__" }
                in
                CallCallees.create
                  ~call_targets:
                    [
                      CallTargetIndexer.create_target
                        call_indexer
                        ~implicit_self:true
                        ~implicit_dunder_call:true
                        ~collapse_tito
                        ~return_type:(Some return_type)
                        ?receiver_type
                        target;
                    ]
                  ())
          |> Option.value ~default:CallCallees.unresolved
      | annotation ->
          if not dunder_call then
            resolve_callees_from_type
              ~resolution
              ~call_indexer
              ~return_type
              ~dunder_call:true
              ~callee_kind
              ~collapse_tito
              annotation
          else
            CallCallees.unresolved)


and resolve_constructor_callee ~resolution ~call_indexer class_type =
  let meta_type = Type.meta class_type in
  match
    ( CallResolution.resolve_attribute_access_ignoring_untracked
        ~resolution
        ~base_type:meta_type
        ~attribute:"__new__",
      CallResolution.resolve_attribute_access_ignoring_untracked
        ~resolution
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
          ~resolution
          ~call_indexer
          ~receiver_type:meta_type
          ~return_type:(lazy class_type)
          ~callee_kind:(Method { is_direct_call = true })
          ~collapse_tito:true
          new_callable_type
      in
      let init_callees =
        resolve_callees_from_type
          ~resolution
          ~call_indexer
          ~receiver_type:meta_type
          ~return_type:(lazy Type.none)
          ~callee_kind:(Method { is_direct_call = true })
          ~collapse_tito:true
          init_callable_type
      in
      (* Technically, `object.__new__` returns `object` and `C.__init__` returns None.
       * In practice, we actually want to use the class type. *)
      let return_type =
        ReturnType.from_annotation ~resolution:(Resolution.global_resolution resolution) class_type
      in
      let set_return_type call_target =
        { call_target with CallTarget.return_type = Some return_type }
      in
      Some
        (CallCallees.create
           ~new_targets:(List.map ~f:set_return_type new_callees.call_targets)
           ~init_targets:(List.map ~f:set_return_type init_callees.call_targets)
           ~unresolved:(new_callees.unresolved || init_callees.unresolved)
           ())


let resolve_callee_from_defining_expression
    ~resolution
    ~call_indexer
    ~callee:{ Node.value = callee; _ }
    ~return_type
    ~implementing_class
  =
  match implementing_class, callee with
  | Type.Top, Expression.Name name when is_all_names callee ->
      (* If implementing_class is unknown, this must be a function rather than a method. We can use
         global resolution on the callee. *)
      GlobalResolution.global
        (Resolution.global_resolution resolution)
        (Ast.Expression.name_to_reference_exn name)
      >>= fun { AttributeResolution.Global.undecorated_signature; _ } ->
      undecorated_signature
      >>| fun undecorated_signature ->
      resolve_callees_from_type
        ~resolution
        ~call_indexer
        ~return_type
        ~callee_kind:Function
        ~collapse_tito:true
        (Type.Callable undecorated_signature)
  | _ -> (
      let implementing_class_name =
        if Type.is_meta implementing_class then
          Type.parameters implementing_class
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
               ~resolution
               ~call_indexer
               ~return_type
               ~receiver_type:implementing_class
               ~callee_kind:(Method { is_direct_call = false })
               ~collapse_tito:true
               callable_type)
      | _ -> None)


let transform_special_calls ~resolution { Call.callee; arguments } =
  match callee, arguments with
  | ( {
        Node.value =
          Expression.Name
            (Name.Attribute
              {
                base = { Node.value = Expression.Name (Name.Identifier "functools"); _ };
                attribute = "partial";
                _;
              });
        _;
      },
      { Call.Argument.value = actual_callable; _ } :: actual_arguments ) ->
      Some { Call.callee = actual_callable; arguments = actual_arguments }
  | ( {
        Node.value =
          Name
            (Name.Attribute
              {
                base = { Node.value = Expression.Name (Name.Identifier "multiprocessing"); _ };
                attribute = "Process";
                _;
              });
        _;
      },
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
  | _ -> SpecialCallResolution.redirect ~resolution { Call.callee; arguments }


let redirect_special_calls ~resolution call =
  match transform_special_calls ~resolution call with
  | Some call -> call
  | None -> Annotated.Call.redirect_special_calls ~resolution call


let resolve_recognized_callees ~resolution ~call_indexer ~callee ~return_type ~callee_type =
  (* Special treatment for a set of hardcoded decorators returning callable classes. *)
  match Node.value callee, callee_type with
  | ( _,
      Type.Parametric
        {
          name = "BoundMethod";
          parameters = [Single (Parametric { name; _ }); Single implementing_class];
        } )
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      resolve_callee_from_defining_expression
        ~resolution
        ~call_indexer
        ~callee
        ~return_type
        ~implementing_class
  | Expression.Name (Name.Attribute { base; _ }), Parametric { name; _ }
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      (* Because of the special class, we don't get a bound method & lose the self argument for
         non-classmethod LRU cache wrappers. Reconstruct self in this case. *)
      CallResolution.resolve_ignoring_optional ~resolution base
      |> fun implementing_class ->
      resolve_callee_from_defining_expression
        ~resolution
        ~call_indexer
        ~callee
        ~return_type
        ~implementing_class
  | Expression.Name name, _
    when is_all_names (Node.value callee)
         && Type.Set.mem SpecialCallResolution.recognized_callable_target_types callee_type ->
      Ast.Expression.name_to_reference name
      >>| Reference.show
      >>| fun name ->
      let collapse_tito = collapse_tito ~resolution ~callee ~callable_type:callee_type in
      let return_type =
        ReturnType.from_annotation
          ~resolution:(Resolution.global_resolution resolution)
          (Lazy.force return_type)
      in
      CallCallees.create
        ~call_targets:
          [
            CallTargetIndexer.create_target
              call_indexer
              ~implicit_self:false
              ~implicit_dunder_call:false
              ~collapse_tito
              ~return_type:(Some return_type)
              (Target.Function name);
          ]
        ()
  | _ -> None


let resolve_callee_ignoring_decorators ~resolution ~call_indexer ~collapse_tito ~return_type callee =
  let global_resolution = Resolution.global_resolution resolution in
  let open UnannotatedGlobalEnvironment in
  let return_type () =
    ReturnType.from_annotation
      ~resolution:(Resolution.global_resolution resolution)
      (Lazy.force return_type)
  in
  match Node.value callee with
  | Expression.Name name when is_all_names (Node.value callee) -> (
      (* Resolving expressions that do not reference local variables or parameters. *)
      let name = Ast.Expression.name_to_reference_exn name in
      match GlobalResolution.resolve_exports global_resolution name with
      | Some
          (ResolvedReference.ModuleAttribute
            { export = ResolvedReference.Exported (Module.Export.Name.Define _); remaining = []; _ })
        ->
          Some
            (CallTargetIndexer.create_target
               call_indexer
               ~implicit_self:false
               ~implicit_dunder_call:false
               ~return_type:(Some (return_type ()))
               ~collapse_tito
               (Target.Function (Reference.show name)))
      | Some
          (ResolvedReference.ModuleAttribute
            {
              from;
              name;
              export = ResolvedReference.Exported Module.Export.Name.Class;
              remaining = [attribute];
              _;
            }) -> (
          let class_name = Reference.create ~prefix:from name |> Reference.show in
          GlobalResolution.class_summary global_resolution (Type.Primitive class_name)
          >>| Node.value
          >>| ClassSummary.attributes
          >>= Identifier.SerializableMap.find_opt attribute
          >>| Node.value
          >>= function
          | { kind = Method { static; _ }; _ } ->
              Some
                (CallTargetIndexer.create_target
                   call_indexer
                   ~implicit_self:(not static)
                   ~implicit_dunder_call:false
                   ~collapse_tito
                   ~return_type:(Some (return_type ()))
                   (Target.Method { Target.class_name; method_name = attribute }))
          | _ -> None)
      | _ -> None)
  | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
      (* Resolve `base.attribute` by looking up the type of `base` or the types of its parent
         classes in the Method Resolution Order. *)
      match CallResolution.resolve_ignoring_optional ~resolution base with
      | Type.Primitive class_name
      | Type.Parametric { name = "type"; parameters = [Single (Type.Primitive class_name)] } -> (
          let find_attribute element =
            match
              GlobalResolution.class_summary global_resolution (Type.Primitive element)
              >>| Node.value
              >>| ClassSummary.attributes
              >>= Identifier.SerializableMap.find_opt attribute
              >>| Node.value
            with
            | Some { ClassSummary.Attribute.kind = Method _; _ } -> Some element
            | _ -> None
          in
          let parent_classes_in_mro =
            GlobalResolution.successors ~resolution:global_resolution class_name
          in
          match List.find_map (class_name :: parent_classes_in_mro) ~f:find_attribute with
          | Some base_class ->
              Some
                (CallTargetIndexer.create_target
                   call_indexer
                   ~implicit_self:true
                   ~implicit_dunder_call:false
                   ~return_type:(Some (return_type ()))
                   ~collapse_tito
                   (Target.Method { Target.class_name = base_class; method_name = attribute }))
          | None -> None)
      | _ -> None)
  | _ -> None


let resolve_regular_callees ~resolution ~call_indexer ~return_type ~callee =
  let callee_type = CallResolution.resolve_ignoring_optional ~resolution callee in
  let recognized_callees =
    resolve_recognized_callees ~resolution ~call_indexer ~callee ~return_type ~callee_type
    |> Option.value ~default:CallCallees.unresolved
  in
  if CallCallees.is_partially_resolved recognized_callees then
    recognized_callees
  else
    let callee_kind = callee_kind ~resolution callee callee_type in
    let collapse_tito = collapse_tito ~resolution ~callee ~callable_type:callee_type in
    let calleees_from_type =
      resolve_callees_from_type
        ~resolution
        ~call_indexer
        ~return_type
        ~callee_kind
        ~collapse_tito
        callee_type
    in
    if CallCallees.is_partially_resolved calleees_from_type then
      calleees_from_type
    else
      resolve_callee_ignoring_decorators
        ~resolution
        ~call_indexer
        ~return_type
        ~collapse_tito
        callee
      >>| (fun target -> CallCallees.create ~call_targets:[target] ())
      |> Option.value ~default:CallCallees.unresolved


let resolve_callees ~resolution ~call_indexer ~call:({ Call.callee; arguments } as call) =
  let higher_order_parameter =
    let get_higher_order_function_targets index { Call.Argument.value = argument; _ } =
      let return_type =
        lazy
          (Expression.Call { callee = argument; arguments = [] }
          |> Node.create_with_default_location
          |> CallResolution.resolve_ignoring_untracked ~resolution)
      in
      match resolve_regular_callees ~resolution ~call_indexer ~return_type ~callee:argument with
      | { CallCallees.call_targets = _ :: _ as regular_targets; _ } ->
          Some { HigherOrderParameter.index; call_targets = regular_targets }
      | _ -> None
    in
    List.find_mapi arguments ~f:get_higher_order_function_targets
  in
  (* Resolving the return type can be costly, hence we prefer the annotation on the callee when
     possible. When that does not work, we fallback to a full resolution of the call expression
     (done lazily). *)
  let return_type =
    lazy
      (Expression.Call call
      |> Node.create_with_default_location
      |> CallResolution.resolve_ignoring_untracked ~resolution)
  in
  let regular_callees = resolve_regular_callees ~resolution ~call_indexer ~return_type ~callee in
  { regular_callees with higher_order_parameter }


let get_defining_attributes ~resolution ~base_annotation ~attribute =
  let rec get_defining_parents annotation =
    match annotation with
    | Type.Union annotations
    | Type.Variable { Type.Variable.Unary.constraints = Type.Variable.Explicit annotations; _ } ->
        List.concat_map annotations ~f:get_defining_parents
    | _ -> [CallResolution.defining_attribute ~resolution annotation attribute]
  in
  base_annotation |> strip_meta |> strip_optional |> get_defining_parents


type attribute_access_properties = {
  property_targets: CallTarget.t list;
  is_attribute: bool;
}

let resolve_attribute_access_properties
    ~resolution
    ~call_indexer
    ~base_annotation
    ~attribute
    ~setter
  =
  let property_targets_of_attribute property =
    let return_type =
      if setter then
        ReturnType.none
      else
        Annotated.Attribute.annotation property
        |> Annotation.annotation
        |> ReturnType.from_annotation ~resolution:(Resolution.global_resolution resolution)
    in
    let parent = Annotated.Attribute.parent property |> Reference.create in
    let property_targets =
      if Type.is_meta base_annotation then
        [Target.create_method (Reference.create ~prefix:parent attribute)]
      else
        let callee = Reference.create ~prefix:parent attribute in
        compute_indirect_targets ~resolution ~receiver_type:base_annotation callee
    in
    let property_targets =
      if setter then
        let to_setter target =
          match target with
          | Target.Override { Target.class_name; method_name } ->
              Target.Override { Target.class_name; method_name = method_name ^ "$setter" }
          | Target.Method { Target.class_name; method_name } ->
              Target.Method { Target.class_name; method_name = method_name ^ "$setter" }
          | _ -> target
        in
        List.map property_targets ~f:to_setter
      else
        property_targets
    in
    List.map
      ~f:
        (CallTargetIndexer.create_target
           call_indexer
           ~implicit_self:true
           ~implicit_dunder_call:false
           ~collapse_tito:true
           ~return_type:(Some return_type))
      property_targets
  in
  let attributes = get_defining_attributes ~resolution ~base_annotation ~attribute in
  let properties, non_properties =
    List.partition_map
      ~f:(function
        | Some property when Annotated.Attribute.property property -> Either.First property
        | attribute -> Either.Second attribute)
      attributes
  in
  let property_targets = List.concat_map ~f:property_targets_of_attribute properties in
  let is_attribute = (not (List.is_empty non_properties)) || List.is_empty attributes in
  { property_targets; is_attribute }


let as_global_reference ~resolution expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) ->
      let reference = Reference.delocalize (Reference.create identifier) in
      if Resolution.is_global resolution ~reference then
        Some reference
      else
        None
  | Name name -> (
      name_to_reference name
      >>= fun reference ->
      GlobalResolution.resolve_exports (Resolution.global_resolution resolution) reference
      >>= function
      | UnannotatedGlobalEnvironment.ResolvedReference.ModuleAttribute
          { from; name; remaining = []; _ } ->
          Some (Reference.combine from (Reference.create name))
      | _ -> None)
  | _ -> None


let resolve_attribute_access_global_targets ~resolution ~base_annotation ~base ~attribute ~special =
  let expression =
    Expression.Name (Name.Attribute { Name.Attribute.base; attribute; special })
    |> Node.create_with_default_location
  in
  match as_global_reference ~resolution expression with
  | Some global -> [global]
  | None ->
      let global_resolution = Resolution.global_resolution resolution in
      let rec find_targets targets = function
        | Type.Union annotations -> List.fold ~init:targets ~f:find_targets annotations
        | Parametric { name = "type"; parameters = [Single annotation] } ->
            (* Access on a class, i.e `Foo.bar`, translated into `Foo.__class__.bar`. *)
            let parent =
              let attribute =
                Type.split annotation
                |> fst
                |> Type.primitive_name
                >>= GlobalResolution.attribute_from_class_name
                      ~transitive:true
                      ~resolution:global_resolution
                      ~name:attribute
                      ~instantiated:annotation
              in
              match attribute with
              | Some attribute when Annotated.Attribute.defined attribute ->
                  Type.Primitive (Annotated.Attribute.parent attribute) |> Type.class_name
              | _ -> Type.class_name annotation
            in
            let attribute = Format.sprintf "__class__.%s" attribute in
            let target = Reference.create ~prefix:parent attribute in
            target :: targets
        | annotation ->
            (* Access on an instance, i.e `self.foo`. *)
            let parents =
              let successors =
                GlobalResolution.class_metadata (Resolution.global_resolution resolution) annotation
                >>| (fun { ClassMetadataEnvironment.successors; _ } -> successors)
                |> Option.value ~default:[]
                |> List.map ~f:(fun name -> Type.Primitive name)
              in
              annotation :: successors
            in
            let add_target targets parent =
              let target = Reference.create ~prefix:(Type.class_name parent) attribute in
              target :: targets
            in
            List.fold ~init:targets ~f:add_target parents
      in
      find_targets [] base_annotation


let resolve_attribute_access ~resolution ~call_indexer ~base ~attribute ~special ~setter =
  let base_annotation = CallResolution.resolve_ignoring_optional ~resolution base in

  let { property_targets; is_attribute } =
    resolve_attribute_access_properties
      ~resolution
      ~call_indexer
      ~base_annotation
      ~attribute
      ~setter
  in

  let global_targets =
    resolve_attribute_access_global_targets ~resolution ~base_annotation ~base ~attribute ~special
    |> List.map ~f:Target.create_object
    |> List.filter ~f:FixpointState.has_model
    |> List.map
         ~f:
           (CallTargetIndexer.create_target
              call_indexer
              ~implicit_self:false
              ~implicit_dunder_call:false
              ~return_type:None
              ~collapse_tito:true)
  in

  { AttributeAccessCallees.property_targets; global_targets; is_attribute }


let resolve_identifier ~resolution ~call_indexer ~identifier =
  Expression.Name (Name.Identifier identifier)
  |> Node.create_with_default_location
  |> as_global_reference ~resolution
  >>| Target.create_object
  |> Option.filter ~f:FixpointState.has_model
  >>| fun global ->
  {
    IdentifierCallees.global_targets =
      [
        CallTargetIndexer.create_target
          call_indexer
          ~implicit_self:false
          ~implicit_dunder_call:false
          ~return_type:None
          ~collapse_tito:true
          global;
      ];
  }


(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraphFixpoint (Context : sig
  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val parent : Reference.t option

  val callees_at_location : UnprocessedLocationCallees.t Location.Table.t

  val call_indexer : CallTargetIndexer.t
end) =
struct
  type assignment_target = { location: Location.t }

  type visitor_t = {
    resolution: Resolution.t;
    assignment_target: assignment_target option;
  }

  let call_indexer = Context.call_indexer

  module NodeVisitor = struct
    type nonrec t = visitor_t

    let expression_visitor ({ resolution; assignment_target } as state) { Node.value; location } =
      CallTargetIndexer.generate_fresh_indices call_indexer;
      let register_targets ~expression_identifier ?(location = location) callees =
        Location.Table.update Context.callees_at_location location ~f:(function
            | None -> UnprocessedLocationCallees.singleton ~expression_identifier ~callees
            | Some existing_callees ->
                UnprocessedLocationCallees.add existing_callees ~expression_identifier ~callees)
      in
      let () =
        match value with
        | Expression.Call call ->
            let call = redirect_special_calls ~resolution call in
            resolve_callees ~resolution ~call_indexer ~call
            |> ExpressionCallees.from_call
            |> register_targets ~expression_identifier:(call_identifier call)
        | Expression.Name (Name.Attribute { Name.Attribute.base; attribute; special }) ->
            let setter =
              match assignment_target with
              | Some { location = assignment_target_location } ->
                  Location.equal assignment_target_location location
              | None -> false
            in
            resolve_attribute_access ~resolution ~call_indexer ~base ~attribute ~special ~setter
            |> ExpressionCallees.from_attribute_access
            |> register_targets ~expression_identifier:attribute
        | Expression.Name (Name.Identifier identifier) ->
            resolve_identifier ~resolution ~call_indexer ~identifier
            >>| ExpressionCallees.from_identifier
            >>| register_targets ~expression_identifier:identifier
            |> ignore
        | Expression.ComparisonOperator comparison -> (
            match ComparisonOperator.override ~location comparison with
            | Some { Node.value = Expression.Call call; _ } ->
                let call = redirect_special_calls ~resolution call in
                resolve_callees ~resolution ~call_indexer ~call
                |> ExpressionCallees.from_call
                |> register_targets ~expression_identifier:(call_identifier call)
            | _ -> ())
        | Expression.FormatString substrings ->
            List.iter substrings ~f:(function
                | Substring.Literal _ -> ()
                | Substring.Format ({ Node.location = expression_location; _ } as expression) ->
                    let { CallCallees.call_targets; _ } =
                      let callee =
                        let method_name =
                          Annotated.Call.resolve_stringify_call ~resolution expression
                        in
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
                        ~resolution
                        ~call_indexer
                        ~return_type:(lazy Type.string)
                        ~callee
                    in

                    if not (List.is_empty call_targets) then
                      let callees =
                        ExpressionCallees.from_format_string { FormatStringCallees.call_targets }
                      in
                      register_targets
                        ~expression_identifier:DefineCallGraph.format_string_expression_identifier
                        ~location:expression_location
                        callees)
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
              ~resolution
              ~call_indexer
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
              ~resolution
              ~call_indexer
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

    let generator_visitor ({ resolution; _ } as state) generator =
      (* Since generators create variables that Pyre sees as scoped within the generator, handle
         them by adding the generator's bindings to the resolution. *)
      {
        state with
        resolution =
          Resolution.resolve_assignment
            resolution
            (Ast.Statement.Statement.generator_assignment generator);
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


    let visit_format_string_children _ _ = true
  end

  module CalleeVisitor = Visit.MakeNodeVisitor (NodeVisitor)

  include Fixpoint.Make (struct
    type t = unit [@@deriving show]

    let bottom = ()

    let less_or_equal ~left:_ ~right:_ = true

    let join _ _ = ()

    let widen ~previous:_ ~next:_ ~iteration:_ = ()

    let forward_statement ~resolution ~statement =
      match Node.value statement with
      | Statement.Assign { Assign.target; value; _ } ->
          CalleeVisitor.visit_expression
            ~state:
              (ref { resolution; assignment_target = Some { location = Node.location target } })
            target;
          CalleeVisitor.visit_expression ~state:(ref { resolution; assignment_target = None }) value
      | _ ->
          CalleeVisitor.visit_statement
            ~state:(ref { resolution; assignment_target = None })
            statement


    let forward ~statement_key _ ~statement =
      let resolution =
        TypeCheck.resolution_with_key
          ~global_resolution:Context.global_resolution
          ~local_annotations:Context.local_annotations
          ~parent:Context.parent
          ~statement_key
          (module TypeCheck.DummyContext)
      in
      forward_statement ~resolution ~statement


    let backward ~statement_key:_ _ ~statement:_ = ()
  end)
end

let call_graph_of_define
    ~environment
    ~define:({ Define.signature = { Define.Signature.name; parent; _ }; _ } as define)
  =
  let timer = Timer.start () in
  let callees_at_location = Location.Table.create () in
  let module DefineFixpoint = DefineCallGraphFixpoint (struct
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations = TypeEnvironment.ReadOnly.get_local_annotations environment name

    let parent = parent

    let callees_at_location = callees_at_location

    let call_indexer = CallTargetIndexer.create ()
  end)
  in
  (* Handle parameters. *)
  let () =
    let resolution =
      TypeCheck.resolution
        (TypeEnvironment.ReadOnly.global_resolution environment)
        (module TypeCheck.DummyContext)
    in
    List.iter
      define.Ast.Statement.Define.signature.parameters
      ~f:(fun { Node.value = { Parameter.value; _ }; _ } ->
        Option.iter value ~f:(fun value ->
            DefineFixpoint.CalleeVisitor.visit_expression
              ~state:(ref { DefineFixpoint.resolution; assignment_target = None })
              value))
  in

  DefineFixpoint.forward ~cfg:(Cfg.create define) ~initial:() |> ignore;
  let call_graph =
    Location.Table.to_alist callees_at_location
    |> List.map ~f:(fun (location, unprocessed_callees) ->
           match String.Map.Tree.to_alist unprocessed_callees with
           | [] -> failwith "unreachable"
           | [(_, callees)] ->
               location, LocationCallees.Singleton (ExpressionCallees.deduplicate callees)
           | _ ->
               ( location,
                 LocationCallees.Compound
                   (Core.String.Map.Tree.map ~f:ExpressionCallees.deduplicate unprocessed_callees) ))
    |> List.filter ~f:(fun (_, callees) ->
           match callees with
           | LocationCallees.Singleton singleton ->
               not (ExpressionCallees.is_empty_attribute_access_callees singleton)
           | LocationCallees.Compound compound ->
               Core.String.Map.Tree.exists compound ~f:(fun callees ->
                   not (ExpressionCallees.is_empty_attribute_access_callees callees)))
    |> Location.Map.of_alist_exn
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


module SharedMemory = struct
  include
    Memory.WithCache.Make
      (Target.SharedMemoryKey)
      (struct
        type t = LocationCallees.t Location.Map.Tree.t

        let prefix = Prefix.make ()

        let description = "call graphs of defines"
      end)

  let add ~callable ~call_graph = add callable (Location.Map.to_tree call_graph)

  let get ~callable = get callable >>| Location.Map.of_tree

  let remove callables = KeySet.of_list callables |> remove_batch
end

let create_callgraph ~store_shared_memory ~environment ~source =
  let fold_defines dependencies = function
    | { Node.value = define; _ } when Define.is_overloaded_function define -> dependencies
    | define ->
        let call_graph_of_define = call_graph_of_define ~environment ~define:(Node.value define) in
        let () =
          if store_shared_memory then
            SharedMemory.add ~callable:(Target.create define) ~call_graph:call_graph_of_define
        in
        let non_object_target = function
          | Target.Object _ -> false
          | _ -> true
        in
        Location.Map.data call_graph_of_define
        |> List.concat_map ~f:LocationCallees.all_targets
        |> List.filter ~f:non_object_target
        |> List.dedup_and_sort ~compare:Target.compare
        |> fun callees -> Target.Map.set dependencies ~key:(Target.create define) ~data:callees
  in
  Preprocessing.defines ~include_nested:true ~include_toplevels:true source
  |> List.fold ~init:Target.Map.empty ~f:fold_defines
