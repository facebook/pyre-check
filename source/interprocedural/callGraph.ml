(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

module CallTarget = struct
  type t = {
    target: Target.t;
    implicit_self: bool;
    implicit_dunder_call: bool;
    collapse_tito: bool;
  }
  [@@deriving compare, eq, show { with_path = false }]

  let create ?(implicit_self = false) ?(implicit_dunder_call = false) ?(collapse_tito = true) target
    =
    { target; implicit_self; implicit_dunder_call; collapse_tito }
end

module HigherOrderParameter = struct
  type t = {
    index: int;
    call_targets: CallTarget.t list;
    return_type: Type.t;
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


  let all_targets { call_targets; _ } =
    List.map ~f:(fun { CallTarget.target; _ } -> target) call_targets
end

module CallCallees = struct
  type t = {
    call_targets: CallTarget.t list;
    new_targets: Target.t list;
    init_targets: Target.t list;
    return_type: Type.t;
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
      ~return_type
      ()
    =
    { call_targets; new_targets; init_targets; return_type; higher_order_parameter; unresolved }


  let create_unresolved return_type =
    {
      call_targets = [];
      new_targets = [];
      init_targets = [];
      return_type;
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
        return_type;
        higher_order_parameter = left_higher_order_parameter;
        unresolved = left_unresolved;
      }
      {
        call_targets = right_call_targets;
        new_targets = right_new_targets;
        init_targets = right_init_targets;
        return_type = _;
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
    { call_targets; new_targets; init_targets; return_type; higher_order_parameter; unresolved }


  let deduplicate
      { call_targets; new_targets; init_targets; return_type; higher_order_parameter; unresolved }
    =
    let call_targets = List.dedup_and_sort ~compare:CallTarget.compare call_targets in
    let new_targets = List.dedup_and_sort ~compare:Target.compare new_targets in
    let init_targets = List.dedup_and_sort ~compare:Target.compare init_targets in
    let higher_order_parameter =
      match higher_order_parameter with
      | Some { HigherOrderParameter.index; call_targets; return_type } ->
          Some
            {
              HigherOrderParameter.index;
              call_targets = List.dedup_and_sort ~compare:CallTarget.compare call_targets;
              return_type;
            }
      | None -> None
    in
    { call_targets; new_targets; init_targets; return_type; higher_order_parameter; unresolved }


  let all_targets { call_targets; new_targets; init_targets; higher_order_parameter; _ } =
    List.map ~f:(fun { CallTarget.target; _ } -> target) call_targets
    |> List.rev_append new_targets
    |> List.rev_append init_targets
    |> List.rev_append
         (higher_order_parameter >>| HigherOrderParameter.all_targets |> Option.value ~default:[])
end

module AttributeAccessCallees = struct
  type t = {
    property_targets: Target.t list;
    global_targets: Target.t list;
    return_type: Type.t;
    is_attribute: bool;
  }
  [@@deriving eq, show { with_path = false }]

  let deduplicate { property_targets; global_targets; return_type; is_attribute } =
    {
      property_targets = List.dedup_and_sort ~compare:Target.compare property_targets;
      global_targets = List.dedup_and_sort ~compare:Target.compare global_targets;
      return_type;
      is_attribute;
    }


  let join
      {
        property_targets = left_property_targets;
        global_targets = left_global_targets;
        return_type;
        is_attribute = left_is_attribute;
      }
      {
        property_targets = right_property_targets;
        global_targets = right_global_targets;
        return_type = _;
        is_attribute = right_is_attribute;
      }
    =
    {
      property_targets = List.rev_append left_property_targets right_property_targets;
      global_targets = List.rev_append left_global_targets right_global_targets;
      return_type;
      is_attribute = left_is_attribute || right_is_attribute;
    }


  let all_targets { property_targets; global_targets; _ } =
    List.rev_append property_targets global_targets
end

module IdentifierCallees = struct
  type t = { global_targets: Target.t list } [@@deriving eq, show { with_path = false }]

  let deduplicate { global_targets } =
    { global_targets = List.dedup_and_sort ~compare:Target.compare global_targets }


  let join { global_targets = left_global_targets } { global_targets = right_global_targets } =
    { global_targets = List.rev_append left_global_targets right_global_targets }


  let all_targets { global_targets } = global_targets
end

module ExpressionCallees = struct
  type t = {
    call: CallCallees.t option;
    attribute_access: AttributeAccessCallees.t option;
    identifier: IdentifierCallees.t option;
  }
  [@@deriving eq, show { with_path = false }]

  let from_call callees = { call = Some callees; attribute_access = None; identifier = None }

  let from_attribute_access properties =
    { call = None; attribute_access = Some properties; identifier = None }


  let from_identifier identifier =
    { call = None; attribute_access = None; identifier = Some identifier }


  let join
      { call = left_call; attribute_access = left_attribute_access; identifier = left_identifier }
      {
        call = right_call;
        attribute_access = right_attribute_access;
        identifier = right_identifier;
      }
    =
    {
      call = Option.merge ~f:CallCallees.join left_call right_call;
      attribute_access =
        Option.merge ~f:AttributeAccessCallees.join left_attribute_access right_attribute_access;
      identifier = Option.merge ~f:IdentifierCallees.join left_identifier right_identifier;
    }


  let deduplicate { call; attribute_access; identifier } =
    {
      call = call >>| CallCallees.deduplicate;
      attribute_access = attribute_access >>| AttributeAccessCallees.deduplicate;
      identifier = identifier >>| IdentifierCallees.deduplicate;
    }


  let all_targets { call; attribute_access; identifier } =
    let call_targets = call >>| CallCallees.all_targets |> Option.value ~default:[] in
    let attribute_access_targets =
      attribute_access >>| AttributeAccessCallees.all_targets |> Option.value ~default:[]
    in
    let identifier_targets =
      identifier >>| IdentifierCallees.all_targets |> Option.value ~default:[]
    in
    call_targets |> List.rev_append attribute_access_targets |> List.rev_append identifier_targets
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
end

let defining_attribute ~resolution parent_type attribute =
  let global_resolution = Resolution.global_resolution resolution in
  Type.split parent_type
  |> fst
  |> Type.primitive_name
  >>= fun class_name ->
  GlobalResolution.attribute_from_class_name
    ~transitive:true
    ~resolution:global_resolution
    ~name:attribute
    ~instantiated:parent_type
    class_name
  >>= fun instantiated_attribute ->
  if Annotated.Attribute.defined instantiated_attribute then
    Some instantiated_attribute
  else
    Resolution.fallback_attribute ~resolution ~name:attribute class_name


let rec resolve_ignoring_optional ~resolution expression =
  let resolve_expression_to_type expression =
    match Resolution.resolve_expression_to_type resolution expression, Node.value expression with
    | ( Type.Callable ({ Type.Callable.kind = Anonymous; _ } as callable),
        Expression.Name (Name.Identifier function_name) )
      when function_name |> String.is_prefix ~prefix:"$local_" ->
        (* Treat nested functions as named callables. *)
        Type.Callable { callable with kind = Named (Reference.create function_name) }
    | annotation, _ -> annotation
  in
  let annotation =
    match Node.value expression with
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        let base_type =
          resolve_ignoring_optional ~resolution base
          |> fun annotation -> Type.optional_value annotation |> Option.value ~default:annotation
        in
        match defining_attribute ~resolution base_type attribute with
        | Some _ -> Resolution.resolve_attribute_access resolution ~base_type ~attribute
        | None -> resolve_expression_to_type expression
        (* Lookup the base_type for the attribute you were interested in *))
    | _ -> resolve_expression_to_type expression
  in
  Type.optional_value annotation |> Option.value ~default:annotation


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
          let parent_type = resolve_ignoring_optional ~resolution base in
          let is_class () =
            parent_type
            |> GlobalResolution.class_definition (Resolution.global_resolution resolution)
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
          GlobalResolution.less_or_equal global_resolution ~left:candidate_type ~right:receiver_type
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
        Resolution.resolve_expression resolution to_simulate
        |> snd
        |> function
        | Type.Callable { Type.Callable.implementation; _ } ->
            Type.Callable.Overload.return_annotation implementation
        | _ -> Type.Top
      in
      not (Type.equal implicit return_annotation)
  | _ -> true


let rec resolve_callees_from_type
    ~resolution
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
                {
                  CallTarget.target;
                  implicit_self = true;
                  implicit_dunder_call = dunder_call;
                  collapse_tito;
                })
              targets
          in
          CallCallees.create ~call_targets:targets ~return_type ()
      | None ->
          let target =
            match callee_kind with
            | Method _ -> Target.create_method name
            | _ -> Target.create_function name
          in
          CallCallees.create
            ~call_targets:
              [
                {
                  CallTarget.target;
                  implicit_self = false;
                  implicit_dunder_call = dunder_call;
                  collapse_tito;
                };
              ]
            ~return_type
            ())
  | Type.Callable { kind = Anonymous; _ } -> CallCallees.create_unresolved return_type
  | Type.Parametric { name = "BoundMethod"; parameters = [Single callable; Single receiver_type] }
    ->
      resolve_callees_from_type
        ~resolution
        ~receiver_type
        ~return_type
        ~callee_kind
        ~collapse_tito
        callable
  | Type.Union (element :: elements) ->
      let first_targets =
        resolve_callees_from_type
          ~resolution
          ~callee_kind
          ?receiver_type
          ~return_type
          ~collapse_tito
          element
      in
      List.fold elements ~init:first_targets ~f:(fun combined_targets new_target ->
          resolve_callees_from_type
            ~resolution
            ?receiver_type
            ~return_type
            ~callee_kind
            ~collapse_tito
            new_target
          |> CallCallees.join combined_targets)
  | Type.Parametric { name = "type"; _ } ->
      resolve_constructor_callee ~resolution ~return_type callable_type
      |> Option.value ~default:(CallCallees.create_unresolved return_type)
  | callable_type -> (
      (* Handle callable classes. `typing.Type` interacts specially with __call__, so we choose to
         ignore it for now to make sure our constructor logic via `cls()` still works. *)
      match
        Resolution.resolve_attribute_access
          resolution
          ~base_type:callable_type
          ~attribute:"__call__"
      with
      | Type.Any
      | Type.Top ->
          CallCallees.create_unresolved return_type
      (* Callable protocol. *)
      | Type.Callable { kind = Anonymous; _ } ->
          Type.primitive_name callable_type
          >>| (fun primitive_callable_name ->
                let target =
                  `Method { Target.class_name = primitive_callable_name; method_name = "__call__" }
                in
                CallCallees.create
                  ~call_targets:
                    [
                      {
                        CallTarget.target;
                        implicit_self = true;
                        implicit_dunder_call = true;
                        collapse_tito;
                      };
                    ]
                  ~return_type
                  ())
          |> Option.value ~default:(CallCallees.create_unresolved return_type)
      | annotation ->
          if not dunder_call then
            resolve_callees_from_type
              ~resolution
              ~return_type
              ~dunder_call:true
              ~callee_kind
              ~collapse_tito
              annotation
          else
            CallCallees.create_unresolved return_type)


and resolve_constructor_callee ~resolution ~return_type class_type =
  match
    ( Resolution.resolve_attribute_access resolution ~base_type:class_type ~attribute:"__new__",
      Resolution.resolve_attribute_access resolution ~base_type:class_type ~attribute:"__init__" )
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
          ~receiver_type:class_type
          ~return_type
          ~callee_kind:(Method { is_direct_call = true })
          ~collapse_tito:true
          new_callable_type
      in
      let init_callees =
        resolve_callees_from_type
          ~resolution
          ~receiver_type:class_type
          ~return_type
          ~callee_kind:(Method { is_direct_call = true })
          ~collapse_tito:true
          init_callable_type
      in
      let new_targets =
        List.map ~f:(fun { CallTarget.target; _ } -> target) new_callees.call_targets
      in
      let init_targets =
        List.map ~f:(fun { CallTarget.target; _ } -> target) init_callees.call_targets
      in
      Some
        (CallCallees.create
           ~new_targets
           ~init_targets
           ~unresolved:(new_callees.unresolved || init_callees.unresolved)
           ~return_type
           ())


let resolve_callee_from_defining_expression
    ~resolution
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
                implementation = { annotation = Type.Any; parameters = Type.Callable.Defined [] };
                overloads = [];
              }
          in
          Some
            (resolve_callees_from_type
               ~resolution
               ~receiver_type:implementing_class
               ~return_type
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


let resolve_recognized_callees ~resolution ~callee ~return_type ~callee_type =
  (* Special treatment for a set of hardcoded decorators returning callable classes. *)
  match Node.value callee, callee_type with
  | ( _,
      Type.Parametric
        {
          name = "BoundMethod";
          parameters = [Single (Parametric { name; _ }); Single implementing_class];
        } )
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      resolve_callee_from_defining_expression ~resolution ~callee ~return_type ~implementing_class
  | Expression.Name (Name.Attribute { base; _ }), Parametric { name; _ }
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      (* Because of the special class, we don't get a bound method & lose the self argument for
         non-classmethod LRU cache wrappers. Reconstruct self in this case. *)
      resolve_ignoring_optional ~resolution base
      |> fun implementing_class ->
      resolve_callee_from_defining_expression ~resolution ~callee ~return_type ~implementing_class
  | Expression.Name name, _
    when is_all_names (Node.value callee)
         && Type.Set.mem SpecialCallResolution.recognized_callable_target_types callee_type ->
      Ast.Expression.name_to_reference name
      >>| Reference.show
      >>| fun name ->
      let collapse_tito = collapse_tito ~resolution ~callee ~callable_type:callee_type in
      CallCallees.create
        ~call_targets:
          [
            {
              CallTarget.target = `Function name;
              implicit_self = false;
              implicit_dunder_call = false;
              collapse_tito;
            };
          ]
        ~return_type
        ()
  | _ -> None


let resolve_callee_ignoring_decorators ~resolution ~collapse_tito callee =
  let global_resolution = Resolution.global_resolution resolution in
  let open UnannotatedGlobalEnvironment in
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
            {
              CallTarget.target = `Function (Reference.show name);
              implicit_self = false;
              implicit_dunder_call = false;
              collapse_tito;
            }
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
          GlobalResolution.class_definition global_resolution (Type.Primitive class_name)
          >>| Node.value
          >>| ClassSummary.attributes
          >>= Identifier.SerializableMap.find_opt attribute
          >>| Node.value
          >>= function
          | { kind = Method { static; _ }; _ } ->
              Some
                {
                  CallTarget.target = `Method { Target.class_name; method_name = attribute };
                  implicit_self = not static;
                  implicit_dunder_call = false;
                  collapse_tito;
                }
          | _ -> None)
      | _ -> None)
  | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
      (* Resolve `base.attribute` by looking up the type of `base`. *)
      match resolve_ignoring_optional ~resolution base with
      | Type.Primitive class_name
      | Type.Parametric { name = "type"; parameters = [Single (Type.Primitive class_name)] } -> (
          GlobalResolution.class_definition global_resolution (Type.Primitive class_name)
          >>| Node.value
          >>| ClassSummary.attributes
          >>= Identifier.SerializableMap.find_opt attribute
          >>| Node.value
          >>= function
          | { kind = Method _; _ } ->
              Some
                {
                  CallTarget.target = `Method { Target.class_name; method_name = attribute };
                  implicit_self = true;
                  implicit_dunder_call = false;
                  collapse_tito;
                }
          | _ -> None)
      | _ -> None)
  | _ -> None


let resolve_regular_callees ~resolution ~return_type ~callee =
  let callee_type = resolve_ignoring_optional ~resolution callee in
  let recognized_callees =
    resolve_recognized_callees ~resolution ~callee ~return_type ~callee_type
    |> Option.value ~default:(CallCallees.create_unresolved return_type)
  in
  if CallCallees.is_partially_resolved recognized_callees then
    recognized_callees
  else
    let callee_kind = callee_kind ~resolution callee callee_type in
    let collapse_tito = collapse_tito ~resolution ~callee ~callable_type:callee_type in
    let calleees_from_type =
      resolve_callees_from_type ~resolution ~return_type ~callee_kind ~collapse_tito callee_type
    in
    if CallCallees.is_partially_resolved calleees_from_type then
      calleees_from_type
    else
      resolve_callee_ignoring_decorators ~resolution ~collapse_tito callee
      >>| (fun target -> CallCallees.create ~call_targets:[target] ~return_type ())
      |> Option.value ~default:(CallCallees.create_unresolved return_type)


let resolve_callees ~resolution ~call =
  let { Call.callee; arguments } = redirect_special_calls ~resolution call in
  let return_type =
    Expression.Call call
    |> Node.create_with_default_location
    |> Resolution.resolve_expression_to_type resolution
  in
  let higher_order_parameter =
    let get_higher_order_function_targets index { Call.Argument.value = argument; _ } =
      match resolve_regular_callees ~resolution ~return_type:Type.none ~callee:argument with
      | { CallCallees.call_targets = _ :: _ as regular_targets; _ } ->
          let return_type =
            Expression.Call { callee = argument; arguments = [] }
            |> Node.create_with_default_location
            |> Resolution.resolve_expression_to_type resolution
          in
          Some { HigherOrderParameter.index; call_targets = regular_targets; return_type }
      | _ -> None
    in
    List.find_mapi arguments ~f:get_higher_order_function_targets
  in
  let regular_callees = resolve_regular_callees ~resolution ~return_type ~callee in
  { regular_callees with higher_order_parameter }


let get_property_defining_parents ~resolution ~base_annotation ~attribute =
  let rec get_defining_parents annotation =
    match annotation with
    | Type.Union annotations
    | Type.Variable { Type.Variable.Unary.constraints = Type.Variable.Explicit annotations; _ } ->
        List.concat_map annotations ~f:get_defining_parents
    | _ -> (
        match defining_attribute ~resolution annotation attribute with
        | Some property when Annotated.Attribute.property property ->
            [Annotated.Attribute.parent property |> Reference.create |> Option.some]
        | _ -> [None])
  in
  base_annotation |> strip_meta |> strip_optional |> get_defining_parents


type attribute_access_properties = {
  property_targets: Target.t list;
  is_attribute: bool;
}

let resolve_attribute_access_properties ~resolution ~base_annotation ~attribute ~setter =
  let defining_parents = get_property_defining_parents ~resolution ~base_annotation ~attribute in
  let property_of_parent = function
    | None -> []
    | Some parent ->
        let property_targets =
          if Type.is_meta base_annotation then
            [Target.create_method (Reference.create ~prefix:parent attribute)]
          else
            let callee = Reference.create ~prefix:parent attribute in
            compute_indirect_targets ~resolution ~receiver_type:base_annotation callee
        in
        if setter then
          let to_setter target =
            match target with
            | `OverrideTarget { Target.class_name; method_name } ->
                `OverrideTarget { Target.class_name; method_name = method_name ^ "$setter" }
            | `Method { Target.class_name; method_name } ->
                `Method { Target.class_name; method_name = method_name ^ "$setter" }
            | _ -> target
          in
          List.map property_targets ~f:to_setter
        else
          property_targets
  in
  let property_targets = List.concat_map ~f:property_of_parent defining_parents in
  let is_attribute =
    List.exists ~f:Option.is_none defining_parents || List.is_empty defining_parents
  in
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


let resolve_attribute_access ~resolution ~base ~attribute ~special ~setter =
  let base_annotation = resolve_ignoring_optional ~resolution base in

  let { property_targets; is_attribute } =
    resolve_attribute_access_properties ~resolution ~base_annotation ~attribute ~setter
  in

  let global_targets =
    resolve_attribute_access_global_targets ~resolution ~base_annotation ~base ~attribute ~special
    |> List.map ~f:Target.create_object
    |> List.filter ~f:FixpointState.has_model
  in

  match property_targets, global_targets, is_attribute with
  | [], [], true -> None
  | _ ->
      let return_type =
        if setter then
          Type.none
        else
          Expression.Name (Name.Attribute { Name.Attribute.base; attribute; special })
          |> Node.create_with_default_location
          |> Resolution.resolve_expression_to_type resolution
      in
      Some { AttributeAccessCallees.property_targets; global_targets; return_type; is_attribute }


let resolve_identifier ~resolution ~identifier =
  Expression.Name (Name.Identifier identifier)
  |> Node.create_with_default_location
  |> as_global_reference ~resolution
  >>| Target.create_object
  |> Option.filter ~f:FixpointState.has_model
  >>| fun global -> { IdentifierCallees.global_targets = [global] }


(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraphFixpoint (Context : sig
  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val parent : Reference.t option

  val callees_at_location : UnprocessedLocationCallees.t Location.Table.t
end) =
struct
  type assignment_target = { location: Location.t }

  type visitor_t = {
    resolution: Resolution.t;
    assignment_target: assignment_target option;
  }

  module NodeVisitor = struct
    type nonrec t = visitor_t

    let expression_visitor ({ resolution; assignment_target } as state) { Node.value; location } =
      let register_targets ~expression_identifier callees =
        Location.Table.update Context.callees_at_location location ~f:(function
            | None -> UnprocessedLocationCallees.singleton ~expression_identifier ~callees
            | Some existing_callees ->
                UnprocessedLocationCallees.add existing_callees ~expression_identifier ~callees)
      in
      let () =
        match value with
        | Expression.Call call ->
            resolve_callees ~resolution ~call
            |> ExpressionCallees.from_call
            |> register_targets ~expression_identifier:(call_identifier call)
        | Expression.Name (Name.Attribute { Name.Attribute.base; attribute; special }) ->
            let setter =
              match assignment_target with
              | Some { location = assignment_target_location } ->
                  Location.equal assignment_target_location location
              | None -> false
            in
            resolve_attribute_access ~resolution ~base ~attribute ~special ~setter
            >>| ExpressionCallees.from_attribute_access
            >>| register_targets ~expression_identifier:attribute
            |> ignore
        | Expression.Name (Name.Identifier identifier) ->
            resolve_identifier ~resolution ~identifier
            >>| ExpressionCallees.from_identifier
            >>| register_targets ~expression_identifier:identifier
            |> ignore
        | Expression.ComparisonOperator comparison -> (
            match ComparisonOperator.override ~location comparison with
            | Some { Node.value = Expression.Call call; _ } ->
                resolve_callees ~resolution ~call
                |> ExpressionCallees.from_call
                |> register_targets ~expression_identifier:(call_identifier call)
            | _ -> ())
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
            resolve_attribute_access ~resolution ~base ~attribute ~special:false ~setter:false
            >>| ExpressionCallees.from_attribute_access
            >>| register_targets ~expression_identifier:attribute
            |> ignore
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
            resolve_attribute_access ~resolution ~base:self ~attribute ~special:true ~setter:true
            >>| ExpressionCallees.from_attribute_access
            >>| register_targets ~expression_identifier:attribute
            |> ignore
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
  let callees_at_location = Location.Table.create () in
  let module DefineFixpoint = DefineCallGraphFixpoint (struct
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations = TypeEnvironment.ReadOnly.get_local_annotations environment name

    let parent = parent

    let callees_at_location = callees_at_location
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
  |> Location.Map.of_alist_exn


module SharedMemory = struct
  include
    Memory.WithCache.Make
      (Target.CallableKey)
      (struct
        type t = LocationCallees.t Location.Map.Tree.t

        let prefix = Prefix.make ()

        let description = "call graphs of defines"

        let unmarshall value = Marshal.from_string value 0
      end)

  let add ~callable ~call_graph = add callable (Location.Map.to_tree call_graph)

  let get ~callable = get callable >>| Location.Map.of_tree

  let get_or_compute ~callable ~environment ~define =
    match get ~callable with
    | Some map -> map
    | None ->
        let call_graph = call_graph_of_define ~environment ~define in
        add ~callable ~call_graph;
        call_graph


  let remove callables = KeySet.of_list callables |> remove_batch
end

let create_callgraph ?(use_shared_memory = false) ~environment ~source =
  let fold_defines dependencies = function
    | { Node.value = define; _ } when Define.is_overloaded_function define -> dependencies
    | define ->
        let call_graph_of_define =
          if use_shared_memory then
            SharedMemory.get_or_compute
              ~callable:(Target.create define)
              ~environment
              ~define:(Node.value define)
          else
            call_graph_of_define ~environment ~define:(Node.value define)
        in
        let non_object_target = function
          | `Object _ -> false
          | _ -> true
        in
        Location.Map.data call_graph_of_define
        |> List.concat_map ~f:LocationCallees.all_targets
        |> List.filter ~f:non_object_target
        |> List.dedup_and_sort ~compare:Target.compare
        |> fun callees ->
        Target.CallableMap.set dependencies ~key:(Target.create define) ~data:callees
  in
  Preprocessing.defines ~include_nested:true source
  |> List.fold ~init:Target.CallableMap.empty ~f:fold_defines
