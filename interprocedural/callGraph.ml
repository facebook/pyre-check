(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast
open Expression
open Pyre

type callees =
  | ConstructorTargets of {
      new_targets: Callable.t list;
      init_targets: Callable.t list;
    }
  | RegularTargets of {
      implicit_self: bool;
      targets: Callable.t list;
    }
[@@deriving eq, show]

let merge_targets left right =
  match left, right with
  | ( Some (RegularTargets { implicit_self = left_implicit_self; targets = left_targets }),
      Some (RegularTargets { implicit_self = right_implicit_self; targets = right_targets }) )
    when Bool.equal left_implicit_self right_implicit_self ->
      Some
        (RegularTargets
           {
             implicit_self = left_implicit_self;
             targets = List.rev_append left_targets right_targets;
           })
  | ( Some (ConstructorTargets { new_targets = left_new_targets; init_targets = left_init_targets }),
      Some
        (ConstructorTargets { new_targets = right_new_targets; init_targets = right_init_targets })
    ) ->
      Some
        (ConstructorTargets
           {
             new_targets = List.rev_append left_new_targets right_new_targets;
             init_targets = List.rev_append left_init_targets right_init_targets;
           })
  | Some left, None -> Some left
  | None, Some right -> Some right
  | None, None -> None
  | _ ->
      (* TODO(T77637504): We should probably error here. *)
      None


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
  let annotation =
    match Node.value expression with
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        let base_type =
          resolve_ignoring_optional ~resolution base
          |> fun annotation -> Type.optional_value annotation |> Option.value ~default:annotation
        in
        match defining_attribute ~resolution base_type attribute with
        | Some _ -> Resolution.resolve_attribute_access resolution ~base_type ~attribute
        | None -> Resolution.resolve_expression_to_type resolution expression
        (* Lookup the base_type for the attribute you were interested in *) )
    | _ -> Resolution.resolve_expression_to_type resolution expression
  in
  Type.optional_value annotation |> Option.value ~default:annotation


type callee_kind =
  | Method
  | Function
  | RecognizedCallableTarget of string

let rec callee_kind ~resolution callee callee_type =
  let is_local identifier = String.is_prefix ~prefix:"$" identifier in
  let rec is_all_names = function
    | Expression.Name (Name.Identifier identifier) when not (is_local identifier) -> true
    | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
        is_all_names (Node.value base)
    | _ -> false
  in
  match callee_type with
  | Type.Parametric { name = "BoundMethod"; _ } -> Method
  | Type.Callable _ -> (
      match Node.value callee with
      | Expression.Name (Name.Attribute { base; _ }) ->
          let is_class =
            resolve_ignoring_optional ~resolution base
            |> GlobalResolution.class_definition (Resolution.global_resolution resolution)
            |> Option.is_some
          in
          if is_class then
            Method
          else
            Function
      | _ -> Function )
  | Type.Union (callee_type :: _) -> callee_kind ~resolution callee callee_type
  | _
    when is_all_names (Node.value callee)
         && Type.Set.mem SpecialCallResolution.recognized_callable_target_types callee_type -> (
      match callee with
      | { Node.value = Expression.Name name; _ } ->
          Ast.Expression.name_to_reference name
          >>| Reference.show
          >>| (fun name -> RecognizedCallableTarget name)
          |> Option.value ~default:Method
      | _ -> Method )
  | _ ->
      (* We must be dealing with a callable class. *)
      Method


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
      Callable.create_override method_name
    else
      Callable.create_method method_name
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
    let target_callable = Callable.create_method implementation_target in
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


let rec resolve_callees_from_type ~resolution ?receiver_type ~callee_kind callable_type =
  match callable_type with
  | Type.Callable { Type.Callable.kind = Type.Callable.Named name; _ } -> (
      match receiver_type with
      | Some receiver_type ->
          Some
            (RegularTargets
               {
                 implicit_self = true;
                 targets = compute_indirect_targets ~resolution ~receiver_type name;
               })
      | None ->
          let target =
            match callee_kind with
            | Method -> Callable.create_method name
            | _ -> Callable.create_function name
          in
          Some (RegularTargets { implicit_self = false; targets = [target] }) )
  | Type.Parametric { name = "BoundMethod"; parameters = [Single callable; Single receiver_type] }
    ->
      resolve_callees_from_type ~resolution ~receiver_type ~callee_kind callable
  | Type.Union (element :: elements) ->
      let first_targets =
        resolve_callees_from_type ~resolution ~callee_kind ?receiver_type element
      in
      List.fold elements ~init:first_targets ~f:(fun combined_targets new_target ->
          resolve_callees_from_type ~resolution ?receiver_type ~callee_kind new_target
          |> merge_targets combined_targets)
  | callable_type ->
      (* Handle callable classes. `typing.Type` interacts specially with __call__, so we choose to
         ignore it for now to make sure our constructor logic via `cls()` still works. *)
      if not (Type.is_meta callable_type) then
        match
          Resolution.resolve_attribute_access
            resolution
            ~base_type:callable_type
            ~attribute:"__call__"
        with
        | Type.Any
        | Type.Top ->
            None
        (* Callable protocol. *)
        | Type.Callable { kind = Anonymous; _ } ->
            Type.primitive_name callable_type
            >>= fun primitive_callable_name ->
            Some
              (RegularTargets
                 {
                   implicit_self = true;
                   targets =
                     [
                       `Method
                         { Callable.class_name = primitive_callable_name; method_name = "__call__" };
                     ];
                 })
        | annotation -> resolve_callees_from_type ~resolution ~callee_kind annotation
      else
        resolve_constructor_callee ~resolution callable_type


and resolve_constructor_callee ~resolution class_type =
  match
    ( Resolution.resolve_attribute_access resolution ~base_type:class_type ~attribute:"__new__",
      Resolution.resolve_attribute_access resolution ~base_type:class_type ~attribute:"__init__" )
  with
  | Type.Any, _
  | Type.Top, _
  | _, Type.Any
  | _, Type.Top ->
      None
  | new_callable_type, init_callable_type -> (
      let new_targets, init_targets =
        ( resolve_callees_from_type
            ~resolution
            ~receiver_type:class_type
            ~callee_kind:Method
            new_callable_type,
          resolve_callees_from_type
            ~resolution
            ~receiver_type:class_type
            ~callee_kind:Method
            init_callable_type )
      in
      match new_targets, init_targets with
      | ( Some (RegularTargets { targets = new_targets; _ }),
          Some (RegularTargets { targets = init_targets; _ }) ) ->
          Some (ConstructorTargets { new_targets; init_targets })
      | _ -> None )


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


let resolve_callees ~resolution ~call =
  let { Call.callee; _ } = redirect_special_calls ~resolution call in
  let callee_type = resolve_ignoring_optional ~resolution callee in
  let callee_kind = callee_kind ~resolution callee callee_type in
  match callee_kind with
  | RecognizedCallableTarget name ->
      Some (RegularTargets { implicit_self = false; targets = [`Function name] })
  | _ -> resolve_callees_from_type ~callee_kind ~resolution callee_type


let get_property_defining_parent ~resolution ~base ~attribute =
  let annotation =
    Resolution.resolve_expression_to_type resolution base |> strip_meta |> strip_optional
  in
  match defining_attribute ~resolution annotation attribute with
  | Some property when Annotated.Attribute.property property ->
      Annotated.Attribute.parent property |> Reference.create |> Option.some
  | _ -> None


let resolve_property_targets ~resolution ~base ~attribute ~setter =
  match get_property_defining_parent ~resolution ~base ~attribute with
  | None -> None
  | Some defining_parent ->
      let targets =
        let targets =
          let receiver_type = resolve_ignoring_optional ~resolution base in
          if Type.is_meta receiver_type then
            [Callable.create_method (Reference.create ~prefix:defining_parent attribute)]
          else
            let callee = Reference.create ~prefix:defining_parent attribute in
            compute_indirect_targets ~resolution ~receiver_type callee
        in
        if setter then
          let to_setter target =
            match target with
            | `OverrideTarget { Callable.class_name; method_name } ->
                `OverrideTarget { Callable.class_name; method_name = method_name ^ "$setter" }
            | `Method { Callable.class_name; method_name } ->
                `Method { Callable.class_name; method_name = method_name ^ "$setter" }
            | _ -> target
          in
          List.map targets ~f:to_setter
        else
          targets
      in
      Some (RegularTargets { implicit_self = true; targets })


(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraph (Context : sig
  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val parent : Reference.t option

  val callees_at_location : callees Location.Table.t
end) =
struct
  type visitor_t = {
    resolution: Resolution.t;
    is_assignment_target: bool;
  }

  module NodeVisitor = struct
    type nonrec t = visitor_t

    let expression_visitor ({ resolution; is_assignment_target } as state) { Node.value; location } =
      let register_targets targets =
        Option.iter targets ~f:(fun data ->
            Location.Table.set Context.callees_at_location ~key:location ~data)
      in
      match value with
      | Expression.Call call ->
          resolve_callees ~resolution ~call |> register_targets;
          state
      | Expression.Name (Name.Attribute { Name.Attribute.base; attribute; _ }) ->
          resolve_property_targets ~resolution ~base ~attribute ~setter:is_assignment_target
          |> register_targets;
          state
      | Expression.ComparisonOperator comparison -> (
          match ComparisonOperator.override ~location comparison with
          | Some { Node.value = Expression.Call call; _ } ->
              resolve_callees ~resolution ~call |> register_targets;
              state
          | _ -> state )
      | _ -> state


    let statement_visitor state _ = state

    let node state = function
      | Visit.Expression expression -> expression_visitor state expression
      | Visit.Statement statement -> statement_visitor state statement
      | _ -> state


    let visit_statement_children _ statement =
      match Node.value statement with
      | Statement.Statement.Assign _ -> false
      | _ -> true
  end

  module CalleeVisitor = Visit.MakeNodeVisitor (NodeVisitor)

  include Fixpoint.Make (struct
    type t = unit [@@deriving show]

    let less_or_equal ~left:_ ~right:_ = true

    let widen ~previous:_ ~next:_ ~iteration:_ = ()

    let forward_statement ~resolution ~statement =
      match Node.value statement with
      | Statement.Statement.Assign { Statement.Assign.target; value; _ } ->
          CalleeVisitor.visit_expression
            ~state:(ref { resolution; is_assignment_target = true })
            target;
          CalleeVisitor.visit_expression
            ~state:(ref { resolution; is_assignment_target = false })
            value
      | _ ->
          CalleeVisitor.visit_statement
            ~state:(ref { resolution; is_assignment_target = false })
            statement


    let forward ~key _ ~statement =
      let resolution =
        TypeCheck.resolution_with_key
          ~global_resolution:Context.global_resolution
          ~local_annotations:Context.local_annotations
          ~parent:Context.parent
          ~key
          (module TypeCheck.DummyContext)
      in
      forward_statement ~resolution ~statement


    let backward ~key:_ _ ~statement:_ = ()
  end)
end

let call_graph_of_define
    ~environment
    ~define:
      ({ Statement.Define.signature = { Statement.Define.Signature.name; parent; _ }; _ } as define)
  =
  let callees_at_location = Location.Table.create () in
  let module DefineFixpoint = DefineCallGraph (struct
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations environment (Node.value name)


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
              ~state:(ref { DefineFixpoint.resolution; is_assignment_target = false })
              value))
  in

  DefineFixpoint.forward ~cfg:(Cfg.create define) ~initial:() |> ignore;
  Location.Table.to_alist callees_at_location |> Location.Map.of_alist_exn
