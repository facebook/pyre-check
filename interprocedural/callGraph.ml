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


let rec resolve_callees_from_type ~resolution ?receiver_type callable_type =
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
          Some (RegularTargets { implicit_self = false; targets = [Callable.create_function name] })
      )
  | Type.Parametric { name = "BoundMethod"; parameters = [Single callable; Single receiver_type] }
    ->
      resolve_callees_from_type ~resolution ~receiver_type callable
  | Type.Union (element :: elements) ->
      let first_targets = resolve_callees_from_type ~resolution ?receiver_type element in
      List.fold elements ~init:first_targets ~f:(fun combined_targets new_target ->
          resolve_callees_from_type ~resolution ?receiver_type new_target
          |> merge_targets combined_targets)
  | _ -> None


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


let resolve_callees ~resolution ~callee =
  resolve_ignoring_optional ~resolution callee |> resolve_callees_from_type ~resolution


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
Fixpoint.Make (struct
  module CalleeVisitor = Visit.Make (struct
    type t = Resolution.t

    let expression resolution { Node.value; location } =
      match value with
      | Expression.Call { Call.callee; _ } ->
          begin
            match resolve_callees ~resolution ~callee with
            | Some targets ->
                Location.Table.set Context.callees_at_location ~key:location ~data:targets
            | None -> ()
          end;
          resolution
      | _ -> resolution


    let statement resolution _ = resolution
  end)

  type t = unit [@@deriving show]

  let less_or_equal ~left:_ ~right:_ = true

  let widen ~previous:_ ~next:_ ~iteration:_ = ()

  let forward_statement ~resolution ~statement =
    CalleeVisitor.visit resolution (Ast.Source.create [statement]) |> ignore;
    ()


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
  DefineFixpoint.forward ~cfg:(Cfg.create define) ~initial:() |> ignore;
  Location.Table.to_alist callees_at_location |> Location.Map.of_alist_exn
