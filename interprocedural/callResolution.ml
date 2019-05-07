(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Expression
open Pyre


let normalize_global ~resolution access =
  (* Determine if this is a constructor call, as we need to add the
     uninitialized object argument for self.
  *)
  let global_type = Resolution.resolve resolution (Access.expression access) in
  if Type.is_meta global_type then
    let dummy_self = {
      Expression.Argument.name = None;
      value = Node.create_with_default_location Expression.False;
    }
    in
    let full_access =
      (Access.Identifier "__init__") :: List.rev access
      |> List.rev
    in
    full_access, [dummy_self]
  else
    access, []


let is_local identifier =
  String.is_prefix ~prefix:"$" identifier


let is_class ~resolution access =
  Reference.from_access access
  |> (fun reference -> Type.Primitive (Reference.show reference))
  |> Resolution.class_definition resolution
  |> Option.is_some


let is_global ~resolution access =
  match access with
  | Access.SimpleAccess (Access.Identifier head::_ as access)
    when not (is_local head)
      && not (String.equal head "super")
      && not (String.equal head "type") ->
      Reference.from_access access
      |> Resolution.global resolution
      |> Option.is_some
      && not (is_class ~resolution (Access.prefix access))
  | _ -> false


(* Figure out what target to pick for an indirect call that resolves to target_name.
   E.g., if the receiver type is A, and A derives from Base, and the target is Base.method, then
   targetting the override tree of Base.method is wrong, as it would include all siblings for A.
   Instead, we have the following cases:
   a) receiver type matches target_name's declaring type -> override target_name
   b) no target_name override entries are subclasses of A -> real target_name
   c) some override entries are subclasses of A -> override all those
   where override name is
    1) the override target if it exists in the override shared mem
    2) the real target otherwise
*)
let compute_indirect_targets ~resolution ~receiver_type target_name =
  let get_class_type = Resolution.parse_reference resolution in
  let get_actual_target method_name =
    if DependencyGraphSharedMemory.overrides_exist method_name then
      Callable.create_override method_name
    else
      Callable.create_method method_name
  in
  let receiver_type =
    let strip_optional_and_meta t =
      t
      |> (fun t -> if Type.is_meta t then Type.single_parameter t else t)
      |> (fun t -> if Type.is_optional t then Type.optional_value t else t)
    in
    strip_optional_and_meta receiver_type
  in
  let declaring_type =
    Reference.prefix target_name
    >>| get_class_type
  in
  if declaring_type >>| Type.equal receiver_type |> Option.value ~default:false then
    (* case a *)
    [get_actual_target target_name]
  else
    match DependencyGraphSharedMemory.get_overriding_types ~member:target_name with
    | None ->
        (* case b *)
        [Callable.create_method target_name]
    | Some overriding_types ->
        let keep_subtypes candidate =
          let candidate_type = get_class_type candidate in
          Resolution.less_or_equal resolution ~left:candidate_type ~right:receiver_type
        in
        match List.filter overriding_types ~f:keep_subtypes with
        | [] ->
            (* case b *)
            [Callable.create_method target_name]
        | subtypes ->
            (* case c *)
            let create_override_target class_name =
              Reference.create ~prefix:class_name (Reference.last target_name)
              |> get_actual_target
            in
            List.map subtypes ~f:create_override_target


let resolve_target ~resolution ?receiver_type function_access =
  let is_super_call = function
    | Access.SimpleAccess (Access.Identifier name :: Access.Call _ :: _) ->
        name = "super"
    | _ ->
        false
  in
  let is_all_names access =
    let rec is_all_names = function
      | [] -> true
      | Access.Identifier name :: rest
        when not (is_local name) ->
          is_all_names rest
      | _ -> false
    in
    match access with
    | Access.SimpleAccess path ->
        is_all_names path
    | _ -> false
  in
  let rec resolve_type callable_type =
    match callable_type, receiver_type with
    | Type.Callable { implicit; kind = Named name; _ }, _
      when is_global ~resolution function_access ->
        [Callable.create_function name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, _
      when is_super_call function_access ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit = None; kind = Named name; _ }, None
      when is_all_names function_access ->
        [Callable.create_function name, None]
    | Type.Callable { implicit; kind = Named name; _ }, _
      when is_all_names function_access ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, Some type_or_class ->
        compute_indirect_targets ~resolution ~receiver_type:type_or_class name
        |> List.map ~f:(fun target -> target, implicit)
    | access_type, _ when Type.is_meta access_type && is_global ~resolution function_access ->
        let global_access =
          match function_access with
          | Access.SimpleAccess access ->
              access
          | _ ->
              failwith "is_global should be used before calling this"
        in
        let access, _ = normalize_global ~resolution global_access in
        [
          Callable.create_method (Reference.from_access access),
          Some { Type.Callable.implicit_annotation = access_type; name = "self" };
        ]
    | Type.Union annotations, _ ->
        List.concat_map ~f:resolve_type annotations
    | _ ->
        []
  in
  let node = Node.create_with_default_location (Expression.Access function_access) in
  resolve_type (Resolution.resolve resolution node)


let get_indirect_targets ~resolution ~receiver ~method_name =
  let receiver_node = Node.create_with_default_location (Access receiver) in
  let receiver_type = Resolution.resolve resolution receiver_node in
  let access = Access.combine receiver_node [Access.Identifier method_name] in
  resolve_target ~resolution ~receiver_type access


let get_global_targets ~resolution ~global =
  resolve_target ~resolution (Access.SimpleAccess global)


let resolve_call_targets ~resolution (access: Access.general_access) =
  let rec visit_each ~accumulator ?receiver_type ~access suffix =
    let receiver = Node.create_with_default_location (Expression.Access access) in
    match suffix with
    | Access.Call _ as next :: tail ->
        let targets = resolve_target ~resolution ?receiver_type access in
        let accumulator = List.rev_append targets accumulator in
        let call = Access.combine receiver [next] in
        let call_node = Node.create_with_default_location (Expression.Access call) in
        let receiver_type = Resolution.resolve resolution call_node in
        visit_each
          ~accumulator
          ~receiver_type
          ~access:call tail
    | Access.Identifier _ as next :: tail ->
        let receiver_type = Resolution.resolve resolution receiver in
        let member = Access.combine receiver [next] in
        visit_each
          ~accumulator
          ~receiver_type
          ~access:member tail
    | [] ->
        accumulator
  in
  match access with
  | Access.SimpleAccess (head :: tail) ->
      let access = Access.SimpleAccess [head] in
      visit_each
        ~accumulator:[]
        ~access
        tail

  | Access.ExpressionAccess ({ access = tail; _ } as base_access) ->
      let access = Access.ExpressionAccess { base_access with access = [] }
      in
      visit_each
        ~accumulator:[]
        ~access
        tail

  | Access.SimpleAccess [] ->
      (* Probably impossible *)
      []
