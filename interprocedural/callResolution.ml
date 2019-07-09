(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Expression
open Pyre

let normalize_global ~resolution reference =
  (* Determine if this is a constructor call, as we need to add the uninitialized object argument
     for self. *)
  let global_type = Resolution.resolve_reference resolution reference in
  if Type.is_meta global_type then
    let dummy_self =
      { Expression.Call.Argument.name = None;
        value = Node.create_with_default_location Expression.False
      }
    in
    let full_reference = Reference.create ~prefix:reference "__init__" in
    full_reference, [dummy_self]
  else
    reference, []


let is_local identifier = String.is_prefix ~prefix:"$" identifier

(* Figure out what target to pick for an indirect call that resolves to target_name.
   E.g., if the receiver type is A, and A derives from Base, and the target is Base.method, then
   targetting the override tree of Base.method is wrong, as it would include all siblings for A.

 * Instead, we have the following cases:
 * a) receiver type matches target_name's declaring type -> override target_name
 * b) no target_name override entries are subclasses of A -> real target_name
 * c) some override entries are subclasses of A -> search upwards for actual implementation,
 *    and override all those where the override name is
 *  1) the override target if it exists in the override shared mem
 *  2) the real target otherwise
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
      |> fun t -> if Type.is_optional t then Type.optional_value t else t
    in
    strip_optional_and_meta receiver_type
  in
  let declaring_type = Reference.prefix target_name >>| get_class_type in
  if declaring_type >>| Type.equal receiver_type |> Option.value ~default:false then (* case a *)
    [get_actual_target target_name]
  else
    match DependencyGraphSharedMemory.get_overriding_types ~member:target_name with
    | None ->
        (* case b *)
        [Callable.create_method target_name]
    | Some overriding_types -> (
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
              Reference.create ~prefix:class_name (Reference.last target_name) |> get_actual_target
            in
            let actual_target =
              let actual_implementation =
                declaring_type
                >>= fun class_type ->
                Callable.get_method_implementation ~resolution ~class_type ~method_name:target_name
              in
              match actual_implementation with
              | Some implementation -> implementation
              | None -> get_actual_target target_name
            in
            actual_target :: List.map subtypes ~f:create_override_target )


let resolve_target ~resolution ?receiver_type callee =
  let callable_type = Resolution.resolve resolution callee in
  let global =
    match Expression.get_identifier_base callee, Node.value callee with
    | Some "super", _
    | Some "type", _ ->
        None
    | Some base, Name name when Expression.is_simple_name name && not (is_local base) ->
        let reference = Reference.from_name_exn name in
        let global = reference |> Resolution.global resolution |> Option.is_some in
        let is_class =
          match Node.value callee with
          | Name (Name.Attribute { base; _ }) ->
              Resolution.resolve resolution base
              |> Resolution.class_definition resolution
              |> Option.is_some
          | _ -> false
        in
        Option.some_if (global && not is_class) reference
    | _ -> None
  in
  let is_super_call =
    let rec is_super callee =
      match Node.value callee with
      | Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
      | Call { callee; _ } -> is_super callee
      | Name (Name.Attribute { base; _ }) -> is_super base
      | _ -> false
    in
    is_super callee
  in
  let is_all_names =
    let rec is_all_names = function
      | Name (Name.Identifier identifier) when not (is_local identifier) -> true
      | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
          is_all_names (Node.value base)
      | _ -> false
    in
    is_all_names (Node.value callee)
  in
  let rec resolve_type callable_type =
    match callable_type, receiver_type, global with
    | Type.Callable { implicit; kind = Named name; _ }, _, Some _ ->
        [Callable.create_function name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, _, _ when is_super_call ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit = None; kind = Named name; _ }, None, _ when is_all_names ->
        [Callable.create_function name, None]
    | Type.Callable { implicit; kind = Named name; _ }, _, _ when is_all_names ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, Some type_or_class, _ ->
        compute_indirect_targets ~resolution ~receiver_type:type_or_class name
        |> List.map ~f:(fun target -> target, implicit)
    | callable_type, _, Some global when Type.is_meta callable_type ->
        let reference, _ = normalize_global ~resolution global in
        [ ( Callable.create_method reference,
            Some { Type.Callable.implicit_annotation = callable_type; name = "self" } ) ]
    | Type.Union annotations, _, _ -> List.concat_map ~f:resolve_type annotations
    | _ -> []
  in
  resolve_type callable_type


let get_indirect_targets ~resolution ~receiver ~method_name =
  let receiver_type = Resolution.resolve resolution receiver in
  let callee =
    Name (Name.Attribute { base = receiver; attribute = method_name; special = false })
    |> Node.create_with_default_location
  in
  resolve_target ~resolution ~receiver_type callee


let get_global_targets ~resolution ~global =
  Name (Reference.name ~location:Location.Reference.any global)
  |> Node.create_with_default_location
  |> resolve_target ~resolution


let resolve_call_targets ~resolution { Call.callee; _ } =
  let receiver_type =
    match Node.value callee with
    | Name (Name.Attribute { base; _ }) -> Some (Resolution.resolve resolution base)
    | _ -> None
  in
  resolve_target ~resolution ?receiver_type callee
