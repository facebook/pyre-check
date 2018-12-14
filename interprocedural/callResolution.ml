(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Expression


let global_prefix ~resolution access =
  let rec module_prefix ~lead ~tail =
    match tail with
    | (Access.Identifier _ as identifier) :: new_tail ->
        let new_lead = lead @ [identifier] in
        let access = lead in
        if Resolution.module_definition resolution access |> Option.is_some then
          module_prefix ~lead:new_lead ~tail:new_tail
        else
          lead, tail
    | _ ->
        lead, tail
  in
  module_prefix ~lead:[] ~tail:access


let normalize_global ~resolution access =
  (* Determine if this is a constructor call, as we need to add the
     uninitialized object argument for self.
  *)
  let global_type = Resolution.resolve resolution (Access.expression access) in
  if Type.is_meta global_type then
    let dummy_self = {
      Expression.Argument.name = None;
      value = Node.create ~location:Location.Reference.any Expression.False;
    }
    in
    let full_access =
      (Access.Identifier (Identifier.create "__init__")) :: List.rev access
      |> List.rev
    in
    full_access, [dummy_self]
  else
    access, []


let is_local identifier =
  String.is_prefix ~prefix:"$" (Identifier.show identifier)


let is_class ~resolution access =
  Access.expression access
  |> Resolution.parse_annotation resolution
  |> Resolution.class_definition resolution
  |> Option.is_some


let is_global ~resolution access =
  match access with
  | Access.Identifier head::_
    when not (is_local head)
      && Identifier.show head <> "super"
      && Identifier.show head <> "type" ->
      Resolution.global resolution access
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
  let get_class_type class_name =
    class_name
    |> Access.expression
    |> Resolution.parse_annotation resolution
  in
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
    Access.prefix target_name
    |> get_class_type
  in
  if Type.equal receiver_type declaring_type then
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
              (class_name @ [List.last_exn target_name])
              |> get_actual_target
            in
            List.map subtypes ~f:create_override_target


let resolve_target ~resolution ?receiver_type access ~reverse_access =
  let is_super_call = function
    | Access.Identifier _ :: Access.Call _ :: Access.Identifier name :: _ ->
        Identifier.show name = "super"
    | _ ->
        false
  in
  let rec is_all_names = function
    | [] -> true
    | Access.Identifier name :: rest
      when not (is_local name) ->
        is_all_names rest
    | _ -> false
  in
  let rec resolve_type callable_type =
    match callable_type, receiver_type with
    | Type.Callable { implicit; kind = Named name; _ }, _
      when is_global ~resolution access ->
        [Callable.create_function name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, _
      when is_super_call reverse_access ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit = None; kind = Named name; _ }, None
      when is_all_names access ->
        [Callable.create_function name, None]
    | Type.Callable { implicit; kind = Named name; _ }, _
      when is_all_names access ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, Some type_or_class ->
        compute_indirect_targets ~resolution  ~receiver_type:type_or_class name
        |> List.map ~f:(fun target -> target, implicit)
    | access_type, _ when Type.is_meta access_type && is_global ~resolution access ->
        let access, _ = normalize_global ~resolution access in
        [
          Callable.create_method access,
          Some { Type.Callable.implicit_annotation = access_type; name = access };
        ]
    | Type.Union annotations, _ ->
        List.concat_map ~f:resolve_type annotations
    | _ ->
        []
  in
  resolve_type (Resolution.resolve resolution (Access.expression access))

let get_indirect_targets ~resolution ~receiver ~method_name =
  let receiver_type = Resolution.resolve resolution (Access.expression receiver) in
  let reverse_access = (Access.Identifier method_name) :: List.rev receiver in
  let access = List.rev reverse_access in
  resolve_target ~resolution ~receiver_type access ~reverse_access


let get_targets ~resolution ~global =
  let reverse_access = List.rev global in
  resolve_target ~resolution global ~reverse_access


let resolve_call_targets ~resolution access =
  let get_receiver_type ~reverse_prefix =
    match reverse_prefix with
    | _member :: receiver ->
        Some (Resolution.resolve resolution (Access.expression (List.rev receiver)))
    | _ ->
        None
  in
  let rec accumulate_targets targets reverse_prefix = function
    | Access.Call _ as head :: tail ->
        let access = List.rev reverse_prefix in
        let receiver_type = get_receiver_type ~reverse_prefix in
        let targets =
          List.rev_append
            (resolve_target ~resolution ?receiver_type access ~reverse_access:reverse_prefix)
            targets
        in
        accumulate_targets targets (head :: reverse_prefix) tail
    | (Access.Identifier name as head) :: tail when not (is_local name) ->
        accumulate_targets targets (head :: reverse_prefix) tail
    | head :: tail ->
        accumulate_targets targets (head :: reverse_prefix) tail
    | [] ->
        targets
  in
  accumulate_targets [] [] access
