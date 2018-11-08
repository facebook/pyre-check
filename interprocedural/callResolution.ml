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
    let full_access =
      (Access.Identifier (Identifier.create "__init__")) :: List.rev access
      |> List.rev
    in
    Some full_access
  else
    None


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


let get_indirect_targets ~resolution ~receiver ~method_name =
  let receiver_type =
    Access.expression receiver
    |> Resolution.resolve resolution
  in
  let targets =
    match receiver_type with
    | Type.Primitive primitive ->
        let access = Access.create_from_identifiers [primitive; method_name] in
        let call_target = Callable.create_real access in
        [call_target]
    | Type.Union annotations ->
        let filter_receivers = function
          | Type.Primitive receiver ->
              Access.create_from_identifiers [receiver; method_name]
              |> Callable.create_real
              |> Option.some
          | _ ->
              None
        in
        List.filter_map annotations ~f:filter_receivers
    | _ ->
        []
  in
  targets


let resolve_indirect_targets ~resolution ~reverse_prefix =
  match reverse_prefix with
  | Access.Identifier method_name :: reverse_prefix ->
      let receiver = List.rev reverse_prefix in
      get_indirect_targets ~resolution ~receiver ~method_name
  | _ ->
      (* Can't identify targets *)
      []


let resolve_call_targets ~resolution access =
  let rec accumulate_targets targets reverse_prefix = function
    | Access.Call _ as head :: tail ->
        let targets =
          let access = List.rev reverse_prefix in
          if Resolution.module_definition resolution access |> Option.is_some then
            (* Global name *)
            let access, _ = normalize_global ~resolution access in
            Callable.create_real access :: targets
          else
            resolve_indirect_targets ~resolution ~reverse_prefix
            @ targets
        in
        accumulate_targets targets (head :: reverse_prefix) tail
    | head :: tail ->
        accumulate_targets targets (head :: reverse_prefix) tail
    | [] ->
        targets
  in
  accumulate_targets [] [] access
