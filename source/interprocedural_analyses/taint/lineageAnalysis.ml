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
open Domains
open Pyre

type panda_type = DataFrame

let get_panda_type resolution = function
  | { Node.value = Expression.Name (Name.Attribute { base; _ }); _ } -> (
      let class_name =
        Resolution.resolve_expression_to_type resolution base |> Type.class_name |> Reference.last
      in
      match class_name with
      | "DataFrame" -> Some DataFrame
      | _ -> None )
  | _ -> None


let is_string_list list =
  let is_string element =
    match element.Node.value with
    | Expression.String _ -> true
    | _ -> false
  in
  List.for_all ~f:is_string list


let store_taint ?(weak = false) { AccessPath.root; path } taint state =
  ForwardState.assign ~weak ~root ~path taint state


let store_taint_option ?(weak = false) access_path taint state =
  match access_path with
  | Some path -> store_taint ~weak path taint state
  | None -> state


let analyze_dataFrame_getitem base_taint argument =
  let collapsed_taint =
    ForwardState.Tree.collapse ~transform:Fn.id base_taint |> ForwardState.Tree.create_leaf
  in
  match argument.Node.value with
  | Expression.String _ -> ForwardState.Tree.read [AccessPath.get_index argument] base_taint
  | List arguments when is_string_list arguments ->
      let fold_taint position taint_so_far arg =
        let label = [AccessPath.get_index arg] in
        let index_label = [Abstract.TreeDomain.Label.Index (string_of_int position)] in
        let taint_tree = ForwardState.Tree.read label base_taint in
        let label_taint = ForwardState.Tree.prepend label taint_tree in
        ForwardState.Tree.prepend index_label taint_tree
        |> ForwardState.Tree.join label_taint
        |> ForwardState.Tree.join taint_so_far
      in
      List.foldi arguments ~init:ForwardState.Tree.empty ~f:fold_taint
  | _ -> collapsed_taint


let analyze_dataFrame_setitem resolution base taint index state =
  let root_path = AccessPath.of_expression ~resolution base in
  match index.Node.value with
  | Expression.String _ ->
      let access_path = root_path >>| AccessPath.extend ~path:[AccessPath.get_index index] in
      store_taint_option access_path taint state
  | List arguments when is_string_list arguments ->
      let fold_state position state_so_far arg =
        let index_label = [Abstract.TreeDomain.Label.Index (string_of_int position)] in
        let access_path = root_path >>| AccessPath.extend ~path:[AccessPath.get_index arg] in
        let taint = ForwardState.Tree.read index_label taint in
        store_taint_option access_path taint state_so_far
      in
      List.foldi arguments ~init:state ~f:fold_state
  | _ -> state


let analyze_dataFrame analyze_expression resolution callee arguments taint state =
  match callee with
  | { Node.value = Expression.Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ } -> (
      match arguments with
      | [{ Call.Argument.value = index; _ }] ->
          let base_taint, state = analyze_expression ~resolution ~state ~expression:base in
          analyze_dataFrame_getitem base_taint index, state
      | _ -> taint, state )
  | { Node.value = Expression.Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } -> (
      match arguments with
      | [{ Call.Argument.value = index; _ }; { Call.Argument.value; _ }] ->
          let taint, state = analyze_expression ~resolution ~state ~expression:value in
          let state = analyze_dataFrame_setitem resolution base taint index state in
          ForwardState.Tree.empty, state
      | _ -> taint, state )
  | _ -> taint, state


let forward_analyze_call ~analyze_expression ~resolution ~callee ~arguments ~taint ~state =
  match get_panda_type resolution callee with
  | Some DataFrame -> analyze_dataFrame analyze_expression resolution callee arguments taint state
  | _ -> taint, state
