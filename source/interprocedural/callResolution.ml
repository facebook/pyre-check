(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Expression
open Pyre

let extract_constant_name { Node.value = expression; _ } =
  match expression with
  | Expression.Constant (Constant.String literal) -> Some literal.value
  | Expression.Constant (Constant.Integer i) -> Some (string_of_int i)
  | Expression.Constant Constant.False -> Some "False"
  | Expression.Constant Constant.True -> Some "True"
  | Expression.Name name -> (
      let name = name_to_reference name >>| Reference.delocalize >>| Reference.last in
      match name with
      (* Heuristic: All uppercase names tend to be enums, so only taint the field in those cases. *)
      | Some name
        when String.for_all name ~f:(fun character ->
                 (not (Char.is_alpha character)) || Char.is_uppercase character) ->
          Some name
      | _ -> None)
  | _ -> None


let is_super ~resolution ~define expression =
  match expression.Node.value with
  | Expression.Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
  | _ ->
      (* We also support explicit calls to superclass constructors. *)
      let annotation = Resolution.resolve_expression_to_type resolution expression in
      if Type.is_meta annotation then
        let type_parameter = Type.single_parameter annotation in
        match type_parameter with
        | Type.Parametric { name = parent_name; _ }
        | Type.Primitive parent_name ->
            let class_name =
              Reference.prefix define.Node.value.Statement.Define.signature.name >>| Reference.show
            in
            class_name
            >>| (fun class_name ->
                  GlobalResolution.is_transitive_successor
                    (Resolution.global_resolution resolution)
                    ~predecessor:class_name
                    ~successor:parent_name)
            |> Option.value ~default:false
        | _ -> false
      else
        false
