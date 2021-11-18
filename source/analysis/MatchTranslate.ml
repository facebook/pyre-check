(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Expression
open Statement

let to_condition ~subject:_ ~case:_ =
  Expression.Constant Constant.True |> Node.create_with_default_location


let and_two left right =
  Expression.BooleanOperator { left; operator = BooleanOperator.And; right }
  |> Ast.Node.create_with_default_location


let name_identifier name =
  Expression.Name (Name.Identifier name) |> Ast.Node.create_with_default_location


let walrus_operator ~target ~value =
  Expression.WalrusOperator { target; value } |> Ast.Node.create_with_default_location


let equals left right =
  Expression.ComparisonOperator { left; operator = ComparisonOperator.Equals; right }
  |> Ast.Node.create_with_default_location


let rec pattern_to_condition ~subject { Ast.Node.location; value = pattern } =
  (match pattern with
  | Match.Pattern.MatchAs { pattern; name } -> (
      let name = name_identifier name in
      let capture = equals (walrus_operator ~target:name ~value:subject) name in
      match pattern >>| pattern_to_condition ~subject:name with
      | Some condition -> and_two capture condition
      | None -> capture)
  | MatchValue value -> equals subject value
  | MatchWildcard -> Expression.Constant Constant.True |> Ast.Node.create_with_default_location
  | _ -> Expression.Constant Constant.False |> Ast.Node.create_with_default_location)
  |> fun expression -> { expression with location }
