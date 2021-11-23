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

let create_boolean_and ~location ~left ~right =
  Expression.BooleanOperator { left; operator = BooleanOperator.And; right }
  |> Node.create ~location


let create_boolean_or ~location ~left ~right =
  Expression.BooleanOperator { left; operator = BooleanOperator.Or; right } |> Node.create ~location


let create_constant ~location constant = Expression.Constant constant |> Node.create ~location

let create_name ~location name = Expression.Name name |> Node.create ~location

let create_attribute_name ~location ~base ~attribute =
  create_name ~location (Name.Attribute { base; attribute; special = false })


let create_identifier_name ~location name = create_name ~location (Name.Identifier name)

let create_walrus ~location ~target ~value =
  Expression.WalrusOperator { target; value } |> Node.create ~location


let create_comparison_equals ~location ~left ~right =
  Expression.ComparisonOperator { left; operator = ComparisonOperator.Equals; right }
  |> Node.create ~location


let create_comparison_is ~location left right =
  Expression.ComparisonOperator { left; operator = ComparisonOperator.Is; right }
  |> Node.create ~location


let create_call ~location ~callee ~arguments =
  Expression.Call { callee; arguments } |> Node.create ~location


let create_getitem ~location ~container ~key =
  create_call
    ~location
    ~callee:
      (create_name
         ~location
         (Name.Attribute { base = container; attribute = "__getitem__"; special = true }))
    ~arguments:[{ Call.Argument.value = key; name = None }]


let create_getitem_index ~location ~sequence ~index =
  create_getitem
    ~location
    ~container:sequence
    ~key:(create_constant ~location (Constant.Integer index))


let create_isinstance ~location object_expression type_expression =
  create_call
    ~location
    ~callee:(create_identifier_name ~location "isinstance")
    ~arguments:
      [
        { Call.Argument.value = object_expression; name = None };
        { Call.Argument.value = type_expression; name = None };
      ]


let create_getattr ~location base attribute =
  create_call
    ~location
    ~callee:(create_identifier_name ~location "getattr")
    ~arguments:
      [
        { Call.Argument.value = base; name = None }; { Call.Argument.value = attribute; name = None };
      ]


let rec pattern_to_condition ~subject { Node.location; value = pattern } =
  match pattern with
  | Match.Pattern.MatchAs { pattern; name } -> (
      let name = create_identifier_name ~location name in
      let capture =
        create_comparison_equals
          ~location
          ~left:(create_walrus ~location ~target:name ~value:subject)
          ~right:name
      in
      match pattern >>| pattern_to_condition ~subject:name with
      | Some condition -> create_boolean_and ~location ~left:capture ~right:condition
      | None -> capture)
  | MatchClass { cls; patterns; keyword_attributes; keyword_patterns } ->
      let of_positional_pattern index =
        let attribute =
          create_getitem_index
            ~location
            ~sequence:(create_attribute_name ~location ~base:subject ~attribute:"__match_args__")
            ~index
        in
        pattern_to_condition ~subject:(create_getattr ~location subject attribute)
      in
      let of_attribute_pattern attribute =
        pattern_to_condition ~subject:(create_attribute_name ~location ~base:subject ~attribute)
      in
      create_isinstance ~location subject (create_name ~location cls)
      :: List.mapi ~f:of_positional_pattern patterns
      @ List.map2_exn ~f:of_attribute_pattern keyword_attributes keyword_patterns
      |> List.reduce_exn ~f:(fun left right -> create_boolean_and ~location ~left ~right)
  | MatchMapping { keys; patterns; _ } ->
      let of_key_pattern key =
        pattern_to_condition ~subject:(create_getitem ~location ~container:subject ~key)
      in
      List.map2_exn keys patterns ~f:of_key_pattern
      |> List.reduce_exn ~f:(fun left right -> create_boolean_and ~location ~left ~right)
  | MatchOr patterns ->
      List.map patterns ~f:(pattern_to_condition ~subject)
      |> List.reduce_exn ~f:(fun left right -> create_boolean_or ~location ~left ~right)
  | MatchSingleton constant ->
      create_comparison_is ~location subject (create_constant ~location constant)
  | MatchSequence patterns ->
      let prefix, _rest, suffix =
        let is_not_star_pattern = function
          | { Ast.Node.value = Match.Pattern.MatchStar _; _ } -> false
          | _ -> true
        in
        let prefix, star_and_suffix = List.split_while patterns ~f:is_not_star_pattern in
        match star_and_suffix with
        | { Node.value = Match.Pattern.MatchStar rest; _ } :: suffix -> prefix, rest, suffix
        | _ -> prefix, None, []
      in
      let suffix_length = List.length suffix in
      let of_prefix_pattern index =
        pattern_to_condition ~subject:(create_getitem_index ~location ~sequence:subject ~index)
      in
      let of_suffix_pattern index =
        pattern_to_condition
          ~subject:(create_getitem_index ~location ~sequence:subject ~index:(index - suffix_length))
      in
      List.mapi prefix ~f:of_prefix_pattern @ List.mapi suffix ~f:of_suffix_pattern
      |> List.reduce_exn ~f:(fun left right -> create_boolean_and ~location ~left ~right)
  | MatchValue value -> create_comparison_equals ~location ~left:subject ~right:value
  | MatchWildcard -> Expression.Constant Constant.True |> Node.create ~location
  | _ -> Expression.Constant Constant.False |> Node.create ~location


let to_condition ~subject ~case:{ Match.Case.pattern = { Node.location; _ } as pattern; guard; _ } =
  match pattern_to_condition ~subject pattern, guard with
  | pattern_condition, None -> pattern_condition
  | { Node.value = Expression.Constant Constant.True; _ }, Some guard -> guard
  | pattern_condition, Some guard ->
      create_boolean_and ~location ~left:pattern_condition ~right:guard
