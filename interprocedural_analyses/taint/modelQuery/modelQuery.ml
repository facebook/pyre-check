(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Interprocedural
open Taint
open Model

let matches_pattern ~pattern name = Re2.matches (Re2.create_exn pattern) name

let matches_constraint query_constraint ~callable =
  match query_constraint with
  | ModelQuery.NameConstraint pattern ->
      matches_pattern ~pattern (Callable.external_target_name callable)


let apply_productions ~resolution ~productions ~callable =
  let definition = Callable.get_module_and_definition ~resolution callable in
  match definition with
  | None -> []
  | Some
      ( _,
        {
          Node.value =
            { Statement.Define.signature = { Statement.Define.Signature.parameters; _ }; _ };
          _;
        } ) ->
      let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
      let apply_production = function
        | ModelQuery.ReturnTaint returned_annotations ->
            List.map returned_annotations ~f:(fun returned_annotation ->
                ReturnAnnotation, returned_annotation)
        | ModelQuery.ParameterTaint { name; taint } -> (
            let parameter =
              List.find_map normalized_parameters ~f:(fun (root, parameter_name, _) ->
                  if Identifier.equal_sanitized parameter_name name then Some root else None)
            in
            match parameter with
            | Some parameter ->
                List.map taint ~f:(fun taint -> ParameterAnnotation parameter, taint)
            | None -> [] )
        | ModelQuery.AllParametersTaint taint ->
            let roots =
              List.map normalized_parameters ~f:(fun (root, _, _) -> ParameterAnnotation root)
            in
            List.cartesian_product roots taint
      in
      List.concat_map productions ~f:apply_production


let apply_query_rule ~resolution ~rule:{ ModelQuery.rule_kind; query; productions } ~callable =
  let kind_matches =
    match callable, rule_kind with
    | `Function _, ModelQuery.FunctionModel
    | `Method _, ModelQuery.MethodModel ->
        true
    | _ -> false
  in

  if kind_matches && List.for_all ~f:(matches_constraint ~callable) query then
    apply_productions ~resolution ~productions ~callable
  else
    []
