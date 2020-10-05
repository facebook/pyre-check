(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
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
        | ModelQuery.PositionalParameterTaint { index; taint } -> (
            let parameter =
              List.find_map normalized_parameters ~f:(fun (root, _, _) ->
                  match root with
                  | AccessPath.Root.PositionalParameter { position; _ } when position = index ->
                      Some root
                  | _ -> None)
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


let apply_query_rule
    ~verbose
    ~resolution
    ~rule:{ ModelQuery.rule_kind; query; productions; name }
    ~callable
  =
  let kind_matches =
    match callable, rule_kind with
    | `Function _, ModelQuery.FunctionModel
    | `Method _, ModelQuery.MethodModel ->
        true
    | _ -> false
  in

  if kind_matches && List.for_all ~f:(matches_constraint ~callable) query then begin
    if verbose then
      Log.info
        "Callable `%a` matches all constraints for the model query rule%s."
        Callable.pretty_print
        (callable :> Callable.t)
        (name |> Option.map ~f:(Format.sprintf " `%s`") |> Option.value ~default:"");
    apply_productions ~resolution ~productions ~callable
  end
  else
    []


let apply_all_rules ~resolution ~scheduler ~configuration ~rule_filter ~rules ~callables ~models =
  let global_resolution = Resolution.global_resolution resolution in
  if List.length rules > 0 then
    let sources_to_keep, sinks_to_keep =
      ModelParser.compute_sources_and_sinks_to_keep ~configuration ~rule_filter
    in
    let apply_rules models callable =
      let taint_to_model =
        List.concat_map rules ~f:(fun rule ->
            apply_query_rule
              ~verbose:configuration.dump_model_query_results
              ~resolution:global_resolution
              ~rule
              ~callable)
      in
      if not (List.is_empty taint_to_model) then
        match
          ModelParser.create_model_from_annotations
            ~resolution
            ~callable
            ~sources_to_keep
            ~sinks_to_keep
            taint_to_model
        with
        | Some model ->
            let models =
              let model =
                match Callable.Map.find models (callable :> Callable.t) with
                | Some existing_model -> Taint.Result.join ~iteration:0 existing_model model
                | None -> model
              in
              Callable.Map.set models ~key:(callable :> Callable.t) ~data:model
            in
            models
        | _ -> models
      else
        models
    in
    let callables =
      List.filter_map callables ~f:(function
          | `Function _ as callable -> Some (callable :> Callable.real_target)
          | `Method _ as callable -> Some (callable :> Callable.real_target)
          | _ -> None)
    in
    let merge_models new_models models =
      Map.merge_skewed new_models models ~combine:(fun ~key:_ left right ->
          Taint.Result.join ~iteration:0 left right)
    in
    let new_models =
      Scheduler.map_reduce
        scheduler
        ~policy:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunk_size:500
             ~preferred_chunks_per_worker:1
             ())
        ~initial:Callable.Map.empty
        ~map:(fun models callables -> List.fold callables ~init:models ~f:apply_rules)
        ~reduce:(fun new_models models ->
          Map.merge_skewed new_models models ~combine:(fun ~key:_ left right ->
              Taint.Result.join ~iteration:0 left right))
        ~inputs:callables
        ()
    in
    merge_models new_models models
  else
    models
