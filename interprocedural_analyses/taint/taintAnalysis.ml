(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Callable = Interprocedural.Callable
open Core
open Pyre

(* Registers the Taint analysis with the interprocedural analysis framework. *)
include TaintResult.Register (struct
  include TaintResult

  let init ~configuration ~environment ~functions:_ =
    let global_resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution environment in
    let models = Model.infer_class_models ~environment in
    let taint = Yojson.Safe.Util.member "taint" configuration in
    let verify =
      Yojson.Safe.Util.member "verify_models" taint
      |> Yojson.Safe.Util.to_bool_option
      |> Option.value ~default:true
    in
    let rule_filter =
      if List.mem ~equal:String.equal (Yojson.Safe.Util.keys taint) "rule_filter" then
        Some
          ( Yojson.Safe.Util.member "rule_filter" taint
          |> Yojson.Safe.Util.to_list
          |> List.map ~f:Yojson.Safe.Util.to_int )
      else
        None
    in
    let create_models ~configuration sources =
      List.fold sources ~init:(models, []) ~f:(fun (models, errors) (path, source) ->
          let { ModelParser.T.models; errors = new_errors } =
            ModelParser.parse
              ~resolution:(Analysis.TypeCheck.resolution global_resolution ())
              ~path
              ~source
              ~configuration
              ?rule_filter
              models
          in
          models, List.rev_append new_errors errors)
    in
    let model_paths =
      Yojson.Safe.Util.member "model_paths" taint
      |> Yojson.Safe.Util.to_list
      |> List.map ~f:Yojson.Safe.Util.to_string
    in
    match model_paths with
    | [] -> models
    | _ -> (
        try
          let paths = List.map model_paths ~f:Path.create_absolute in
          let configuration = Configuration.create ~rule_filter ~paths in
          Configuration.register configuration;
          let models, errors = Model.get_model_sources ~paths |> create_models ~configuration in
          List.iter errors ~f:(fun error -> Log.error "%s" error);
          if verify && not (List.is_empty errors) then
            raise (Model.InvalidModel (List.hd_exn errors));
          models
        with
        | exn ->
            Log.error "Error getting taint models.";
            Log.error "%s" (Exn.to_string exn);
            raise exn )


  let analyze ~callable:_ ~environment ~qualifier ~define ~mode existing_model =
    let forward, result, triggered_sinks =
      ForwardAnalysis.run ~environment ~qualifier ~define ~existing_model
    in
    let backward =
      BackwardAnalysis.run ~environment ~qualifier ~define ~existing_model ~triggered_sinks
    in
    let model =
      if mode = Normal then
        { forward; backward; mode }
      else
        { empty_model with mode }
    in
    result, model


  let analyze ~callable ~environment ~qualifier ~define ~existing =
    match existing with
    | Some ({ mode = SkipAnalysis; _ } as model) ->
        let () = Log.info "Skipping taint analysis of %a" Callable.pretty_print callable in
        [], model
    | Some ({ mode; _ } as model) -> analyze ~callable ~environment ~qualifier ~define ~mode model
    | None -> analyze ~callable ~environment ~qualifier ~define ~mode:Normal empty_model
end)
