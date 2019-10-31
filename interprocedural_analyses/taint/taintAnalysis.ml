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
    (* Parse models *)
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
      let global_resolution =
        Analysis.AnnotatedGlobalEnvironment.ReadOnly.resolution environment
      in
      List.fold sources ~init:Callable.Map.empty ~f:(fun models (path, source) ->
          Model.parse
            ~resolution:(Analysis.TypeCheck.resolution global_resolution ())
            ~path
            ~source
            ~configuration
            ~verify
            ?rule_filter
            models)
    in
    let model_directories =
      Yojson.Safe.Util.member "model_directories" taint
      |> Yojson.Safe.Util.to_list
      |> List.map ~f:Yojson.Safe.Util.to_string
    in
    match model_directories with
    | [] -> Callable.Map.empty
    | _ -> (
      try
        let directories = List.map model_directories ~f:Path.create_absolute in
        let configuration = Configuration.create ~rule_filter ~directories in
        Configuration.register configuration;
        Model.get_model_sources ~directories |> create_models ~configuration
      with
      | exn ->
          Log.error "Error getting taint models: %s" (Exn.to_string exn);
          raise exn )


  let analyze ~callable:_ ~environment ~define ~mode existing_model =
    let forward, result = ForwardAnalysis.run ~environment ~define ~existing_model in
    let backward = BackwardAnalysis.run ~environment ~define ~existing_model in
    let model =
      if mode = Normal then
        { forward; backward; mode }
      else
        { empty_model with mode }
    in
    result, model


  let analyze ~callable ~environment ~define ~existing =
    match existing with
    | Some ({ mode = SkipAnalysis; _ } as model) ->
        let () = Log.info "Skipping taint analysis of %a" Callable.pretty_print callable in
        [], model
    | Some ({ mode; _ } as model) -> analyze ~callable ~environment ~define ~mode model
    | None -> analyze ~callable ~environment ~define ~mode:Normal empty_model
end)
