(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Interprocedural

module Analyzer = struct
  let initialize_configuration
      ~static_analysis_configuration:{ Configuration.StaticAnalysis.configuration; _ }
    =
    TypeInferenceSharedMemory.register_configuration configuration


  let initialize_models
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~functions:_
      ~stubs:_
    =
    { Result.initial_models = Callable.Map.empty; skip_overrides = Ast.Reference.Set.empty }


  let analyze ~environment ~callable:_ ~qualifier ~define ~existing:_ =
    let configuration = TypeInferenceSharedMemory.get_configuration () in
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    let ast_environment = GlobalResolution.ast_environment global_resolution in
    let maybe_source = AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier in
    let lookup = TypeInferenceData.lookup ~configuration ~global_resolution in
    let result =
      let errors =
        match maybe_source with
        | None -> []
        | Some ({ Ast.Source.source_path; _ } as source) ->
            if Inference.skip_infer ~configuration source_path then
              []
            else
              Inference.infer_for_define ~configuration ~global_resolution ~source ~define
      in
      List.fold
        ~init:
          (TypeInferenceData.LocalResult.from_signature
             ~global_resolution
             ~lookup
             ~qualifier
             define)
        ~f:(TypeInferenceData.LocalResult.add_missing_annotation_error ~global_resolution ~lookup)
        errors
    in
    result, TypeInferenceDomain.bottom


  let report = TypeInferenceReporting.report
end

include TypeInferenceResult
include Analyzer
include TypeInferenceResult.Register (Analyzer)
