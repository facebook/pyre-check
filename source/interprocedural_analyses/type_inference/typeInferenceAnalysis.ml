(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
      ~callables:_
      ~stubs:_
    =
    AnalysisResult.InitializedModels.empty


  let analyze ~environment ~callable:_ ~qualifier ~define ~existing:_ =
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    let result =
      (* FIXME: Actually do something as opposed to always returning empty result. *)
      TypeInferenceLocal.empty_infer_for_define ~global_resolution ~qualifier ~define
    in
    result, TypeInferenceDomain.bottom


  let report = TypeInferenceReporting.report
end

include TypeInferenceResult
include Analyzer
include TypeInferenceResult.Register (Analyzer)
