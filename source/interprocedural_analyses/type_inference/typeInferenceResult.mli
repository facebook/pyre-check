(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include
  Interprocedural.AnalysisResult.ANALYSIS_RESULT_WITH_REGISTRATION
    with type result = TypeInferenceData.LocalResult.t
     and type call_model = TypeInferenceDomain.t
