(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val abstract_kind : Interprocedural.AnalysisKind.abstract

include
  Interprocedural.AnalysisResult.ANALYSIS
    with type result := TypeInferenceResult.result
     and type call_model := TypeInferenceResult.call_model
