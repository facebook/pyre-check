(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Domain = TypeInferenceDomain
module Result = TypeInferenceResult
module Analysis = TypeInferenceAnalysis

module Private = struct
  module SharedMemory = TypeInferenceSharedMemory
end
