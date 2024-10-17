(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PyrePysaAnalysis is a sort of choke-point for all Pysa uses of Pyre's `Analysis`
 * *functions*. Its purpose is mainly to better enable coordination and planning:
 * by always using `PyrePysaAnalysis.some_function_name` rather than using `Analysis`
 * directly we allow Pyre and Pysa contributors to:
 * - see at a glance all of the functionality used directly by Pysa
 * - much more easily search for where and how such functionality is used
 *
 * Having this ability to more clearly see all of the dependencies is very helpful
 * for considering how we might loosen the coupling - in particular, we want to
 * understand what else would be needed if we exposed a high-quality typed AST.
 *)

module Cfg = Cfg
module Fixpoint = Fixpoint
module DecoratorPreprocessing = DecoratorPreprocessing
module ClassSummary = ClassSummary
module SharedMemoryKeys = SharedMemoryKeys

let qualified_name_of_define = FunctionDefinition.qualified_name_of_define

module Testing = struct
  module AnalysisError = AnalysisError
end
