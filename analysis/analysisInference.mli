(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

module Annotation = AnalysisAnnotation
module Environment = AnalysisEnvironment
module Error = AnalysisError
module Lookup = AnalysisLookup
module Coverage = AnalysisCoverage
module Cfg = AnalysisCfg
module CallGraph = AnalysisCallGraph
module TypeCheck = AnalysisTypeCheck

open TypeCheck


module State : sig
  include module type of struct include TypeCheck.State end

  val initial_backward
    :  ?configuration: Configuration.t
    -> environment: (module Environment.Handler)
    -> Statement.Define.t Node.t
    -> forward:t
    -> t

  val update_only_existing_annotations: t -> t -> t
  val check_entry: t -> t
end

module Fixpoint : AnalysisFixpoint.Fixpoint with type state := State.t

val backward_fixpoint
  :  Cfg.t
  -> initial_forward:State.t
  -> initialize_backward:(forward:State.t -> State.t)
  -> Fixpoint.t

val infer
  :  Configuration.t
  -> (module Environment.Handler)
  -> (module CallGraph.Handler)
  -> ?mode_override: Source.mode
  -> Source.t
  -> Result.t
