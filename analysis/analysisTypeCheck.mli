(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement

module Annotation = AnalysisAnnotation
module Environment = AnalysisEnvironment
module Error = AnalysisError
module Lookup = AnalysisLookup
module Coverage = AnalysisCoverage
module Cfg = AnalysisCfg
module CallGraph = AnalysisCallGraph
module Resolution = AnalysisResolution


module State : sig
  type t = {
    configuration: Configuration.t;
    resolution: Resolution.t;
    errors: Error.t Location.Map.t;
    define: Define.t Node.t;
    lookup: Lookup.t option;
  }
  [@@deriving eq, show]

  val create
    :  ?configuration: Configuration.t
    -> environment: (module Environment.Handler)
    -> annotations: (Access.t * Annotation.t) list
    -> define: Statement.Define.t Node.t
    -> ?lookup: Lookup.t
    -> unit
    -> t

  val errors: t -> Error.t list
  val coverage: t -> Coverage.t

  val initial
    :  ?configuration: Configuration.t
    -> ?lookup: Lookup.t
    -> (module Environment.Handler)
    -> Statement.Define.t Node.t
    -> t

  include AnalysisFixpoint.State with type t := t
end

module Fixpoint : AnalysisFixpoint.Fixpoint with type state := State.t

module Result : sig
  type t = {
    errors: Error.t list;
    lookup: Lookup.t option;
    coverage: Coverage.t;
  }
end

val check
  :  Configuration.t
  -> (module Environment.Handler)
  -> (module CallGraph.Handler)
  -> ?mode_override: Source.mode
  -> Source.t
  -> Result.t
