(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Kind = AnalysisKind


val one_analysis_pass:
  analyses:Kind.abstract list
  -> Fixpoint.step
  -> environment: (module Analysis.Environment.Handler)
  -> callables:Callable.t list
  -> int  (* Returns num processed for progress *)

(* Returns number of iterations. *)
val compute_fixpoint
  :  configuration:Configuration.Analysis.t
  -> scheduler:Scheduler.t
  -> environment: (module Analysis.Environment.Handler)
  -> analyses:Kind.abstract list
  -> dependencies:DependencyGraph.t
  -> all_callables:Callable.t list
  -> Fixpoint.Epoch.t
  -> int

val externalize: AnalysisKind.abstract -> Callable.t -> Yojson.Safe.json list

val extract_errors:
  Scheduler.t
  -> configuration: Configuration.Analysis.t
  -> Callable.t list
  -> InterproceduralError.t list

val save_results:
  configuration: Configuration.StaticAnalysis.t ->
  analyses: AnalysisKind.abstract list
  -> Callable.t list
  -> unit
