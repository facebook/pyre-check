(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module Kind = AnalysisKind


val one_analysis_pass:
  analyses:Kind.abstract list
  -> Fixpoint.step
  -> callables:Callable.t list -> unit

(* Returns number of iterations. *)
val compute_fixpoint
  :  configuration:Configuration.t
  -> scheduler:Scheduler.t
  -> analyses:Kind.abstract list
  -> caller_map:Analysis.CallGraph.t
  -> all_callables:Callable.t list
  -> Fixpoint.Epoch.t
  -> int

val summaries: Callable.t -> Yojson.Safe.json list

val extract_errors:
  Scheduler.t
  -> configuration: Configuration.t
  -> Callable.t list
  -> InterproceduralError.t list
