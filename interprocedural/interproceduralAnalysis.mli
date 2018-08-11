(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis

module Callable = InterproceduralCallable
module Fixpoint = InterproceduralFixpoint
module Kind = InterproceduralAnalysisKind


val one_analysis_pass:
  analyses:Kind.abstract list
  -> Fixpoint.step
  -> schedule:Callable.t list -> unit

(* Returns number of iterations. *)
val compute_fixpoint
  :  ?workers:Hack_parallel.Std.Worker.t list
  -> analyses:Kind.abstract list
  -> caller_map:CallGraph.t
  -> all_callables:Callable.t list
  -> Fixpoint.Epoch.t
  -> int

val summaries: Callable.t -> Yojson.Safe.json list
