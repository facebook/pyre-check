(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Analysis

type result = {
  handles: File.Handle.t list;
  module_tracker: ModuleTracker.t;
  environment: (module Environment.Handler);
  errors: Error.t list
}

val analyze_sources
  :  ?open_documents:string Pyre.Path.Map.t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:(module Environment.Handler) ->
  handles:File.Handle.t list ->
  unit ->
  Error.t list

val check : scheduler:Scheduler.t option -> configuration:Configuration.Analysis.t -> result
