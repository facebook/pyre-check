(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val find_stubs_and_sources : Configuration.Analysis.t -> Pyre.Path.t list

type parse_sources_result = {
  parsed: File.Handle.t list;
  syntax_error: File.Handle.t list;
  system_error: File.Handle.t list
}

val parse_sources
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  preprocessing_state:ProjectSpecificPreprocessing.state option ->
  files:File.t list ->
  parse_sources_result

val parse_all
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  Pyre.Path.t list ->
  File.Handle.t list
