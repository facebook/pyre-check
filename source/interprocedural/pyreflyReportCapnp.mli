(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Cap'n Proto parsing functions for pyrefly report files. *)

module ProjectFile : sig
  val from_path_exn : PyrePath.t -> PyreflyReport.ProjectFile.t
end

module ModuleDefinitionsFile : sig
  val from_path_exn
    :  pyrefly_directory:PyrePath.t ->
    PyreflyReport.ModuleInfoFilename.t ->
    PyreflyReport.ModuleDefinitionsFile.t
end

module ModuleTypeOfExpressions : sig
  val from_path_exn
    :  pyrefly_directory:PyrePath.t ->
    PyreflyReport.ModuleInfoFilename.t ->
    PyreflyReport.ModuleTypeOfExpressions.t
end

module ModuleCallGraphs : sig
  val from_path_exn
    :  pyrefly_directory:PyrePath.t ->
    PyreflyReport.ModuleInfoFilename.t ->
    PyreflyReport.ModuleCallGraphs.t
end

module TypeErrors : sig
  val from_path_exn : pyrefly_directory:PyrePath.t -> PyreflyReport.TypeErrors.t
end
