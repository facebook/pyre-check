(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Taint: here we expose modules for the `pyrelib.interprocedural` library *)

module PyreflyApi = PyreflyApi
module PyrePysaApi = PyrePysaApi
module FixpointAnalysis = FixpointAnalysis
module Target = Target
module CallablesSharedMemory = CallablesSharedMemory
module CallGraph = CallGraph
module CallGraphFixpoint = CallGraphFixpoint
module CallResolution = CallResolution
module OverrideGraph = OverrideGraph
module DependencyGraph = DependencyGraph
module Error = Error
module ClassHierarchyGraph = ClassHierarchyGraph
module ClassInterval = ClassInterval
module ClassIntervalSet = ClassIntervalSet
module ClassIntervalSetGraph = ClassIntervalSetGraph
module FetchCallables = FetchCallables
module TargetGraph = TargetGraph
module GlobalConstants = GlobalConstants
module ChangedPaths = ChangedPaths
module SaveLoadSharedMemory = SaveLoadSharedMemory
module NewlineDelimitedJson = NewlineDelimitedJson
module RepositoryPath = RepositoryPath
module IntraproceduralProfiler = IntraproceduralProfiler
module CallGraphProfiler = CallGraphProfiler
module TypeOfExpressionSharedMemory = TypeOfExpressionSharedMemory
module ExpressionIdentifier = ExpressionIdentifier
