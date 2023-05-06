(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Taint: here we expose modules for the `pyrelib.interprocedural` library *)

module FixpointAnalysis = FixpointAnalysis
module Target = Target
module CallGraph = CallGraph
module CallResolution = CallResolution
module DecoratorHelper = DecoratorHelper
module OverrideGraph = OverrideGraph
module DependencyGraph = DependencyGraph
module DependencyGraphSharedMemory = DependencyGraphSharedMemory
module Error = Error
module ClassHierarchyGraph = ClassHierarchyGraph
module ClassInterval = ClassInterval
module ClassIntervalSet = ClassIntervalSet
module ClassIntervalSetGraph = ClassIntervalSetGraph
module FetchCallables = FetchCallables
module TargetGraph = TargetGraph
module GlobalConstants = GlobalConstants
module ChangedPaths = ChangedPaths
module Metrics = Metrics
