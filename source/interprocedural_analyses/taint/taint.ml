(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module AccessPath = AccessPath
module BackwardAnalysis = BackwardAnalysis
module CallGraphBuilder = CallGraphBuilder
module TaintConfiguration = TaintConfiguration
module PartialSinkConverter = PartialSinkConverter
module Domains = Domains
module SanitizeTransform = SanitizeTransform
module TaintTransform = TaintTransform
module TaintTransforms = TaintTransforms
module TaintProfiler = TaintProfiler
module Features = Features
module Issue = Issue
module ForwardAnalysis = ForwardAnalysis
module MissingFlow = MissingFlow
module Model = Model
module ModelParser = ModelParser
module ModelVerifier = ModelVerifier
module ModelVerificationError = ModelVerificationError
module AnnotationParser = AnnotationParser
module ClassModels = ClassModels
module Result = TaintResult
module Reporting = TaintReporting
module Sinks = Sinks
module Sources = Sources
module ExitStatus = ExitStatus
module Fixpoint = Fixpoint
module Registry = Registry
module Cache = Cache
