(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Taint: here we expose modules for the `pyrelib.taint` library *)

module AccessPath = AccessPath
module BackwardAnalysis = BackwardAnalysis
module Rule = Rule
module SourceSinkFilter = SourceSinkFilter
module TaintConfiguration = TaintConfiguration
module Domains = Domains
module MultiSourcePostProcessing = MultiSourcePostProcessing
module Sanitize = Sanitize
module SanitizeTransform = SanitizeTransform
module SanitizeTransformSet = SanitizeTransformSet
module TaintTransform = TaintTransform
module TaintTransforms = TaintTransforms
module TaintProfiler = TaintProfiler
module Features = Features
module Issue = Issue
module IssueHandle = IssueHandle
module ForwardAnalysis = ForwardAnalysis
module MissingFlow = MissingFlow
module Model = Model
module ModelParseResult = ModelParseResult
module ModelParser = ModelParser
module ModelQueryExecution = ModelQueryExecution
module ModelVerifier = ModelVerifier
module ModelVerificationError = ModelVerificationError
module AnnotationParser = AnnotationParser
module ClassModels = ClassModels
module Reporting = TaintReporting
module Sinks = Sinks
module Sources = Sources
module Fixpoint = TaintFixpoint
module Registry = Registry
module Cache = Cache
module CallModel = CallModel
