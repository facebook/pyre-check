(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module AccessPath = AccessPath
module BackwardAnalysis = BackwardAnalysis
module CallGraphBuilder = CallGraphBuilder
module TaintConfiguration = Configuration
module PartialSinkConverter = PartialSinkConverter
module Domains = Domains
module Features = Features
module Flow = Flow
module ForwardAnalysis = ForwardAnalysis
module ModelParser = ModelParser

module Model = struct
  include Model
  include ModelParser.T

  let parse = ModelParser.parse

  let verify_model_syntax = ModelParser.verify_model_syntax

  (* Exposed for testing. *)
  let demangle_class_attribute = ModelVerifier.demangle_class_attribute
end

module Result = TaintResult
module Sinks = Sinks
module Sources = Sources
