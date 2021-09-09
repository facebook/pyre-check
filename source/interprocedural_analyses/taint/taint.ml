(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module AccessPath = AccessPath
module BackwardAnalysis = BackwardAnalysis
module CallGraphBuilder = CallGraphBuilder
module TaintConfiguration = TaintConfiguration
module PartialSinkConverter = PartialSinkConverter
module Domains = Domains
module Features = Features
module Flow = Flow
module ForwardAnalysis = ForwardAnalysis
module ModelParser = ModelParser
module AnnotationParser = AnnotationParser

module Model = struct
  include Model
  include ModelParser.T

  let parse = ModelParser.parse

  let verify_model_syntax = ModelParser.verify_model_syntax

  let display_verification_error = ModelVerificationError.display

  let verification_error_to_json = ModelVerificationError.to_json

  let register_verification_errors = ModelVerificationError.register

  module ModelVerificationError = ModelVerificationError.T

  type taint_annotation = ModelParser.T.taint_annotation =
    | Sink of {
        sink: Sinks.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
        leaf_names: Features.LeafName.t list;
        leaf_name_provided: bool;
      }
    | Source of {
        source: Sources.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
        leaf_names: Features.LeafName.t list;
        leaf_name_provided: bool;
      }
    | Tito of {
        tito: Sinks.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
      }
    | AddFeatureToArgument of {
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
      }
    | Sanitize of sanitize_annotation list
  [@@deriving show, compare]

  (* Exposed for testing. *)
  let demangle_class_attribute = ModelVerifier.demangle_class_attribute

  let compute_sources_and_sinks_to_keep = ModelParser.compute_sources_and_sinks_to_keep

  let create_model_from_annotations = ModelParser.create_callable_model_from_annotations

  let resolve_global = ModelVerifier.resolve_global

  module Global = ModelVerifier.Global
end

module Result = TaintResult
module Reporting = TaintReporting
module Sinks = Sinks
module Sources = Sources
