(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis

module Request : sig
  type t =
    | Attributes of Reference.t
    | Batch of t list
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | Defines of Reference.t list
    | DumpCallGraph
    | ExpressionLevelCoverage of string list
    | GlobalLeaks of {
        qualifiers: Reference.t list;
        parse_errors: string list;
      }
    | LessOrEqual of Expression.t * Expression.t
    | ModelQuery of {
        path: PyrePath.t;
        query_name: string;
      }
    | ModulesOfPath of PyrePath.t
    | PathOfModule of Reference.t
    | ReferencesUsedByFile of string
    | SaveServerState of PyrePath.t
    | Superclasses of Reference.t list
    | Type of Expression.t
    | TypesInFiles of string list
    | ValidateTaintModels of {
        path: string option;
        verify_dsl: bool;
      }
  [@@deriving equal, show]
end

module Response : sig
  module Base : sig
    type attribute_kind =
      | Regular
      | Property
    [@@deriving equal]

    type attribute = {
      name: string;
      annotation: Type.t;
      kind: attribute_kind;
      final: bool;
    }
    [@@deriving equal, to_yojson]

    type type_at_location = {
      location: Location.t;
      annotation: Type.t;
    }
    [@@deriving equal, to_yojson]

    type types_at_path = {
      path: string;
      types: type_at_location list;
    }
    [@@deriving equal, to_yojson]

    type coverage_at_path = {
      path: string;
      total_expressions: int;
      coverage_gaps: LocationBasedLookup.coverage_gap_by_location list;
    }
    [@@deriving equal, to_yojson]

    type error_at_path = {
      path: string;
      error: string;
    }
    [@@deriving equal, to_yojson, show]

    type coverage_response_at_path =
      | CoverageAtPath of coverage_at_path
      | ErrorAtPath of error_at_path
    [@@deriving equal, to_yojson]

    type callee_with_instantiated_locations = {
      callee: Analysis.Callgraph.callee;
      locations: Location.WithPath.t list;
    }
    [@@deriving equal]

    type callees = {
      caller: Reference.t;
      callees: callee_with_instantiated_locations list;
    }
    [@@deriving equal]

    type parameter_representation = {
      parameter_name: string;
      parameter_annotation: Expression.t option;
    }
    [@@deriving equal]

    type define = {
      define_name: Reference.t;
      parameters: parameter_representation list;
      return_annotation: Expression.t option;
    }
    [@@deriving equal]

    type superclasses_mapping = {
      class_name: Reference.t;
      superclasses: Reference.t list;
    }
    [@@deriving equal]

    type position = {
      line: int;
      character: int;
    }
    [@@deriving equal, to_yojson]

    type range = {
      start: position;
      end_: position;
    }
    [@@deriving equal, to_yojson]

    type global_leak_errors = {
      global_leaks: Analysis.AnalysisError.Instantiated.t list;
      query_errors: string list;
    }
    [@@deriving equal, to_yojson]

    type taint_model = {
      callable: string;
      model: Yojson.Safe.t;
    }
    [@@deriving equal, to_yojson]

    type t =
      | Boolean of bool
      | Callees of Analysis.Callgraph.callee list
      | CalleesWithLocation of callee_with_instantiated_locations list
      | Callgraph of callees list
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | ExpressionLevelCoverageResponse of coverage_response_at_path list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundModels of taint_model list
      | FoundModules of Ast.Reference.t list
      | FoundPath of string
      | GlobalLeakErrors of global_leak_errors
      | ModelVerificationErrors of Taint.ModelVerificationError.t list
      | ReferenceTypesInPath of types_at_path
      | Success of string
      | Superclasses of superclasses_mapping list
      | Type of Type.t
      | TypesByPath of types_at_path list
    [@@deriving equal, to_yojson]
  end

  type t =
    | Single of Base.t
    | Batch of t list
    | Error of string
  [@@deriving equal, to_yojson]

  val create_type_at_location : Location.t * Type.t -> Base.type_at_location
end

val parse_request : string -> (Request.t, string) Core.Result.t

val process_request
  :  type_environment:Analysis.TypeEnvironment.TypeEnvironmentReadOnly.t ->
  build_system:BuildSystem.t ->
  Request.t ->
  Response.t

(* A handy wrapper that invokes `parse_request` followed by `process_request`. *)
val parse_and_process_request
  :  overlaid_environment:Analysis.OverlaidEnvironment.t ->
  build_system:BuildSystem.t ->
  string ->
  string option ->
  Response.t
