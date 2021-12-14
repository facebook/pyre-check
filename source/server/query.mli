(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module Request : sig
  type t =
    | Attributes of Reference.t
    | Batch of t list
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | Defines of Reference.t list
    | DumpCallGraph
    | Help of string
    | IsCompatibleWith of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | PathOfModule of Reference.t
    | SaveServerState of PyrePath.t
    | Superclasses of Reference.t list
    | Type of Expression.t
    | TypesInFiles of string list
    | ValidateTaintModels of string option
    | InlineDecorators of {
        function_reference: Reference.t;
        decorators_to_skip: Reference.t list;
      }
  [@@deriving sexp, compare]

  val inline_decorators : ?decorators_to_skip:Reference.t list -> Reference.t -> t
end

module Response : sig
  module Base : sig
    type attribute_kind =
      | Regular
      | Property
    [@@deriving sexp, compare]

    type attribute = {
      name: string;
      annotation: Type.t;
      kind: attribute_kind;
      final: bool;
    }
    [@@deriving sexp, compare, to_yojson]

    type type_at_location = {
      location: Location.t;
      annotation: Type.t;
    }
    [@@deriving sexp, compare, to_yojson]

    type types_at_path = {
      path: string;
      types: type_at_location list;
    }
    [@@deriving sexp, compare, to_yojson]

    type compatibility = {
      actual: Type.t;
      expected: Type.t;
      result: bool;
    }
    [@@derving sexp, compare]

    type callee_with_instantiated_locations = {
      callee: Analysis.Callgraph.callee;
      locations: Location.WithPath.t list;
    }
    [@@deriving sexp, compare]

    type callees = {
      caller: Reference.t;
      callees: callee_with_instantiated_locations list;
    }
    [@@deriving sexp, compare]

    type parameter_representation = {
      parameter_name: string;
      parameter_annotation: Expression.t option;
    }
    [@@deriving sexp, compare]

    type define = {
      define_name: Reference.t;
      parameters: parameter_representation list;
      return_annotation: Expression.t option;
    }
    [@@deriving sexp, compare]

    type superclasses_mapping = {
      class_name: Reference.t;
      superclasses: Reference.t list;
    }
    [@@deriving sexp, compare]

    type t =
      | Boolean of bool
      | Callees of Analysis.Callgraph.callee list
      | CalleesWithLocation of callee_with_instantiated_locations list
      | Callgraph of callees list
      | Compatibility of compatibility
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundPath of string
      | FunctionDefinition of Statement.Define.t
      | Help of string
      | ModelVerificationErrors of Taint.ModelVerificationError.t list
      | Success of string
      | Superclasses of superclasses_mapping list
      | Type of Type.t
      | TypesByPath of types_at_path list
    [@@deriving sexp, compare, to_yojson]
  end

  type t =
    | Single of Base.t
    | Batch of t list
    | Error of string
  [@@deriving sexp, compare, to_yojson]

  val create_type_at_location : Location.t * Type.t -> Base.type_at_location
end

val help : unit -> string

val parse_request : string -> (Request.t, string) Core.Result.t

(* TODO (T82533515): `environment` should really be typed as `TypeEnvironment.ReadOnly.t`. *)
val process_request
  :  environment:Analysis.TypeEnvironment.t ->
  build_system:BuildSystem.t ->
  configuration:Configuration.Analysis.t ->
  Request.t ->
  Response.t

(* A handy wrapper that invokes `parse_request` followed by `process_request`. *)
(* TODO (T82533515): `environment` should really be typed as `TypeEnvironment.ReadOnly.t`. *)
val parse_and_process_request
  :  environment:Analysis.TypeEnvironment.t ->
  build_system:BuildSystem.t ->
  configuration:Configuration.Analysis.t ->
  string ->
  Response.t
