(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Pyre

exception InvalidQuery of string

module Request : sig
  type t =
    | Attributes of Reference.t
    | Batch of t list
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | Defines of Reference.t list
    | DumpCallGraph
    | DumpClassHierarchy
    | Help of string
    | IsCompatibleWith of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | PathOfModule of Reference.t
    | SaveServerState of Path.t
    | Superclasses of Expression.t list
    | Type of Expression.t
    | TypesInFiles of Path.t list
    | ValidateTaintModels of Path.t option
  [@@deriving eq, show]
end

module Response : sig
  module Base : sig
    type attribute_kind =
      | Regular
      | Property

    type attribute = {
      name: string;
      annotation: Type.t;
      kind: attribute_kind;
      final: bool;
    }
    [@@deriving eq, show, to_yojson]

    type type_at_location = {
      location: Location.t;
      annotation: Type.t;
    }
    [@@deriving eq, show, to_yojson]

    type types_at_path = {
      path: PyrePath.t;
      types: type_at_location list;
    }
    [@@deriving eq, show, to_yojson]

    type compatibility = {
      actual: Type.t;
      expected: Type.t;
      result: bool;
    }
    [@@derving eq, show]

    type callee_with_instantiated_locations = {
      callee: Analysis.Callgraph.callee;
      locations: Location.WithPath.t list;
    }
    [@@deriving eq, show]

    type callees = {
      caller: Reference.t;
      callees: callee_with_instantiated_locations list;
    }
    [@@deriving eq, show]

    type parameter_representation = {
      parameter_name: string;
      parameter_annotation: Expression.t option;
    }
    [@@deriving eq, show]

    type define = {
      define_name: Reference.t;
      parameters: parameter_representation list;
      return_annotation: Expression.t option;
    }
    [@@deriving eq, show]

    type superclasses_mapping = {
      class_name: Reference.t;
      superclasses: Type.t list;
    }
    [@@deriving eq, show]

    type t =
      | Boolean of bool
      | Callees of Analysis.Callgraph.callee list
      | CalleesWithLocation of callee_with_instantiated_locations list
      | Callgraph of callees list
      | ClassHierarchy of Yojson.Safe.t
      | Compatibility of compatibility
      | Errors of Analysis.AnalysisError.Instantiated.t list
      | FoundAttributes of attribute list
      | FoundDefines of define list
      | FoundPath of string
      | Help of string
      | ModelVerificationErrors of Taint.Model.ModelVerificationError.t list
      | Success of string
      | Superclasses of superclasses_mapping list
      | Type of Type.t
      | TypesByPath of types_at_path list
    [@@deriving eq, show, to_yojson]
  end

  type t =
    | Single of Base.t
    | Batch of t list
    | Error of string
  [@@deriving eq, show, to_yojson]

  val create_type_at_location : Location.t * Type.t -> Base.type_at_location
end

val help : unit -> string

val parse_query : configuration:Configuration.Analysis.t -> string -> Request.t
