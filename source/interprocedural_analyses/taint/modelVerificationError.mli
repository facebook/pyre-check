(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module IncompatibleModelError : sig
  type reason =
    | UnexpectedPositionalOnlyParameter of {
        name: string;
        position: int;
        valid_positions: int list;
      }
    | UnexpectedNamedParameter of string
    | UnexpectedStarredParameter
    | UnexpectedDoubleStarredParameter
    | InvalidNamedParameterPosition of {
        name: string;
        position: int;
        valid_roots: Analysis.TaintAccessPath.Root.t list;
      }
  [@@deriving equal, compare]

  type t = {
    reason: reason;
    overload: Interprocedural.PyrePysaApi.ModelQueries.FunctionSignature.t option;
  }
  [@@deriving equal, compare]

  val strip_overload : t -> t
end

module FormatStringError : sig
  type t =
    | InvalidExpression of Expression.t
    | InvalidIdentifier of string
    | InvalidIdentifierForFind of {
        identifier: string;
        find: string;
      }
    | InvalidIdentifierForContext of string
    | InvalidIdentifierInIntegerExpression of string
  [@@deriving equal, compare, show]

  val description : t -> string
end

module SourceLocation : sig
  type t = {
    path: string;
    location: Location.t option;
  }
  [@@deriving equal, compare]

  val pp : Format.formatter -> t -> unit

  val show : t -> string
end

type kind =
  | ParseError
  | UnexpectedStatement of Statement.t
  | InvalidDefaultValue of {
      callable_name: string;
      name: string;
      expression: Expression.t;
    }
  | IncompatibleModelError of {
      name: string;
      callable_signatures: Interprocedural.PyrePysaApi.ModelQueries.FunctionSignature.t list;
      errors: IncompatibleModelError.t list;
      define_location: SourceLocation.t option;
    }
  | ImportedFunctionModel of {
      name: Reference.t;
      actual_name: Reference.t;
      define_location: SourceLocation.t option;
    }
  | ModelQueryUnsupportedNamedParameter of string
  | ModelQueryUnnamedParameter of Expression.t
  | ModelQueryMissingRequiredParameter of string
  | ModelQueryDuplicateParameter of string
  | InvalidModelQueryNameClause of Expression.t
  | InvalidModelQueryWhereClause of {
      expression: Expression.t;
      find_clause_kind: string;
    }
  | InvalidModelQueryModelClause of {
      expression: Expression.t;
      find_clause_kind: string;
    }
  | InvalidParameterExclude of Expression.t
  | InvalidIsTransitive of Expression.t
  | InvalidIncludesSelf of Expression.t
  | InvalidModelQueryClauseArguments of {
      callee: Expression.t;
      arguments: Expression.Call.Argument.t list;
    }
  | InvalidArgumentsClause of Expression.t
  | InvalidNameClause of Expression.t
  | InvalidTypeAnnotationClause of Expression.t
  | InvalidDecoratorClause of Expression.t
  | InvalidTaintAnnotation of {
      taint_annotation: Expression.t;
      reason: string;
    }
  | InvalidAccessPath of {
      access_path: Expression.t;
      reason: string;
    }
  | MissingAttribute of {
      class_name: string;
      class_location: SourceLocation.t option;
      attribute_name: string;
    }
  | MissingSymbol of {
      module_name: string;
      module_path: string option;
      symbol_name: string;
    }
  | MissingClass of {
      class_name: string;
      module_name: string;
      module_path: string option;
    }
  | ModelingClassAsDefine of {
      name: string;
      class_location: SourceLocation.t option;
    }
  | ModelingModuleAsDefine of {
      name: string;
      module_path: string option;
    }
  | ModelingAttributeAsDefine of {
      name: string;
      attribute_location: SourceLocation.t option;
    }
  | ModelingClassAsAttribute of {
      name: string;
      class_location: SourceLocation.t option;
    }
  | ModelingModuleAsAttribute of {
      name: string;
      module_path: string option;
    }
  | ModelingCallableAsAttribute of {
      name: string;
      callable_location: SourceLocation.t option;
    }
  | ModelingCallableAsClass of {
      name: string;
      callable_location: SourceLocation.t option;
    }
  | ModelingModuleAsClass of {
      name: string;
      module_path: string option;
    }
  | ModelingAttributeAsClass of {
      name: string;
      attribute_location: SourceLocation.t option;
    }
  | BaseModuleNotInEnvironment of {
      module_name: string;
      name: string;
    }
  | GroupedBaseModuleNotInEnvironment of {
      module_name: string;
      count: int;
    }
  | UnexpectedDecorators of {
      name: Reference.t;
      unexpected_decorators: Expression.t list;
    }
  | InvalidIdentifier of Expression.t
  | ClassBodyNotEllipsis of string
  | DefineBodyNotEllipsis of string
  | UnexpectedTaintAnnotation of string
  | UnexpectedModelExpression of Expression.t
  | UnsupportedFindClause of string
  | InvalidFindClauseType of Expression.t
  | InvalidReturnAnnotation of {
      model_name: string;
      annotation: string;
    }
  | UnsupportedConstraint of Expression.t
  | UnsupportedConstraintCallee of Expression.t
  | UnsupportedClassConstraint of Expression.t
  | UnsupportedClassConstraintCallee of Expression.t
  | UnsupportedDecoratorConstraint of Expression.t
  | UnsupportedDecoratorConstraintCallee of Expression.t
  | UnsupportedFullyQualifiedCalleeInClassConstraint
  | UnsupportedIfCondition of Expression.t
  | UnsupportedVersionConstant of string
  | UnsupportedComparisonOperator of Expression.ComparisonOperator.operator
  | UnsupportedPlatformComparison of Expression.ComparisonOperator.operator
  | DeprecatedConstraint of {
      deprecated: string;
      suggested: string;
    }
  | InvalidModelForTaint of {
      model_name: string;
      error: string;
    }
  | InvalidAnnotationForAttributeModel of {
      name: Reference.t;
      annotation: string;
    }
  | DuplicateNameClauses of string
  | NoOutputFromModelQuery of string
  | NoOutputFromModelQueryGroup of string
  | ExpectedModelsAreMissing of {
      model_query_name: string;
      models: string list;
    }
  | UnexpectedModelsArePresent of {
      model_query_name: string;
      models: string list;
    }
  | ModelQueryInExpectedModelsClause of {
      model_query_name: string;
      model_source: string;
    }
  | InvalidExpectedModelsClause of {
      model_query_name: string;
      models_clause: Expression.t;
    }
  | InvalidModelQueryMode of {
      mode_name: string;
      error: string;
    }
  | InvalidReadFromCacheArguments of Expression.t
  | InvalidReadFromCacheConstraint of Expression.t
  | InvalidWriteToCacheArguments of Expression.t
  | InvalidWriteToCacheName of FormatStringError.t
  | MutuallyExclusiveReadWriteToCache
  | MutuallyExclusiveTaintWriteToCache
  | InvalidCrossRepositoryTaintAnchorString of {
      argument: string;
      value: Expression.t;
    }
  | InvalidCrossRepositoryTaintAnchorFormatString of {
      argument: string;
      error: FormatStringError.t;
    }
  | UnmatchedPartialSinkKind of Sinks.PartialSink.t
  | DeprecatedIsAnnotatedType of {
      expression: Expression.t;
      find_clause_kind: string;
    }
  | DeprecatedParametricTaintAnnotation of string
  | UnsupportedOriginalTypeAnnotation of {
      expression: Expression.t;
      find_clause_kind: string;
    }
  | DeprecatedAnnotationEquals of { expression: Expression.t }
  | DeprecatedAnnotationMatches of { expression: Expression.t }
[@@deriving equal, compare]

type t = {
  kind: kind;
  path: PyrePath.t option;
  location: Location.t;
}
[@@deriving equal, compare, show]

val to_json : t -> Yojson.Safe.t

val display : t -> string

exception ModelVerificationErrors of t list

val log_model_verification_errors : raise_exception:bool -> t list -> unit
