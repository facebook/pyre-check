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
        valid_positions: int list;
      }
  [@@deriving sexp, compare]

  type t = {
    reason: reason;
    overload: Type.t Type.Callable.overload option;
  }
  [@@deriving sexp, compare]

  val strip_overload : t -> t
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
      callable_type: Type.Callable.t;
      errors: IncompatibleModelError.t list;
    }
  | ImportedFunctionModel of {
      name: Reference.t;
      actual_name: Reference.t;
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
      attribute_name: string;
    }
  | MissingSymbol of {
      module_name: string;
      symbol_name: string;
    }
  | ModelingClassAsDefine of string
  | ModelingModuleAsDefine of string
  | ModelingAttributeAsDefine of string
  | ModelingClassAsAttribute of string
  | ModelingModuleAsAttribute of string
  | ModelingCallableAsAttribute of string
  | NotInEnvironment of {
      module_name: string;
      name: string;
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
  | UnsupportedIfCondition of Expression.t
  | UnsupportedVersionConstant of string
  | UnsupportedComparisonOperator of Expression.ComparisonOperator.operator
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
  | InvalidWriteToCacheNameExpression of Expression.t
  | InvalidWriteToCacheNameIdentifier of Identifier.t
  | InvalidWriteToCacheIdentifierForFind of {
      identifier: string;
      find: string;
    }
  | MutuallyExclusiveReadWriteToCache
  | MutuallyExclusiveTaintWriteToCache
[@@deriving sexp, compare]

type t = {
  kind: kind;
  path: PyrePath.t option;
  location: Location.t;
}
[@@deriving sexp, compare, show]

val to_json : t -> Yojson.Safe.t

val display : t -> string

exception ModelVerificationErrors of t list

val verify_models_and_dsl : t list -> bool -> unit
