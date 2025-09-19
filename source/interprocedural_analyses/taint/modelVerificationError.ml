(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ModelVerificationError: defines any error that can be produced by parsing
 * pysa model files (`.pysa`). *)

open Core
open Ast
module AccessPath = Analysis.TaintAccessPath
module PyrePysaApi = Interprocedural.PyrePysaApi

module IncompatibleModelError = struct
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
        valid_roots: AccessPath.Root.t list;
      }
  [@@deriving equal, compare, show]

  type t = {
    reason: reason;
    overload: PyrePysaApi.ModelQueries.FunctionSignature.t option;
  }
  [@@deriving equal, compare, show]

  let strip_overload { reason; _ } = { reason; overload = None }
end

module FormatStringError = struct
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

  let description = function
    | InvalidExpression expression ->
        Format.asprintf "unsupported expression `%a`" Expression.pp expression
    | InvalidIdentifier identifier -> Format.asprintf "unknown identifier `%s`" identifier
    | InvalidIdentifierForFind { identifier; find } ->
        Format.asprintf "invalid identifier `%s` for find=\"%s\"" identifier find
    | InvalidIdentifierForContext identifier ->
        Format.asprintf "identifier `%s` is invalid in this context" identifier
    | InvalidIdentifierInIntegerExpression identifier ->
        Format.asprintf "identifier `%s` cannot be used in an integer expression" identifier
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
      callable_signatures: PyrePysaApi.ModelQueries.FunctionSignature.t list;
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
  | MissingClass of { class_name: string }
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
  | UnsupportedFullyQualifiedCalleeInClassConstraint
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
[@@deriving equal, compare, show]

type t = {
  kind: kind;
  path: PyrePath.t option;
  location: Location.t;
}
[@@deriving equal, compare, show]

let pp_function_signature
    formatter
    { PyrePysaApi.ModelQueries.FunctionSignature.parameters; return_annotation }
  =
  let () = Format.fprintf formatter "def " in
  let () =
    match parameters with
    | PyrePysaApi.ModelQueries.FunctionParameters.List parameters ->
        let pp_parameter index parameter =
          let () =
            if index > 0 then
              Format.fprintf formatter ", "
          in
          let () =
            match parameter with
            | PyrePysaApi.ModelQueries.FunctionParameter.PositionalOnly { name = Some name; _ }
            | Named { name; _ }
            | KeywordOnly { name; _ }
            | Variable { name = Some name; _ }
            | Keywords { name = Some name; _ } ->
                Format.fprintf formatter "%s" (Identifier.sanitized name)
            | PositionalOnly { name = None; position; _ } ->
                Format.fprintf formatter "__arg%d" position
            | Variable { name = None; _ } -> Format.fprintf formatter "*args"
            | Keywords { name = None; _ } -> Format.fprintf formatter "**kwargs"
          in
          let () =
            match PyrePysaApi.ModelQueries.FunctionParameter.annotation parameter with
            | Some annotation ->
                Format.fprintf formatter ": %a" PyrePysaApi.PysaType.pp_concise annotation
            | None -> ()
          in
          let () =
            if PyrePysaApi.ModelQueries.FunctionParameter.has_default parameter then
              Format.fprintf formatter " = ..."
          in
          ()
        in
        Format.fprintf formatter "(";
        List.iteri parameters ~f:pp_parameter;
        Format.fprintf formatter ")";
        ()
    | PyrePysaApi.ModelQueries.FunctionParameters.Ellipsis -> Format.fprintf formatter "(...)"
    | PyrePysaApi.ModelQueries.FunctionParameters.ParamSpec ->
        Format.fprintf formatter "(ParamSpec[T])"
  in
  Format.fprintf formatter " -> %a: ..." PyrePysaApi.PysaType.pp_concise return_annotation


let pp_function_signatures formatter signatures =
  let pp_overload index signature =
    let () = if index > 0 then Format.fprintf formatter " | " in
    pp_function_signature formatter signature
  in
  List.iteri ~f:pp_overload signatures


let description error =
  match error with
  | ParseError -> "Syntax error."
  | UnexpectedStatement statement ->
      Format.sprintf "Unexpected statement: `%s`" (Statement.show statement)
  | InvalidDefaultValue { callable_name; name; expression } ->
      Format.sprintf
        "Default values of `%s`'s parameters must be `...`. Did you mean to write `%s: %s`?"
        callable_name
        name
        (Expression.show expression)
  | IncompatibleModelError { name; callable_signatures; errors } ->
      let errors =
        List.map errors ~f:(fun { reason; overload } ->
            let reason =
              match reason with
              | UnexpectedPositionalOnlyParameter { name; valid_positions = []; _ } ->
                  Format.sprintf "unexpected positional only parameter: `%s`" name
              | UnexpectedPositionalOnlyParameter
                  { name; position; valid_positions = [valid_position] } ->
                  Format.sprintf
                    "unexpected positional only parameter: `%s` at position: %d, expected position \
                     %d"
                    name
                    position
                    valid_position
              | UnexpectedPositionalOnlyParameter { name; position; valid_positions } ->
                  Format.sprintf
                    "unexpected positional only parameter: `%s` at position: %d (%d not in {%s})"
                    name
                    position
                    position
                    (List.map ~f:string_of_int valid_positions |> String.concat ~sep:", ")
              | UnexpectedNamedParameter name ->
                  Format.sprintf "unexpected named parameter: `%s`" name
              | UnexpectedStarredParameter -> "unexpected star parameter"
              | UnexpectedDoubleStarredParameter -> "unexpected star star parameter"
              | InvalidNamedParameterPosition { name; position; valid_roots } ->
                  Format.sprintf
                    "invalid position %d for named parameter `%s` (valid options are {%s})"
                    position
                    name
                    (List.map ~f:AccessPath.Root.show valid_roots |> String.concat ~sep:", ")
            in
            match overload with
            | Some overload ->
                Format.asprintf "%s in overload `%a`" reason pp_function_signature overload
            | None -> reason)
      in
      let reasons =
        match errors with
        | [error] -> Format.sprintf "Reason: %s." error
        | errors -> Format.sprintf "Reasons:\n%s" (String.concat errors ~sep:"\n")
      in
      Format.asprintf
        "Model signature parameters for `%s` do not match implementation `%a`. %s"
        name
        pp_function_signatures
        callable_signatures
        reasons
  | ImportedFunctionModel { name; actual_name } ->
      Format.asprintf
        "The modelled function `%a` is an imported function, please model `%a` directly."
        Reference.pp
        name
        Reference.pp
        actual_name
  | ModelQueryUnsupportedNamedParameter name ->
      Format.asprintf
        "Unsupported named parameter `%s` in model query (expected: name, find, where, model, \
         expected_models, unexpected_models)."
        name
  | ModelQueryUnnamedParameter argument ->
      Format.asprintf
        "Unsupported unnamed parameter `%a` in model query (expected: name, find, where, model, \
         expected_models, unexpected_models)."
        Expression.pp
        argument
  | ModelQueryMissingRequiredParameter name ->
      Format.asprintf "Missing required parameter `%s` in model query." name
  | ModelQueryDuplicateParameter name ->
      Format.asprintf "Duplicate parameter `%s` in model query." name
  | InvalidModelQueryNameClause argument ->
      Format.asprintf
        "Expected string literal for `name` argument, got `%a`."
        Expression.pp
        argument
  | InvalidModelQueryWhereClause { expression; find_clause_kind } ->
      Format.asprintf
        "`%s` is not a valid constraint for model queries with find clause of kind `%s`."
        (Expression.show expression)
        find_clause_kind
  | InvalidModelQueryModelClause { expression; find_clause_kind } ->
      Format.asprintf
        "`%s` is not a valid model for model queries with find clause of kind `%s`."
        (Expression.show expression)
        find_clause_kind
  | InvalidArgumentsClause expression ->
      Format.asprintf "`%s` is not a valid arguments clause." (Expression.show expression)
  | InvalidNameClause expression ->
      Format.asprintf "`%s` is not a valid name clause." (Expression.show expression)
  | InvalidTypeAnnotationClause expression ->
      Format.asprintf "`%s` is not a valid type annotation clause." (Expression.show expression)
  | InvalidDecoratorClause expression ->
      Format.asprintf "`%s` is not a valid decorator clause." (Expression.show expression)
  | InvalidParameterExclude expression ->
      Format.asprintf
        "The AllParameters exclude must be either a string or a list of strings, got: `%s`."
        (Expression.show expression)
  | InvalidIsTransitive expression ->
      Format.asprintf
        "The Extends and AnyChild `is_transitive` attribute must be either True or False, got: \
         `%s`."
        (Expression.show expression)
  | InvalidIncludesSelf expression ->
      Format.asprintf
        "The Extends and AnyChild `includes_self` attribute must be either True or False, got: \
         `%s`."
        (Expression.show expression)
  | InvalidModelQueryClauseArguments { callee; arguments } ->
      Format.asprintf
        "Unsupported arguments for `%a`: `%a`."
        Expression.pp
        callee
        Expression.pp
        (Node.create_with_default_location
           (Expression.Expression.Call { Expression.Call.callee; arguments; origin = None }))
  | InvalidTaintAnnotation { taint_annotation; reason } ->
      Format.asprintf
        "`%s` is an invalid taint annotation: %s"
        (Expression.show taint_annotation)
        reason
  | InvalidAccessPath { access_path; reason } ->
      Format.asprintf "`%s` is an invalid access path: %s" (Expression.show access_path) reason
  | UnexpectedDecorators { name; unexpected_decorators } ->
      let decorators = List.map unexpected_decorators ~f:Expression.show in
      let property_decorator_message =
        if List.exists decorators ~f:(String.is_substring ~substring:"property") then
          " If you're looking to model a custom property decorator, use the @property decorator."
        else
          ""
      in
      Format.asprintf
        "Unexpected decorators found when parsing model for `%a`: `%s`.%s"
        Reference.pp
        name
        (String.concat decorators ~sep:", ")
        property_decorator_message
  | InvalidFindClauseType clause_type ->
      Format.sprintf "Find clauses must be strings, got: `%s`" (Expression.show clause_type)
  | InvalidIdentifier expression ->
      Format.sprintf
        "Invalid identifier: `%s`. Expected a fully-qualified name."
        (Expression.show expression)
  | InvalidReturnAnnotation { model_name; annotation } ->
      Format.sprintf "Invalid model for `%s`: Invalid return annotation `%s`." model_name annotation
  | ClassBodyNotEllipsis class_name ->
      Format.sprintf "Class model for `%s` must have a body of `...`." class_name
  | DefineBodyNotEllipsis model_name ->
      Format.sprintf "Callable model for `%s` must have a body of `...`." model_name
  | MissingAttribute { class_name; attribute_name } ->
      Format.sprintf "Class `%s` has no attribute `%s`." class_name attribute_name
  | MissingSymbol { module_name; symbol_name } ->
      Format.sprintf "Module `%s` does not define `%s`." module_name symbol_name
  | MissingClass { class_name } -> Format.sprintf "Class `%s` does not exist." class_name
  | ModelingClassAsDefine class_name ->
      Format.sprintf
        "The class `%s` is not a valid define - did you mean to model `%s.__init__()`?"
        class_name
        class_name
  | ModelingModuleAsDefine module_name ->
      Format.sprintf "The module `%s` is not a valid define." module_name
  | ModelingAttributeAsDefine attribute_name ->
      Format.sprintf
        "The attribute `%s` is not a valid define - did you mean to use `%s: ...`?"
        attribute_name
        attribute_name
  | ModelingClassAsAttribute class_name ->
      Format.sprintf
        "The class `%s` is not a valid attribute - did you mean to model `%s.__init__()`?"
        class_name
        class_name
  | ModelingModuleAsAttribute module_name ->
      Format.sprintf "The module `%s` is not a valid attribute." module_name
  | ModelingCallableAsAttribute callable_name ->
      Format.sprintf
        "The function, method or property `%s` is not a valid attribute - did you mean to use `def \
         %s(): ...`?"
        callable_name
        callable_name
  | NotInEnvironment { module_name; name } ->
      Format.sprintf
        "`%s` is not part of the environment, no module `%s` in search path."
        name
        module_name
  | UnexpectedTaintAnnotation taint_annotation ->
      Format.sprintf "Unexpected taint annotation `%s`" taint_annotation
  | UnsupportedConstraint constraint_name ->
      Format.sprintf "Unsupported constraint expression: `%s`" (Expression.show constraint_name)
  | UnsupportedConstraintCallee callee ->
      Format.sprintf "Unsupported callee for constraint: `%s`" (Expression.show callee)
  | UnsupportedClassConstraint constraint_name ->
      Format.sprintf
        "Unsupported class constraint expression: `%s`"
        (Expression.show constraint_name)
  | UnsupportedClassConstraintCallee callee ->
      Format.sprintf "Unsupported callee for class constraint: `%s`" (Expression.show callee)
  | UnsupportedDecoratorConstraint constraint_name ->
      Format.sprintf
        "Unsupported decorator constraint expression: `%s`"
        (Expression.show constraint_name)
  | UnsupportedDecoratorConstraintCallee callee ->
      Format.sprintf "Unsupported callee for decorator constraint: `%s`" (Expression.show callee)
  | UnsupportedFullyQualifiedCalleeInClassConstraint ->
      "Unsupported `fully_qualified_callee` constraint within a class constraint"
  | UnsupportedIfCondition condition ->
      Format.sprintf
        "Unsupported if condition: `%s`. If conditions need to be of the form: `sys.version \
         operator version_tuple`. All models inside the if-block (along with those in else-if and \
         else block, if present) will be ignored."
        (Expression.show condition)
  | UnsupportedVersionConstant error ->
      Format.sprintf "Unsupported element type in version tuple in if condition: %s" error
  | UnsupportedComparisonOperator operator ->
      Format.asprintf
        "The operator `%a` in the if condition is not supported"
        Expression.ComparisonOperator.pp_comparison_operator
        operator
  | DeprecatedConstraint { deprecated; suggested } ->
      Format.sprintf "Constraint `%s` is deprecated, use `%s` instead." deprecated suggested
  | UnsupportedFindClause clause -> Format.sprintf "Unsupported find clause `%s`" clause
  | UnexpectedModelExpression expression ->
      Format.sprintf "Unexpected model expression: `%s`" (Expression.show expression)
  | InvalidModelForTaint { model_name; error } ->
      Format.sprintf "Invalid model for `%s`: %s" model_name error
  | InvalidAnnotationForAttributeModel { name; annotation } ->
      Format.sprintf
        "Invalid annotation for attribute model `%s`: `%s`."
        (Reference.show name)
        annotation
  | DuplicateNameClauses name ->
      Format.sprintf
        "Multiple model queries have the same name `%s`. Model\n\
        \   query names should be unique within each file."
        name
  | NoOutputFromModelQuery model_query_name ->
      Format.sprintf "Model Query `%s` output no models." model_query_name
  | NoOutputFromModelQueryGroup logging_group_name ->
      Format.sprintf "Model Query group `%s` output no models." logging_group_name
  | ExpectedModelsAreMissing { model_query_name; models } ->
      let starting_string =
        Format.sprintf
          "The output of ModelQuery `%s` did not match the following expected models: ["
          model_query_name
      in
      let strings = List.map models ~f:(fun model -> Format.sprintf "\"%s\"; " model) in
      List.fold strings ~init:starting_string ~f:( ^ ) ^ "]"
  | UnexpectedModelsArePresent { model_query_name; models } ->
      let starting_string =
        Format.sprintf
          "The output of ModelQuery `%s` matched the following unexpected models: ["
          model_query_name
      in
      let strings = List.map models ~f:(fun model -> Format.sprintf "\"%s\"; " model) in
      List.fold strings ~init:starting_string ~f:( ^ ) ^ "]"
  | ModelQueryInExpectedModelsClause { model_query_name; model_source } ->
      Format.sprintf
        "In ModelQuery `%s`: Model string `%s` is a ModelQuery, not a model.\n\
        \    Please make sure that the model string is a syntactically correct model."
        model_query_name
        model_source
  | InvalidExpectedModelsClause { model_query_name; models_clause } ->
      Format.asprintf
        "In ModelQuery `%s`: Clause `%s` is not a valid expected_models or unexpected_models clause.\n\
        \   The clause should be a list of syntactically correct model strings."
        model_query_name
        (Expression.show models_clause)
  | InvalidModelQueryMode { mode_name; error } -> Format.asprintf "`%s`: %s" mode_name error
  | InvalidReadFromCacheArguments constraint_expression ->
      Format.asprintf
        "Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and \
         `name` with string literal arguments, got `%a`"
        Expression.pp
        constraint_expression
  | InvalidReadFromCacheConstraint constraint_expression ->
      Format.asprintf
        "Invalid constraint: `read_from_cache` clause cannot be nested under `AnyOf` or `Not` \
         clauses in `%a`"
        Expression.pp
        constraint_expression
  | InvalidWriteToCacheArguments model_expression ->
      Format.asprintf
        "Invalid arguments for `WriteToCache` clause: expected a named parameter `kind` with a \
         literal string argument, and a named parameter `name` with a format string argument, got \
         `%a`"
        Expression.pp
        model_expression
  | InvalidWriteToCacheName error ->
      Format.asprintf
        "Invalid argument for the parameter `name` of `WriteToCache`: %s"
        (FormatStringError.description error)
  | MutuallyExclusiveReadWriteToCache ->
      "WriteToCache and read_from_cache cannot be used in the same model query"
  | MutuallyExclusiveTaintWriteToCache ->
      "WriteToCache cannot be used with other taint annotations in the same model query"
  | InvalidCrossRepositoryTaintAnchorString { argument; value } ->
      Format.asprintf
        "Expected string for %s of CrossRepositoryTaintAnchor, got `%a`"
        argument
        Expression.pp
        value
  | InvalidCrossRepositoryTaintAnchorFormatString { argument; error } ->
      Format.asprintf
        "Invalid %s for CrossRepositoryTaintAnchor: %s"
        argument
        (FormatStringError.description error)
  | UnmatchedPartialSinkKind partial_sink ->
      Format.asprintf
        "Cannot find any matching partial sink for `%s`"
        (Sinks.show (Sinks.PartialSink partial_sink))
  | DeprecatedIsAnnotatedType { expression; find_clause_kind } ->
      Format.asprintf
        "In `%a`: `is_annotated_type` is deprecated for find clause of kind `%s`."
        Expression.pp
        expression
        find_clause_kind
  | DeprecatedParametricTaintAnnotation annotation ->
      Format.sprintf "Parametric taint annotation `%s` is deprecated." annotation
  | UnsupportedOriginalTypeAnnotation { expression; find_clause_kind } ->
      Format.asprintf
        "In `%a`: `original.equals` or `original.matches` is not supported for find clause of kind \
         `%s`. Use `fully_qualified.equals` or `fully_qualified.matches` instead."
        Expression.pp
        expression
        find_clause_kind
  | DeprecatedAnnotationEquals { expression } ->
      Format.asprintf
        "In `%a`: `.equals` is deprecated. Use `fully_qualified.equals` or `original.matches` \
         instead."
        Expression.pp
        expression
  | DeprecatedAnnotationMatches { expression } ->
      Format.asprintf
        "In `%a`: `.matches` is deprecated. Use `fully_qualified.matches` or `original.matches` \
         instead."
        Expression.pp
        expression


let code { kind; _ } =
  match kind with
  | InvalidDefaultValue _ -> 1
  | IncompatibleModelError _ -> 2
  | ImportedFunctionModel _ -> 3
  | MissingAttribute _ -> 5
  | NotInEnvironment _ -> 6
  | UnexpectedDecorators _ -> 7
  | InvalidParameterExclude _ -> 8
  | InvalidTaintAnnotation _ -> 9
  | ModelingClassAsDefine _ -> 10
  | InvalidModelQueryWhereClause _ -> 11
  | InvalidModelQueryModelClause _ -> 12
  | InvalidIsTransitive _ -> 13
  | InvalidModelQueryClauseArguments _ -> 14
  | InvalidIdentifier _ -> 15
  | UnexpectedStatement _ -> 16
  | ModelingModuleAsDefine _ -> 17
  | ModelingAttributeAsDefine _ -> 18
  | ModelingClassAsAttribute _ -> 19
  | ModelingModuleAsAttribute _ -> 20
  | ModelingCallableAsAttribute _ -> 21
  | ClassBodyNotEllipsis _ -> 22
  | DefineBodyNotEllipsis _ -> 23
  | InvalidNameClause _ -> 24
  | ParseError -> 25
  | InvalidArgumentsClause _ -> 26
  | InvalidTypeAnnotationClause _ -> 27
  | MissingSymbol _ -> 28
  | UnsupportedConstraintCallee _ -> 29
  | UnexpectedTaintAnnotation _ -> 30
  | UnexpectedModelExpression _ -> 31
  | UnsupportedFindClause _ -> 32
  | UnsupportedConstraint _ -> 33
  | InvalidFindClauseType _ -> 34
  | InvalidReturnAnnotation _ -> 35
  | InvalidModelForTaint _ -> 36
  | InvalidAnnotationForAttributeModel _ -> 38
  | InvalidDecoratorClause _ -> 39
  | DuplicateNameClauses _ -> 40
  | NoOutputFromModelQuery _ -> 41
  | ExpectedModelsAreMissing _ -> 42
  | UnexpectedModelsArePresent _ -> 43
  | ModelQueryInExpectedModelsClause _ -> 44
  | InvalidExpectedModelsClause _ -> 45
  | UnsupportedClassConstraint _ -> 46
  | InvalidAccessPath _ -> 47
  | InvalidModelQueryMode _ -> 48
  | InvalidIncludesSelf _ -> 49
  | UnsupportedClassConstraintCallee _ -> 50
  | UnsupportedDecoratorConstraint _ -> 51
  | UnsupportedDecoratorConstraintCallee _ -> 52
  | InvalidReadFromCacheArguments _ -> 53
  | InvalidWriteToCacheArguments _ -> 54
  | InvalidWriteToCacheName _ -> 55
  (* 56-57: unused *)
  | InvalidReadFromCacheConstraint _ -> 58
  | MutuallyExclusiveReadWriteToCache -> 59
  | MutuallyExclusiveTaintWriteToCache -> 60
  | DeprecatedConstraint _ -> 61
  | ModelQueryUnsupportedNamedParameter _ -> 62
  | ModelQueryUnnamedParameter _ -> 63
  | ModelQueryMissingRequiredParameter _ -> 64
  | ModelQueryDuplicateParameter _ -> 65
  | InvalidModelQueryNameClause _ -> 66
  | NoOutputFromModelQueryGroup _ -> 67
  | UnsupportedIfCondition _ -> 68
  | UnsupportedVersionConstant _ -> 69
  | UnsupportedComparisonOperator _ -> 70
  | InvalidCrossRepositoryTaintAnchorString _ -> 71
  | InvalidCrossRepositoryTaintAnchorFormatString _ -> 72
  | UnmatchedPartialSinkKind _ -> 73
  | UnsupportedFullyQualifiedCalleeInClassConstraint -> 74
  | MissingClass _ -> 75
  | DeprecatedIsAnnotatedType _ -> 76
  | DeprecatedParametricTaintAnnotation _ -> 77
  | UnsupportedOriginalTypeAnnotation _ -> 78
  | DeprecatedAnnotationEquals _ -> 79
  | DeprecatedAnnotationMatches _ -> 80


let display { kind = error; path; location } =
  let model_origin =
    match path with
    | None -> ""
    | Some path -> Format.sprintf "%s:%d: " (PyrePath.absolute path) Location.(location.start.line)
  in
  Format.sprintf "%s%s" model_origin (description error)


let to_json ({ kind; path; location } as error) =
  let path =
    match path with
    | None -> `Null
    | Some path -> `String (PyrePath.absolute path)
  in
  `Assoc
    [
      "description", `String (description kind);
      "line", `Int Location.(location.start.line);
      "column", `Int Location.(location.start.column);
      "stop_line", `Int Location.(location.stop.line);
      "stop_column", `Int Location.(location.stop.column);
      "path", path;
      "code", `Int (code error);
    ]


exception ModelVerificationErrors of t list

let verify_models_and_dsl ~raise_exception errors =
  if not (List.is_empty errors) then
    (* Exit or log errors, depending on whether models need to be verified. *)
    if not raise_exception then begin
      Log.error "Found %d model verification errors!" (List.length errors);
      List.iter errors ~f:(fun error -> Log.error "%s" (display error))
    end
    else
      raise (ModelVerificationErrors errors)
