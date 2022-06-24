(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

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
        valid_positions: int list;
      }
  [@@deriving sexp, compare, show]

  type t = {
    reason: reason;
    overload: Type.t Type.Callable.overload option;
  }
  [@@deriving sexp, compare, show]

  let strip_overload { reason; _ } = { reason; overload = None }
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
  | InvalidModelQueryClauses of Expression.Call.Argument.t list
  | InvalidModelQueryWhereClause of {
      expression: Expression.t;
      find_clause_kind: string;
    }
  | InvalidModelQueryModelClause of {
      expression: Expression.t;
      find_clause_kind: string;
    }
  | InvalidParameterExclude of Expression.t
  | InvalidExtendsIsTransitive of Expression.t
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
  | UnsupportedCallee of Expression.t
  | UnexpectedTaintAnnotation of string
  | UnexpectedModelExpression of Expression.t
  | UnsupportedFindClause of string
  | InvalidFindClauseType of Expression.t
  | InvalidReturnAnnotation of {
      model_name: string;
      annotation: string;
    }
  | UnsupportedConstraint of Expression.t
  | InvalidModelForTaint of {
      model_name: string;
      error: string;
    }
  | NoCorrespondingCallable of string
  | InvalidAnnotationForAttributeModel of {
      name: Reference.t;
      annotation: string;
    }
  | DuplicateNameClauses of string
  | NoOutputFromModelQuery of string
[@@deriving sexp, compare, show]

type t = {
  kind: kind;
  path: PyrePath.t option;
  location: Location.t;
}
[@@deriving sexp, compare, show]

let description error =
  match error with
  | ParseError -> "Syntax error."
  | UnexpectedStatement _ -> "Unexpected statement."
  | InvalidDefaultValue { callable_name; name; expression } ->
      Format.sprintf
        "Default values of `%s`'s parameters must be `...`. Did you mean to write `%s: %s`?"
        callable_name
        name
        (Expression.show expression)
  | IncompatibleModelError { name; callable_type; errors } ->
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
              | InvalidNamedParameterPosition { name; position; valid_positions } ->
                  Format.sprintf
                    "invalid position for named parameter `%s` (%d not in {%s})"
                    name
                    position
                    (List.map ~f:string_of_int valid_positions |> String.concat ~sep:", ")
            in
            match overload with
            | Some overload ->
                Format.asprintf
                  "%s in overload `%s`"
                  reason
                  (Type.show_for_hover
                     (Type.Callable { kind = Anonymous; implementation = overload; overloads = [] }))
            | None -> reason)
      in
      let reasons =
        match errors with
        | [error] -> Format.sprintf "Reason: %s." error
        | errors -> Format.sprintf "Reasons:\n%s" (String.concat errors ~sep:"\n")
      in
      Format.asprintf
        "Model signature parameters for `%s` do not match implementation `%s`. %s"
        name
        (Type.show_for_hover (Type.Callable callable_type))
        reasons
  | ImportedFunctionModel { name; actual_name } ->
      Format.asprintf
        "The modelled function `%a` is an imported function, please model `%a` directly."
        Reference.pp
        name
        Reference.pp
        actual_name
  | InvalidModelQueryClauses clause_list ->
      Format.asprintf
        "The model query arguments at `%s` are invalid: expected a name, find, where and model \
         clause."
        (List.map clause_list ~f:Expression.Call.Argument.show |> String.concat ~sep:", ")
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
  | InvalidExtendsIsTransitive expression ->
      Format.asprintf
        "The Extends is_transitive must be either True or False, got: `%s`."
        (Expression.show expression)
  | InvalidModelQueryClauseArguments { callee; arguments } ->
      Format.asprintf
        "Unsupported arguments for callee `%s`: `%s`."
        (Expression.show callee)
        (List.map arguments ~f:Expression.Call.Argument.show |> String.concat ~sep:", ")
  | InvalidTaintAnnotation { taint_annotation; reason } ->
      Format.asprintf
        "`%s` is an invalid taint annotation: %s"
        (Expression.show taint_annotation)
        reason
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
      Format.sprintf "Unsupported constraint: %s" (Expression.show constraint_name)
  | UnsupportedCallee callee -> Format.sprintf "Unsupported callee: %s" (Expression.show callee)
  | UnsupportedFindClause clause -> Format.sprintf "Unsupported find clause `%s`" clause
  | UnexpectedModelExpression expression ->
      Format.sprintf "Unexpected model expression: `%s`" (Expression.show expression)
  | InvalidModelForTaint { model_name; error } ->
      Format.sprintf "Invalid model for `%s`: %s" model_name error
  | NoCorrespondingCallable callable ->
      Format.sprintf "No callable corresponding to `%s` found." callable
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
      Format.sprintf "ModelQuery `%s` output no models." model_query_name


let code { kind; _ } =
  match kind with
  | InvalidDefaultValue _ -> 1
  | IncompatibleModelError _ -> 2
  | ImportedFunctionModel _ -> 3
  | InvalidModelQueryClauses _ -> 4
  | MissingAttribute _ -> 5
  | NotInEnvironment _ -> 6
  | UnexpectedDecorators _ -> 7
  | InvalidParameterExclude _ -> 8
  | InvalidTaintAnnotation _ -> 9
  | ModelingClassAsDefine _ -> 10
  | InvalidModelQueryWhereClause _ -> 11
  | InvalidModelQueryModelClause _ -> 12
  | InvalidExtendsIsTransitive _ -> 13
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
  | UnsupportedCallee _ -> 29
  | UnexpectedTaintAnnotation _ -> 30
  | UnexpectedModelExpression _ -> 31
  | UnsupportedFindClause _ -> 32
  | UnsupportedConstraint _ -> 33
  | InvalidFindClauseType _ -> 34
  | InvalidReturnAnnotation _ -> 35
  | InvalidModelForTaint _ -> 36
  | NoCorrespondingCallable _ -> 37
  | InvalidAnnotationForAttributeModel _ -> 38
  | InvalidDecoratorClause _ -> 39
  | DuplicateNameClauses _ -> 40
  | NoOutputFromModelQuery _ -> 41


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


module SharedMemory =
  Memory.WithCache.Make
    (Memory.SingletonKey)
    (struct
      type nonrec t = t list

      let prefix = Prefix.make ()

      let description = "Model verification errors"
    end)

let register errors =
  let () =
    if SharedMemory.mem Memory.SingletonKey.key then
      SharedMemory.remove_batch (SharedMemory.KeySet.singleton Memory.SingletonKey.key)
  in
  SharedMemory.add Memory.SingletonKey.key errors


exception ModelVerificationErrors of t list

let verify_models_and_dsl errors verify =
  register errors;
  if not (List.is_empty errors) then
    (* Exit or log errors, depending on whether models need to be verified. *)
    if not verify then begin
      Log.error "Found %d model verification errors!" (List.length errors);
      List.iter errors ~f:(fun error -> Log.error "%s" (display error))
    end
    else
      raise (ModelVerificationErrors errors)


let get () = SharedMemory.get Memory.SingletonKey.key |> Option.value ~default:[]
