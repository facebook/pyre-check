(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast

module T = struct
  type incompatible_model_error_reason =
    | UnexpectedPositionalOnlyParameter of string
    | UnexpectedPositionalParameter of string
    | UnexpectedNamedParameter of string
    | UnexpectedStarredParameter
    | UnexpectedDoubleStarredParameter
  [@@deriving sexp, compare, eq, show]

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
        reasons: incompatible_model_error_reason list;
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
    | InvalidTaintAnnotation of {
        taint_annotation: Expression.t;
        reason: string;
      }
    | MissingAttribute of {
        class_name: string;
        attribute_name: string;
      }
    | ModelingClassAsDefine of string
    | ModelingModuleAsDefine of string
    | ModelingAttributeAsDefine of string
    | ModelingClassAsAttribute of string
    | ModelingModuleAsAttribute of string
    | ModelingCallableAsAttribute of string
    | NotInEnvironment of string
    | UnexpectedDecorators of {
        name: Reference.t;
        unexpected_decorators: Statement.Decorator.t list;
      }
    | InvalidIdentifier of Expression.t
    | ClassBodyNotEllipsis of string
    | DefineBodyNotEllipsis of string
    (* TODO(T81363867): Remove this variant. *)
    | UnclassifiedError of {
        model_name: string;
        message: string;
      }
  [@@deriving sexp, compare, eq, show]

  type t = {
    kind: kind;
    path: Path.t option;
    location: Location.t;
  }
  [@@deriving sexp, compare, eq, show]
end

include T

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
  | IncompatibleModelError { name; callable_type; reasons } ->
      let reasons =
        List.map reasons ~f:(function
            | UnexpectedPositionalOnlyParameter name ->
                Format.sprintf "unexpected positional only parameter: `%s`" name
            | UnexpectedPositionalParameter name ->
                Format.sprintf "unexpected positional parameter: `%s`" name
            | UnexpectedNamedParameter name ->
                Format.sprintf "unexpected named parameter: `%s`" name
            | UnexpectedStarredParameter -> "unexpected star parameter"
            | UnexpectedDoubleStarredParameter -> "unexpected star star parameter")
      in
      Format.asprintf
        "Model signature parameters for `%s` do not match implementation `%s`. Reason%s: %s."
        name
        (Type.show_for_hover (Type.Callable callable_type))
        (if List.length reasons > 1 then "s" else "")
        (String.concat reasons ~sep:"; ")
  | ImportedFunctionModel { name; actual_name } ->
      Format.asprintf
        "The modelled function `%a` is an imported function, please model `%a` directly."
        Reference.pp
        name
        Reference.pp
        actual_name
  | InvalidModelQueryClauses clause_list ->
      Format.asprintf
        "The model query arguments at `%s` are invalid: expected a find, where and model clause."
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
      let decorators =
        List.map unexpected_decorators ~f:Statement.Decorator.to_expression
        |> List.map ~f:Expression.show
      in
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
  | InvalidIdentifier expression ->
      Format.sprintf
        "Invalid identifier: `%s`. Expected a fully-qualified name."
        (Expression.show expression)
  | ClassBodyNotEllipsis class_name ->
      Format.sprintf "Class model for `%s` must have a body of `...`." class_name
  | DefineBodyNotEllipsis model_name ->
      Format.sprintf "Callable model for `%s` must have a body of `...`." model_name
  | UnclassifiedError { model_name; message } ->
      Format.sprintf "Invalid model for `%s`: %s" model_name message
  | MissingAttribute { class_name; attribute_name } ->
      Format.sprintf "Class `%s` has no attribute `%s`." class_name attribute_name
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
  | NotInEnvironment name -> Format.sprintf "`%s` is not part of the environment!" name


let code { kind; _ } =
  match kind with
  | UnclassifiedError _ -> -1
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


let display { kind = error; path; location } =
  let model_origin =
    match path with
    | None -> ""
    | Some path -> Format.sprintf "%s:%d: " (Path.absolute path) Location.(location.start.line)
  in
  Format.sprintf "%s%s" model_origin (description error)


let to_json ({ kind; path; location } as error) =
  let path =
    match path with
    | None -> `Null
    | Some path -> `String (Path.absolute path)
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

      let unmarshall value = Marshal.from_string value 0
    end)

let register errors =
  let () =
    if SharedMemory.mem Memory.SingletonKey.key then
      SharedMemory.remove_batch (SharedMemory.KeySet.singleton Memory.SingletonKey.key)
  in
  SharedMemory.add Memory.SingletonKey.key errors


let get () = SharedMemory.get Memory.SingletonKey.key |> Option.value ~default:[]
