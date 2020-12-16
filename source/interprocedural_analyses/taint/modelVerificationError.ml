(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast

type incompatible_model_error_reason =
  | UnexpectedPositionalOnlyParameter of string
  | UnexpectedNamedParameter of string
  | UnexpectedStarredParameter
  | UnexpectedDoubleStarredParameter

type kind =
  | GlobalVerificationError of {
      name: string;
      message: string;
    }
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
  (* TODO(T81363867): Remove this variant. *)
  | UnclassifiedError of string

type t = {
  kind: kind;
  path: Path.t option;
  location: Location.t;
}

let display { kind = error; path; location } =
  let model_origin =
    match path with
    | None -> ""
    | Some path ->
        Format.sprintf " defined in `%s:%d`" (Path.absolute path) Location.(location.start.line)
  in
  match error with
  | GlobalVerificationError { name; message } ->
      Format.asprintf "Invalid model for `%s`%s: %s" name model_origin message
  | InvalidDefaultValue { callable_name; name; expression } ->
      Format.sprintf
        "Invalid model for `%s`%s: Default values of parameters must be `...`. Did you mean to \
         write `%s: %s`?"
        callable_name
        model_origin
        name
        (Expression.show expression)
  | IncompatibleModelError { name; callable_type; reasons } ->
      let reasons =
        List.map reasons ~f:(function
            | UnexpectedPositionalOnlyParameter name ->
                Format.sprintf "unexpected positional only parameter: `%s`" name
            | UnexpectedNamedParameter name ->
                Format.sprintf "unexpected named parameter: `%s`" name
            | UnexpectedStarredParameter -> "unexpected star parameter"
            | UnexpectedDoubleStarredParameter -> "unexpected star star parameter")
      in
      Format.asprintf
        "Invalid model for `%s`%s: Model signature parameters do not match implementation `%s`. \
         Reason%s: %s."
        name
        model_origin
        (Type.show_for_hover (Type.Callable callable_type))
        (if List.length reasons > 1 then "s" else "")
        (String.concat reasons ~sep:"; ")
  | ImportedFunctionModel { name; actual_name } ->
      Format.asprintf
        "Invalid model for `%a`%s: The modelled function is an imported function `%a`, please \
         model it directly."
        Reference.pp
        name
        model_origin
        Reference.pp
        actual_name
  | UnclassifiedError error -> error
