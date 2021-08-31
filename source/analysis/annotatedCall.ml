(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

let redirect_special_calls
    ~resolution
    ({ Call.callee = { Node.location; value }; arguments } as call)
  =
  let callee base method_name =
    {
      Node.location;
      value = Expression.Name (Name.Attribute { base; attribute = method_name; special = true });
    }
  in
  match value, arguments with
  (* str() takes an optional encoding and errors - if these are present, the call shouldn't be
     redirected: https://docs.python.org/3/library/stdtypes.html#str *)
  | Name (Name.Identifier "str"), [{ Call.Argument.value; _ }] -> (
      let string_callee = callee value "__str__" in
      match
        Resolution.resolve_expression_to_type resolution string_callee |> Type.callable_name
      with
      | Some name ->
          let callee =
            if Reference.equal name (Reference.create "object.__str__") then
              callee value "__repr__"
            else
              string_callee
          in
          { Call.callee; arguments = [] }
      | _ -> { Call.callee = string_callee; arguments = [] })
  | Name (Name.Identifier "abs"), [{ Call.Argument.value; _ }] ->
      { Call.callee = callee value "__abs__"; arguments = [] }
  | Name (Name.Identifier "repr"), [{ Call.Argument.value; _ }] ->
      { Call.callee = callee value "__repr__"; arguments = [] }
  | _ -> call
