(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

let attribute_access ~location base method_name =
  {
    Node.location;
    value = Expression.Name (Name.Attribute { base; attribute = method_name; special = true });
  }


let resolve_stringify_call ~resolution expression =
  let string_callee =
    Node.create_with_default_location
      (Expression.Name (Name.Attribute { base = expression; attribute = "__str__"; special = true }))
  in

  try
    match Resolution.resolve_expression_to_type resolution string_callee |> Type.callable_name with
    | Some name ->
        if Reference.equal name (Reference.create "object.__str__") then "__repr__" else "__str__"
    | _ -> "__str__"
  with
  | ClassHierarchy.Untracked _ -> "__str__"


let redirect_special_calls
    ~resolution
    ({ Call.callee = { Node.location; value }; arguments } as call)
  =
  match value, arguments with
  (* str() takes an optional encoding and errors - if these are present, the call shouldn't be
     redirected: https://docs.python.org/3/library/stdtypes.html#str *)
  | Name (Name.Identifier "str"), [{ Call.Argument.value; _ }] ->
      let callee = attribute_access value (resolve_stringify_call ~resolution value) ~location in
      { Call.callee; arguments = [] }
  | Name (Name.Identifier "abs"), [{ Call.Argument.value; _ }] ->
      { Call.callee = attribute_access value "__abs__" ~location; arguments = [] }
  | Name (Name.Identifier "repr"), [{ Call.Argument.value; _ }] ->
      { Call.callee = attribute_access value "__repr__" ~location; arguments = [] }
  | _ -> call
