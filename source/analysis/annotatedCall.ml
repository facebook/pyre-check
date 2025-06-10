(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Ast
open Expression

let attribute_access ~location ~base ~method_name ~origin =
  {
    Node.location;
    value = Expression.Name (Name.Attribute { base; attribute = method_name; origin });
  }


type resolved_stringify =
  | Str
  | Repr

let resolve_stringify_call ~resolution expression =
  let string_callee =
    Node.create_with_default_location
      (Expression.Name
         (Name.Attribute
            {
              base = expression;
              attribute = "__str__";
              origin =
                Some
                  (Origin.create
                     ?base:(Ast.Expression.origin expression)
                     ~location:(Node.location expression)
                     Origin.ResolveStrCall);
            }))
  in

  try
    match Resolution.resolve_expression_to_type resolution string_callee |> Type.callable_name with
    | Some name ->
        if Reference.equal name (Reference.create "object.__str__") then
          Repr
        else
          Str
    | _ -> Str
  with
  | ClassHierarchy.Untracked _ -> Str


let redirect_special_calls
    ~resolution
    ~location:call_location
    ({ Call.callee = { Node.location = callee_location; value }; arguments; origin = call_origin }
    as call)
  =
  match value, arguments with
  (* str() takes an optional encoding and errors - if these are present, the call shouldn't be
     redirected: https://docs.python.org/3/library/stdtypes.html#str *)
  | Name (Name.Identifier "str"), [{ Call.Argument.value; _ }] ->
      let method_name, origin_kind =
        match resolve_stringify_call ~resolution value with
        | Str -> "__str__", Origin.StrCallToDunderStr
        | Repr -> "__repr__", Origin.StrCallToDunderRepr
      in
      let origin = Some (Origin.create ?base:call_origin ~location:call_location origin_kind) in
      let callee = attribute_access ~location:callee_location ~base:value ~method_name ~origin in
      { Call.callee; arguments = []; origin }
  | Name (Name.Identifier "abs"), [{ Call.Argument.value; _ }] ->
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.AbsCall) in
      {
        Call.callee =
          attribute_access ~location:callee_location ~base:value ~method_name:"__abs__" ~origin;
        arguments = [];
        origin;
      }
  | Name (Name.Identifier "repr"), [{ Call.Argument.value; _ }] ->
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.ReprCall) in
      {
        Call.callee =
          attribute_access ~location:callee_location ~base:value ~method_name:"__repr__" ~origin;
        arguments = [];
        origin;
      }
  | _ -> call
