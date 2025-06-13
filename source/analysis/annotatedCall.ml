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

let resolve_stringify_call ~resolve_expression_to_type expression =
  let string_callee =
    Node.create
      ~location:(Node.location expression)
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
    match resolve_expression_to_type string_callee |> Type.callable_name with
    | Some name ->
        if Reference.equal name (Reference.create "object.__str__") then
          Repr
        else
          Str
    | _ -> Str
  with
  | ClassHierarchy.Untracked _ -> Str


let redirect_special_calls
    ~resolve_expression_to_type
    ~location:call_location
    ({ Call.callee = { Node.location = callee_location; value }; arguments; origin = call_origin }
    as call)
  =
  match value, arguments with
  (* str() takes an optional encoding and errors - if these are present, the call shouldn't be
     redirected: https://docs.python.org/3/library/stdtypes.html#str *)
  | Name (Name.Identifier "str"), [{ Call.Argument.value; _ }] ->
      let method_name =
        match resolve_stringify_call ~resolve_expression_to_type value with
        | Str -> "__str__"
        | Repr -> "__repr__"
      in
      let origin = Some (Origin.create ?base:call_origin ~location:call_location Origin.StrCall) in
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
