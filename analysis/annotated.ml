(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression

module Callable = AnnotatedCallable
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine
module Access = AnnotatedAccess
module Signature = AnnotatedSignature


(* In general, python expressions can be self-referential. This non-recursive resolution only checks
   literals and annotations found in the resolution map, without any resolutions/joins. *)
let rec resolve_literal ~resolution expression =
  match Node.value expression with
  | Access access ->
      begin
        match Expression.Access.name_and_arguments ~call:access with
        | Some { Expression.Access.callee; _ } ->
            let class_name =
              Expression.Access.create callee
              |> Expression.Access.expression
              |> Resolution.parse_annotation resolution
            in
            let is_defined =
              Resolution.class_definition resolution class_name
              |> Option.is_some
            in
            if is_defined then
              class_name
            else
              Type.Top
        | None ->
            Type.Top
      end
  | Await expression ->
      resolve_literal ~resolution expression
      |> Type.awaitable_value

  | Complex _ ->
      Type.complex

  | False ->
      Type.bool

  | Float _ ->
      Type.float

  | Integer _ ->
      Type.integer

  | String { StringLiteral.kind; _ } ->
      begin
        match kind with
        | StringLiteral.Bytes -> Type.bytes
        | _ -> Type.string
      end

  | True ->
      Type.bool

  | Tuple elements ->
      Type.tuple (List.map elements ~f:(resolve_literal ~resolution))

  | Expression.Yield _ ->
      Type.yield Type.Top

  | _ ->
      Type.Top
