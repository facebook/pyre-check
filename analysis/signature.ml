(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast


type t = {
  constraints: Type.t Type.Map.t;
  instantiated: Statement.define;
  location: Location.t;
}


type normal = {
  annotation: Type.t;
  value: Expression.t;
}


type kind =
  | Normal of normal
  | Starred of Type.t


type argument = kind Node.t
