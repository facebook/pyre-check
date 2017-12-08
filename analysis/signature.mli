(** Copyright 2016-present Facebook. All rights reserved. **)

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
