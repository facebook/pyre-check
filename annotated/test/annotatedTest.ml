(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Statement

open Test


module Assign = Annotated.Assign
module Class = Annotated.Class
module Define = Annotated.Define
module Attribute = Annotated.Attribute
module Method = Annotated.Method
module Access = Annotated.Access


let configuration = Configuration.create ()


let populate source =
  let environment =
    let environment = Environment.Builder.create ~configuration () in
    Environment.populate
      ~configuration
      (Environment.handler ~configuration environment)
      [parse source];
    environment
  in
  Environment.handler ~configuration environment


let resolution environment =
  Environment.resolution environment ()


let value option =
  Option.value_exn option


let variable name =
  Type.Variable { Type.variable = Identifier.create name; constraints = [] }
