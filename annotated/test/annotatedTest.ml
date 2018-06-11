(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Statement

open Test


let configuration =
  Configuration.create ()


let populate_with_sources sources =
  let environment =
    let environment = Environment.Builder.create ~configuration () in
    Environment.populate
      ~configuration
      (Environment.handler ~configuration environment)
      sources;
    environment
  in
  Environment.handler ~configuration environment


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
  Environment.resolution
    environment
    ~define:(Define.create_toplevel ~qualifier:[] ~statements:[])
    ()


let value option =
  Option.value_exn option
