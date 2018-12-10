(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

open Test


let configuration =
  Configuration.Analysis.create ()


let populate_with_sources sources =
  let environment =
    let environment = Environment.Builder.create () in
    Service.Environment.populate
      ~configuration
      (Environment.handler ~configuration environment)
      sources;
    environment
  in
  Environment.handler ~configuration environment


let populate source =
  let environment =
    let environment = Environment.Builder.create () in
    Service.Environment.populate
      ~configuration
      (Environment.handler ~configuration environment)
      (parse source :: typeshed_stubs);
    environment
  in
  Environment.handler ~configuration environment


let value option =
  Option.value_exn option
