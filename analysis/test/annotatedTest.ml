(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Test

let configuration = Configuration.Analysis.create ()

let populate_with_sources sources =
  let environment = Environment.in_process_handler () in
  Test.populate ~configuration environment sources;
  environment


let populate source =
  let environment = Environment.in_process_handler () in
  Test.populate ~configuration environment (parse source :: typeshed_stubs ());
  environment


let value option = Option.value_exn option
