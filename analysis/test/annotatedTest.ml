(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Test

let configuration = Configuration.Analysis.create ()

let populate_with_sources sources = Test.environment ~configuration ~sources ()

let populate source =
  Test.environment ~configuration ~sources:(parse source :: typeshed_stubs ()) ()


let value option = Option.value_exn option
