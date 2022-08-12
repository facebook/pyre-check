(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let run_server _configuration_file = failwith "not implemented yet"

let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Start a new Pyre server for code navigation purpose"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
