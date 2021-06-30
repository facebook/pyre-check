(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let run_infer _configuration_file = Log.warning "Comming soon..."

let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs type inference"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_infer filename))
