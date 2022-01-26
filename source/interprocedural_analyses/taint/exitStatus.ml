(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Ok
  | TaintConfigurationError

let exit_code = function
  | Ok -> 0
  (* 1-10 are reserved for pyre check, see CheckCommand.ExitStatus *)
  | TaintConfigurationError -> 10
