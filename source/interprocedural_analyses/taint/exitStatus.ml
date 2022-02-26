(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | TaintConfigurationError
  | ModelVerificationError

let exit_code = function
  (* 10-19 are reserved for pysa, see AnalyzeCommand.ExitStatus *)
  | TaintConfigurationError -> 10
  | ModelVerificationError -> 11
