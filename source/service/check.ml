(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis

let check ~scheduler ~configuration ~populate_call_graph =
  let environment =
    EnvironmentControls.create ~populate_call_graph configuration
    |> ErrorsEnvironment.create_for_production
  in
  let () = ErrorsEnvironment.check_and_preprocess environment ~scheduler in
  environment
