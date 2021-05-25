(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open TypeInferenceData
open Interprocedural

let get_result callable = Fixpoint.get_result callable |> Result.get_result TypeInferenceResult.kind

let make_global_result ~global_resolution ~callables =
  let add_local_result global_result callable =
    callable
    |> get_result
    >>| GlobalResult.add_local_result ~global_resolution global_result
    |> Option.value ~default:global_result
  in
  callables |> List.fold ~init:GlobalResult.empty ~f:add_local_result
