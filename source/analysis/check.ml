(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
module Error = AnalysisError

module type Signature = sig
  val name : string

  val run
    :  configuration:Configuration.Analysis.t ->
    environment:TypeEnvironment.ReadOnly.t ->
    source:Source.t ->
    Error.t list
end

let checks : (module Signature) String.Map.t =
  let checks : (string * (module Signature)) list =
    [
      "awaitable", (module AwaitableCheck);
      "immutable_collection", (module ImmutableCollectionCheck);
      "liveness", (module LivenessCheck);
      "uninitialized_local", (module UninitializedLocalCheck);
    ]
  in
  String.Map.of_alist_exn checks


let get_check_to_run ~check_name = Map.find checks check_name
