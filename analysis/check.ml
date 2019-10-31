(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
module Error = AnalysisError

module type Signature = sig
  val name : string

  val run
    :  configuration:Configuration.Analysis.t ->
    environment:AnnotatedGlobalEnvironment.ReadOnly.t ->
    source:Source.t ->
    Error.t list
end

let checks : (module Signature) String.Map.t =
  let checks : (string * (module Signature)) list =
    [
      "awaitable", (module AwaitableCheck);
      "deobfuscation", (module DeobfuscationCheck);
      "immutable_collection", (module ImmutableCollectionCheck);
      "inference", (module Inference);
      "liveness", (module LivenessCheck);
      "typeCheck", (module TypeCheck);
    ]
  in
  String.Map.of_alist_exn checks


let get_check_to_run ~check_name = Map.find checks check_name

let create_check ~configuration:{ Configuration.Analysis.infer; additional_checks; _ }
    : (module Signature)
  =
  let checks_to_run = if infer then ["inference"] else "typeCheck" :: additional_checks in
  let find name =
    match Map.find checks name with
    | Some check -> Some check
    | None ->
        Log.warning "Could not find check `%s`." name;
        None
  in
  let filtered_checks = List.filter_map checks_to_run ~f:find in
  let module AggregatedCheck : Signature = struct
    let name = String.concat checks_to_run ~sep:", "

    let run ~configuration ~environment ~source =
      let run_one_check (module Check : Signature) =
        Check.run ~configuration ~environment ~source
      in
      List.concat_map filtered_checks ~f:run_one_check
  end
  in
  (module AggregatedCheck)
