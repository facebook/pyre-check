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
    global_resolution:GlobalResolution.t ->
    source:Source.t ->
    Error.t list
end

let checks : (module Signature) String.Map.t =
  let checks : (string * (module Signature)) list =
    [ "awaitable", (module AwaitableCheck);
      "deobfuscation", (module DeobfuscationCheck);
      "immutable_collection", (module ImmutableCollectionCheck);
      "inference", (module Inference);
      "typeCheck", (module TypeCheck) ]
  in
  String.Map.of_alist_exn checks


let checks ~configuration:{ Configuration.Analysis.infer; additional_checks; _ }
    : (module Signature) list
  =
  let checks_to_run = if infer then ["inference"] else "typeCheck" :: additional_checks in
  let find name =
    match Map.find checks name with
    | Some check -> Some check
    | None ->
        Log.warning "Could not find check `%s`." name;
        None
  in
  List.filter_map checks_to_run ~f:find
