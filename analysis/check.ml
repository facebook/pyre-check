(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast

module Error = AnalysisError


module type Signature = sig
  val name: string
  val run
    :  configuration: Configuration.Analysis.t
    -> environment: (module Environment.Handler)
    -> source: Source.t
    -> Error.t list
end


let checks: (module Signature) String.Map.t =
  let checks: (string * (module Signature)) list =
    [
      "awaitable", (module AwaitableCheck);
      "constantPropagation", (module ConstantPropagationCheck);
    ]
  in
  String.Map.of_alist_exn checks


let additional_checks
    ~configuration:{ Configuration.Analysis.additional_checks; _ }: (module Signature) list =
  List.filter_map additional_checks ~f:(Map.find checks)
