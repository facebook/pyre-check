(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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


let additional_checks: (module Signature) list =
  [
    (module AwaitableCheck);
  ]
