(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Core
open TaintAccessPath
open TaintDomains


module SharedMemory = Hack_parallel.Std.SharedMem


module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
end


module TaintModelValue = struct
  type t = TaintModel.t
  let prefix = Prefix.make ()
  let description = "TaintModel"
end


module Models = SharedMemory.WithCache (AccessKey) (TaintModelValue)


let add_model ~define model =
  Models.add define model


let get_model ~define =
  Models.get define
