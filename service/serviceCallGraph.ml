(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression

module SharedMemory = Hack_parallel.Std.SharedMem


module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
end


module AccessValue = struct
  type t = Access.t list
  let prefix = Prefix.make ()
  let description = "Access"
end


module StringKey = struct
  type t = string
  let to_string = ident
  let compare = String.compare
end


module AccessKeyValue = struct
  type t = Access.t list
  let prefix = Prefix.make ()
  let description = "Caller keys"
end

module Overrides = SharedMemory.WithCache (AccessKey) (AccessValue)

module CallerKeys = SharedMemory.WithCache (StringKey) (AccessKeyValue)

module CallEdges = SharedMemory.WithCache (AccessKey) (AccessValue)

let add_callers ~path =
  CallerKeys.add path


let get_callers ~path =
  CallerKeys.get path


let add_call_edges ~caller ~callees =
  CallEdges.add caller callees


let get_call_edges ~caller =
  CallEdges.get caller


let get_overrides =
  Overrides.get
