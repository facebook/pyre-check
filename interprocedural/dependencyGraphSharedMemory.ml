(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module SharedMemory = Memory


module Targets = struct
  type t = Callable.t list
  let prefix = Prefix.make ()
  let description = "Targets"
end


module HandleKey = struct
  type t = File.Handle.t
  let to_string = File.Handle.show
  let compare = File.Handle.compare
end


module Overrides = SharedMemory.WithCache (Callable.Key) (Targets)

module CallerKeys = SharedMemory.WithCache (HandleKey) (Targets)

module CallEdges = SharedMemory.WithCache (Callable.Key) (Targets)


let add_callers ~path =
  CallerKeys.add path


let get_callers ~path =
  CallerKeys.get path


let add_call_edges ~caller ~callees =
  CallEdges.add caller callees


let get_call_edges ~caller =
  CallEdges.get caller


let add_overrides ~ancestor ~children =
  Overrides.add ancestor children


let get_overrides ~ancestor =
  Overrides.get ancestor
