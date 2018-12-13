(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module SharedMemory = Memory


module Targets = struct
  type t = Callable.t list
  let prefix = Prefix.make ()
  let description = "Targets"
end


module Overrides = SharedMemory.WithCache (Callable.Key) (Targets)


let add_overrides ~ancestor ~children =
  Overrides.add ancestor children


let get_overrides ~ancestor =
  Overrides.get ancestor
