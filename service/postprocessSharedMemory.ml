(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast

module SharedMemory = Hack_parallel.Std.SharedMem


module StringKey = struct
  type t = string
  let to_string = ident
  let compare = String.compare
end


module LocationKey = struct
  type t = Location.t
  let to_string = Location.Reference.to_string
  let compare = Location.Reference.compare
end


module IgnoreValue = struct
  type t = Ast.Ignore.t
  let prefix = Prefix.make ()
  let description = "Ignore"
end


module LocationListValue = struct
  type t = Location.t list
  let prefix = Prefix.make ()
  let description = "Location list"
end


module ModeValue = struct
  type t = Source.mode
  let prefix = Prefix.make ()
  let description = "Mode"
end


module IgnoreLines = SharedMemory.WithCache (LocationKey) (IgnoreValue)
module IgnoreKeys = SharedMemory.WithCache (StringKey) (LocationListValue)
module ErrorModes = SharedMemory.WithCache (StringKey) (ModeValue)
