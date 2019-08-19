(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
module SharedMemory = Memory

module IgnoreValue = struct
  type t = Ast.Ignore.t list [@@deriving eq]

  let prefix = Prefix.make ()

  let description = "Ignore"

  let unmarshall value = Marshal.from_string value 0
end

module LocationListValue = struct
  type t = Location.t list [@@deriving eq]

  let prefix = Prefix.make ()

  let description = "Location list"

  let unmarshall value = Marshal.from_string value 0
end

module IgnoreLines =
  SharedMemory.NoCache.Make (Analysis.SharedMemoryKeys.LocationKey) (IgnoreValue)
module IgnoreKeys =
  SharedMemory.NoCache.Make (Analysis.SharedMemoryKeys.StringKey) (LocationListValue)
