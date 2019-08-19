(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  type out = int

  let from_string = Core.Int.of_string
end

module StringKey = struct
  type t = string

  let to_string = Fn.id

  let compare = String.compare

  type out = string

  let from_string x = x
end

module ReferenceKey = struct
  type nonrec t = Reference.t

  let to_string = Reference.show

  let compare = Reference.compare

  type out = Reference.t

  let from_string name = Reference.create name
end

module ReferenceDependencyKey = Memory.DependencyKey.Make (ReferenceKey)

module LocationKey = struct
  type t = Location.t

  let to_string = Location.Reference.show

  let compare = Location.Reference.compare

  type out = string

  let from_string = Fn.id
end
