(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

module SharedMemory = Memory


module OverrideTypes = struct
  type t = Access.t list
  let prefix = Prefix.make ()
  let description = "overriding types"
end


module AccessKey = struct
  type t = Access.t
  let compare = Access.compare
  let to_string = Access.show
end


(* Maps the method access to the next subtypes that override that method *)
module Overrides = SharedMemory.WithCache(AccessKey)(OverrideTypes)


let add_overriding_types ~member ~subtypes =
  Overrides.add member subtypes


let get_overriding_types ~member =
  Overrides.get member


let overrides_exist member =
  Overrides.mem member
