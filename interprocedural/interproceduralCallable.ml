(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Analysis
open Pyre

module SharedMemory = Hack_parallel.Std.SharedMem


type real_target = [ `RealTarget of Access.t ]
[@@deriving show, compare]


type override_target = [ `OverrideTarget of Access.t ]
[@@deriving show, compare]


type t = [ real_target | override_target ]
[@@deriving show, compare]


type target_with_stored_result = real_target


let make_real access = `RealTarget access
let make_override access = `OverrideTarget access


let get_real_access = function
  | `RealTarget access -> access


let get_override_access = function
  | `OverrideTarget access -> access


module Key = struct
  type nonrec t = t
  let to_string = show
  let compare = compare
end


module RealKey = struct
  type t = real_target
  let to_string = show_real_target
  let compare = compare_real_target
end


module FileOfDefinition = SharedMemory.WithCache (RealKey)
    (struct
      type t = File.Handle.t
      let prefix = Prefix.make ()
      let description = "File of definition"
    end)


module Set = Caml.Set.Make(Key)


let add_definition callable handle =
  FileOfDefinition.add callable handle


let get_definition (`RealTarget access as callable) =
  let open Statement in
  FileOfDefinition.get callable
  >>= AstSharedMemory.get_source
  >>| Preprocessing.defines
  >>= List.find ~f:(fun { Node.value = { Define.name; _ } ; _ } -> name = access)
