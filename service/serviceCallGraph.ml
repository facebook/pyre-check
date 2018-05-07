(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis
open Ast
open Expression

module SharedMemory = Hack_parallel.Std.SharedMem


let in_process_handler () =
  CallGraph.create ()
  |> CallGraph.handler


module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
end


module AccessValue = struct
  type t = Access.t
  let prefix = Prefix.make ()
  let description = "Access"
end


module Overrides = SharedMemory.WithCache (AccessKey) (AccessValue)


let shared_memory_handler () =
  (* Creates handler for shared memory storage. *)
  (module struct
    let register_overload ~access ~overload =
      Overrides.remove_batch (Overrides.KeySet.singleton access);
      Overrides.add access overload
  end: CallGraph.Handler)
