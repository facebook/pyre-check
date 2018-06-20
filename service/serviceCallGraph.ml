(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Expression
open Pyre

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

let shared_memory_handler () =
  (* Creates handler for shared memory storage. *)
  (module struct
    let register_overload ~access ~overload =
      Overrides.remove_batch (Overrides.KeySet.singleton access);
      Overrides.add access [overload]

    let register_caller ~path ~caller =
      let callers =
        CallerKeys.get path
        >>| (fun callers -> Set.add (Access.Set.of_list callers) caller)
        |> Option.value ~default:(Set.add Access.Set.empty caller)
      in
      Access.Set.to_list callers
      |> CallerKeys.add path

    let register_call_edge ~caller ~callee =
      let callees =
        CallEdges.get caller
        >>| (fun callees -> Set.add (Access.Set.of_list callees) callee)
        |> Option.value ~default:(Set.add Access.Set.empty callee)
      in
      Access.Set.to_list callees
      |> CallEdges.add caller

    let callers ~path =
      CallerKeys.get path

    let callees ~caller =
      CallEdges.get caller

  end: CallGraph.Handler)
