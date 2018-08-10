(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement

module SharedMemory = Hack_parallel.Std.SharedMem

module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
end

type annotations =
  (AnalysisResolution.Annotation.t Access.Map.Tree.t) Int.Map.Tree.t

module TypeAnnotationsValue = struct
  type t = annotations
  let prefix = Prefix.make ()
  let description = "Node type resolution"
end

(** A map of function definitions (indexed by Access.t key) to
    to annotations for each statement *)
include SharedMemory.WithCache (AccessKey) (TypeAnnotationsValue)


let remove accesses =
  accesses
  |> List.filter ~f:mem
  |> Fn.compose remove_batch KeySet.of_list
