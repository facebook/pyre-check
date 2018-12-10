(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression

module SharedMemory = Memory


module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
end

type annotation_map = {
  precondition: Annotation.t Access.Map.Tree.t;
  postcondition: Annotation.t Access.Map.Tree.t;
}

type annotations =
  annotation_map Int.Map.Tree.t

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
