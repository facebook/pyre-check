(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Mapping from a class name to its class interval set, stored in the ocaml heap. *)
module Heap : sig
  type t = ClassIntervalSet.t ClassHierarchyGraph.ClassNameMap.t

  val from_class_hierarchy : ClassHierarchyGraph.t -> t
end

(** Mapping from a class name to its class interval set, stored in shared memory. *)
module SharedMemory : sig
  val add : class_name:ClassHierarchyGraph.class_name -> interval:ClassIntervalSet.t -> unit

  val get : class_name:ClassHierarchyGraph.class_name -> ClassIntervalSet.t option

  val from_heap : Heap.t -> unit

  val of_type : Type.t option -> ClassIntervalSet.t

  val of_definition : Ast.Statement.Define.t Ast.Node.t -> ClassIntervalSet.t
end
