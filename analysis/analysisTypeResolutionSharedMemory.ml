(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Ast
open Statement

module SharedMemory = Hack_parallel.Std.SharedMem

module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
end

(** Maps a key, unique to each statement for a function CFG, to type
    annotations.  They key is computed from a tuple CFG node ID and and statement
    index (see Fixpoint.forward) *)
module TypeAnnotationsValue = struct
  type annotations =
    {
      key: int;
      annotations: (Access.t * AnalysisResolution.Annotation.t) list;
    }
  type t = annotations list
  let prefix = Prefix.make ()
  let description = "Node type resolution"
end

(** A map of function definitions (indexed by Access.t key) to
    to annotations for each statement *)
include SharedMemory.WithCache (AccessKey) (TypeAnnotationsValue)
