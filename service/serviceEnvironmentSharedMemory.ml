(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Statement

module SharedMemory = Hack_parallel.Std.SharedMem


(** Keys *)
module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
end

module TypeKey = struct
  type t = Type.t
  let to_string = Type.serialize
  let compare = Type.compare
end

module IntKey = struct
  type t = int
  let to_string = Int.to_string
  let compare = Int.compare
end

module StringKey = struct
  type t = string
  let to_string = ident
  let compare = String.compare
end

module LocationKey = struct
  type t = Location.t
  let to_string = Location.to_string
  let compare = Location.compare
end


(** Values *)
module FunctionKeyValue = struct
  type t = Access.t list
  let prefix = Prefix.make ()
  let description = "Function keys"
end

module GlobalKeyValue = struct
  type t = Access.t list
  let prefix = Prefix.make ()
  let description = "Global keys"
end

module AliasKeyValue = struct
  type t = Type.t list
  let prefix = Prefix.make ()
  let description = "Alias keys"
end

module ClassKeyValue = struct
  type t = Type.t list
  let prefix = Prefix.make ()
  let description = "Class keys"
end

module ProtocolValue = struct
  type t = Type.t list
  let prefix = Prefix.make ()
  let description = "Protocols"
end

module DependentKeyValue = struct
  type t = string list
  let prefix = Prefix.make ()
  let description = "Dependent keys"
end

module ClassValue = struct
  type t = Analysis.Environment.class_representation
  let prefix = Prefix.make ()
  let description = "Class"
end

module FunctionValue = struct
  type t = (Define.t Node.t) list
  let prefix = Prefix.make ()
  let description = "Function"
end

module AliasValue = struct
  type t = Type.t
  let prefix = Prefix.make ()
  let description = "Alias"
end

module GlobalValue = struct
  type t = Resolution.global
  let prefix = Prefix.make ()
  let description = "Global"
end

module DependentValue = struct
  type t = string list
  let prefix = Prefix.make ()
  let description = "Dependent"
end

module OrderIndexValue = struct
  type t = int
  let prefix = Prefix.make ()
  let description = "Order indices"
end

module OrderAnnotationValue = struct
  type t = Type.t
  let prefix = Prefix.make ()
  let description = "Order annotations"
end

module EdgeValue = struct
  type t = TypeOrder.Target.t list
  let prefix = Prefix.make ()
  let description = "Edges"
end

module BackedgeValue = struct
  type t = TypeOrder.Target.t list
  let prefix = Prefix.make ()
  let description = "Backedges"
end

module OrderKeyValue = struct
  type t = int list
  let prefix = Prefix.make ()
  let description = "Order keys"
end

(** Shared memory maps *)
module FunctionDefinitions = SharedMemory.WithCache (AccessKey) (FunctionValue)

module ClassDefinitions = SharedMemory.WithCache (TypeKey) (ClassValue)

module Aliases = SharedMemory.WithCache (TypeKey) (AliasValue)

module Globals = SharedMemory.WithCache (AccessKey) (GlobalValue)

module Dependents = SharedMemory.WithCache (StringKey) (DependentValue)

module Protocols = SharedMemory.WithCache (StringKey) (ProtocolValue)

(** Keys *)
module FunctionKeys = SharedMemory.WithCache (StringKey) (FunctionKeyValue)

module ClassKeys = SharedMemory.WithCache (StringKey) (ClassKeyValue)

module GlobalKeys = SharedMemory.WithCache (StringKey) (GlobalKeyValue)

module AliasKeys = SharedMemory.WithCache (StringKey) (AliasKeyValue)

module DependentKeys = SharedMemory.WithCache (StringKey) (DependentKeyValue)

(** Type order maps *)
module OrderIndices = SharedMemory.WithCache (TypeKey) (OrderIndexValue)

module OrderAnnotations = SharedMemory.WithCache (IntKey) (OrderAnnotationValue)

module OrderEdges = SharedMemory.WithCache (IntKey) (EdgeValue)

module OrderBackedges = SharedMemory.WithCache (IntKey) (BackedgeValue)

module OrderKeys = SharedMemory.WithCache (StringKey) (OrderKeyValue)

let heap_size () =
  SharedMemory.heap_size ()
  |> Float.of_int
  |> (fun size -> size /. 1.0e6)
  |> Int.of_float
