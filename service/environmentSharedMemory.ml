(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Statement

module SharedMemory = Memory


(** Keys *)
module AccessKey = struct
  type t = Access.t
  let to_string = Access.show
  let compare = Access.compare
  type out = string
  let from_string x = x
end

module TypeKey = struct
  type t = Type.t
  let to_string = Type.serialize
  let compare = Type.compare
  type out = string
  let from_string x = x
end

module IntKey = struct
  type t = int
  let to_string = Int.to_string
  let compare = Int.compare
  type out = int
  let from_string = Int.of_string
end

module StringKey = struct
  type t = string
  let to_string = ident
  let compare = String.compare
  type out = string
  let from_string x = x
end

module FileHandleKey = struct
  type t = File.Handle.t
  let to_string = File.Handle.show
  let compare = File.Handle.compare
  type out = string
  let from_string x = x
end

module LocationKey = struct
  type t = Location.t
  let to_string = Location.Reference.show
  let compare = Location.Reference.compare
  type out = string
  let from_string x = x
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
  type t = Access.t list
  let prefix = Prefix.make ()
  let description = "Dependent keys"
end

module ClassValue = struct
  type t = Analysis.Resolution.class_representation
  let prefix = Prefix.make ()
  let description = "Class"
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
  type t = File.Handle.Set.Tree.t
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
  type t = Analysis.TypeOrder.Target.t list
  let prefix = Prefix.make ()
  let description = "Edges"
end

module BackedgeValue = struct
  type t = Analysis.TypeOrder.Target.t list
  let prefix = Prefix.make ()
  let description = "Backedges"
end

module OrderKeyValue = struct
  type t = int list
  let prefix = Prefix.make ()
  let description = "Order keys"
end

module ConfigurationValue = struct
  type t = Configuration.Analysis.t
  let prefix = Prefix.make ()
  let description = "Configuration"
end

module ErrorsValue = struct
  type t = (File.Handle.t * (Analysis.Error.t list)) list
  let prefix = Prefix.make ()
  let description = "All errors"
end

(** Shared memory maps *)
module ClassDefinitions = Decodable.WithCache (TypeKey) (ClassValue)

module Aliases = Decodable.NoCache (TypeKey) (AliasValue)

module Globals = Decodable.WithCache (AccessKey) (GlobalValue)

module Dependents = Decodable.WithCache (AccessKey) (DependentValue)

module Protocols = Decodable.WithCache (StringKey) (ProtocolValue)

(** Keys *)
module FunctionKeys = Decodable.WithCache (FileHandleKey) (FunctionKeyValue)

module ClassKeys = Decodable.WithCache (FileHandleKey) (ClassKeyValue)

module GlobalKeys = Decodable.WithCache (FileHandleKey) (GlobalKeyValue)

module AliasKeys = Decodable.WithCache (FileHandleKey) (AliasKeyValue)

module DependentKeys = Decodable.WithCache (FileHandleKey) (DependentKeyValue)

(** Type order maps *)
module OrderIndices = Decodable.WithCache (TypeKey) (OrderIndexValue)

module OrderAnnotations = Decodable.WithCache (IntKey) (OrderAnnotationValue)

module OrderEdges = Decodable.WithCache (IntKey) (EdgeValue)

module OrderBackedges = Decodable.WithCache (IntKey) (BackedgeValue)

module OrderKeys = Decodable.WithCache (StringKey) (OrderKeyValue)

module StoredConfiguration = Decodable.NoCache (StringKey) (ConfigurationValue)

module ServerErrors = Decodable.NoCache (StringKey) (ErrorsValue)

let heap_size () =
  SharedMemory.heap_size ()
  |> Float.of_int
  |> (fun size -> size /. 1.0e6)
  |> Int.of_float
