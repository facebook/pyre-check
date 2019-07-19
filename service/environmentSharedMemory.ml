(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
module SharedMemory = Memory

(** Keys *)
module StringKey = struct
  type t = string

  let to_string = ident

  let compare = String.compare

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
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Function keys"
end

module GlobalKeyValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Global keys"
end

module AliasKeyValue = struct
  type t = Identifier.t list

  let prefix = Prefix.make ()

  let description = "Alias keys"
end

module ClassKeyValue = struct
  type t = Identifier.t list

  let prefix = Prefix.make ()

  let description = "Class keys"
end

module DependentKeyValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Dependent keys"
end

module ClassValue = struct
  type t = Statement.Class.t Node.t

  let prefix = Prefix.make ()

  let description = "Class"
end

module ClassMetadataValue = struct
  type t = Analysis.GlobalResolution.class_metadata

  let prefix = Prefix.make ()

  let description = "Class metadata"
end

module AliasValue = struct
  type t = Type.alias

  let prefix = Prefix.make ()

  let description = "Alias"
end

module GlobalValue = struct
  type t = GlobalResolution.global

  let prefix = Prefix.make ()

  let description = "Global"
end

module DependentValue = struct
  type t = Reference.Set.Tree.t

  let prefix = Prefix.make ()

  let description = "Dependent"
end

module OrderIndexValue = struct
  type t = int

  let prefix = Prefix.make ()

  let description = "Order indices"
end

module OrderAnnotationValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Order annotations"
end

module EdgeValue = struct
  type t = Analysis.ClassHierarchy.Target.t list

  let prefix = Prefix.make ()

  let description = "Edges"
end

module BackedgeValue = struct
  type t = Analysis.ClassHierarchy.Target.Set.Tree.t

  let prefix = Prefix.make ()

  let description = "Backedges"
end

module OrderKeyValue = struct
  type t = int list

  let prefix = Prefix.make ()

  let description = "Order keys"
end

module IntValue = struct
  type t = int

  let prefix = Prefix.make ()

  let description = "Refcount of implicit submodules"
end

module ModuleValue = struct
  type t = Module.t

  let prefix = Prefix.make ()

  let description = "Module"
end

module UndecoratedFunctionValue = struct
  type t = Type.t Type.Callable.overload

  let prefix = Prefix.make ()

  let description = "Undecorated functions"
end

module ClassDefinitions = Memory.WithCache (StringKey) (ClassValue)
(** Shared memory maps *)

module Modules = Memory.WithCache (Reference.Key) (ModuleValue)
module ClassMetadata = Memory.WithCache (StringKey) (ClassMetadataValue)
module ImplicitSubmodules = Memory.NoCache (Reference.Key) (IntValue)
module Aliases = Memory.NoCache (StringKey) (AliasValue)
module Globals = Memory.WithCache (Reference.Key) (GlobalValue)
module Dependents = Memory.WithCache (Reference.Key) (DependentValue)
module UndecoratedFunctions = Memory.WithCache (Reference.Key) (UndecoratedFunctionValue)

module FunctionKeys = Memory.WithCache (Reference.Key) (FunctionKeyValue)
(** Keys *)

module ClassKeys = Memory.WithCache (Reference.Key) (ClassKeyValue)
module GlobalKeys = Memory.WithCache (Reference.Key) (GlobalKeyValue)
module AliasKeys = Memory.WithCache (Reference.Key) (AliasKeyValue)
module DependentKeys = Memory.WithCache (Reference.Key) (DependentKeyValue)

module OrderIndices = Memory.WithCache (StringKey) (OrderIndexValue)
(** Type order maps *)

module OrderAnnotations = Memory.WithCache (Ast.SharedMemory.IntKey) (OrderAnnotationValue)
module OrderEdges = Memory.WithCache (Ast.SharedMemory.IntKey) (EdgeValue)
module OrderBackedges = Memory.WithCache (Ast.SharedMemory.IntKey) (BackedgeValue)
module OrderKeys = Memory.WithCache (Memory.SingletonKey) (OrderKeyValue)
