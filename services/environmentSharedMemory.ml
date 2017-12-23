(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Expression
open Statement

module SharedMemory = Hack_parallel.Std.SharedMem


(** Keys *)
module AccessKey = struct
  type t = Expression.t Access.t
  let to_string = Format.asprintf "%a" Expression.pp_expression_access_list
  let compare = Access.compare Expression.compare
end

module TypeKey = struct
  type t = Type.t
  let to_string = Format.asprintf "%a" Type.pp
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


(** Values *)
module FunctionKeyValue = struct
  type t = Expression.t Access.t list
  let prefix = Prefix.make ()
  let description = "Function keys"
end

module GlobalKeyValue = struct
  type t = Expression.t Access.t list
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
  type t = Statement.t Class.t
  let prefix = Prefix.make ()
  let description = "Class"
end

module FunctionValue = struct
  type t = (Statement.define Node.t) list
  let prefix = Prefix.make ()
  let description = "Function"
end

module AliasValue = struct
  type t = Type.t
  let prefix = Prefix.make ()
  let description = "Alias"
end

module GlobalValue = struct
  type t = Annotation.t
  let prefix = Prefix.make ()
  let description = "Global"
end

module DependentValue = struct
  type t = string list
  let prefix = Prefix.make ()
  let description = "Dependent"
end

module ClassDefinitionKeyValue = struct
  type t = Type.t list
  let prefix = Prefix.make ()
  let description = "Class definition keys"
end

module OrderTable = struct
  type t = (
    (int * TypeOrder.Target.t list) list
    * (int * TypeOrder.Target.t list) list
    * (Type.t * int) list
    * (int * Type.t) list
  )
  let prefix = Prefix.make ()
  let description = "TypeOrder"
end


(** Shared memory maps *)
module FunctionDefinitions = SharedMemory.WithCache (AccessKey) (FunctionValue)

module ClassDefinitions = SharedMemory.WithCache (TypeKey) (ClassValue)

module ClassDefinitionsKeys = SharedMemory.WithCache (StringKey) (ClassDefinitionKeyValue)

module Aliases = SharedMemory.WithCache (TypeKey) (AliasValue)

module Globals = SharedMemory.WithCache (AccessKey) (GlobalValue)

module Dependents = SharedMemory.WithCache (StringKey) (DependentValue)

module Order = SharedMemory.WithCache (StringKey) (OrderTable)

module FunctionKeys = SharedMemory.WithCache (StringKey) (FunctionKeyValue)

module ClassKeys = SharedMemory.WithCache (StringKey) (ClassKeyValue)

module AliasKeys = SharedMemory.WithCache (StringKey) (AliasKeyValue)

module Protocols = SharedMemory.WithCache (StringKey) (ProtocolValue)

module GlobalKeys = SharedMemory.WithCache (StringKey) (GlobalKeyValue)

module DependentKeys = SharedMemory.WithCache (StringKey) (DependentKeyValue)
