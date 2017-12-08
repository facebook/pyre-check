(** Copyright 2016-present Facebook. All rights reserved. **)

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
module AccessSetValue = struct
  type t = Expression.t Access.t list
  let prefix = Prefix.make ()
  let description = "Access hashset"
end

module TypeSetValue = struct
  type t = Type.t list
  let prefix = Prefix.make ()
  let description = "Type hashset"
end

module StringSetValue = struct
  type t = string list
  let prefix = Prefix.make ()
  let description = "String hashset"
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

module IndexValue = struct
  type t = int
  let prefix = Prefix.make ()
  let description = "Index"
end

module TypeValue = struct
  type t = Type.t
  let prefix = Prefix.make ()
  let description = "Type"
end

module TypeValues = struct
  type t = Type.t list
  let prefix = Prefix.make ()
  let description = "Types"
end

module TargetsValue = struct
  type t = TypeOrder.Target.t list
  let prefix = Prefix.make ()
  let description = "Targets"
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

module ClassDefinitionsKeys = SharedMemory.WithCache (StringKey) (TypeValues)

module Aliases = SharedMemory.WithCache (TypeKey) (AliasValue)

module Globals = SharedMemory.WithCache (AccessKey) (GlobalValue)

module Dependents = SharedMemory.WithCache (StringKey) (DependentValue)

module Order = SharedMemory.WithCache (StringKey) (OrderTable)

module FunctionKeys = SharedMemory.WithCache (StringKey) (AccessSetValue)

module ClassKeys = SharedMemory.WithCache (StringKey) (TypeSetValue)

module AliasKeys = SharedMemory.WithCache (StringKey) (TypeSetValue)

module Protocols = SharedMemory.WithCache (StringKey) (TypeSetValue)

module GlobalKeys = SharedMemory.WithCache (StringKey) (AccessSetValue)

module DependentKeys = SharedMemory.WithCache (StringKey) (StringSetValue)
