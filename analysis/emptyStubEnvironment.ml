(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
module PreviousEnvironment = UnannotatedGlobalEnvironment

module EmptyStubCache = ManagedCache.Make (struct
  module PreviousEnvironment = UnannotatedGlobalEnvironment
  module Key = SharedMemoryKeys.ReferenceKey

  module Value = struct
    type t = bool

    let prefix = Prefix.make ()

    let description = "is from empty stub result"

    let unmarshall value = Marshal.from_string value 0

    let compare = Bool.compare
  end

  module KeySet = Reference.Set
  module HashableKey = Reference

  let produce_value unannotated_global_environment reference ~track_dependencies =
    let dependency = Option.some_if track_dependencies (SharedMemoryKeys.FromEmptyStub reference) in
    let ast_environment =
      UnannotatedGlobalEnvironment.ReadOnly.ast_environment unannotated_global_environment
    in
    let rec is_empty_stub ~lead ~tail =
      match tail with
      | head :: tail -> (
          let lead = lead @ [head] in
          let reference = Reference.create_from_list lead in
          match
            AstEnvironment.ReadOnly.get_module_metadata ast_environment ?dependency reference
          with
          | Some definition when Module.empty_stub definition -> true
          | Some _ -> is_empty_stub ~lead ~tail
          | _ -> false )
      | _ -> false
    in
    is_empty_stub ~lead:[] ~tail:(Reference.as_list reference)


  let filter_upstream_dependency = function
    | SharedMemoryKeys.FromEmptyStub reference -> Some reference
    | _ -> None
end)

include EmptyStubCache

module ReadOnly = struct
  include ReadOnly

  let from_empty_stub = get

  let unannotated_global_environment = upstream_environment
end

module EmptyStubReadOnly = ReadOnly
