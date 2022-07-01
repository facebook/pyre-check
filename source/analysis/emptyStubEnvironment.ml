(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

    let equal = Bool.equal
  end

  module KeySet = Reference.Set
  module HashableKey = Reference

  let lazy_incremental = false

  let produce_value unannotated_global_environment reference ~dependency =
    let rec is_empty_stub ~lead ~tail =
      match tail with
      | head :: tail -> (
          let lead = lead @ [head] in
          let reference = Reference.create_from_list lead in
          match
            UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata
              unannotated_global_environment
              ?dependency
              reference
          with
          | Some definition when Module.empty_stub definition -> true
          | Some _ -> is_empty_stub ~lead ~tail
          | _ -> false)
      | _ -> false
    in
    is_empty_stub ~lead:[] ~tail:(Reference.as_list reference)


  let filter_upstream_dependency = function
    | SharedMemoryKeys.FromEmptyStub reference -> Some reference
    | _ -> None


  let trigger_to_dependency reference = SharedMemoryKeys.FromEmptyStub reference

  let overlay_owns_key module_tracker_overlay key =
    ModuleTracker.Overlay.owns_reference module_tracker_overlay key
end)

include EmptyStubCache

module ReadOnly = struct
  include ReadOnly

  let from_empty_stub = get

  let unannotated_global_environment = upstream_environment
end

module EmptyStubReadOnly = ReadOnly
