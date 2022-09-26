(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* EmptyStubEnvironment: layer of the environment stack
 * - upstream: UnannotatedGlobalEnvironment
 * - downstream: AliasEnvironment
 * - key: the name of a global, as a Reference.t
 * - value: boolean... is the name part of an empty stub package?
 *
 * Pyre treats empty stub files as indicating that all names from
 * modules in a package should be treated as having `Any` type; this
 * is a shortcut useful for gradually typing large libraries because
 * it lets stub authors fill out the API module-by-module.
 *
 * This environment is responsible for determining when the name of
 * some global is in a package with an empty stub, so that we can
 * treat it "as if" it were explicitly annotated as having Any type.
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
