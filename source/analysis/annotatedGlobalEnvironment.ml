(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
module PreviousEnvironment = AttributeResolution

module GlobalLocationValue = struct
  type t = Location.WithModule.t option

  let prefix = Prefix.make ()

  let description = "Global Locations"

  let equal = Memory.equal_from_compare (Option.compare Location.WithModule.compare)
end

module Common = struct
  let show_key = Reference.show
end

let produce_global_location attribute_resolution name ~dependency =
  let unannotated_global_environment =
    AttributeResolution.ReadOnly.unannotated_global_environment attribute_resolution
  in
  let class_location =
    Reference.show name
    |> UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
         unannotated_global_environment
         ?dependency
    >>| fun { Node.location; value = { ClassSummary.qualifier; _ } } ->
    Location.with_module ~module_reference:qualifier location
  in
  match class_location with
  | Some location -> Some location
  | None ->
      let extract_location = function
        | UnannotatedGlobal.Define ({ UnannotatedGlobal.UnannotatedDefine.location; _ } :: _) ->
            Some location
        | SimpleAssign { target_location; _ } -> Some target_location
        | TupleAssign { target_location; _ } -> Some target_location
        | _ -> None
      in
      UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
        unannotated_global_environment
        ?dependency
        name
      >>= extract_location


module GlobalLocationTable = Environment.EnvironmentTable.WithCache (struct
  include Common
  module PreviousEnvironment = AttributeResolution
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = GlobalLocationValue

  type trigger = Reference.t [@@deriving sexp, compare]

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let produce_value = produce_global_location

  let filter_upstream_dependency = function
    | SharedMemoryKeys.AnnotateGlobalLocation name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.AnnotateGlobalLocation name

  let equal_value = Option.equal [%compare.equal: Location.WithModule.t]

  let overlay_owns_key module_tracker_overlay =
    ModuleTracker.Overlay.owns_reference module_tracker_overlay
end)

include GlobalLocationTable

module ReadOnly = struct
  include GlobalLocationTable.ReadOnly

  let get_global_location = get

  let attribute_resolution read_only = upstream_environment read_only

  let class_metadata_environment read_only =
    attribute_resolution read_only |> AttributeResolution.ReadOnly.class_metadata_environment


  let ast_environment environment =
    class_metadata_environment environment
    |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
    |> ClassHierarchyEnvironment.ReadOnly.alias_environment
    |> AliasEnvironment.ReadOnly.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment


  let project_qualifiers environment =
    ast_environment environment |> AstEnvironment.ReadOnly.project_qualifiers
end

module UpdateResult = GlobalLocationTable.UpdateResult
module AnnotatedReadOnly = ReadOnly
