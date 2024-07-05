(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AnnotatedGlobalEnvironment: layer of the environment stack
 * - upstream: AttributeResolution
 * - downstream: TypeEnvironment
 *   - also used by GlobalResolution, which is not an environment
 *     layer per se but is an interface in between type checking and
 *     all lower layers.
 * - key: name of a global, as a Reference.t
 * - value: Location.WithModule.t option
 *
 * This layer of the environment handles location information for globals,
 * which is needed in type checking. The lower-level layers all ignore location
 * in their invalidation logic, which is why we need this layer (so that we
 * know to re-compute errors that include locations after line number changes).
 *
 * Note: The name is not related to location information - that's because
 * it is named mostly for how it is used: it's the last environment
 * layer *prior* to type checking, that contains information we can
 * compute with just simple processing of sources rather than expensive
 * constraint solving.
 *)

open Core
open Ast
open Pyre
module PreviousEnvironment = FunctionDefinitionEnvironment

module GlobalLocationValue = struct
  type t = Location.WithModule.t option

  let prefix = Hack_parallel.Std.Prefix.make ()

  let description = "Global Locations"

  let equal = Memory.equal_from_compare (Option.compare Location.WithModule.compare)
end

module IncomingDataComputation = struct
  module Queries = struct
    type t = {
      get_class_summary: Identifier.t -> ClassSummary.t Node.t option;
      get_unannotated_global: Reference.t -> Module.UnannotatedGlobal.t option;
    }
  end

  let produce_location_of_global Queries.{ get_class_summary; get_unannotated_global } name =
    let class_location =
      Reference.show name
      |> get_class_summary
      >>| fun { Node.location; value = { ClassSummary.qualifier; _ } } ->
      Location.with_module ~module_reference:qualifier location
    in
    match class_location with
    | Some location -> Some location
    | None ->
        let open Module.UnannotatedGlobal in
        let extract_location = function
          | Define ({ location; _ } :: _) -> Some location
          | SimpleAssign { target_location; _ } -> Some target_location
          | TupleAssign { target_location; _ } -> Some target_location
          | _ -> None
        in
        get_unannotated_global name >>= extract_location
end

module GlobalLocationTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = GlobalLocationValue

  let show_key = Reference.show

  type trigger = Reference.t [@@deriving sexp, compare]

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let produce_value function_definition_environment key ~dependency =
    let unannotated_global_environment =
      function_definition_environment
      |> FunctionDefinitionEnvironment.ReadOnly.attribute_resolution
      |> AttributeResolution.ReadOnly.class_metadata_environment
      |> ClassSuccessorMetadataEnvironment.ReadOnly.unannotated_global_environment
    in
    let queries =
      IncomingDataComputation.Queries.
        {
          get_class_summary =
            UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
              ?dependency
              unannotated_global_environment;
          get_unannotated_global =
            UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
              ?dependency
              unannotated_global_environment;
        }
    in
    IncomingDataComputation.produce_location_of_global queries key


  let filter_upstream_dependency = function
    | SharedMemoryKeys.AnnotateGlobalLocation name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.AnnotateGlobalLocation name

  let equal_value = Option.equal [%compare.equal: Location.WithModule.t]

  let overlay_owns_key source_code_api =
    SourceCodeIncrementalApi.Overlay.owns_reference source_code_api
end)

include GlobalLocationTable

module ReadOnly = struct
  include GlobalLocationTable.ReadOnly

  let location_of_global = get

  let function_definition_environment read_only = upstream_environment read_only

  let attribute_resolution read_only =
    function_definition_environment read_only
    |> FunctionDefinitionEnvironment.ReadOnly.attribute_resolution


  let class_metadata_environment read_only =
    attribute_resolution read_only |> AttributeResolution.ReadOnly.class_metadata_environment


  let unannotated_global_environment read_only =
    class_metadata_environment read_only
    |> ClassSuccessorMetadataEnvironment.ReadOnly.unannotated_global_environment


  let get_tracked_source_code_api environment =
    unannotated_global_environment environment
    |> UnannotatedGlobalEnvironment.ReadOnly.get_tracked_source_code_api


  let get_untracked_source_code_api environment =
    unannotated_global_environment environment
    |> UnannotatedGlobalEnvironment.ReadOnly.get_untracked_source_code_api
end

module UpdateResult = GlobalLocationTable.UpdateResult
module AnnotatedReadOnly = ReadOnly
