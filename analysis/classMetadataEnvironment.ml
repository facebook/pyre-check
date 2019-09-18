(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

type t = { class_hierarchy_environment: ClassHierarchyEnvironment.ReadOnly.t }

let create class_hierarchy_environment = { class_hierarchy_environment }

type dependency = TypeCheckSource of Reference.t [@@deriving show, compare, sexp]

module DependencyKey = Memory.DependencyKey.Make (struct
  type nonrec t = dependency

  let to_string dependency = sexp_of_dependency dependency |> Sexp.to_string_mach

  let compare = compare_dependency

  type out = dependency

  let from_string string = Sexp.of_string string |> dependency_of_sexp
end)

type class_metadata = {
  successors: Type.Primitive.t list;
  is_test: bool;
  is_final: bool;
  extends_placeholder_stub_class: bool;
}
[@@deriving eq, compare, show]

module ReadOnly = struct
  type t = {
    get_class_metadata: ?dependency:dependency -> Type.Primitive.t -> class_metadata option;
    class_hierarchy_environment: ClassHierarchyEnvironment.ReadOnly.t;
  }

  let class_hierarchy_environment { class_hierarchy_environment; _ } = class_hierarchy_environment

  let get_class_metadata { get_class_metadata; _ } = get_class_metadata
end

module ClassMetadataValue = struct
  type t = class_metadata

  let prefix = Prefix.make ()

  let description = "Class metadata"

  let unmarshall value = Marshal.from_string value 0

  let compare = compare_class_metadata
end

module ClassMetadata =
  Memory.DependencyTrackedTableWithCache (SharedMemoryKeys.StringKey) (DependencyKey)
    (ClassMetadataValue)

module UpdateResult = struct
  type t = {
    triggered_dependencies: DependencyKey.KeySet.t;
    upstream: ClassHierarchyEnvironment.UpdateResult.t;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let upstream { upstream; _ } = upstream
end

let register_class_metadata { class_hierarchy_environment } class_name ~track_dependencies =
  let unannotated_global_environment_dependency =
    Option.some_if
      track_dependencies
      (UnannotatedGlobalEnvironment.RegisterClassMetadata class_name)
  in
  let alias_environment =
    ClassHierarchyEnvironment.ReadOnly.alias_environment class_hierarchy_environment
  in
  let unannotated_global_environment =
    alias_environment |> AliasEnvironment.ReadOnly.unannotated_global_environment
  in
  let add definition =
    let successors annotation =
      let linearization =
        let dependency =
          Option.some_if
            track_dependencies
            (ClassHierarchyEnvironment.RegisterClassMetadata class_name)
        in
        ClassHierarchy.method_resolution_order_linearize
          ~get_successors:
            (ClassHierarchyEnvironment.ReadOnly.get_edges class_hierarchy_environment ?dependency)
          annotation
      in
      match linearization with
      | _ :: successors -> successors
      | [] -> []
    in
    let ast_environment =
      unannotated_global_environment |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment
    in
    let successors = successors class_name in
    let is_final =
      definition |> fun { Node.value = definition; _ } -> Statement.Class.is_final definition
    in
    let in_test =
      let is_unit_test { Node.value = definition; _ } = Statement.Class.is_unit_test definition in
      let successor_classes =
        List.filter_map
          ~f:
            (UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
               ?dependency:unannotated_global_environment_dependency
               unannotated_global_environment)
          successors
      in
      List.exists ~f:is_unit_test successor_classes
    in
    let extends_placeholder_stub_class =
      let alias_environment_dependency =
        Option.some_if track_dependencies (AliasEnvironment.RegisterClassMetadata class_name)
      in
      let ast_environment_dependency =
        Option.some_if track_dependencies (AstEnvironment.RegisterClassMetadata class_name)
      in
      definition
      |> AnnotatedBases.extends_placeholder_stub_class
           ~aliases:
             (AliasEnvironment.ReadOnly.get_alias
                alias_environment
                ?dependency:alias_environment_dependency)
           ~from_empty_stub:
             (AstEnvironment.ReadOnly.from_empty_stub
                ast_environment
                ?dependency:ast_environment_dependency)
    in
    ClassMetadata.add
      class_name
      { is_test = in_test; successors; is_final; extends_placeholder_stub_class }
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
    unannotated_global_environment
    class_name
    ?dependency:unannotated_global_environment_dependency
  >>| add
  |> Option.value ~default:()


let update environment ~scheduler ~configuration upstream_update =
  let update ~names_to_update ~track_dependencies () =
    let register names =
      List.iter names ~f:(register_class_metadata environment ~track_dependencies)
    in
    Scheduler.iter scheduler ~configuration ~f:register ~inputs:(Set.to_list names_to_update)
  in
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let dependencies =
        ClassHierarchyEnvironment.UpdateResult.triggered_dependencies upstream_update
        |> ClassHierarchyEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | ClassHierarchyEnvironment.RegisterClassMetadata name -> Some name
               | _ -> None)
        |> Type.Primitive.Set.of_list
      in
      let dependencies =
        ClassHierarchyEnvironment.UpdateResult.upstream upstream_update
        |> AliasEnvironment.UpdateResult.triggered_dependencies
        |> AliasEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | AliasEnvironment.RegisterClassMetadata name -> Some name
               | _ -> None)
        |> Type.Primitive.Set.of_list
        |> Type.Primitive.Set.union dependencies
      in
      let dependencies =
        ClassHierarchyEnvironment.UpdateResult.upstream upstream_update
        |> AliasEnvironment.UpdateResult.upstream
        |> UnannotatedGlobalEnvironment.UpdateResult.triggered_dependencies
        |> UnannotatedGlobalEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | UnannotatedGlobalEnvironment.RegisterClassMetadata name -> Some name
               | _ -> None)
        |> Type.Primitive.Set.of_list
        |> Type.Primitive.Set.union dependencies
      in
      let dependencies =
        ClassHierarchyEnvironment.UpdateResult.upstream upstream_update
        |> AliasEnvironment.UpdateResult.upstream
        |> UnannotatedGlobalEnvironment.UpdateResult.upstream
        |> AstEnvironment.UpdateResult.triggered_dependencies
        |> AstEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | AstEnvironment.RegisterClassMetadata name -> Some name
               | _ -> None)
        |> Type.Primitive.Set.of_list
        |> Type.Primitive.Set.union dependencies
      in
      let names_to_update =
        ClassHierarchyEnvironment.UpdateResult.upstream upstream_update
        |> AliasEnvironment.UpdateResult.upstream
        |> UnannotatedGlobalEnvironment.UpdateResult.added_classes
        |> Set.union dependencies
      in
      let (), triggered_dependencies =
        let keys = names_to_update |> Set.to_list |> ClassMetadata.KeySet.of_list in
        DependencyKey.Transaction.empty
        |> ClassMetadata.add_to_transaction ~keys
        |> DependencyKey.Transaction.execute
             ~update:(update ~names_to_update ~track_dependencies:true)
      in
      { UpdateResult.triggered_dependencies; upstream = upstream_update }
  | _ ->
      let current_and_previous =
        ClassHierarchyEnvironment.UpdateResult.upstream upstream_update
        |> AliasEnvironment.UpdateResult.upstream
        |> UnannotatedGlobalEnvironment.UpdateResult.current_classes_and_removed_classes
      in
      Set.to_list current_and_previous
      |> ClassMetadata.KeySet.of_list
      |> ClassMetadata.remove_batch;
      update ~names_to_update:current_and_previous () ~track_dependencies:false;

      {
        UpdateResult.triggered_dependencies = DependencyKey.KeySet.empty;
        upstream = upstream_update;
      }


let read_only { class_hierarchy_environment } =
  { ReadOnly.class_hierarchy_environment; get_class_metadata = ClassMetadata.get }
