(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Statement

type t = { class_metadata_environment: ClassMetadataEnvironment.ReadOnly.t }

let create class_metadata_environment = { class_metadata_environment }

let class_metadata_environment { class_metadata_environment } = class_metadata_environment

let class_hierarchy_environment environment =
  class_metadata_environment environment
  |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment


let alias_environment environment =
  class_hierarchy_environment environment |> ClassHierarchyEnvironment.ReadOnly.alias_environment


let unannotated_global_environment environment =
  alias_environment environment |> AliasEnvironment.ReadOnly.unannotated_global_environment


module ReadOnly = struct
  type t = {
    get_global:
      ?dependency:SharedMemoryKeys.dependency -> Reference.t -> GlobalResolution.global option;
    class_metadata_environment: ClassMetadataEnvironment.ReadOnly.t;
    hash_to_key_map: unit -> string String.Map.t;
    serialize_decoded: Memory.decodable -> (string * string * string option) option;
    decoded_equal: Memory.decodable -> Memory.decodable -> bool option;
  }

  let class_metadata_environment { class_metadata_environment; _ } = class_metadata_environment

  let get_global { get_global; _ } = get_global

  let hash_to_key_map { hash_to_key_map; _ } = hash_to_key_map ()

  let serialize_decoded { serialize_decoded; _ } = serialize_decoded

  let decoded_equal { decoded_equal; _ } = decoded_equal

  let resolution_implementation ?dependency environment =
    let class_metadata_environment = class_metadata_environment environment in
    GlobalResolution.create
      ?dependency
      ~class_metadata_environment
      ~global:(get_global environment ?dependency)
      (module AnnotatedClass)


  let resolution = resolution_implementation ?dependency:None

  let dependency_tracked_resolution environment ~dependency =
    resolution_implementation ~dependency environment


  let ast_environment environment =
    class_metadata_environment environment
    |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
    |> ClassHierarchyEnvironment.ReadOnly.alias_environment
    |> AliasEnvironment.ReadOnly.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment
end

module GlobalValue = struct
  type t = GlobalResolution.global

  let prefix = Prefix.make ()

  let description = "Global"

  let unmarshall value = Marshal.from_string value 0

  let compare = GlobalResolution.compare_global
end

module Globals =
  Memory.DependencyTrackedTableWithCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.DependencyKey)
    (GlobalValue)

module UpdateResult = struct
  type t = {
    triggered_dependencies: SharedMemoryKeys.DependencyKey.KeySet.t;
    upstream: ClassMetadataEnvironment.UpdateResult.t;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let upstream { upstream; _ } = upstream
end

let annotate_global environment name ~track_dependencies =
  let dependency = Option.some_if track_dependencies (SharedMemoryKeys.AnnotateGlobal name) in
  let resolution =
    GlobalResolution.create
      ?dependency
      ~class_metadata_environment:(class_metadata_environment environment)
      ~global:(fun _ -> None)
      (module Annotated.Class)
  in
  let register_class_meta_annotation { Node.location; _ } =
    let primitive = Type.Primitive (Reference.show name) in
    Annotation.create_immutable ~global:true (Type.meta primitive)
    |> Node.create ~location
    |> Globals.add name
  in
  let register_unannotated_global global =
    let register_assignment_global ~name ~target_location ~is_explicit annotation =
      let original =
        if is_explicit then
          None
        else if
          (* Treat literal globals as having been explicitly annotated. *)
          Type.is_partially_typed annotation
        then
          Some Type.Top
        else
          None
      in
      Annotation.create_immutable ~global:true ~original annotation
      |> Node.create ~location:target_location
      |> Globals.add name
    in
    match global with
    | UnannotatedGlobalEnvironment.Define (head :: _ as defines) ->
        let create_overload
            { Node.location; Node.value = { Define.signature = { name; parent; _ }; _ } as define }
          =
          let parent =
            if Define.is_class_method define then
              parent >>| Reference.show >>| (fun name -> Type.Primitive name) >>| Type.meta
            else
              None
          in
          Node.create ~location define
          |> ResolvedCallable.apply_decorators ~resolution
          |> (fun overload -> [Define.is_overloaded_method define, overload])
          |> ResolvedCallable.create_callable ~resolution ~parent ~name:(Reference.show name)
        in
        List.map defines ~f:create_overload
        |> Type.Callable.from_overloads
        >>| (fun callable -> Type.Callable callable)
        >>| Annotation.create_immutable ~global:true
        >>| Node.create ~location:(Node.location head)
        >>| Globals.add name
        |> Option.value ~default:()
    | SimpleAssign { explicit_annotation; value; target_location } ->
        let explicit_annotation =
          explicit_annotation
          >>| Expression.delocalize
          >>| Type.create
                ~aliases:(AliasEnvironment.ReadOnly.get_alias (alias_environment environment))
          >>= fun annotation -> Option.some_if (not (Type.is_type_alias annotation)) annotation
        in
        let annotation =
          match explicit_annotation with
          | Some explicit -> explicit
          | None -> GlobalResolution.resolve_literal resolution value
        in
        register_assignment_global
          ~name
          ~target_location
          ~is_explicit:(Option.is_some explicit_annotation)
          annotation
    | TupleAssign { value; target_location; index; total_length } ->
        let extracted =
          match GlobalResolution.resolve_literal resolution value with
          | Type.Tuple (Type.Bounded (Concrete parameters))
            when List.length parameters = total_length ->
              List.nth parameters index
              (* This should always be Some, but I don't think its worth being fragile here *)
              |> Option.value ~default:Type.Top
          | Type.Tuple (Type.Unbounded parameter) -> parameter
          | _ -> Type.Top
        in
        register_assignment_global ~name ~target_location ~is_explicit:false extracted
    | _ -> ()
  in
  let class_lookup =
    Reference.show name
    |> UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
         (unannotated_global_environment environment)
         ?dependency
  in
  match class_lookup with
  | Some retrieved_class -> register_class_meta_annotation retrieved_class
  | None ->
      UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
        (unannotated_global_environment environment)
        ?dependency
        name
      >>| register_unannotated_global
      |> Option.value ~default:()


let update environment ~scheduler ~configuration upstream_result =
  let update ~names_to_update ~track_dependencies () =
    let annotate = List.iter ~f:(annotate_global environment ~track_dependencies) in
    Scheduler.iter scheduler ~configuration ~f:annotate ~inputs:(Set.to_list names_to_update)
  in
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let dependencies =
        ClassMetadataEnvironment.UpdateResult.triggered_dependencies upstream_result
        |> SharedMemoryKeys.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | SharedMemoryKeys.AnnotateGlobal name -> Some name
               | _ -> None)
        |> Reference.Set.of_list
      in
      let dependencies =
        ClassMetadataEnvironment.UpdateResult.upstream upstream_result
        |> ClassHierarchyEnvironment.UpdateResult.triggered_dependencies
        |> SharedMemoryKeys.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | SharedMemoryKeys.AnnotateGlobal name -> Some name
               | _ -> None)
        |> Reference.Set.of_list
        |> Set.union dependencies
      in
      let dependencies =
        ClassMetadataEnvironment.UpdateResult.upstream upstream_result
        |> ClassHierarchyEnvironment.UpdateResult.upstream
        |> AliasEnvironment.UpdateResult.triggered_dependencies
        |> SharedMemoryKeys.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | SharedMemoryKeys.AnnotateGlobal name -> Some name
               | _ -> None)
        |> Reference.Set.of_list
        |> Set.union dependencies
      in
      let dependencies =
        ClassMetadataEnvironment.UpdateResult.upstream upstream_result
        |> ClassHierarchyEnvironment.UpdateResult.upstream
        |> AliasEnvironment.UpdateResult.upstream
        |> UnannotatedGlobalEnvironment.UpdateResult.triggered_dependencies
        |> SharedMemoryKeys.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | SharedMemoryKeys.AnnotateGlobal name -> Some name
               | _ -> None)
        |> Reference.Set.of_list
        |> Set.union dependencies
      in
      let dependencies =
        ClassMetadataEnvironment.UpdateResult.upstream upstream_result
        |> ClassHierarchyEnvironment.UpdateResult.upstream
        |> AliasEnvironment.UpdateResult.upstream
        |> UnannotatedGlobalEnvironment.UpdateResult.upstream
        |> AstEnvironment.UpdateResult.triggered_dependencies
        |> SharedMemoryKeys.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | SharedMemoryKeys.AnnotateGlobal name -> Some name
               | _ -> None)
        |> Reference.Set.of_list
        |> Set.union dependencies
      in
      let names_to_update =
        let upstream =
          ClassMetadataEnvironment.UpdateResult.upstream upstream_result
          |> ClassHierarchyEnvironment.UpdateResult.upstream
          |> AliasEnvironment.UpdateResult.upstream
        in
        let new_classes =
          UnannotatedGlobalEnvironment.UpdateResult.current_classes_and_removed_classes upstream
          |> Set.to_list
          |> List.map ~f:Reference.create
        in
        let new_ugs =
          UnannotatedGlobalEnvironment.UpdateResult.current_and_previous_unannotated_globals
            upstream
        in
        List.fold ~init:(Set.union new_ugs dependencies) ~f:Set.add new_classes
      in
      let globals = Globals.KeySet.of_list (Set.to_list dependencies) in
      let (), triggered_dependencies =
        let update = update ~names_to_update ~track_dependencies:true in
        SharedMemoryKeys.DependencyKey.Transaction.empty
        |> Globals.add_to_transaction ~keys:globals
        |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update
      in
      { UpdateResult.triggered_dependencies; upstream = upstream_result }
  | _ ->
      let upstream =
        ClassMetadataEnvironment.UpdateResult.upstream upstream_result
        |> ClassHierarchyEnvironment.UpdateResult.upstream
        |> AliasEnvironment.UpdateResult.upstream
      in
      let current_and_previous_classes =
        UnannotatedGlobalEnvironment.UpdateResult.current_classes_and_removed_classes upstream
        |> Set.to_list
        |> List.map ~f:Reference.create
      in
      let current_and_previous_unannotated_globals =
        UnannotatedGlobalEnvironment.UpdateResult.current_and_previous_unannotated_globals upstream
      in
      let names_to_update =
        List.fold
          ~init:current_and_previous_unannotated_globals
          ~f:Set.add
          current_and_previous_classes
      in
      Set.to_list names_to_update |> Globals.KeySet.of_list |> Globals.remove_batch;
      update ~names_to_update ~track_dependencies:false ();
      {
        UpdateResult.triggered_dependencies = SharedMemoryKeys.DependencyKey.KeySet.empty;
        upstream = upstream_result;
      }


let read_only ({ class_metadata_environment } as environment) =
  let hash_to_key_map () =
    let keys =
      UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals
        (unannotated_global_environment environment)
    in
    Globals.compute_hashes_to_keys ~keys
  in
  let serialize_decoded decoded =
    match decoded with
    | Globals.Decoded (key, value) ->
        let value = value >>| Node.value >>| Annotation.sexp_of_t >>| Sexp.to_string in
        Some (GlobalValue.description, Reference.show key, value)
    | _ -> None
  in
  let decoded_equal first second =
    match first, second with
    | Globals.Decoded (_, first), Globals.Decoded (_, second) ->
        Some (Option.equal Annotation.equal (first >>| Node.value) (second >>| Node.value))
    | _ -> None
  in
  {
    ReadOnly.class_metadata_environment;
    get_global = Globals.get;
    hash_to_key_map;
    serialize_decoded;
    decoded_equal;
  }
