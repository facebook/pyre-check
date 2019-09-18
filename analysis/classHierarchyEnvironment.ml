(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Expression
open Statement

type t = { alias_environment: AliasEnvironment.ReadOnly.t }

type dependency =
  | TypeCheckSource of Reference.t
  | RegisterClassMetadata of Type.Primitive.t
[@@deriving show, compare, sexp]

module DependencyKey = Memory.DependencyKey.Make (struct
  type nonrec t = dependency

  let to_string dependency = sexp_of_dependency dependency |> Sexp.to_string_mach

  let compare = compare_dependency

  type out = dependency

  let from_string string = Sexp.of_string string |> dependency_of_sexp
end)

let create alias_environment = { alias_environment }

let unannotated_global_environment { alias_environment } =
  AliasEnvironment.ReadOnly.unannotated_global_environment alias_environment


module ReadOnly = struct
  type t = {
    get_edges: ?dependency:dependency -> IndexTracker.t -> ClassHierarchy.Target.t list option;
    get_backedges: IndexTracker.t -> ClassHierarchy.Target.Set.Tree.t option;
    alias_environment: AliasEnvironment.ReadOnly.t;
  }

  let get_edges { get_edges; _ } = get_edges

  let get_backedges { get_backedges; _ } = get_backedges

  let alias_environment { alias_environment; _ } = alias_environment
end

module UpdateResult = struct
  type t = {
    triggered_dependencies: DependencyKey.KeySet.t;
    upstream: AliasEnvironment.UpdateResult.t;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let upstream { upstream; _ } = upstream
end

module EdgeValue = struct
  type t = ClassHierarchy.Target.t list [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "Edges"

  let unmarshall value = Marshal.from_string value 0
end

module BackedgeValue = struct
  type t = ClassHierarchy.Target.Set.Tree.t

  let prefix = Prefix.make ()

  let description = "Backedges"

  let unmarshall value = Marshal.from_string value 0
end

module Edges =
  Memory.DependencyTrackedTableWithCache (IndexTracker.IndexKey) (DependencyKey) (EdgeValue)
module Backedges = Memory.WithCache.Make (IndexTracker.IndexKey) (BackedgeValue)

let edges = Edges.get ?dependency:None

let backedges key = Backedges.get key >>| ClassHierarchy.Target.Set.of_tree

let set_backedges ~index:key ~targets:data =
  let value = ClassHierarchy.Target.Set.to_tree data in
  Backedges.remove_batch (Backedges.KeySet.singleton key);
  Backedges.add key value


let disconnect_incoming_backedges_of_successors ~indices_to_disconnect =
  let all_successors =
    let all_successors = IndexTracker.Hash_set.create () in
    let add_successors key =
      match edges key with
      | Some successors ->
          List.iter successors ~f:(fun { ClassHierarchy.Target.target; _ } ->
              Hash_set.add all_successors target)
      | None -> ()
    in
    IndexTracker.Set.iter indices_to_disconnect ~f:add_successors;
    all_successors
  in
  let remove_backedges successor =
    backedges successor
    >>| (fun current_predecessors ->
          let new_predecessors =
            Set.filter
              ~f:(fun { ClassHierarchy.Target.target; _ } ->
                not (IndexTracker.Set.mem indices_to_disconnect target))
              current_predecessors
          in
          set_backedges ~index:successor ~targets:new_predecessors)
    |> ignore
  in
  Hash_set.iter all_successors ~f:remove_backedges


let get_parents ({ alias_environment } as environment) ~track_dependencies name =
  let object_index = IndexTracker.index "object" in
  let parse_annotation =
    let dependency = Option.some_if track_dependencies (AliasEnvironment.ClassConnect name) in
    AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
      ?dependency
      alias_environment
  in
  let dependency =
    Option.some_if track_dependencies (UnannotatedGlobalEnvironment.ClassConnect name)
  in
  (* Register normal annotations. *)
  let extract_supertype { Expression.Call.Argument.value; _ } =
    let value = Expression.delocalize value in
    match Node.value value with
    | Call _
    | Name _ -> (
        let supertype, parameters = parse_annotation ~allow_untracked:true value |> Type.split in
        match supertype with
        | Type.Top ->
            Statistics.event
              ~name:"superclass of top"
              ~section:`Environment
              ~normals:["unresolved name", Expression.show value]
              ();
            None
        | Type.Primitive primitive
          when not
                 (UnannotatedGlobalEnvironment.ReadOnly.class_exists
                    ?dependency
                    (unannotated_global_environment environment)
                    primitive) ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype;
            None
        | Type.Primitive supertype -> Some (supertype, parameters)
        | _ -> None )
    | _ -> None
  in
  let bases ({ Node.value = { Class.bases; _ }; _ } as definition) =
    let inferred_base = AnnotatedBases.inferred_generic_base definition ~parse_annotation in
    inferred_base @ bases
  in
  let add_special_parents parents =
    let simples = List.map ~f:(fun parent -> parent, Type.OrderedTypes.Concrete []) in
    match parents, name with
    | _, "int" -> simples ["float"; "numbers.Integral"]
    | _, "float" -> simples ["complex"; "numbers.Rational"; "numbers.Real"]
    | _, "complex" -> simples ["numbers.Complex"]
    | _, "numbers.Complex" -> simples ["numbers.Number"]
    | [], _ -> simples ["object"]
    | _ -> parents
  in
  let is_not_primitive_cycle (parent, _) = not (String.equal name parent) in
  let convert_to_targets =
    List.map ~f:(fun (name, parameters) ->
        { ClassHierarchy.Target.target = IndexTracker.index name; parameters })
  in
  let deduplicate targets =
    let deduplicate (visited, sofar) ({ ClassHierarchy.Target.target; _ } as edge) =
      if Set.mem visited target then
        visited, sofar
      else
        Set.add visited target, edge :: sofar
    in
    List.fold targets ~f:deduplicate ~init:(IndexTracker.Set.empty, []) |> snd |> List.rev
  in
  let remove_extra_edges_to_object targets =
    let not_object_edge { ClassHierarchy.Target.target; _ } =
      not (IndexTracker.equal target object_index)
    in
    match List.filter targets ~f:not_object_edge with
    | [] -> targets
    | filtered -> filtered
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
    ?dependency
    (unannotated_global_environment environment)
    name
  >>| bases
  (* Don't register metaclass=abc.ABCMeta, etc. superclasses. *)
  >>| List.filter ~f:(fun { Expression.Call.Argument.name; _ } -> Option.is_none name)
  >>| List.filter_map ~f:extract_supertype
  >>| add_special_parents
  >>| List.filter ~f:is_not_primitive_cycle
  >>| convert_to_targets
  >>| deduplicate
  >>| remove_extra_edges_to_object


let add_backedges class_name =
  let predecessor = IndexTracker.index class_name in
  let handle_targets targets =
    let add_backedge { ClassHierarchy.Target.target = successor; parameters } =
      let predecessors =
        backedges successor |> Option.value ~default:ClassHierarchy.Target.Set.empty
      in
      set_backedges
        ~index:successor
        ~targets:(Set.add predecessors { ClassHierarchy.Target.target = predecessor; parameters })
    in
    if not (Backedges.mem predecessor) then
      set_backedges ~index:predecessor ~targets:ClassHierarchy.Target.Set.empty;
    List.iter targets ~f:add_backedge
  in
  Edges.get predecessor |> Option.iter ~f:handle_targets


let update environment ~scheduler ~configuration upstream_update =
  let update ~names_to_update ~track_dependencies () =
    let connect environment names =
      let set_edges name =
        let targets = get_parents environment name ~track_dependencies in
        Option.iter targets ~f:(Edges.add (IndexTracker.index name))
      in
      List.iter names ~f:set_edges
    in
    Scheduler.iter
      scheduler
      ~configuration
      ~f:(connect environment)
      ~inputs:(Set.to_list names_to_update);
    Set.iter names_to_update ~f:add_backedges
  in
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let dependencies =
        AliasEnvironment.UpdateResult.triggered_dependencies upstream_update
        |> AliasEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | AliasEnvironment.ClassConnect name -> Some name
               | _ -> None)
        |> Type.Primitive.Set.of_list
      in
      let dependencies =
        AliasEnvironment.UpdateResult.upstream upstream_update
        |> UnannotatedGlobalEnvironment.UpdateResult.triggered_dependencies
        |> UnannotatedGlobalEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | UnannotatedGlobalEnvironment.ClassConnect name -> Some name
               | _ -> None)
        |> Type.Primitive.Set.of_list
        |> Type.Primitive.Set.union dependencies
      in
      let dependencies =
        AliasEnvironment.UpdateResult.upstream upstream_update
        |> UnannotatedGlobalEnvironment.UpdateResult.upstream
        |> AstEnvironment.UpdateResult.triggered_dependencies
        |> AstEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | AstEnvironment.ClassConnect name -> Some name
               | _ -> None)
        |> Type.Primitive.Set.of_list
        |> Type.Primitive.Set.union dependencies
      in
      let names_to_update =
        AliasEnvironment.UpdateResult.upstream upstream_update
        |> UnannotatedGlobalEnvironment.UpdateResult.added_classes
        |> Set.union dependencies
      in
      let keys_to_invalidate = IndexTracker.indices dependencies in
      disconnect_incoming_backedges_of_successors ~indices_to_disconnect:keys_to_invalidate;
      let (), triggered_dependencies =
        let keys = keys_to_invalidate |> IndexTracker.Set.to_list |> Edges.KeySet.of_list in
        Backedges.remove_batch keys;
        DependencyKey.Transaction.empty
        |> Edges.add_to_transaction ~keys
        |> DependencyKey.Transaction.execute
             ~update:(update ~names_to_update ~track_dependencies:true)
      in
      { UpdateResult.triggered_dependencies; upstream = upstream_update }
  | _ ->
      let upstream = AliasEnvironment.UpdateResult.upstream upstream_update in
      let current_and_previous =
        upstream |> UnannotatedGlobalEnvironment.UpdateResult.current_classes_and_removed_classes
      in
      let current_and_previous_indices = IndexTracker.indices current_and_previous in
      disconnect_incoming_backedges_of_successors
        ~indices_to_disconnect:current_and_previous_indices;
      let () =
        let s = current_and_previous_indices |> IndexTracker.Set.to_list |> Edges.KeySet.of_list in
        Edges.remove_batch s;
        Backedges.remove_batch s
      in
      update ~names_to_update:current_and_previous () ~track_dependencies:false;

      {
        UpdateResult.triggered_dependencies = DependencyKey.KeySet.empty;
        upstream = upstream_update;
      }


let read_only { alias_environment } =
  { ReadOnly.alias_environment; get_edges = Edges.get; get_backedges = Backedges.get }
