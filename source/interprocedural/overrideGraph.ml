(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Statement
module GlobalResolution = Analysis.GlobalResolution
module TypeEnvironment = Analysis.TypeEnvironment
module AstEnvironment = Analysis.AstEnvironment

(* See `.mli` for documentation of modules and functions. *)

module Heap = struct
  type t = Reference.t list Target.Map.t

  let empty = Target.Map.empty

  let of_alist_exn = Target.Map.of_alist_exn

  let fold graph ~init ~f =
    Target.Map.fold graph ~init ~f:(fun ~key:member ~data:subtypes -> f ~member ~subtypes)


  let equal left right = Target.Map.equal (List.equal Reference.equal) left right

  let pp formatter overrides =
    let pp_pair formatter (member, subtypes) =
      Format.fprintf
        formatter
        "@,%a -> %s"
        Target.pp_internal
        member
        (List.map ~f:Reference.show subtypes |> String.concat ~sep:", ")
    in
    let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
    Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs (Target.Map.to_alist overrides)


  let show = Format.asprintf "%a" pp

  let from_source ~environment ~include_unit_tests ~source =
    let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    if (not include_unit_tests) && GlobalResolution.source_is_unit_test resolution ~source then
      Target.Map.empty
    else
      let timer = Timer.start () in
      let class_method_overrides { Node.value = { Class.body; name = class_name; _ }; _ } =
        let get_method_overrides child_method =
          let method_name = Define.unqualified_name child_method in
          let ancestor =
            try
              GlobalResolution.overrides (Reference.show class_name) ~name:method_name ~resolution
            with
            | Analysis.ClassHierarchy.Untracked untracked_type ->
                Log.warning
                  "Found untracked type `%s` when looking for a parent of `%a.%s`. The method will \
                   be considered has having no parent, which could lead to false negatives."
                  untracked_type
                  Reference.pp
                  class_name
                  method_name;
                None
          in
          ancestor
          >>= fun ancestor ->
          let parent_annotation = Analysis.Annotated.Attribute.parent ancestor in
          let ancestor_parent =
            Type.Primitive parent_annotation
            |> Type.expression
            |> Expression.show
            |> Reference.create
          in
          (* This special case exists only for `type`. Our override lookup for a class C first looks
             at the regular MRO. If that fails, it looks for Type[C]'s MRO. However, when C is type,
             this causes a cycle to get registered. *)
          if Reference.equal ancestor_parent class_name then
            None
          else
            let kind =
              if Define.is_property_setter child_method then
                Target.PropertySetter
              else
                Target.Normal
            in
            Some
              ( Target.Method { class_name = Reference.show ancestor_parent; method_name; kind },
                class_name )
        in
        let extract_define = function
          | { Node.value = Statement.Define define; _ } -> Some define
          | _ -> None
        in
        let overrides =
          body |> List.filter_map ~f:extract_define |> List.filter_map ~f:get_method_overrides
        in
        Statistics.performance
          ~randomly_log_every:1000
          ~always_log_time_threshold:1.0 (* Seconds *)
          ~name:"Overrides built"
          ~section:`DependencyGraph
          ~normals:["class", Reference.show class_name]
          ~timer
          ();
        overrides
      in
      let record_overrides map (ancestor_method, overriding_type) =
        let update_types = function
          | Some types -> overriding_type :: types
          | None -> [overriding_type]
        in
        Target.Map.update map ancestor_method ~f:update_types
      in
      let record_overrides_list map relations = List.fold relations ~init:map ~f:record_overrides in
      Preprocessing.classes source
      |> List.map ~f:class_method_overrides
      |> List.fold ~init:Target.Map.empty ~f:record_overrides_list
      |> Target.Map.map ~f:(List.dedup_and_sort ~compare:Reference.compare)


  let skip_overrides ~to_skip overrides =
    Target.Map.filter_keys
      ~f:(fun override -> not (Reference.Set.mem to_skip (Target.define_name override)))
      overrides


  type cap_overrides_result = {
    overrides: t;
    skipped_overrides: Target.t list;
  }

  let cap_overrides ~maximum_overrides overrides =
    (* Keep the information of whether we're skipping overrides in a ref that we accumulate while we
       filter the map. *)
    let skipped_overrides = ref [] in
    let keep_override_edge ~key:member ~data:subtypes =
      let number_of_overrides = List.length subtypes in
      match maximum_overrides with
      | Some cap ->
          if number_of_overrides < cap then
            true
          else begin
            Log.info
              "Omitting overrides for `%s`, as it has %d overrides."
              (Target.show_pretty member)
              number_of_overrides;
            skipped_overrides := member :: !skipped_overrides;
            false
          end
      | None ->
          if number_of_overrides > 50 then
            Log.warning
              "`%s` has %d overrides, this might slow down the analysis considerably."
              (Target.show_pretty member)
              number_of_overrides;
          true
    in
    let overrides = Target.Map.filteri overrides ~f:keep_override_edge in
    { overrides; skipped_overrides = !skipped_overrides }


  type serializable = Reference.t list Target.Map.Tree.t

  let to_serializable = Target.Map.to_tree

  let of_serializable = Target.Map.of_tree
end

module SharedMemory = struct
  module T =
    Memory.WithCache.Make
      (Target.SharedMemoryKey)
      (struct
        type t = Reference.t list

        let prefix = Prefix.make ()

        let description = "overriding types"
      end)

  type t = Handle

  let get_for_testing_only () = Handle

  let add_overriding_types Handle ~member ~subtypes = T.add member subtypes

  let get_overriding_types Handle ~member = T.get member

  let overrides_exist Handle member = T.mem member

  let from_heap overrides =
    let record_override_edge ~key:member ~data:subtypes =
      add_overriding_types Handle ~member ~subtypes
    in
    let () = Target.Map.iteri overrides ~f:record_override_edge in
    Handle


  let cleanup Handle overrides = overrides |> Target.Map.keys |> T.KeySet.of_list |> T.remove_batch

  let expand_override_targets Handle callees =
    let rec expand_and_gather expanded = function
      | (Target.Function _ | Target.Method _ | Target.Object _) as real -> real :: expanded
      | Target.Override _ as override ->
          let make_override at_type = Target.create_derived_override override ~at_type in
          let overrides =
            let member = Target.get_corresponding_method override in
            T.get member |> Option.value ~default:[] |> List.map ~f:make_override
          in
          Target.get_corresponding_method override
          :: List.fold overrides ~f:expand_and_gather ~init:expanded
    in
    List.fold callees ~init:[] ~f:expand_and_gather |> List.dedup_and_sort ~compare:Target.compare
end

let get_source ~environment qualifier =
  let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
  AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier


type whole_program_overrides = {
  override_graph_heap: Heap.t;
  override_graph_shared_memory: SharedMemory.t;
  skipped_overrides: Target.t list;
}

let build_whole_program_overrides
    ~scheduler
    ~environment
    ~include_unit_tests
    ~skip_overrides
    ~maximum_overrides
    ~qualifiers
  =
  let overrides =
    let combine ~key:_ left right = List.rev_append left right in
    let build_overrides overrides qualifier =
      match get_source ~environment qualifier with
      | None -> overrides
      | Some source ->
          let new_overrides =
            Heap.from_source ~environment ~include_unit_tests ~source
            |> Heap.skip_overrides ~to_skip:skip_overrides
          in
          Map.merge_skewed overrides new_overrides ~combine
    in
    Scheduler.map_reduce
      scheduler
      ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
      ~initial:Heap.empty
      ~map:(fun _ qualifiers -> List.fold qualifiers ~init:Heap.empty ~f:build_overrides)
      ~reduce:(Map.merge_skewed ~combine)
      ~inputs:qualifiers
      ()
  in
  let { Heap.overrides = override_graph_heap; skipped_overrides } =
    Heap.cap_overrides ~maximum_overrides overrides
  in
  let override_graph_shared_memory = SharedMemory.from_heap override_graph_heap in
  { override_graph_heap; override_graph_shared_memory; skipped_overrides }
