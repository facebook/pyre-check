(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Statement
open Analysis

type t = Target.t list Target.Map.t

type callgraph = Target.t list Target.CallableMap.t

module CallGraphSharedMemory = Memory.Serializer (struct
  type t = Target.t list Target.CallableMap.Tree.t

  module Serialized = struct
    type t = Target.t list Target.CallableMap.Tree.t

    let prefix = Prefix.make ()

    let description = "Call graph"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

type overrides = Reference.t list Reference.Map.t

module OverridesSharedMemory = Memory.Serializer (struct
  type t = Reference.t list Reference.Map.Tree.t

  module Serialized = struct
    type t = Reference.t list Reference.Map.Tree.t

    let prefix = Prefix.make ()

    let description = "Overrides"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

let empty = Target.Map.empty

let empty_callgraph = Target.CallableMap.empty

let empty_overrides = Reference.Map.empty

(* Returns forest of nodes in reverse finish time order. *)
let depth_first_search edges nodes =
  let visited = Target.Hashable.Table.create ~size:(2 * List.length nodes) () in
  let rec visit accumulator node =
    if Hashtbl.mem visited node then
      accumulator
    else (
      Hashtbl.add_exn visited ~key:node ~data:();
      let successors = Target.Map.find edges node |> Option.value ~default:[] in
      node :: List.fold successors ~init:accumulator ~f:visit)
  in
  let partition accumulator node =
    match visit [] node with
    | [] -> accumulator
    | tree -> tree :: accumulator
  in
  List.fold ~init:[] ~f:partition nodes


let reverse_edges edges =
  let reverse_edges = Target.Hashable.Table.create () in
  let walk_reverse_edges ~key:caller ~data:callees =
    let walk_callees callee =
      match Target.Hashable.Table.find reverse_edges callee with
      | None -> Target.Hashable.Table.add_exn reverse_edges ~key:callee ~data:[caller]
      | Some callers ->
          Target.Hashable.Table.set reverse_edges ~key:callee ~data:(caller :: callers)
    in
    List.iter callees ~f:walk_callees
  in
  let () = Target.Map.iteri edges ~f:walk_reverse_edges in
  let accumulate ~key ~data map =
    Target.Map.set map ~key ~data:(List.dedup_and_sort ~compare:Target.compare data)
  in
  Target.Hashable.Table.fold reverse_edges ~init:Target.Map.empty ~f:accumulate


let reverse call_graph =
  let reverse ~key ~data reverse_map =
    List.fold data ~init:reverse_map ~f:(fun reverse_map callee ->
        Target.Map.add_multi reverse_map ~key:callee ~data:key)
  in
  Map.fold call_graph ~init:Target.Map.empty ~f:reverse


let pp_partitions formatter partitions =
  let print_partition partitions =
    let print_partition index accumulator nodes =
      let nodes_to_string = List.map ~f:Target.show nodes |> String.concat ~sep:" " in
      let partition = Format.sprintf "Partition %d: [%s]" index nodes_to_string in
      accumulator ^ "\n" ^ partition
    in
    List.foldi partitions ~init:"" ~f:print_partition
  in
  Format.fprintf formatter "%s" (print_partition partitions)


let partition ~edges =
  let result = depth_first_search edges (Target.Map.keys edges) in
  let reverse_edges = reverse_edges edges in
  let partitions = depth_first_search reverse_edges (List.concat result) in
  Log.log ~section:`DependencyGraph "%a" pp_partitions partitions;
  partitions


let pp formatter edges =
  let pp_edge (callable, data) =
    let targets =
      List.map data ~f:Target.show |> List.sort ~compare:String.compare |> String.concat ~sep:" "
    in
    Format.fprintf formatter "%s -> [%s]\n" (Target.show callable) targets
  in
  let compare (left, _) (right, _) = String.compare (Target.show left) (Target.show right) in
  Target.Map.to_alist edges |> List.sort ~compare |> List.iter ~f:pp_edge


let dump call_graph ~path =
  let module Buffer = Caml.Buffer in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "{\n";
  let remove_trailing_comma () =
    Buffer.truncate buffer (Buffer.length buffer - 2);
    Buffer.add_string buffer "\n"
  in
  let add_edges ~key:source ~data:targets =
    let add_edge target = Format.asprintf "    \"%s\",\n" target |> Buffer.add_string buffer in
    if not (List.is_empty targets) then (
      Format.asprintf "  \"%s\": [\n" (Target.external_target_name source)
      |> Buffer.add_string buffer;
      List.map targets ~f:Target.external_target_name
      |> List.sort ~compare:String.compare
      |> List.iter ~f:add_edge;
      remove_trailing_comma ();
      Buffer.add_string buffer "  ],\n")
  in
  Map.iteri call_graph ~f:add_edges;
  remove_trailing_comma ();
  Buffer.add_string buffer "}";

  (* Write to file. *)
  Log.warning "Emitting the contents of the call graph to `%s`" (Path.absolute path);
  path |> File.create ~content:(Buffer.contents buffer) |> File.write


let from_callgraph callgraph =
  let add ~key ~data result =
    let key = (key :> Target.t) in
    Target.Map.set result ~key ~data
  in
  Target.CallableMap.fold callgraph ~f:add ~init:Target.Map.empty


let union left right =
  let combine ~key:_ left right = List.rev_append left right in
  Map.merge_skewed ~combine left right


let from_overrides overrides =
  let override_map, all_overrides =
    let add ~key:method_name ~data:subtypes (override_map, all_overrides) =
      let key = Target.create_override method_name in
      let data =
        List.map subtypes ~f:(fun at_type -> Target.create_derived_override key ~at_type)
      in
      ( Target.Map.set override_map ~key ~data,
        Target.Set.union all_overrides (Target.Set.of_list data) )
    in
    Reference.Map.fold overrides ~f:add ~init:(Target.Map.empty, Target.Set.empty)
  in
  let connect_overrides_to_methods override_graph =
    let overrides_to_methods =
      let override_to_method_edge override =
        match override with
        | `OverrideTarget _ as override ->
            let corresponding_method = Target.get_corresponding_method override in
            Some (override, [corresponding_method])
        | _ -> None
      in
      Target.Map.keys override_graph
      |> List.filter_map ~f:override_to_method_edge
      |> Target.Map.of_alist_exn
    in
    union overrides_to_methods override_graph
  in
  (* Create empty entries for leaves, so connect_overrides_to_methods can add self-links *)
  Target.Set.fold
    (fun override override_map ->
      if not (Target.Map.mem override_map override) then
        Target.Map.set override_map ~key:override ~data:[]
      else
        override_map)
    all_overrides
    override_map
  |> connect_overrides_to_methods


let create_overrides ~environment ~source =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let resolution =
    TypeCheck.resolution
      global_resolution
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module TypeCheck.DummyContext)
  in
  let resolution = Resolution.global_resolution resolution in
  if GlobalResolution.source_is_unit_test resolution ~source then
    Reference.Map.empty
  else
    let class_method_overrides { Node.value = { Class.body; name; _ }; _ } =
      let get_method_overrides class_ child_method =
        let method_name = Define.unqualified_name child_method in
        GlobalResolution.overrides (Reference.show class_) ~name:method_name ~resolution
        >>= fun ancestor ->
        let parent_annotation = Annotated.Attribute.parent ancestor in
        let ancestor_parent =
          Type.Primitive parent_annotation |> Type.expression |> Expression.show |> Reference.create
        in
        (* This special case exists only for `type`. Our override lookup for a class C first looks
           at the regular MRO. If that fails, it looks for Type[C]'s MRO. However, when C is type,
           this causes a cycle to get registered. *)
        if Reference.equal ancestor_parent class_ then
          None
        else
          let method_name =
            if Define.is_property_setter child_method then
              method_name ^ "$setter"
            else
              method_name
          in
          Some (Reference.create ~prefix:ancestor_parent method_name, class_)
      in
      let extract_define = function
        | { Node.value = Statement.Define define; _ } -> Some define
        | _ -> None
      in
      let methods = List.filter_map ~f:extract_define body in
      let class_name = Node.value name in
      List.filter_map methods ~f:(get_method_overrides class_name)
    in
    let record_overrides map (ancestor_method, overriding_type) =
      let update_types = function
        | Some types -> overriding_type :: types
        | None -> [overriding_type]
      in
      Reference.Map.update map ancestor_method ~f:update_types
    in
    let record_overrides_list map relations = List.fold relations ~init:map ~f:record_overrides in
    Preprocessing.classes source
    |> List.map ~f:class_method_overrides
    |> List.fold ~init:Reference.Map.empty ~f:record_overrides_list
    |> Map.map ~f:(List.dedup_and_sort ~compare:Reference.compare)


let expand_callees callees =
  let rec expand_and_gather expanded = function
    | (#Target.callable_t | #Target.object_t) as real -> real :: expanded
    | #Target.override_t as override ->
        let make_override at_type = Target.create_derived_override override ~at_type in
        let overrides =
          let member = Target.get_override_reference override in
          DependencyGraphSharedMemory.get_overriding_types ~member
          |> Option.value ~default:[]
          |> List.map ~f:make_override
        in
        Target.get_corresponding_method override
        :: List.fold overrides ~f:expand_and_gather ~init:expanded
  in
  List.fold callees ~init:[] ~f:expand_and_gather |> List.dedup_and_sort ~compare:Target.compare


type prune_result = {
  dependencies: t;
  pruned_callables: Target.t list;
}

let prune dependency_graph ~callables_with_dependency_information =
  let initial_callables =
    List.filter_map callables_with_dependency_information ~f:(fun (callable, is_internal) ->
        Option.some_if is_internal callable)
  in
  (* We have an implicit edge from a method to the override it corresponds to during the analysis.
     During the pruning, we make the edges from the method to the override explicit to make sure the
     DFS captures the interesting overrides. *)
  let callables_to_keep =
    depth_first_search dependency_graph initial_callables
    |> List.concat
    |> List.dedup_and_sort ~compare:Target.compare
  in
  let dependency_graph =
    (* We only keep the keys which were in the original dependency graph to avoid introducing
       spurious override leaves. *)
    let to_edge callable =
      Target.Map.find dependency_graph callable >>| fun values -> callable, values
    in
    Target.Map.of_alist_exn (List.filter_map callables_to_keep ~f:to_edge)
  in
  let callables_to_keep = Target.Set.of_list callables_to_keep in
  {
    dependencies = dependency_graph;
    pruned_callables =
      List.filter_map callables_with_dependency_information ~f:(fun (callable, _) ->
          Option.some_if (Target.Set.mem callable callables_to_keep) callable);
  }
