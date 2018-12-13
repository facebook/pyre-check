(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Statement
open Analysis


type t = (Callable.t list) Callable.Map.t
type callgraph = (Callable.t list) Callable.RealMap.t


let empty = Callable.Map.empty
let empty_callgraph = Callable.RealMap.empty


let create_callgraph ~environment ~source =
  let fold_defines
      dependencies
      ({ Node.value = { Define.name = caller; parent; _ }; _ } as define) =
    let cfg = Cfg.create define.value in
    let caller_callable = Callable.create define in
    let fold_cfg ~key:node_id ~data:node callees =
      let statements = Cfg.Node.statements node in
      let fold_statements index callees statement =
        let resolution =
          TypeCheck.resolution_with_key
            ~environment
            ~parent
            ~access:caller
            ~key:(Some ([%hash: int * int](node_id, index)))
        in
        let process_access callees access =
          let add_call_edge callees callee =
            Log.log
              ~section:`DependencyGraph
              "Adding call edge %a -> %a"
              Callable.pp caller_callable
              Callable.pp callee;
            callee :: callees
          in
          let new_callees = CallResolution.resolve_call_targets ~resolution access in
          List.fold new_callees ~f:add_call_edge ~init:callees
        in
        Visit.collect_accesses statement
        |> List.fold ~init:callees ~f:process_access
      in
      List.foldi statements ~init:callees ~f:fold_statements
    in
    let callees =
      Hashtbl.fold cfg ~init:[] ~f:fold_cfg
      |> List.dedup_and_sort ~compare:Callable.compare
    in
    Callable.RealMap.set dependencies ~key:caller_callable ~data:callees
  in
  Preprocessing.defines source
  |> List.fold ~init:Callable.RealMap.empty ~f:fold_defines


(* Returns forest of nodes in reverse finish time order. *)
let depth_first_search edges nodes =
  let visited = Callable.Hashable.Table.create ~size:(2 * List.length nodes) () in
  let rec visit accumulator node =
    if Hashtbl.mem visited node then
      accumulator
    else
      begin
        Hashtbl.add_exn visited ~key:node ~data:();
        let successor =
          Callable.Map.find edges node
          |> Option.value ~default:[]
        in
        node :: (List.fold successor ~init:accumulator ~f:visit)
      end
  in
  let partition accumulator node =
    match visit [] node with
    | [] -> accumulator
    | tree -> tree :: accumulator
  in
  List.fold ~init:[] ~f:partition nodes


let reverse_edges edges =
  let reverse_edges = Callable.Hashable.Table.create () in
  let walk_reverse_edges ~key:caller ~data:callees =
    let walk_callees callee =
      match Callable.Hashable.Table.find reverse_edges callee with
      | None ->
          Callable.Hashable.Table.add_exn reverse_edges ~key:callee ~data:[caller]
      | Some callers ->
          Callable.Hashable.Table.set reverse_edges ~key:callee ~data:(caller :: callers)
    in
    List.iter callees ~f:walk_callees
  in
  let () = Callable.Map.iteri edges ~f:walk_reverse_edges in
  let accumulate ~key ~data map =
    Callable.Map.set map ~key ~data:(List.dedup_and_sort ~compare:Callable.compare data)
  in
  Callable.Hashable.Table.fold reverse_edges ~init:Callable.Map.empty ~f:accumulate


let reverse call_graph =
  let reverse ~key ~data reverse_map =
    List.fold
      data
      ~init:reverse_map
      ~f:(fun reverse_map callee -> Callable.Map.add_multi reverse_map ~key:callee ~data:key)
  in
  Map.fold call_graph ~init:Callable.Map.empty ~f:reverse


let pp_partitions formatter partitions =
  let print_partition partitions =
    let print_partition index accumulator nodes =
      let nodes_to_string =
        List.map ~f:Callable.show nodes
        |> String.concat ~sep:" "
      in
      let partition = Format.sprintf "Partition %d: [%s]" index nodes_to_string in
      accumulator ^ "\n" ^ partition
    in
    List.foldi partitions ~init:"" ~f:print_partition
  in
  Format.fprintf formatter "%s" (print_partition partitions)


let partition ~edges =
  let result = depth_first_search edges (Callable.Map.keys edges) in
  let reverse_edges = reverse_edges edges in
  let partitions = depth_first_search reverse_edges (List.concat result) in
  Log.log ~section:`DependencyGraph "%a" pp_partitions partitions;
  partitions


let pp formatter edges =
  let pp_edge (callable, data) =
    let targets =
      List.map data ~f:Callable.show
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:" "
    in
    Format.fprintf formatter "%s -> [%s]\n" (Callable.show callable) targets
  in
  let compare (left, _) (right, _) =
    String.compare (Callable.show left) (Callable.show right)
  in
  Callable.Map.to_alist edges
  |> List.sort ~compare
  |> List.iter ~f:pp_edge


let dump call_graph ~configuration =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "{\n";

  let remove_trailing_comma () =
    Buffer.truncate buffer (Buffer.length buffer - 2);
    Buffer.add_string buffer "\n"
  in

  let add_edges ~key:source ~data:targets =
    let add_edge target =
      Format.asprintf
        "    \"%s\",\n"
        (Callable.external_target_name target)
      |> Buffer.add_string buffer
    in
    if not (List.is_empty targets) then
      begin
        Format.asprintf "  \"%s\": [\n" (Callable.external_target_name source)
        |> Buffer.add_string buffer;
        List.iter targets ~f:add_edge;
        remove_trailing_comma ();
        Buffer.add_string buffer "  ],\n"
      end
  in
  Map.iteri call_graph ~f:add_edges;
  remove_trailing_comma ();

  Buffer.add_string buffer "}";

  (* Write to file. *)
  Path.create_relative
    ~root:(Configuration.Analysis.pyre_root configuration)
    ~relative:"call_graph.json"
  |> File.create ~content:(Buffer.contents buffer)
  |> File.write


let from_callgraph callgraph =
  let add ~key ~data result =
    let key = (key :> Callable.t ) in
    Callable.Map.set result ~key ~data
  in
  Callable.RealMap.fold callgraph ~f:add ~init:Callable.Map.empty
