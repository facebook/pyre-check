(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Pyre

module Annotation = AnalysisAnnotation
module Cfg = AnalysisCfg
module Environment = AnalysisEnvironment
module Preprocessing = AnalysisPreprocessing
module TypeResolutionSharedMemory = AnalysisTypeResolutionSharedMemory


type t = (Access.t list) Access.Map.t

let create ~environment ~source =
  let fold_defines
      call_graph
      { Node.value = ({ Define.name = caller; _ } as define); _ } =
    let open TypeResolutionSharedMemory in
    let cfg = Cfg.create define in
    let annotation_lookup =
      let fold_annotations map { key; annotations } =
        Map.set map ~key ~data:annotations
      in
      TypeResolutionSharedMemory.get caller
      |> Option.value ~default:[]
      |> List.fold ~init:Int.Map.empty ~f:fold_annotations
    in
    let fold_cfg ~key:node_id ~data:node call_graph =
      let statements = Cfg.Node.statements node in
      let fold_statements statement_index call_graph statement =
        let annotations =
          Map.find
            annotation_lookup
            ([%hash: int * int] (node_id, statement_index))
          |> Option.value ~default:[]
          |> Access.Map.of_alist_exn
        in
        let resolution = Environment.resolution environment ~annotations () in
        let fold_accesses call_graph { Node.value = access; _ } =
          let add_call_edge call_graph ~resolution:_ ~resolved:_ ~element =
            let open Annotation.Type in
            let open Annotated.Access in
            let add_call_edge call_graph caller callee =
              Log.log
                ~section:`CallGraph
                "Adding call edge %a -> %a"
                Access.pp caller
                Access.pp callee;
              let update_callees = function
                | Some callees -> callee :: callees
                | None -> [callee]
              in
              Access.Map.update call_graph caller ~f:update_callees
            in
            match element with
            | Element.Signature {
                Element.signature =
                  Annotated.Signature.Found {
                    Annotated.Signature.callable = {
                      Callable.kind = Callable.Named callee;
                      _;
                    };
                    _;
                  };
                _;
              } ->
                add_call_edge call_graph caller callee
            | _ ->
                call_graph
          in
          Annotated.Access.create access
          |> Annotated.Access.fold ~resolution ~initial:call_graph ~f:add_call_edge
        in
        Visit.collect_accesses_with_location statement
        |> List.fold ~init:call_graph ~f:fold_accesses
      in
      List.foldi statements ~init:call_graph ~f:fold_statements
    in
    Hashtbl.fold cfg ~init:call_graph ~f:fold_cfg
  in
  Preprocessing.defines source
  |> List.fold ~init:Access.Map.empty ~f:fold_defines


(* Returns forest of nodes in reverse finish time order. *)
let depth_first_search edges nodes =
  let visited = Hashtbl.Poly.create ~size:(2 * List.length nodes) () in
  let rec visit accumulator node =
    if Hashtbl.mem visited node then
      accumulator
    else
      begin
        Hashtbl.add_exn visited ~key:node ~data:();
        let successor =
          Hashtbl.find edges node
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
  let reverse_edges = Access.Table.create () in
  let walk_reverse_edges ~key:caller ~data:callees =
    let walk_callees callee =
      match Access.Table.find reverse_edges callee with
      | None -> Access.Table.add_exn reverse_edges ~key:callee ~data:[caller]
      | Some callers -> Access.Table.set reverse_edges ~key:callee ~data:(caller :: callers)
    in
    List.iter callees ~f:walk_callees
  in
  Access.Table.iteri edges ~f:walk_reverse_edges;
  reverse_edges


let reverse call_graph =
  let reverse ~key ~data reverse_map =
    List.fold
      data
      ~init:reverse_map
      ~f:(fun reverse_map callee -> Access.Map.add_multi reverse_map ~key:callee ~data:key)
  in
  Map.fold call_graph ~init:Access.Map.empty ~f:reverse


let pp_partitions formatter partitions =
  let print_partition partitions =
    let print_partition index accumulator nodes =
      let nodes_to_string = Sexp.to_string [%message (nodes: Access.t list)] in
      let partition = Format.sprintf "Partition %d: %s" index nodes_to_string in
      accumulator ^ "\n" ^ partition
    in
    List.foldi partitions ~init:"" ~f:print_partition
  in
  Format.fprintf formatter "%s" (print_partition partitions)


let partition ~edges =
  let edges = Access.Map.to_alist edges |> Access.Table.of_alist_exn in
  let reverse_edges = reverse_edges edges in
  let result = depth_first_search edges (Access.Table.keys edges) in
  let partitions = depth_first_search reverse_edges (List.concat result) in
  Log.log ~section:`CallGraph "%a" pp_partitions partitions;
  partitions
