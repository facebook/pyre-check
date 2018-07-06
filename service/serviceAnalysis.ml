(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Statement
open Pyre

let overrides_of_source environment source =
  let open Annotated in
  let resolution = Environment.resolution environment () in
  let filter_overrides child_method =
    Method.overrides child_method ~resolution
    >>| fun ancestor_method -> (Method.name ancestor_method, Method.name child_method)
  in
  let record_overrides map (ancestor_method, child_method) =
    let update_children = function
      | Some children -> child_method :: children
      | None -> [child_method]
    in
    Statement.Access.Map.update map ancestor_method ~f:update_children
  in
  Preprocessing.classes source
  |> List.concat_map ~f:(Fn.compose Class.methods Class.create)
  |> List.filter_map ~f:filter_overrides
  |> List.fold ~init:Statement.Access.Map.empty ~f:record_overrides


let analyze ~scheduler:_ ~configuration:_ ~environment ~handles =
  Log.print "Analysis";
  let record_call_graph path =
    let record_call_graph call_graph =
      CallGraphSharedMemory.add_callers ~path (Access.Map.keys call_graph);
      let add_call_graph ~key:caller ~data:callees =
        CallGraphSharedMemory.add_call_edges ~caller ~callees
      in
      Access.Map.iteri call_graph ~f:add_call_graph
    in
    AstSharedMemory.get_source path
    >>| (fun source -> Analysis.CallGraph.create ~environment ~source)
    >>| record_call_graph
    |> ignore
  in
  List.iter handles ~f:record_call_graph;

  let record_overrides handle =
    let record_overrides overrides_map =
      let record_override_edge ~key:ancestor ~data:children =
        CallGraphSharedMemory.add_overrides ~ancestor ~children
      in
      Access.Map.iteri overrides_map ~f:record_override_edge
    in
    AstSharedMemory.get_source handle
    >>| overrides_of_source environment
    >>| record_overrides
    |> ignore
  in
  List.iter handles ~f:record_overrides
