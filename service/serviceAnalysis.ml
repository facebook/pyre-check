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


let record_and_merge_call_graph environment call_graphs path source =
  let record_and_merge_call_graph path map call_graph =
    CallGraphSharedMemory.add_callers ~path (Access.Map.keys call_graph);
    let add_call_graph ~key:caller ~data:callees =
      CallGraphSharedMemory.add_call_edges ~caller ~callees
    in
    Access.Map.iteri call_graph ~f:add_call_graph;
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  Analysis.CallGraph.create ~environment ~source
  |> record_and_merge_call_graph path call_graphs


let record_overrides environment source =
  let record_overrides overrides_map =
    let record_override_edge ~key:ancestor ~data:children =
      CallGraphSharedMemory.add_overrides ~ancestor ~children
    in
    Access.Map.iteri overrides_map ~f:record_override_edge
  in
  overrides_of_source environment source
  |> record_overrides


let record_path_of_definitions path source =
  let defines = Preprocessing.defines source in
  let record_definition { Node.value = { Define.name; _ }; _ } =
    Interprocedural.Callable.add_definition (`RealTarget name) path
  in
  List.iter ~f:record_definition defines;
  defines


let analyze ~scheduler:_ ~configuration:_ ~environment ~handles:paths =
  Log.print "Analysis";
  let _call_graph =
    let build_call_graph map path =
      AstSharedMemory.get_source path
      >>| record_and_merge_call_graph environment map path
      |> Option.value ~default:map
    in
    List.fold paths ~init:Access.Map.empty ~f:build_call_graph
  in

  let record_overrides path =
    AstSharedMemory.get_source path
    >>| record_overrides environment
    |> ignore
  in
  List.iter paths ~f:record_overrides;

  let record_path_of_definitions path =
    AstSharedMemory.get_source path
    >>| record_path_of_definitions path
    |> ignore
  in
  List.iter paths ~f:record_path_of_definitions
