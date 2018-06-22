(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Statement
open Pyre

module AstSharedMemory = ServiceAstSharedMemory
module CallGraph = ServiceCallGraph


let call_graph_of_source environment source =
  let make_resolution define annotations =
    Environment.resolution
      environment
      ~define
      ~annotations
      ()
  in
  let fold_defines
      call_graph
      { Node.value = ({ Define.name = caller; _ } as define); _ } =
    let open TypeResolutionSharedMemory.TypeAnnotationsValue in
    let cfg = Cfg.create define in
    let annotation_lookup =
      let option_exn option = Option.value_exn option in
      let fold_annotations map { key; annotations } =
        Int.Map.set map ~key ~data:annotations
      in
      TypeResolutionSharedMemory.get caller
      |> option_exn
      |> List.fold ~init:Int.Map.empty ~f:fold_annotations
    in
    let fold_cfg ~key:node_id ~data:node call_graph =
      let statements = Cfg.Node.statements node in
      let fold_statements statement_index call_graph statement =
        let annotations =
          Int.Map.find_exn
            annotation_lookup
            ([%hash: int * int] (node_id, statement_index))
          |> Access.Map.of_alist_exn
        in
        let resolution = make_resolution define annotations in
        let fold_accesses call_graph { Node.value = access; _ } =
          let add_call_edge call_graph ~resolution:_ ~resolved ~element:_ =
            let open Annotation.Type in
            let open Record.Callable in
            match Annotation.annotation resolved with
            | Callable { kind = Callable.Named callee; _ } ->
                let update_callees = function
                  | Some callees -> callee :: callees
                  | None -> [callee]
                in
                Access.Map.update call_graph caller ~f:update_callees
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


let overrides_of_source environment source =
  let open Annotated in
  let define = Statement.Define.create_toplevel ~qualifier:[] ~statements:[] in
  let resolution = Environment.resolution environment ~define () in
  let filter_overrides child_method =
    Method.overloads child_method ~resolution
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
      CallGraph.add_callers ~path (Access.Map.keys call_graph);
      let add_call_graph ~key:caller ~data:callees =
        CallGraph.add_call_edges ~caller ~callees
      in
      Access.Map.iteri call_graph ~f:add_call_graph
    in
    AstSharedMemory.get_source path
    >>| call_graph_of_source environment
    >>| record_call_graph
    |> ignore
  in
  List.iter handles ~f:record_call_graph;

  let record_overrides handle =
    let record_overrides overrides_map =
      let record_override_edge ~key:ancestor ~data:children =
        CallGraph.add_overrides ~ancestor ~children
      in
      Access.Map.iteri overrides_map ~f:record_override_edge
    in
    AstSharedMemory.get_source handle
    >>| overrides_of_source environment
    >>| record_overrides
    |> ignore
  in
  List.iter handles ~f:record_overrides
