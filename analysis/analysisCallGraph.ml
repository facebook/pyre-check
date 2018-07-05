open Core

open Ast
open Statement
open Pyre

module Annotation = AnalysisAnnotation
module Cfg = AnalysisCfg
module Environment = AnalysisEnvironment
module Preprocessing = AnalysisPreprocessing
module TypeResolutionSharedMemory = AnalysisTypeResolutionSharedMemory


let of_source environment source =
  let fold_defines
      call_graph
      { Node.value = ({ Define.name = caller; _ } as define); _ } =
    let open TypeResolutionSharedMemory in
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
        let resolution = Environment.resolution environment ~annotations () in
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
