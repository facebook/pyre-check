(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open Ast
open Analysis

type t = {
  get_source_code_api: unit -> Analysis.SourceCodeApi.t;
  get_type_check_qualifiers: unit -> Ast.Reference.t list;
}

let create_for_testing
    ?(get_source_code_api = fun () -> failwith "get_source_code_api implementation is not provided")
    ?(get_type_check_qualifiers = fun () -> [])
    ()
  =
  { get_source_code_api; get_type_check_qualifiers }


let create_module_path ~should_type_check relative =
  { ModulePath.Raw.relative; priority = 0 } |> ModulePath.create ~should_type_check


let build_index ~configuration module_paths =
  let add_to_index sofar module_path =
    Map.update sofar (ModulePath.qualifier module_path) ~f:(function
        | None -> module_path
        | Some old_module_path ->
            let compare_result =
              ModulePath.Raw.priority_aware_compare
                ~configuration
                (ModulePath.raw old_module_path)
                (ModulePath.raw module_path)
            in
            if compare_result > 0 then module_path else old_module_path)
  in
  let init =
    Map.empty
      (module struct
        include Reference
        include Comparator.Make (Reference)
      end)
  in
  List.fold module_paths ~init ~f:add_to_index


let create
    ~controls
    ~loader:{ FileLoader.load }
    ~listing:{ Sourcedb.Listing.all_sources; all_dependencies }
    ()
  =
  let dependency_module_paths =
    all_dependencies () |> List.map ~f:(create_module_path ~should_type_check:false)
  in
  let source_module_paths =
    all_sources () |> List.map ~f:(create_module_path ~should_type_check:true)
  in
  (* This is needed purely to satisfy the interface of ModulePath.Raw.priority_aware_compare *)
  let dummy_configuration = Configuration.Analysis.create ~source_paths:[] () in
  let source_index = build_index ~configuration:dummy_configuration source_module_paths in
  let dependency_index = build_index ~configuration:dummy_configuration dependency_module_paths in
  let merged_index =
    (* Source files always take priority over dependency files *)
    (* TODO: Revisit this decision once we get more results from production *)
    let combine ~key:_ source_value _dependency_value = source_value in
    Map.merge_skewed source_index dependency_index ~combine
  in
  let look_up_qualifier qualifier =
    (* Currently we are not handling implicit modules and hence not fully backward-compatible *)
    (* TODO: Revisit this decision once we get more results from production *)
    match Map.find merged_index qualifier with
    | None -> SourceCodeApi.ModuleLookup.NotFound
    | Some module_path -> SourceCodeApi.ModuleLookup.Explicit module_path
  in
  let parse_result_of_qualifier qualifier =
    let open Option.Monad_infix in
    Map.find merged_index qualifier
    >>| fun (ModulePath.{ raw = { Raw.relative; _ }; _ } as module_path) ->
    load relative |> Parsing.parse_result_of_load_result ~controls module_path
  in
  let source_code_api =
    SourceCodeApi.create ~controls ~look_up_qualifier ~parse_result_of_qualifier
  in
  let type_check_qualifiers =
    List.map source_module_paths ~f:ModulePath.qualifier
    |> List.dedup_and_sort ~compare:Reference.compare
  in
  {
    get_source_code_api = (fun () -> source_code_api);
    get_type_check_qualifiers = (fun () -> type_check_qualifiers);
  }


let get_source_code_api { get_source_code_api; _ } = get_source_code_api ()

let get_type_check_qualifiers { get_type_check_qualifiers; _ } = get_type_check_qualifiers ()

let get_source_code_incremental_api { get_source_code_api; _ } =
  let read_only =
    let get_tracked_api ~dependency:_ = get_source_code_api () in
    SourceCodeIncrementalApi.ReadOnly.create ~get_tracked_api ~get_untracked_api:get_source_code_api
  in
  let overlay () = failwith "Buck based source code api does not support overlay" in
  let update ~scheduler:_ _ =
    failwith "Buck based source code api does not support incremental update"
  in
  let global_module_paths_api =
    (fun () ->
      failwith "Buck based source code api is not expected to perform global module listing")
    |> GlobalModulePathsApi.create
  in
  SourceCodeIncrementalApi.Base.create ~read_only ~overlay ~update ~global_module_paths_api
