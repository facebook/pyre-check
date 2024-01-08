(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module wraps together all of the preprocessing logic
 * that lives *downstream* of a "raw source" in Pyre.
 *
 * There are three reasons this code lives in a separate module from
 * `preprocessing.ml`:
 * (1) The transforms in preprocessing are simple, local functions but
 *     we also resolve wildcard imports, which potentially needs nonlocal
 *     information.
 * (2) The simple transforms in preprocessing are split into two phases, and
 *     only the second is relevant to the "raw source" to "processed source"
 *     conversion; the first phase happens before we store raw sources.
 * (3) We still support a decorator processing pass for pysa, which both
 *     requires nonlocal information and relies on shared memory.
 *     We'd like to strip this out, but we really don't want `preprocessing.ml`
 *     to use shared memory (this would break our build graph), so we need a place
 *     to put this glue logic.
 *)

open Core
open Ast
open Pyre
open PyreParser

let wildcard_exports_of ({ Source.module_path; _ } as source) =
  let open Expression in
  let open UnannotatedGlobal in
  let extract_dunder_all = function
    | {
        Collector.Result.name = "__all__";
        unannotated_global =
          SimpleAssign { value = { Node.value = Expression.(List names | Tuple names); _ }; _ };
      } ->
        let to_identifier = function
          | { Node.value = Expression.Constant (Constant.String { value = name; _ }); _ } ->
              Some name
          | _ -> None
        in
        Some (List.filter_map ~f:to_identifier names)
    | _ -> None
  in
  let unannotated_globals = Collector.from_source source in
  match List.find_map unannotated_globals ~f:extract_dunder_all with
  | Some names -> names |> List.dedup_and_sort ~compare:Identifier.compare
  | _ ->
      let unannotated_globals =
        (* Stubs have a slightly different rule with re-export *)
        let filter_unaliased_import = function
          | {
              Collector.Result.unannotated_global =
                Imported
                  (ImportEntry.Module { implicit_alias; _ } | ImportEntry.Name { implicit_alias; _ });
              _;
            } ->
              not implicit_alias
          | _ -> true
        in
        if ModulePath.is_stub module_path then
          List.filter unannotated_globals ~f:filter_unaliased_import
        else
          unannotated_globals
      in
      List.map unannotated_globals ~f:(fun { Collector.Result.name; _ } -> name)
      |> List.filter ~f:(fun name -> not (String.is_prefix name ~prefix:"_"))
      |> List.dedup_and_sort ~compare:Identifier.compare


let expand_wildcard_imports ~raw_source_of_qualifier source =
  let open Statement in
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let get_transitive_exports qualifier =
      let module Visitor = Visit.MakeStatementVisitor (struct
        type t = Reference.t list

        let visit_children _ = false

        let statement _ collected_imports { Node.value; _ } =
          match value with
          | Statement.Import { Import.from = Some from; imports }
            when List.exists imports ~f:(fun { Node.value = { Import.name; _ }; _ } ->
                     String.equal (Reference.show name) "*") ->
              Node.value from :: collected_imports
          | _ -> collected_imports
      end)
      in
      let visited_modules = Reference.Hash_set.create () in
      let transitive_exports = Identifier.Hash_set.create () in
      let worklist = Queue.of_list [qualifier] in
      let rec search_wildcard_imports () =
        match Queue.dequeue worklist with
        | None -> ()
        | Some qualifier ->
            let _ =
              match Hash_set.strict_add visited_modules qualifier with
              | Error _ -> ()
              | Ok () -> (
                  match raw_source_of_qualifier qualifier with
                  | None
                  | Some (Result.Error _) ->
                      ()
                  | Some (Result.Ok source) ->
                      wildcard_exports_of source |> List.iter ~f:(Hash_set.add transitive_exports);
                      Visitor.visit [] source |> Queue.enqueue_all worklist)
            in
            search_wildcard_imports ()
      in
      search_wildcard_imports ();
      Hash_set.to_list transitive_exports |> List.sort ~compare:Identifier.compare


    let statement state ({ Node.value; _ } as statement) =
      match value with
      | Statement.Import { Import.from = Some from; imports } -> (
          let starred_import =
            List.find imports ~f:(fun { Node.value = { Import.name; _ }; _ } ->
                String.equal (Reference.show name) "*")
          in
          match starred_import with
          | Some _ ->
              let expanded_import =
                match get_transitive_exports (Node.value from) with
                | [] -> []
                | exports ->
                    List.map exports ~f:(fun name ->
                        {
                          Node.value = { Import.name = Reference.create name; alias = Some name };
                          location = Location.any;
                        })
                    |> (fun expanded ->
                         Statement.Import { Import.from = Some from; imports = expanded })
                    |> fun value -> [{ statement with Node.value }]
              in
              state, expanded_import
          | None -> state, [statement])
      | _ -> state, [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let processed_source_of_qualifier ~raw_source_of_qualifier qualifier =
  (* Preprocessing a module depends on the module itself is implicitly assumed in `update`. No need
     to explicitly record the dependency. *)
  raw_source_of_qualifier qualifier
  >>| function
  | Result.Ok source ->
      expand_wildcard_imports ~raw_source_of_qualifier source
      |> Preprocessing.preprocess_after_wildcards
      |> DecoratorPreprocessing.preprocess_source ~get_source:(fun qualifier ->
             raw_source_of_qualifier qualifier >>= Result.ok)
  | Result.Error
      {
        Parsing.ParserError.module_path =
          { ModulePath.raw = { relative; _ }; qualifier; _ } as module_path;
        _;
      } ->
      (* Files that have parser errors fall back into getattr-any. *)
      let fallback_source = ["import typing"; "def __getattr__(name: str) -> typing.Any: ..."] in
      let typecheck_flags = Source.TypecheckFlags.parse ~qualifier fallback_source in
      let statements = Parser.parse_exn ~relative fallback_source in
      Parsing.create_source ~typecheck_flags ~module_path statements
      |> Preprocessing.preprocess_after_wildcards
