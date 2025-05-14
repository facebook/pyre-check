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

let expand_wildcard_imports ~parse_result_of_qualifier source =
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
                  match parse_result_of_qualifier qualifier with
                  | None
                  | Some (Result.Error _) ->
                      ()
                  | Some (Result.Ok source) ->
                      Module.wildcard_exports_of_raw_source source
                      |> List.iter ~f:(Hash_set.add transitive_exports);
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


let source_of_qualifier ~string_annotation_preserve_location ~parse_result_of_qualifier qualifier =
  (* Preprocessing a module depends on the module itself is implicitly assumed in `update`. No need
     to explicitly record the dependency. *)
  parse_result_of_qualifier qualifier
  >>| function
  | Result.Ok source ->
      expand_wildcard_imports ~parse_result_of_qualifier source
      |> Preprocessing.preprocess_after_wildcards ~string_annotation_preserve_location
      |> DecoratorPreprocessing.preprocess_source ~get_source:(fun qualifier ->
             parse_result_of_qualifier qualifier >>= Result.ok)
  | Result.Error
      {
        Parsing.ParseResult.Error.module_path =
          { ModulePath.raw = { relative; _ }; qualifier; _ } as module_path;
        _;
      } ->
      (* Files that have parser errors fall back into getattr-any. *)
      let fallback_source = ["import typing"; "def __getattr__(name: str) -> typing.Any: ..."] in
      let typecheck_flags = Source.TypecheckFlags.parse ~qualifier fallback_source in
      let statements = PyreMenhirParser.Parser.parse_exn ~relative fallback_source in
      Parsing.create_source ~typecheck_flags ~module_path statements
      |> Preprocessing.preprocess_after_wildcards ~string_annotation_preserve_location
