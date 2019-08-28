(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Pyre
open PyreParser

type parse_result =
  | Success of Source.t
  | SyntaxError of string
  | SystemError of string

let parse_source ~configuration ({ SourcePath.relative; qualifier; _ } as source_path) =
  let parse_lines lines =
    let metadata = Source.Metadata.parse ~qualifier lines in
    try
      let statements = Parser.parse ~relative lines in
      Success
        (Source.create_from_source_path
           ~docstring:(Statement.extract_docstring statements)
           ~metadata
           ~source_path
           statements)
    with
    | Parser.Error error -> SyntaxError error
    | Failure error -> SystemError error
  in
  let path = SourcePath.full_path ~configuration source_path in
  match File.lines (File.create path) with
  | Some lines -> parse_lines lines
  | None ->
      let message = Format.asprintf "Cannot open file %a" Path.pp path in
      SystemError message


module RawParseResult = struct
  type t = {
    parsed: Reference.t list;
    syntax_error: SourcePath.t list;
    system_error: SourcePath.t list;
  }

  let empty = { parsed = []; syntax_error = []; system_error = [] }

  let merge
      { parsed = left_parsed; syntax_error = left_syntax_error; system_error = left_system_error }
      {
        parsed = right_parsed;
        syntax_error = right_syntax_error;
        system_error = right_system_error;
      }
    =
    {
      parsed = left_parsed @ right_parsed;
      syntax_error = left_syntax_error @ right_syntax_error;
      system_error = left_system_error @ right_system_error;
    }
end

let parse_raw_sources ~configuration ~scheduler ~ast_environment source_paths =
  let parse_and_categorize
      ({ RawParseResult.parsed; syntax_error; system_error } as result)
      source_path
    =
    match parse_source ~configuration source_path with
    | Success ({ Source.qualifier; _ } as source) ->
        let source = Preprocessing.preprocess_phase0 source in
        AstEnvironment.Raw.add_source ast_environment source;
        { result with parsed = qualifier :: parsed }
    | SyntaxError message ->
        Log.log ~section:`Parser "%s" message;
        { result with syntax_error = source_path :: syntax_error }
    | SystemError message ->
        Log.error "%s" message;
        { result with system_error = source_path :: system_error }
  in
  Scheduler.map_reduce
    scheduler
    ~configuration
    ~initial:RawParseResult.empty
    ~map:(fun _ -> List.fold ~init:RawParseResult.empty ~f:parse_and_categorize)
    ~reduce:RawParseResult.merge
    ~inputs:source_paths
    ()


let expand_wildcard_imports ~ast_environment ({ Source.qualifier; _ } as source) =
  let open Statement in
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let get_transitive_exports ~dependency ~ast_environment qualifier =
      let module Visitor = Visit.MakeStatementVisitor (struct
        type t = Reference.t list

        let visit_children _ = false

        let statement _ collected_imports { Node.value; _ } =
          match value with
          | Statement.Import { Statement.Import.from = Some from; imports }
            when List.exists imports ~f:(fun { Statement.Import.name; _ } ->
                     String.equal (Reference.show name) "*") ->
              from :: collected_imports
          | _ -> collected_imports
      end)
      in
      let visited_modules = Reference.Hash_set.create () in
      let transitive_exports = Reference.Hash_set.create () in
      let worklist = Queue.of_list [qualifier] in
      let rec search_wildcard_imports () =
        match Queue.dequeue worklist with
        | None -> ()
        | Some qualifier ->
            let _ =
              match Hash_set.strict_add visited_modules qualifier with
              | Error _ -> ()
              | Ok () -> (
                match
                  AstEnvironment.Raw.get_wildcard_exports ast_environment qualifier ~dependency
                with
                | None -> ()
                | Some exports -> (
                    List.iter exports ~f:(fun export ->
                        if not (String.equal (Reference.show export) "*") then
                          Hash_set.add transitive_exports export);
                    match AstEnvironment.Raw.get_source ast_environment qualifier ~dependency with
                    | None -> ()
                    | Some source -> Visitor.visit [] source |> Queue.enqueue_all worklist ) )
            in
            search_wildcard_imports ()
      in
      search_wildcard_imports ();
      Hash_set.to_list transitive_exports |> List.sort ~compare:Reference.compare


    let statement state ({ Node.value; _ } as statement) =
      match value with
      | Import { Import.from = Some from; imports }
        when List.exists imports ~f:(fun { Import.name; _ } ->
                 String.equal (Reference.show name) "*") ->
          let expanded_import =
            match get_transitive_exports from ~ast_environment ~dependency:qualifier with
            | [] -> statement
            | exports ->
                List.map exports ~f:(fun name -> { Import.name; alias = None })
                |> (fun expanded -> Import { Import.from = Some from; imports = expanded })
                |> fun value -> { statement with Node.value }
          in
          state, [expanded_import]
      | _ -> state, [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let process_sources ~configuration ~scheduler ~preprocessing_state ~ast_environment qualifiers =
  let process_sources_job =
    let process qualifier =
      match AstEnvironment.Raw.get_source ast_environment qualifier ~dependency:qualifier with
      | None -> ()
      | Some source ->
          let source =
            match preprocessing_state with
            | Some state -> ProjectSpecificPreprocessing.preprocess ~state source
            | None -> source
          in
          let stored =
            expand_wildcard_imports ~ast_environment source
            |> Preprocessing.preprocess_phase1
            |> Plugin.apply_to_ast
          in
          AstEnvironment.add_source ast_environment stored
    in
    List.iter ~f:process
  in
  Scheduler.iter scheduler ~configuration ~f:process_sources_job ~inputs:qualifiers


type parse_sources_result = {
  parsed: Reference.t list;
  syntax_error: SourcePath.t list;
  system_error: SourcePath.t list;
}

let parse_sources ~configuration ~scheduler ~preprocessing_state ~ast_environment source_paths =
  let { RawParseResult.parsed; syntax_error; system_error } =
    parse_raw_sources ~configuration ~scheduler ~ast_environment source_paths
  in
  process_sources ~configuration ~scheduler ~preprocessing_state ~ast_environment parsed;
  SharedMem.invalidate_caches ();
  { parsed; syntax_error; system_error }


let log_parse_errors ~syntax_error ~system_error =
  let syntax_errors = List.length syntax_error in
  let system_errors = List.length system_error in
  let count = syntax_errors + system_errors in
  if count > 0 then (
    let hint =
      if syntax_errors > 0 && not (Log.is_enabled `Parser) then
        Format.asprintf
          " Run `pyre %s` without `--hide-parse-errors` for more details%s."
          ( try Array.nget Sys.argv 1 with
          | _ -> "restart" )
          (if system_errors > 0 then " on the syntax errors" else "")
      else
        ""
    in
    let details =
      let to_string count description =
        Format.sprintf "%d %s%s" count description (if count == 1 then "" else "s")
      in
      if syntax_errors > 0 && system_errors > 0 then
        Format.sprintf
          ": %s, %s"
          (to_string syntax_errors "syntax error")
          (to_string system_errors "system error")
      else if syntax_errors > 0 then
        " due to syntax errors"
      else
        " due to system errors"
    in
    Log.warning "Could not parse %d file%s%s!%s" count (if count > 1 then "s" else "") details hint;
    let trace list =
      List.map list ~f:(fun { SourcePath.relative; _ } -> relative) |> String.concat ~sep:";"
    in
    Statistics.event
      ~flush:true
      ~name:"parse errors"
      ~integers:["syntax errors", syntax_errors; "system errors", system_errors]
      ~normals:
        ["syntax errors trace", trace syntax_error; "system errors trace", trace system_error]
      () )


let parse_all ~scheduler ~configuration module_tracker =
  let timer = Timer.start () in
  Log.info "Parsing %d stubs and sources..." (ModuleTracker.explicit_module_count module_tracker);
  let ast_environment = AstEnvironment.create module_tracker in
  let { parsed; syntax_error; system_error } =
    let preprocessing_state =
      ProjectSpecificPreprocessing.initial (fun qualifier ->
          ModuleTracker.lookup module_tracker qualifier |> Option.is_some)
    in
    ModuleTracker.source_paths module_tracker
    |> parse_sources
         ~configuration
         ~scheduler
         ~preprocessing_state:(Some preprocessing_state)
         ~ast_environment
  in
  log_parse_errors ~syntax_error ~system_error;
  Statistics.performance ~name:"sources parsed" ~timer ();
  List.filter_map parsed ~f:(AstEnvironment.get_source ast_environment), ast_environment


let update ~configuration ~scheduler ~ast_environment module_updates =
  let reparse_source_paths, removed_modules =
    let categorize = function
      | ModuleTracker.IncrementalUpdate.New source_path -> `Fst source_path
      | ModuleTracker.IncrementalUpdate.Delete qualifier -> `Snd qualifier
    in
    List.partition_map module_updates ~f:categorize
  in
  let changed_modules =
    let reparse_modules =
      List.map reparse_source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
    in
    List.append removed_modules reparse_modules
  in
  let update_raw_sources () =
    let { RawParseResult.syntax_error; system_error; _ } =
      parse_raw_sources ~configuration ~scheduler ~ast_environment reparse_source_paths
    in
    log_parse_errors ~syntax_error ~system_error
  in
  let raw_dependencies =
    AstEnvironment.Raw.update_and_compute_dependencies
      ast_environment
      changed_modules
      ~update:update_raw_sources
  in
  let update_processed_sources () =
    process_sources
      ~configuration
      ~scheduler
      ~preprocessing_state:None
      ~ast_environment
      raw_dependencies
  in
  AstEnvironment.update_and_compute_dependencies
    ast_environment
    raw_dependencies
    ~update:update_processed_sources
