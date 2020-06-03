(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open Pyre
open PyreParser

type t = { module_tracker: ModuleTracker.t }

module RawSourceValue = struct
  type t = Source.t

  let prefix = Prefix.make ()

  let description = "Unprocessed source"

  let compare = Source.compare

  let unmarshall value = Marshal.from_string value 0
end

module RawSources =
  DependencyTrackedMemory.DependencyTrackedTableNoCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.DependencyKey)
    (RawSourceValue)

let create module_tracker = { module_tracker }

let wildcard_exports_of ({ Source.source_path = { SourcePath.is_stub; _ }; _ } as source) =
  let open Expression in
  let open UnannotatedGlobal in
  let extract_dunder_all = function
    | {
        Collector.Result.name = "__all__";
        unannotated_global =
          SimpleAssign { value = { Node.value = Expression.(List names | Tuple names); _ }; _ };
      } ->
        let to_identifier = function
          | { Node.value = Expression.String { value = name; _ }; _ } -> Some name
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
        if is_stub then
          List.filter unannotated_globals ~f:filter_unaliased_import
        else
          unannotated_globals
      in
      List.map unannotated_globals ~f:(fun { Collector.Result.name; _ } -> name)
      |> List.filter ~f:(fun name -> not (String.is_prefix name ~prefix:"_"))
      |> List.dedup_and_sort ~compare:Identifier.compare


module Raw = struct
  let add_source _ ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
    RawSources.add qualifier source


  let update_and_compute_dependencies _ ~update ~scheduler ~configuration qualifiers =
    let keys = RawSources.KeySet.of_list qualifiers in
    SharedMemoryKeys.DependencyKey.Transaction.empty ~scheduler ~configuration
    |> RawSources.add_to_transaction ~keys
    |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update


  let get_source _ = RawSources.get

  let remove_sources _ qualifiers = RawSources.KeySet.of_list qualifiers |> RawSources.remove_batch
end

type parse_result =
  | Success of Source.t
  | SyntaxError of string
  | SystemError of string

let parse_source ~configuration ({ SourcePath.relative; qualifier; _ } as source_path) =
  let parse_lines lines =
    let metadata = Source.Metadata.parse ~qualifier lines in
    try
      let statements = Parser.parse ~relative lines in
      Success (Source.create_from_source_path ~metadata ~source_path statements)
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
    | Success ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) ->
        let source = Preprocessing.preprocess_phase0 source in
        Raw.add_source ast_environment source;
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
    ~policy:
      (Scheduler.Policy.fixed_chunk_count
         ~minimum_chunks_per_worker:1
         ~minimum_chunk_size:100
         ~preferred_chunks_per_worker:5
         ())
    ~configuration
    ~initial:RawParseResult.empty
    ~map:(fun _ -> List.fold ~init:RawParseResult.empty ~f:parse_and_categorize)
    ~reduce:RawParseResult.merge
    ~inputs:source_paths
    ()


let expand_wildcard_imports ?dependency ~ast_environment source =
  let open Statement in
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let get_transitive_exports ?dependency ~ast_environment qualifier =
      let module Visitor = Visit.MakeStatementVisitor (struct
        type t = Reference.t list

        let visit_children _ = false

        let statement _ collected_imports { Node.value; _ } =
          match value with
          | Statement.Import { Import.from = Some from; imports }
            when List.exists imports ~f:(fun { Import.name = { Node.value = name; _ }; _ } ->
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
                  match Raw.get_source ast_environment qualifier ?dependency with
                  | None -> ()
                  | Some source ->
                      wildcard_exports_of source |> List.iter ~f:(Hash_set.add transitive_exports);
                      Visitor.visit [] source |> Queue.enqueue_all worklist )
            in
            search_wildcard_imports ()
      in
      search_wildcard_imports ();
      Hash_set.to_list transitive_exports |> List.sort ~compare:Identifier.compare


    let statement state ({ Node.value; _ } as statement) =
      match value with
      | Statement.Import { Import.from = Some from; imports } -> (
          let starred_import =
            List.find imports ~f:(fun { Import.name = { Node.value = name; _ }; _ } ->
                String.equal (Reference.show name) "*")
          in
          match starred_import with
          | Some { Import.name = { Node.location; _ }; _ } ->
              let expanded_import =
                match get_transitive_exports (Node.value from) ~ast_environment ?dependency with
                | [] -> statement
                | exports ->
                    List.map exports ~f:(fun name ->
                        {
                          Import.name = Node.create ~location (Reference.create name);
                          alias = Some (Node.create ~location name);
                        })
                    |> (fun expanded ->
                         Statement.Import { Import.from = Some from; imports = expanded })
                    |> fun value -> { statement with Node.value }
              in
              state, [expanded_import]
          | None -> state, [statement] )
      | _ -> state, [statement]
  end)
  in
  Transform.transform () source |> Transform.source


let get_and_preprocess_source ?dependency ({ module_tracker } as ast_environment) qualifier =
  let module_exists qualifier =
    ModuleTracker.lookup_source_path module_tracker qualifier |> Option.is_some
  in
  (* Preprocessing a module depends on the module itself is implicitly assumed in `update`. No need
     to explicitly record the dependency. *)
  Raw.get_source ast_environment qualifier ?dependency:None
  >>| fun source ->
  let source = ProjectSpecificPreprocessing.preprocess ~module_exists source in
  expand_wildcard_imports ?dependency ~ast_environment source |> Preprocessing.preprocess_phase1


type parse_sources_result = {
  parsed: Reference.t list;
  syntax_error: SourcePath.t list;
  system_error: SourcePath.t list;
}

let parse_sources ~configuration ~scheduler ~ast_environment source_paths =
  let { RawParseResult.parsed; syntax_error; system_error } =
    parse_raw_sources ~configuration ~scheduler ~ast_environment source_paths
  in
  { parsed = List.sort parsed ~compare:Reference.compare; syntax_error; system_error }


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


module UpdateResult = struct
  type t = {
    triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
    invalidated_modules: Reference.t list;
    syntax_error: SourcePath.t list;
    system_error: SourcePath.t list;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let invalidated_modules { invalidated_modules; _ } = invalidated_modules

  let syntax_errors { syntax_error; _ } = syntax_error

  let system_errors { system_error; _ } = system_error

  let create_for_testing () =
    {
      triggered_dependencies = SharedMemoryKeys.DependencyKey.RegisteredSet.empty;
      invalidated_modules = [];
      syntax_error = [];
      system_error = [];
    }
end

type trigger =
  | Update of ModuleTracker.IncrementalUpdate.t list
  | ColdStart

let update
    ~configuration:({ Configuration.Analysis.incremental_style; _ } as configuration)
    ~scheduler
    ({ module_tracker } as ast_environment)
  = function
  | Update module_updates -> (
      let reparse_source_paths, removed_modules, updated_submodules =
        let categorize = function
          | ModuleTracker.IncrementalUpdate.NewExplicit source_path -> `Fst source_path
          | ModuleTracker.IncrementalUpdate.Delete qualifier -> `Snd qualifier
          | ModuleTracker.IncrementalUpdate.NewImplicit qualifier -> `Trd qualifier
        in
        List.partition3_map module_updates ~f:categorize
      in
      match incremental_style with
      | Configuration.Analysis.Shallow ->
          let directly_changed_modules =
            List.map reparse_source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
          in
          Raw.remove_sources ast_environment (List.append removed_modules directly_changed_modules);
          let { parsed; syntax_error; system_error } =
            parse_sources ~configuration ~scheduler ~ast_environment reparse_source_paths
          in
          {
            UpdateResult.triggered_dependencies = SharedMemoryKeys.DependencyKey.RegisteredSet.empty;
            invalidated_modules = List.append updated_submodules parsed;
            syntax_error;
            system_error;
          }
      | _ ->
          let changed_modules =
            let reparse_modules =
              List.map reparse_source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
            in
            List.concat [removed_modules; updated_submodules; reparse_modules]
          in
          let update_raw_sources () =
            let ({ RawParseResult.syntax_error; system_error; _ } as result) =
              parse_raw_sources ~configuration ~scheduler ~ast_environment reparse_source_paths
            in
            log_parse_errors ~syntax_error ~system_error;
            result
          in
          let { RawParseResult.syntax_error; system_error; _ }, triggered_dependencies =
            Profiling.track_duration_and_shared_memory
              "Parse Raw Sources"
              ~tags:["phase_name", "Parsing"]
              ~f:(fun _ ->
                Raw.update_and_compute_dependencies
                  ast_environment
                  changed_modules
                  ~update:update_raw_sources
                  ~scheduler
                  ~configuration)
          in
          let invalidated_modules =
            let fold_key registered sofar =
              match SharedMemoryKeys.DependencyKey.get_key registered with
              | SharedMemoryKeys.WildcardImport qualifier -> RawSources.KeySet.add qualifier sofar
              | _ -> sofar
            in
            SharedMemoryKeys.DependencyKey.RegisteredSet.fold
              fold_key
              triggered_dependencies
              (RawSources.KeySet.of_list changed_modules)
            |> RawSources.KeySet.elements
          in
          { UpdateResult.triggered_dependencies; invalidated_modules; syntax_error; system_error } )
  | ColdStart ->
      let timer = Timer.start () in
      Log.info
        "Parsing %d stubs and sources..."
        (ModuleTracker.explicit_module_count module_tracker);
      let ast_environment = create module_tracker in
      let { parsed; syntax_error; system_error } =
        ModuleTracker.source_paths module_tracker
        |> parse_sources ~configuration ~scheduler ~ast_environment
      in
      log_parse_errors ~syntax_error ~system_error;
      Statistics.performance
        ~name:"sources parsed"
        ~phase_name:"Parsing and preprocessing"
        ~timer
        ();
      {
        UpdateResult.invalidated_modules = parsed;
        triggered_dependencies = SharedMemoryKeys.DependencyKey.RegisteredSet.empty;
        syntax_error;
        system_error;
      }


let get_source_path { module_tracker } = ModuleTracker.lookup_source_path module_tracker

(* Both `load` and `store` are no-ops here since `Sources` and `WildcardExports` are in shared
   memory, and `Memory.load_shared_memory`/`Memory.save_shared_memory` will take care of the
   (de-)serialization for us. *)
let store _ = ()

let load = create

let shared_memory_hash_to_key_map qualifiers = RawSources.compute_hashes_to_keys ~keys:qualifiers

let serialize_decoded decoded =
  match decoded with
  | RawSources.Decoded (key, value) ->
      Some (RawSourceValue.description, Reference.show key, Option.map value ~f:Source.show)
  | _ -> None


let decoded_equal first second =
  match first, second with
  | RawSources.Decoded (_, first), RawSources.Decoded (_, second) ->
      Some (Option.equal Source.equal first second)
  | _ -> None


module ReadOnly = struct
  type t = {
    get_processed_source: track_dependency:bool -> Reference.t -> Source.t option;
    get_raw_source: Reference.t -> Source.t option;
    get_source_path: Reference.t -> SourcePath.t option;
    is_module: Reference.t -> bool;
    all_explicit_modules: unit -> Reference.t list;
    is_module_tracked: Reference.t -> bool;
  }

  let create
      ?(get_processed_source = fun ~track_dependency:_ _ -> None)
      ?(get_raw_source = fun _ -> None)
      ?(get_source_path = fun _ -> None)
      ?(is_module = fun _ -> false)
      ?(all_explicit_modules = fun _ -> [])
      ?(is_module_tracked = fun _ -> false)
      ()
    =
    {
      get_processed_source;
      get_raw_source;
      get_source_path;
      is_module;
      all_explicit_modules;
      is_module_tracked;
    }


  let get_processed_source { get_processed_source; _ } ?(track_dependency = false) =
    get_processed_source ~track_dependency


  let get_raw_source { get_raw_source; _ } = get_raw_source

  let get_source_path { get_source_path; _ } = get_source_path

  let get_relative read_only qualifier =
    let open Option in
    get_source_path read_only qualifier >>| fun { SourcePath.relative; _ } -> relative


  let get_real_path ~configuration read_only qualifier =
    get_source_path read_only qualifier >>| SourcePath.full_path ~configuration


  let get_real_path_relative
      ~configuration:({ Configuration.Analysis.local_root; _ } as configuration)
      read_only
      qualifier
    =
    (* SourcePath.relative refers to the renamed path when search paths are provided with a root and
       subdirectory. Instead, find the real filesystem relative path for the qualifier. *)
    get_real_path ~configuration read_only qualifier
    >>= fun path -> PyrePath.get_relative_to_root ~root:local_root ~path


  let is_module { is_module; _ } = is_module

  let is_module_tracked { is_module_tracked; _ } = is_module_tracked

  let all_explicit_modules { all_explicit_modules; _ } = all_explicit_modules ()
end

let read_only ({ module_tracker } as environment) =
  let get_processed_source ~track_dependency qualifier =
    let dependency =
      if track_dependency then
        Some
          (SharedMemoryKeys.DependencyKey.Registry.register
             (SharedMemoryKeys.WildcardImport qualifier))
      else
        None
    in
    get_and_preprocess_source ?dependency environment qualifier
  in
  let is_module_tracked qualifier = ModuleTracker.is_module_tracked module_tracker qualifier in
  {
    ReadOnly.get_processed_source;
    get_raw_source = RawSources.get;
    get_source_path = get_source_path environment;
    is_module = ModuleTracker.is_module_tracked module_tracker;
    all_explicit_modules = (fun () -> ModuleTracker.tracked_explicit_modules module_tracker);
    is_module_tracked;
  }
