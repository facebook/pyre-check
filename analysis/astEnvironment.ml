(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open Pyre
open PyreParser

type dependency = TypeCheckSource of Reference.t [@@deriving show, compare, sexp]

(* We cache results of `from_empty_stub` here since module definition lookup requires shared memory
   lookup, which can be expensive *)
module FromEmptyStubCache = struct
  let cache = Reference.Table.create ()

  let clear () = Reference.Table.clear cache
end

module DependencyKey = Memory.DependencyKey.Make (struct
  type nonrec t = dependency

  let to_string dependency = sexp_of_dependency dependency |> Sexp.to_string_mach

  let compare = compare_dependency

  type out = dependency

  let from_string string = Sexp.of_string string |> dependency_of_sexp
end)

type t = { module_tracker: ModuleTracker.t }

module RawSourceValue = struct
  type t = Source.t

  let prefix = Prefix.make ()

  let description = "Unprocessed source"

  let compare left right = Int.compare (Source.hash left) (Source.hash right)

  let unmarshall value = Marshal.from_string value 0
end

module RawSources =
  Memory.DependencyTrackedTableWithCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.ReferenceDependencyKey)
    (RawSourceValue)

module RawWildcardExportsValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Unprocessed wildcard exports"

  let compare = List.compare Reference.compare

  let unmarshall value = Marshal.from_string value 0
end

module RawWildcardExports =
  Memory.DependencyTrackedTableWithCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.ReferenceDependencyKey)
    (RawWildcardExportsValue)

module SourceValue = struct
  type t = Source.t

  let prefix = Prefix.make ()

  let description = "AST"

  let compare left right = Int.compare (Source.hash left) (Source.hash right)

  let unmarshall value = Marshal.from_string value 0
end

module Sources =
  Memory.DependencyTrackedTableNoCache (SharedMemoryKeys.ReferenceKey) (DependencyKey)
    (SourceValue)

module WildcardExportsValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Wildcard exports"

  let compare = List.compare Reference.compare

  let unmarshall value = Marshal.from_string value 0
end

module WildcardExports =
  Memory.DependencyTrackedTableWithCache (SharedMemoryKeys.ReferenceKey) (DependencyKey)
    (WildcardExportsValue)

module ModuleMetadataValue = struct
  type t = Module.t

  let prefix = Prefix.make ()

  let description = "Module"

  let unmarshall value = Marshal.from_string value 0

  let compare = Module.compare
end

module ModuleMetadata =
  Memory.DependencyTrackedTableWithCache (SharedMemoryKeys.ReferenceKey) (DependencyKey)
    (ModuleMetadataValue)

let create module_tracker = { module_tracker }

module Raw = struct
  let add_source _ ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
    RawSources.add qualifier source;
    RawWildcardExports.write_through qualifier (Source.wildcard_exports_of source)


  let update_and_compute_dependencies _ ~update qualifiers =
    let keys = RawSources.KeySet.of_list qualifiers in
    let update_result, dependency_set =
      SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
      |> RawSources.add_to_transaction ~keys
      |> RawWildcardExports.add_to_transaction ~keys
      |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute ~update
    in
    ( update_result,
      List.fold qualifiers ~init:dependency_set ~f:(fun sofar qualifier ->
          SharedMemoryKeys.ReferenceDependencyKey.KeySet.add qualifier sofar)
      |> SharedMemoryKeys.ReferenceDependencyKey.KeySet.elements )


  let get_source _ = RawSources.get

  let get_wildcard_exports _ = RawWildcardExports.get
end

let add_source _ ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
  Sources.add qualifier source;
  WildcardExports.write_through qualifier (Source.wildcard_exports_of source);
  ModuleMetadata.add qualifier (Module.create source)


let remove_sources _ qualifiers =
  let keys = Sources.KeySet.of_list qualifiers in
  RawSources.remove_batch keys;
  Sources.remove_batch keys;
  RawWildcardExports.remove_batch keys;
  WildcardExports.remove_batch keys;
  ModuleMetadata.remove_batch keys


let update_and_compute_dependencies _ ~update qualifiers =
  let keys = Sources.KeySet.of_list qualifiers in
  let (), dependency_set =
    DependencyKey.Transaction.empty
    |> Sources.add_to_transaction ~keys
    |> WildcardExports.add_to_transaction ~keys
    |> ModuleMetadata.add_to_transaction ~keys
    |> DependencyKey.Transaction.execute ~update
  in
  dependency_set


let get_source _ = Sources.get

type parse_result =
  | Success of Source.t
  | SyntaxError of string
  | SystemError of string

let parse_source ~configuration ({ SourcePath.relative; qualifier; _ } as source_path) =
  let parse_lines lines =
    let metadata = Source.Metadata.parse ~qualifier lines in
    try
      let open Statement in
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
    ~configuration
    ~initial:RawParseResult.empty
    ~map:(fun _ -> List.fold ~init:RawParseResult.empty ~f:parse_and_categorize)
    ~reduce:RawParseResult.merge
    ~inputs:source_paths
    ()


let expand_wildcard_imports
    ~ast_environment
    ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
  =
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
          | Statement.Import { Import.from = Some from; imports }
            when List.exists imports ~f:(fun { Import.name; _ } ->
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
                match Raw.get_wildcard_exports ast_environment qualifier ~dependency with
                | None -> ()
                | Some exports -> (
                    List.iter exports ~f:(fun export ->
                        if not (String.equal (Reference.show export) "*") then
                          Hash_set.add transitive_exports export);
                    match Raw.get_source ast_environment qualifier ~dependency with
                    | None -> ()
                    | Some source -> Visitor.visit [] source |> Queue.enqueue_all worklist ) )
            in
            search_wildcard_imports ()
      in
      search_wildcard_imports ();
      Hash_set.to_list transitive_exports |> List.sort ~compare:Reference.compare


    let statement state ({ Node.value; _ } as statement) =
      match value with
      | Statement.Import { Import.from = Some from; imports }
        when List.exists imports ~f:(fun { Import.name; _ } ->
                 String.equal (Reference.show name) "*") ->
          let expanded_import =
            match get_transitive_exports from ~ast_environment ~dependency:qualifier with
            | [] -> statement
            | exports ->
                List.map exports ~f:(fun name -> { Import.name; alias = None })
                |> (fun expanded ->
                     Statement.Import { Import.from = Some from; imports = expanded })
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
      match Raw.get_source ast_environment qualifier ~dependency:qualifier with
      | None -> ()
      | Some source ->
          let source =
            match preprocessing_state with
            | Some state -> ProjectSpecificPreprocessing.preprocess ~state source
            | None -> source
          in
          let stored =
            expand_wildcard_imports ~ast_environment source |> Preprocessing.preprocess_phase1
          in
          add_source ast_environment stored
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
    triggered_dependencies: DependencyKey.KeySet.t;
    reparsed: Reference.t list;
    syntax_error: SourcePath.t list;
    system_error: SourcePath.t list;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let reparsed { reparsed; _ } = reparsed

  let syntax_errors { syntax_error; _ } = syntax_error

  let system_errors { system_error; _ } = system_error

  let create_for_testing () =
    {
      triggered_dependencies = DependencyKey.KeySet.empty;
      reparsed = [];
      syntax_error = [];
      system_error = [];
    }
end

type trigger =
  | Update of ModuleTracker.IncrementalUpdate.t list
  | ColdStart

let update
    ~configuration:({ Configuration.Analysis.ignore_dependencies; _ } as configuration)
    ~scheduler
    ({ module_tracker } as ast_environment)
  = function
  | Update module_updates ->
      let reparse_source_paths, removed_modules, updated_submodules =
        let categorize = function
          | ModuleTracker.IncrementalUpdate.NewExplicit source_path -> `Fst source_path
          | ModuleTracker.IncrementalUpdate.Delete qualifier -> `Snd qualifier
          | ModuleTracker.IncrementalUpdate.NewImplicit qualifier -> `Trd qualifier
        in
        List.partition3_map module_updates ~f:categorize
      in
      if ignore_dependencies then (
        let directly_changed_modules =
          List.map reparse_source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
        in
        remove_sources ast_environment (List.append removed_modules directly_changed_modules);
        let { parsed; syntax_error; system_error } =
          parse_sources
            ~configuration
            ~scheduler
            ~preprocessing_state:None
            ~ast_environment
            reparse_source_paths
        in
        {
          UpdateResult.triggered_dependencies = DependencyKey.KeySet.empty;
          reparsed = List.append updated_submodules parsed;
          syntax_error;
          system_error;
        } )
      else
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
        let { RawParseResult.syntax_error; system_error; _ }, raw_dependencies =
          Raw.update_and_compute_dependencies
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
        let triggered_dependencies =
          update_and_compute_dependencies
            ast_environment
            raw_dependencies
            ~update:update_processed_sources
        in
        {
          UpdateResult.triggered_dependencies;
          reparsed = raw_dependencies;
          syntax_error;
          system_error;
        }
  | ColdStart ->
      let timer = Timer.start () in
      Log.info
        "Parsing %d stubs and sources..."
        (ModuleTracker.explicit_module_count module_tracker);
      let ast_environment = create module_tracker in
      let { parsed; syntax_error; system_error } =
        let preprocessing_state =
          ProjectSpecificPreprocessing.initial (fun qualifier ->
              ModuleTracker.lookup_source_path module_tracker qualifier |> Option.is_some)
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
      {
        UpdateResult.reparsed = parsed;
        triggered_dependencies = DependencyKey.KeySet.empty;
        syntax_error;
        system_error;
      }


let get_wildcard_exports _ = WildcardExports.get

let get_source_path { module_tracker } = ModuleTracker.lookup_source_path module_tracker

(* Both `load` and `store` are no-ops here since `Sources` and `WildcardExports` are in shared
   memory, and `Memory.load_shared_memory`/`Memory.save_shared_memory` will take care of the
   (de-)serialization for us. *)
let store _ = ()

let load = create

let shared_memory_hash_to_key_map qualifiers =
  let extend_map map ~new_map =
    Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
  in
  RawSources.compute_hashes_to_keys ~keys:qualifiers
  |> extend_map ~new_map:(RawWildcardExports.compute_hashes_to_keys ~keys:qualifiers)
  |> extend_map ~new_map:(Sources.compute_hashes_to_keys ~keys:qualifiers)
  |> extend_map ~new_map:(WildcardExports.compute_hashes_to_keys ~keys:qualifiers)


let serialize_decoded decoded =
  match decoded with
  | RawSources.Decoded (key, value) ->
      Some (SourceValue.description, Reference.show key, Option.map value ~f:Source.show)
  | RawWildcardExports.Decoded (key, value) ->
      Some
        ( WildcardExportsValue.description,
          Reference.show key,
          Option.map value ~f:(List.to_string ~f:Reference.show) )
  | Sources.Decoded (key, value) ->
      Some (SourceValue.description, Reference.show key, Option.map value ~f:Source.show)
  | WildcardExports.Decoded (key, value) ->
      Some
        ( WildcardExportsValue.description,
          Reference.show key,
          Option.map value ~f:(List.to_string ~f:Reference.show) )
  | _ -> None


let decoded_equal first second =
  match first, second with
  | RawSources.Decoded (_, first), RawSources.Decoded (_, second) ->
      Some (Option.equal Source.equal first second)
  | RawWildcardExports.Decoded (_, first), RawWildcardExports.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | Sources.Decoded (_, first), Sources.Decoded (_, second) ->
      Some (Option.equal Source.equal first second)
  | WildcardExports.Decoded (_, first), WildcardExports.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | _ -> None


module ReadOnly = struct
  type t = {
    get_source: Reference.t -> Source.t option;
    get_wildcard_exports: Reference.t -> Reference.t list option;
    get_source_path: Reference.t -> SourcePath.t option;
    is_module: Reference.t -> bool;
    all_explicit_modules: unit -> Reference.t list;
    get_module_metadata: ?dependency:dependency -> Reference.t -> Module.t option;
  }

  let create
      ?(get_source = fun _ -> None)
      ?(get_wildcard_exports = fun _ -> None)
      ?(get_source_path = fun _ -> None)
      ?(is_module = fun _ -> false)
      ?(all_explicit_modules = fun _ -> [])
      ?(get_module_metadata = fun ?dependency:_ _ -> None)
      ()
    =
    {
      get_source;
      get_wildcard_exports;
      get_source_path;
      is_module;
      all_explicit_modules;
      get_module_metadata;
    }


  let get_source { get_source; _ } = get_source

  let get_source_path { get_source_path; _ } = get_source_path

  let get_wildcard_exports { get_wildcard_exports; _ } = get_wildcard_exports

  let get_relative read_only qualifier =
    let open Option in
    get_source_path read_only qualifier >>| fun { SourcePath.relative; _ } -> relative


  let get_real_path_relative
      ~configuration:({ Configuration.Analysis.local_root; _ } as configuration)
      read_only
      qualifier
    =
    (* SourcePath.relative refers to the renamed path when search paths are provided with a root
       and subdirectory. Instead, find the real filesystem relative path for the qualifier. *)
    let open Option in
    get_source_path read_only qualifier
    >>| SourcePath.full_path ~configuration
    >>= fun path -> PyrePath.get_relative_to_root ~root:local_root ~path


  let is_module { is_module; _ } = is_module

  let all_explicit_modules { all_explicit_modules; _ } = all_explicit_modules ()

  let get_module_metadata { get_module_metadata; _ } = get_module_metadata

  let from_empty_stub { get_module_metadata; _ } ?dependency reference =
    let rec is_empty_stub ~lead ~tail =
      match tail with
      | head :: tail -> (
          let lead = lead @ [head] in
          let reference = Reference.create_from_list lead in
          match get_module_metadata ?dependency reference with
          | Some definition when Module.empty_stub definition -> true
          | Some _ -> is_empty_stub ~lead ~tail
          | _ -> false )
      | _ -> false
    in
    let compute () = is_empty_stub ~lead:[] ~tail:(Reference.as_list reference) in
    if Option.is_some dependency then
      compute ()
    else
      Hashtbl.find_or_add FromEmptyStubCache.cache reference ~default:(fun _ -> compute ())
end

let read_only ({ module_tracker } as environment) =
  let get_module_metadata ?dependency qualifier =
    match Reference.as_list qualifier with
    | ["future"; "builtins"]
    | ["builtins"] ->
        Some (Module.create_implicit ~empty_stub:true ())
    | _ -> (
      match ModuleMetadata.get ?dependency qualifier with
      | Some _ as result -> result
      | None -> (
        match ModuleTracker.is_module_tracked module_tracker qualifier with
        | true -> Some (Module.create_implicit ())
        | false -> None ) )
  in
  {
    ReadOnly.get_source = get_source environment;
    get_wildcard_exports = get_wildcard_exports environment;
    get_source_path = get_source_path environment;
    is_module = ModuleTracker.is_module_tracked module_tracker;
    all_explicit_modules = (fun () -> ModuleTracker.tracked_explicit_modules module_tracker);
    get_module_metadata;
  }
