(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open Pyre
open PyreParser

module ParserError = struct
  type t = {
    source_path: SourcePath.t;
    location: Location.t;
    is_suppressed: bool;
    message: string;
  }
  [@@deriving sexp, compare, hash]
end

module RawSourceValue = struct
  type t = (Source.t, ParserError.t) Result.t

  let prefix = Prefix.make ()

  let description = "Unprocessed source"

  let compare = Result.compare Source.compare ParserError.compare
end

module RawSources = struct
  include
    DependencyTrackedMemory.DependencyTrackedTableNoCache
      (SharedMemoryKeys.ReferenceKey)
      (SharedMemoryKeys.DependencyKey)
      (RawSourceValue)

  let add_parsed_source _ ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
    add qualifier (Result.Ok source)


  let add_unparsed_source _ ({ ParserError.source_path = { SourcePath.qualifier; _ }; _ } as error) =
    add qualifier (Result.Error error)


  let update_and_compute_dependencies _ ~update ~scheduler qualifiers =
    let keys = KeySet.of_list qualifiers in
    SharedMemoryKeys.DependencyKey.Transaction.empty ~scheduler
    |> add_to_transaction ~keys
    |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update


  let remove_sources _ qualifiers = KeySet.of_list qualifiers |> remove_batch
end

type t = {
  module_tracker: ModuleTracker.t;
  additional_preprocessing: (Source.t -> Source.t) option;
}

let create ?additional_preprocessing module_tracker = { module_tracker; additional_preprocessing }

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
        if is_stub then
          List.filter unannotated_globals ~f:filter_unaliased_import
        else
          unannotated_globals
      in
      List.map unannotated_globals ~f:(fun { Collector.Result.name; _ } -> name)
      |> List.filter ~f:(fun name -> not (String.is_prefix name ~prefix:"_"))
      |> List.dedup_and_sort ~compare:Identifier.compare


type parse_result =
  | Success of Source.t
  | Error of {
      location: Location.t;
      message: string;
      is_suppressed: bool;
    }

let create_source ~typecheck_flags ~source_path statements =
  Source.create_from_source_path
    ~collect_format_strings_with_ignores:Visit.collect_format_strings_with_ignores
    ~typecheck_flags
    ~source_path
    statements


let parse_source
    ~configuration:({ Configuration.Analysis.enable_type_comments; _ } as configuration)
    ~context
    ~module_tracker
    ({ SourcePath.qualifier; _ } as source_path)
  =
  let parse raw_code =
    let typecheck_flags = Source.TypecheckFlags.parse ~qualifier (String.split raw_code ~on:'\n') in
    match
      PyreNewParser.parse_module ~enable_type_comment:enable_type_comments ~context raw_code
    with
    | Ok statements -> Success (create_source ~typecheck_flags ~source_path statements)
    | Error { PyreNewParser.Error.line; column; end_line; end_column; message } ->
        let is_suppressed =
          let { Source.TypecheckFlags.local_mode; ignore_codes; _ } = typecheck_flags in
          match Source.mode ~configuration ~local_mode with
          | Source.Declare -> true
          | _ ->
              (* NOTE: The number needs to be updated when the error code changes. *)
              List.exists ignore_codes ~f:(Int.equal 404)
        in
        let location =
          (* CPython set line/column number to -1 in some exceptional cases. *)
          let replace_invalid_position number = if number <= 0 then 1 else number in
          let start =
            {
              Location.line = replace_invalid_position line;
              column = replace_invalid_position column;
            }
          in
          let stop =
            (* Work around CPython bug where the end location sometimes precedes start location. *)
            if [%compare: int * int] (line, column) (end_line, end_column) > 0 then
              start
            else
              {
                Location.line = replace_invalid_position end_line;
                column = replace_invalid_position end_column;
              }
          in
          { Location.start; stop }
        in
        Error { location; message; is_suppressed }
  in
  match ModuleTracker.ReadOnly.get_raw_code module_tracker source_path with
  | Ok raw_code -> parse raw_code
  | Error message ->
      Error
        {
          location =
            {
              Location.start = { Location.line = 1; column = 1 };
              stop = { Location.line = 1; column = 1 };
            };
          message;
          is_suppressed = false;
        }


let load_raw_source ~ast_environment:({ module_tracker; _ } as ast_environment) source_path =
  let module_tracker = ModuleTracker.read_only module_tracker in
  let configuration = ModuleTracker.ReadOnly.configuration module_tracker in
  let do_parse context =
    match parse_source ~configuration ~context ~module_tracker source_path with
    | Success source ->
        let source =
          let {
            Configuration.Analysis.python_major_version;
            python_minor_version;
            python_micro_version;
            _;
          }
            =
            configuration
          in
          Preprocessing.replace_version_specific_code
            ~major_version:python_major_version
            ~minor_version:python_minor_version
            ~micro_version:python_micro_version
            source
          |> Preprocessing.preprocess_phase0
        in
        RawSources.add_parsed_source ast_environment source
    | Error { location; message; is_suppressed } ->
        RawSources.add_unparsed_source
          ast_environment
          { ParserError.source_path; location; message; is_suppressed }
  in
  PyreNewParser.with_context do_parse


let load_raw_sources ~scheduler ~ast_environment source_paths =
  Scheduler.iter
    scheduler
    ~policy:
      (Scheduler.Policy.fixed_chunk_count
         ~minimum_chunks_per_worker:1
         ~minimum_chunk_size:100
         ~preferred_chunks_per_worker:5
         ())
    ~f:(List.iter ~f:(load_raw_source ~ast_environment))
    ~inputs:source_paths


module LazyRawSources = struct
  let load ~ast_environment:({ module_tracker; _ } as ast_environment) qualifier =
    let module_tracker = ModuleTracker.read_only module_tracker in
    match ModuleTracker.ReadOnly.lookup_source_path module_tracker qualifier with
    | Some source_path ->
        load_raw_source ~ast_environment source_path;
        true
    | None -> false


  let get ~ast_environment ?dependency qualifier =
    match RawSources.get ?dependency qualifier with
    | Some _ as source -> source
    | None ->
        if load ~ast_environment qualifier then
          RawSources.get ?dependency qualifier
        else
          None
end

let expand_wildcard_imports ?dependency ~ast_environment source =
  let open Statement in
  let module Transform = Transform.MakeStatementTransformer (struct
    include Transform.Identity

    type t = unit

    let get_transitive_exports ?dependency ~ast_environment:_ qualifier =
      let module Visitor = Visit.MakeStatementVisitor (struct
        type t = Reference.t list

        let visit_children _ = false

        let statement _ collected_imports { Node.value; _ } =
          match value with
          | Statement.Import { Import.from = Some from; imports }
            when List.exists imports ~f:(fun { Node.value = { Import.name; _ }; _ } ->
                     String.equal (Reference.show name) "*") ->
              from :: collected_imports
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
                  match LazyRawSources.get ~ast_environment qualifier ?dependency with
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
                match get_transitive_exports from ~ast_environment ?dependency with
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


let get_and_preprocess_source
    ?dependency
    ({ additional_preprocessing; _ } as ast_environment)
    qualifier
  =
  let preprocessing =
    match additional_preprocessing with
    | Some additional_preprocessing ->
        fun source -> Preprocessing.preprocess_phase1 source |> additional_preprocessing
    | None -> Preprocessing.preprocess_phase1
  in
  (* Preprocessing a module depends on the module itself is implicitly assumed in `update`. No need
     to explicitly record the dependency. *)
  LazyRawSources.get ~ast_environment qualifier ?dependency:None
  >>| function
  | Result.Ok source ->
      expand_wildcard_imports ?dependency ~ast_environment source
      |> preprocessing
      |> InlineDecorator.inline_decorators ~get_source:(fun qualifier ->
             LazyRawSources.get ~ast_environment qualifier >>= Result.ok)
  | Result.Error
      { ParserError.source_path = { SourcePath.qualifier; relative; _ } as source_path; _ } ->
      (* Files that have parser errors fall back into getattr-any. *)
      let fallback_source = ["import typing"; "def __getattr__(name: str) -> typing.Any: ..."] in
      let typecheck_flags = Source.TypecheckFlags.parse ~qualifier fallback_source in
      let statements = Parser.parse_exn ~relative fallback_source in
      create_source ~typecheck_flags ~source_path statements |> preprocessing


module UpdateResult = struct
  type t = {
    triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
    invalidated_modules: Reference.t list;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let invalidated_modules { invalidated_modules; _ } = invalidated_modules

  let create_for_testing () =
    {
      triggered_dependencies = SharedMemoryKeys.DependencyKey.RegisteredSet.empty;
      invalidated_modules = [];
    }
end

type trigger =
  | Update of ModuleTracker.IncrementalUpdate.t list
  | ColdStart

let update ~scheduler ({ module_tracker = upstream_tracker; _ } as ast_environment) trigger =
  let module_tracker = ModuleTracker.read_only upstream_tracker in
  let { Configuration.Analysis.incremental_style; _ } =
    ModuleTracker.ReadOnly.configuration module_tracker
  in
  match trigger with
  | ColdStart ->
      (* The one external module we want to try to force load is `builtins.pyi`. This improves
         performance later and also will prevent problems with lazy qualifier lookups if, in the
         future, there are ever nested classes in builtins. *)
      let _ = LazyRawSources.load ~ast_environment Reference.empty in
      let invalidated_modules =
        Reference.empty
        ::
        (ModuleTracker.ReadOnly.source_paths module_tracker
        |> List.filter ~f:SourcePath.is_in_project
        |> List.map ~f:SourcePath.qualifier)
      in
      {
        UpdateResult.invalidated_modules;
        triggered_dependencies = SharedMemoryKeys.DependencyKey.RegisteredSet.empty;
      }
  | Update module_updates -> (
      let changed_source_paths, removed_modules, new_implicits =
        let categorize = function
          | ModuleTracker.IncrementalUpdate.NewExplicit source_path -> `Fst source_path
          | ModuleTracker.IncrementalUpdate.Delete qualifier -> `Snd qualifier
          | ModuleTracker.IncrementalUpdate.NewImplicit qualifier -> `Trd qualifier
        in
        List.partition3_map module_updates ~f:categorize
      in
      (* We only want to eagerly reparse sources that have been cached. We have to also invalidate
         sources that are now deleted or changed from explicit to implicit. *)
      let reparse_source_paths =
        List.filter changed_source_paths ~f:(fun { SourcePath.qualifier; _ } ->
            RawSources.mem qualifier)
      in
      let reparse_modules =
        reparse_source_paths |> List.map ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
      in
      let modules_with_invalidated_raw_source =
        List.concat [removed_modules; new_implicits; reparse_modules]
      in
      (* Because type checking relies on AstEnvironment.UpdateResult.invalidated_modules to
         determine which files require re-type-checking, we have to include all new non-external
         modules, even though they don't really require us to update data in the push phase, or else
         they'll never be checked. *)
      let reparse_modules_union_in_project_modules =
        let fold qualifiers { SourcePath.qualifier; _ } = Reference.Set.add qualifiers qualifier in
        List.filter changed_source_paths ~f:SourcePath.is_in_project
        |> List.fold ~init:(Reference.Set.of_list reparse_modules) ~f:fold
        |> Reference.Set.to_list
      in
      let invalidated_modules_before_preprocessing =
        List.concat [removed_modules; new_implicits; reparse_modules_union_in_project_modules]
      in
      match incremental_style with
      | Configuration.Analysis.Shallow ->
          RawSources.remove_sources ast_environment modules_with_invalidated_raw_source;
          load_raw_sources ~scheduler ~ast_environment reparse_source_paths;
          {
            UpdateResult.triggered_dependencies = SharedMemoryKeys.DependencyKey.RegisteredSet.empty;
            invalidated_modules = invalidated_modules_before_preprocessing;
          }
      | Configuration.Analysis.FineGrained ->
          let update_raw_sources () =
            load_raw_sources ~scheduler ~ast_environment reparse_source_paths
          in
          let _, triggered_dependencies =
            Profiling.track_duration_and_shared_memory
              "Parse Raw Sources"
              ~tags:["phase_name", "Parsing"]
              ~f:(fun _ ->
                RawSources.update_and_compute_dependencies
                  ast_environment
                  modules_with_invalidated_raw_source
                  ~update:update_raw_sources
                  ~scheduler)
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
              (RawSources.KeySet.of_list invalidated_modules_before_preprocessing)
            |> RawSources.KeySet.elements
          in
          { UpdateResult.triggered_dependencies; invalidated_modules })


(* Both `load` and `store` are no-ops here since `Sources` and `WildcardExports` are in shared
   memory, and `Memory.load_shared_memory`/`Memory.save_shared_memory` will take care of the
   (de-)serialization for us. *)
let store _ = ()

let load = create ?additional_preprocessing:None

module ReadOnly = struct
  type t = {
    module_tracker: ModuleTracker.ReadOnly.t;
    get_processed_source: track_dependency:bool -> Reference.t -> Source.t option;
    get_raw_source: Reference.t -> (Source.t, ParserError.t) Result.t option;
  }

  let create
      ~module_tracker
      ?(get_processed_source = fun ~track_dependency:_ _ -> None)
      ?(get_raw_source = fun _ -> None)
      ()
    =
    { module_tracker; get_processed_source; get_raw_source }


  let configuration { module_tracker; _ } = ModuleTracker.ReadOnly.configuration module_tracker

  let get_source_path { module_tracker; _ } =
    ModuleTracker.ReadOnly.lookup_source_path module_tracker


  let is_module_tracked { module_tracker; _ } =
    ModuleTracker.ReadOnly.is_module_tracked module_tracker


  let all_explicit_modules { module_tracker; _ } =
    ModuleTracker.ReadOnly.tracked_explicit_modules module_tracker


  let get_processed_source { get_processed_source; _ } ?(track_dependency = false) =
    get_processed_source ~track_dependency


  let get_raw_source { get_raw_source; _ } = get_raw_source

  let get_relative read_only qualifier =
    let open Option in
    get_source_path read_only qualifier >>| fun { SourcePath.relative; _ } -> relative


  let get_real_path read_only qualifier =
    let configuration = configuration read_only in
    get_source_path read_only qualifier >>| SourcePath.full_path ~configuration


  let get_real_path_relative read_only qualifier =
    (* SourcePath.relative refers to the renamed path when search paths are provided with a root and
       subdirectory. Instead, find the real filesystem relative path for the qualifier. *)
    let { Configuration.Analysis.local_root; _ } = configuration read_only in
    get_real_path read_only qualifier
    >>= fun path -> PyrePath.get_relative_to_root ~root:local_root ~path
end

let remove_sources = RawSources.remove_sources

let read_only ({ module_tracker; _ } as ast_environment) =
  let get_processed_source ~track_dependency qualifier =
    let dependency =
      if track_dependency then
        Some
          (SharedMemoryKeys.DependencyKey.Registry.register
             (SharedMemoryKeys.WildcardImport qualifier))
      else
        None
    in
    get_and_preprocess_source ?dependency ast_environment qualifier
  in
  {
    module_tracker = ModuleTracker.read_only module_tracker;
    ReadOnly.get_processed_source;
    get_raw_source = LazyRawSources.get ~ast_environment;
  }


let module_tracker { module_tracker; _ } = module_tracker

let configuration { module_tracker; _ } = ModuleTracker.configuration module_tracker

let with_additional_preprocessing ~additional_preprocessing environment =
  { environment with additional_preprocessing }
